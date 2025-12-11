# Load packages
library(dplyr)
library(stringr)
library(rotl)
library(ape)

# Establish filepaths
input_path <- file.path("data-raw", "data-out-ak")

# Read data
load(file.path(input_path, "mnnpc_accepted_taxa.rds"))

# Prepare taxa for which to retrieve phylogenetic tree data
strings_to_remove <- paste(x = c(" ecological group",
                                 " 1",
                                 " 2",
                                 " 3",
                                 " 4",
                                 " genus",
                                 " and genus",
                                 " and genus canopy",
                                 " and genus subcanopy",
                                 " and genus sub-canopy",
                                 " and genus understory",
                                 " canopy",
                                 " subcanopy",
                                 " sub-canopy",
                                 " understory"), 
                           collapse = "|", sep = "")

# Create initial lookup between MNNPC taxon names and names to submit via rotl
mnnpc_accepted_phylo_taxa_lookup <- mnnpc_accepted_taxa |>
  dplyr::mutate("search_name" = stringr::str_remove_all(string = taxon_name,
                                                        pattern = strings_to_remove)) |>
  dplyr::mutate("search_name" = stringr::str_remove_all(string = search_name,
                                                        pattern = "\\svar.*$")) |>
  dplyr::mutate("search_name" = stringr::str_remove_all(string = search_name,
                                                        pattern = "\\ssubsp.*$"))

# Extract names to submit to rotl
taxa_phylo <- mnnpc_phylo_taxa_lookup |>
  dplyr::distinct(search_name) |>
  dplyr::pull(search_name)

# Search for taxa available in OTL via rotl
rotl_results <- rotl::tnrs_match_names(names = taxa_phylo, context_name = "Land plants")

matched_taxa <- rotl_results |>
  dplyr::mutate(
    "phylo" = dplyr::case_when(
      is.na(ott_id) ~ FALSE,
      !is.na(ott_id) ~ TRUE
    )
  ) |>
  dplyr::mutate(
    "search_name" = dplyr::case_when(
      search_string == "x agrohordeum macounii" ~ "x Agrohordeum macounii",
      search_string == "x elyhordeum" ~ "x Elyhordeum",
      search_string == "x elyhordeum macounii" ~ "x Elyhordeum macounii",
      TRUE ~ stringr::str_to_sentence(string = search_string)
    )
  ) |>
  dplyr::select(search_name, "matched_name" = "unique_name", phylo, ott_id) |>
  dplyr::distinct() |>
  dplyr::mutate("ottid" = ifelse(!is.na(ott_id), paste0("ott", as.numeric(ott_id)), NA), .keep = "unused")

# Join OTT codes onto lookup
mnnpc_phylo_taxa_lookup <- mnnpc_phylo_taxa_lookup |>
  dplyr::left_join(matched_taxa, by = "search_name", relationship = "many-to-many")

# Retrieve OTT codes for taxa present in OTL
ott_in_tree <- rotl::ott_id(rotl_results)[rotl::is_in_tree(rotl::ott_id(rotl_results))]

ott_in_tree_tbl <- tibble::enframe(ott_in_tree) |>
  dplyr::mutate("ottid" = paste0("ott", as.numeric(value)))

all(ott_in_tree_tbl$name %in% mnnpc_accepted_phylo_taxa_lookup$matched_name)

# Retrieve and save tree in Newick format
mnnpc_phlyo_tree_raw <- rotl::tol_induced_subtree(ott_ids = ott_in_tree, label_format = "id")

rotl::tol_induced_subtree(ott_ids = ott_in_tree, label_format = "id", file = file.path(".", "data-raw", "data-out-zm", "mnnpc_phlyo_tree.txt"))

# Read tree back into R
mnnpc_phlyo_tree_string <- readLines(file.path(".", "data-raw", "data-out-zm", "mnnpc_phlyo_tree.txt"))

mnnpc_phlyo_tree_phylo <- ape::read.tree(text = mnnpc_phlyo_tree_string)

# Calculate branch lengths
mnnpc_phlyo_tree_phylo <- ape::compute.brlen(mnnpc_phlyo_tree_phylo, method = "Grafen")

# Replace tip labels, we need to do this as there are some incorrectly formatted names returned, and inexact, but acceptable, matches
tip_labels <- mnnpc_phlyo_tree_phylo$tip.label

new_tip_labels_all <- setNames(mnnpc_accepted_phylo_taxa_lookup$search_name, mnnpc_accepted_phylo_taxa_lookup$ottid)

new_tip_labels <- new_tip_labels_all[tip_labels]

all(tip_labels == names(new_tip_labels))

mnnpc_phlyo_tree_phylo$tip.label <- new_tip_labels

mnnpc_phlyo_tree <- ape::write.tree(mnnpc_phlyo_tree_phylo)

# Save tree
usethis::use_data(mnnpc_phlyo_tree, internal = FALSE, overwrite = TRUE)

# Save lookup between MNNPC names and names use to retrieve tree data
mnnpc_phylo_taxa_lookup <- mnnpc_accepted_phylo_taxa_lookup
usethis::use_data(mnnpc_phylo_taxa_lookup, internal = FALSE, overwrite = TRUE)
