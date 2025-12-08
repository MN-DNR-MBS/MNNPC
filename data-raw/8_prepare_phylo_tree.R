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

mnnpc_accepted_phylo_taxa_lookup <- mnnpc_accepted_taxa |>
  dplyr::mutate("phylo_taxon_name" = stringr::str_remove_all(string = taxon_name,
                                                             pattern = strings_to_remove)) |>
  dplyr::mutate("phylo_taxon_name" = stringr::str_remove_all(string = phylo_taxon_name,
                                                             pattern = "\\svar.*$")) |>
  dplyr::mutate("phylo_taxon_name" = stringr::str_remove_all(string = phylo_taxon_name,
                                                             pattern = "\\ssubsp.*$"))

taxa_phylo <- mnnpc_accepted_phylo_taxa_lookup |>
  dplyr::distinct(phylo_taxon_name) |>
  dplyr::pull(phylo_taxon_name)

rotl_results <- rotl::tnrs_match_names(names = taxa_phylo, context_name = "Land plants")

matched_taxa <- rotl_results |>
  dplyr::mutate(
    "phylo" = dplyr::case_when(
      is.na(ott_id) ~ FALSE,
      !is.na(ott_id) ~ TRUE
    )
  ) |>
  dplyr::mutate(
    "phylo_taxon_name" = dplyr::case_when(
      search_string == "x agrohordeum macounii" ~ "x Agrohordeum macounii",
      search_string == "x elyhordeum" ~ "x Elyhordeum",
      search_string == "x elyhordeum macounii" ~ "x Elyhordeum macounii",
      TRUE ~ stringr::str_to_sentence(string = search_string)
    )
  ) |>
  dplyr::select(phylo_taxon_name, phylo) |>
  dplyr::distinct()

mnnpc_accepted_phylo_taxa_lookup <- mnnpc_accepted_phylo_taxa_lookup |>
  dplyr::left_join(matched_taxa, by = "phylo_taxon_name", relationship = "many-to-many")

usethis::use_data(mnnpc_accepted_phylo_taxa_lookup, internal = FALSE, overwrite = TRUE)

ott_in_tree <- rotl::ott_id(rotl_results)[rotl::is_in_tree(rotl::ott_id(rotl_results))]

rotl::tol_induced_subtree(ott_ids = ott_in_tree, label_format = "name", file = file.path(".", "data-raw", "data-out-zm", "mnnpc_phlyo_tree.txt"))

mnnpc_phlyo_tree <- readLines(file.path(".", "data-raw", "data-out-zm", "mnnpc_phlyo_tree.txt"))

usethis::use_data(mnnpc_phlyo_tree, internal = FALSE, overwrite = TRUE)
