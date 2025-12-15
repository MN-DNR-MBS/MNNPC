# Establish filepaths
input_path <- file.path("data-raw", "data-out-ak")

# Source functions
source("./data-raw/2_releve_processing_AK.R")

# Load objects created by A.K.
load(file.path(input_path, "mnnpc_accepted_taxa.rds"))
load(file.path(input_path, "mnnpc_community_attributes.rds"))
load(file.path(input_path, "mnnpc_example_data.rds"))
load(file.path(input_path, "mnnpc_floristic_tables.rds"))
load(file.path(input_path, "mnnpc_taxa_lookup.rds"))
load(file.path(input_path, "mnnpc_taxonomic_backbone.rds"))
load(file.path(input_path, "raw_releve_example.rds"))

# Example releve
example_releve <- example_releve |>
  dplyr::select(year, group, relnumb, physcode, minht, maxht, taxon, scov)

usethis::use_data(example_releve, internal = FALSE, overwrite = TRUE)

# Taxon -> Analysis group aggregation lookup 
mnnpc_taxa_conv <- mnnpc_taxa_lookup |>
  dplyr::select(taxon_name, "analysis_group" = "recommended_taxon_name") |>
  dplyr::distinct()

usethis::use_data(mnnpc_taxa_conv, internal = FALSE, overwrite = TRUE)

# Accepted taxa
mnnpc_accepted_taxa <- mnnpc_accepted_taxa

usethis::use_data(mnnpc_accepted_taxa, internal = FALSE, overwrite = TRUE)

# Community attributes
mnnpc_community_attributes <- mnnpc_community_attributes

usethis::use_data(mnnpc_community_attributes, internal = FALSE, overwrite = TRUE)

# Example data
st_croix_raw <- mnnpc_example_data[["St. Croix State Forest"]]
st_croix_processed <- st_croix_raw |>
  rel_proc_fun() |>
  dplyr::mutate("Year" = as.integer(Year)) |>
  dplyr::select(Year, Group, Quadrat, Species, Cover)

isTRUE(all(all(st_croix_processed$Cover > 0), all(st_croix_processed$Cover <= 100)))

earthworm_forests_raw <- mnnpc_example_data[["Earthworm-Invaded Forests"]]
earthworm_forests_processed <- earthworm_forests_raw |>
  rel_proc_fun() |>
  dplyr::mutate(
    "Cover" = dplyr::case_when(
      Cover > 100 ~ 100,
      Cover < 0 ~ 0,
      TRUE ~ Cover
    )
  ) |>
  dplyr::mutate("Year" = as.integer(Year)) |>
  dplyr::select(Year, Group, Quadrat, Species, Cover)

isTRUE(all(all(earthworm_forests_processed$Cover > 0), all(earthworm_forests_processed$Cover <= 100)))

mnnpc_example_data[["Earthworm-Invaded Forests"]] <- earthworm_forests_processed
mnnpc_example_data[["St. Croix State Forest"]] <- st_croix_processed

usethis::use_data(mnnpc_example_data, internal = FALSE, overwrite = TRUE)

# Floristic tables
mnnpc_floristic_tables <- mnnpc_floristic_tables

usethis::use_data(mnnpc_floristic_tables, internal = FALSE, overwrite = TRUE)

# Taxonomic backbone
mnnpc_taxonomic_backbone <- mnnpc_taxonomic_backbone |>
  dplyr::left_join(mnnpc_taxa_lookup, by = "taxon_name") |>
  dplyr::select(id,
                informal_group, 
                taxon_name, 
                rank, 
                scientific_name,
                full_name,
                common_name,
                species, genus, family, order, class, phylum, kingdom,
                origin, 
                publication
                )

usethis::use_data(mnnpc_taxonomic_backbone, internal = FALSE, overwrite = TRUE)

# Taxa lookup
mnnpc_taxa_lookup <- mnnpc_taxa_lookup

usethis::use_data(mnnpc_taxa_lookup, internal = FALSE, overwrite = TRUE)
