# Establish filepaths
input_path <- file.path("data-raw", "data-out-ak")

# Source functions
# source("./data-raw/2_releve_processing_AK.R")

# Load objects created by A.K.
load(file.path(input_path, "mnnpc_accepted_taxa.rds"))
load(file.path(input_path, "mnnpc_community_attributes.rds"))
load(file.path(input_path, "mnnpc_example_data.rds"))
load(file.path(input_path, "mnnpc_floristic_tables.rds"))
load(file.path(input_path, "mnnpc_taxa_lookup.rds"))
load(file.path(input_path, "mnnpc_taxonomic_backbone.rds"))
load(file.path(input_path, "mnnpc_example_releve.rds"))
load(file.path(input_path, "mnnpc_hybrid_crosswalk.rds"))

# Example releve

# check columns
colnames(mnnpc_example_releve)

# save
usethis::use_data(mnnpc_example_releve, internal = FALSE, overwrite = TRUE, compress = "xz")

# Accepted taxa

# check
head(mnnpc_accepted_taxa)

# save
usethis::use_data(mnnpc_accepted_taxa, internal = FALSE, overwrite = TRUE, compress = "xz")

# Community attributes
mnnpc_community_attributes <- mnnpc_community_attributes |>
  dplyr::mutate(
    "ecs_section" = dplyr::case_when(
      is.na(ecs_section) ~ "statewide",
      TRUE ~ ecs_section
    )
  )

usethis::use_data(mnnpc_community_attributes, internal = FALSE, overwrite = TRUE, compress = "xz")

# Example data
# st_croix_raw <- mnnpc_example_data[["St. Croix State Forest"]]
# st_croix_processed <- st_croix_raw |>
#   dplyr::mutate("year" = as.integer(year)) |>
  # dplyr::filter(!(taxon %in% c("Polytrichum", 
  #                              "Unknown moss",
  #                              "Unknown")))

# st_croix_processed |>
#   dplyr::filter(!(taxon %in% mnnpc_accepted_taxa$taxon_name))

# isTRUE(all(all(st_croix_processed$scov > 0), all(st_croix_processed$scov <= 100)))

# earthworm_forests_raw <- mnnpc_example_data[["Earthworm-Invaded Forests"]]
# earthworm_forests_processed <- earthworm_forests_raw |>
  # dplyr::mutate(
  #   "scov" = dplyr::case_when(
  #     scov > 100 ~ 100,
  #     scov < 0 ~ 0,
  #     TRUE ~ scov
  #   )
  # ) |>
  # dplyr::mutate("year" = as.integer(year)) |>
  # dplyr::filter(!(taxon %in% c("Unknown", 
  #                              "Non-sphagnum moss",
  #                              "Unknown moss",
  #                              "Unknown bryophytes"))) |>
  # dplyr::mutate(
  #   "taxon" = dplyr::case_when(
  #     taxon == "Lonicera xbella" ~ "Lonicera x bella",
  #     TRUE ~ taxon
  #   )
  # )

# isTRUE(all(all(earthworm_forests_processed$scov > 0), all(earthworm_forests_processed$scov <= 100)))

# mnnpc_example_data[["Earthworm-Invaded Forests"]] <- earthworm_forests_processed
# mnnpc_example_data[["St. Croix State Forest"]] <- st_croix_processed

# check variables
str(mnnpc_example_data[["St. Croix State Forest"]]) # year is integer
str(mnnpc_example_data[["Earthworm-Invaded Forests"]])

# check that all taxa have accepted names
mnnpc_example_data[["St. Croix State Forest"]] |>
  dplyr::filter(!(taxon %in% mnnpc_taxa_lookup$taxon_name))

mnnpc_example_data[["Earthworm-Invaded Forests"]] |>
  dplyr::filter(!(taxon %in% mnnpc_taxa_lookup$taxon_name))

# check scov values
isTRUE(all(all(mnnpc_example_data[["St. Croix State Forest"]]$scov > 0), 
           all(mnnpc_example_data[["St. Croix State Forest"]]$scov <= 100)))
isTRUE(all(all(mnnpc_example_data[["Earthworm-Invaded Forests"]]$scov > 0), 
           all(mnnpc_example_data[["Earthworm-Invaded Forests"]]$scov <= 100)))

# save
usethis::use_data(mnnpc_example_data, internal = FALSE, overwrite = TRUE, compress = "xz")

# Floristic tables
# mnnpc_floristic_tables <- mnnpc_floristic_tables

usethis::use_data(mnnpc_floristic_tables, internal = FALSE, overwrite = TRUE, compress = "xz")

# Taxonomic backbone
# mnnpc_taxonomic_backbone <- mnnpc_taxonomic_backbone |>
#   dplyr::left_join(mnnpc_taxa_lookup, by = "taxon_name") |>
#   dplyr::select(id,
#                 informal_group, 
#                 taxon_name, 
#                 rank, 
#                 scientific_name,
#                 common_name,
#                 species, genus, family, order, class, phylum, kingdom,
#                 origin, 
#                 publication
#                 )

# check columns
colnames(mnnpc_taxonomic_backbone)

# save
usethis::use_data(mnnpc_taxonomic_backbone, internal = FALSE, overwrite = TRUE, compress = "xz")

# Taxa lookup
# mnnpc_taxa_lookup <- mnnpc_taxa_lookup

usethis::use_data(mnnpc_taxa_lookup, internal = FALSE, overwrite = TRUE, compress = "xz")

# Hybrid crosswalk
# mnnpc_hybrid_crosswalk <- mnnpc_hybrid_crosswalk

usethis::use_data(mnnpc_hybrid_crosswalk, internal = FALSE, overwrite = TRUE, compress = "xz")
