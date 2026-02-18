# Establish filepaths
input_path <- file.path("data-raw", "data-out-ak")

# Load objects created by A.K.
load(file.path(input_path, "mnnpc_accepted_taxa.rds"))
load(file.path(input_path, "mnnpc_community_attributes.rds"))
load(file.path(input_path, "mnnpc_example_data.rds"))
load(file.path(input_path, "mnnpc_example_releve.rds"))
# load(file.path(input_path, "mnnpc_floristic_table_data.rds"))
load(file.path(input_path, "mnnpc_floristic_tables.rds"))
load(file.path(input_path, "mnnpc_hybrid_crosswalk.rds"))
load(file.path(input_path, "mnnpc_taxa_lookup.rds"))
load(file.path(input_path, "mnnpc_taxonomic_backbone.rds"))

load(file.path(input_path, "releve_plot_data.rds")) # Check
load(file.path(input_path, "releve_species_grouped_data.rds")) # Check
load(file.path(input_path, "releve_species_ungrouped_data.rds")) # Check

# Example releve ---------------------------------------------------------

# check 
colnames(mnnpc_example_releve)

# adjust
mnnpc_example_releve <- mnnpc_example_releve |>
  dplyr::filter(outside_of_plot == "f") |>
  dplyr::select(-outside_of_plot)

# save
usethis::use_data(mnnpc_example_releve, internal = FALSE, overwrite = TRUE, compress = "xz")

# Accepted taxa ---------------------------------------------------------

# check
head(mnnpc_accepted_taxa)
all(mnnpc_accepted_taxa$taxon_name %in% mnnpc_taxa_lookup$taxon_name)
setdiff(mnnpc_accepted_taxa$taxon_name, mnnpc_taxa_lookup$taxon_name)

# adjust
mnnpc_accepted_taxa <- mnnpc_accepted_taxa |>
  dplyr::arrange(taxon_name)

# save
usethis::use_data(mnnpc_accepted_taxa, internal = FALSE, overwrite = TRUE, compress = "xz")

# Community attributes --------------------------------------------------

# check
head(mnnpc_community_attributes)

# adjust

# save
usethis::use_data(mnnpc_community_attributes, internal = FALSE, overwrite = TRUE, compress = "xz")

# Example data ---------------------------------------------------------
st_croix_raw <- mnnpc_example_data[["St. Croix State Forest"]]
st_croix_processed <- st_croix_raw |>
  dplyr::filter(outside_of_plot == "f") |>
  dplyr::select(-outside_of_plot) |>
  dplyr::mutate("taxon" = stringr::str_remove_all(string = taxon, pattern = "\\ss.s.$|\\ss.l.$")) |>
  dplyr::filter(!(taxon %in% c("Polytrichum",
                               "Unknown moss",
                               "Unknown",
                               "Unknown bryophytes",
                               "Non-sphagnum moss"))) |>
  dplyr::mutate(
    "scov" = dplyr::case_when(
      scov == "x" ~ "+",
      TRUE ~ scov
    )
  )

earthworm_forests_raw <- mnnpc_example_data[["Earthworm-Invaded Forests"]]
earthworm_forests_processed <- earthworm_forests_raw  |>
  dplyr::filter(outside_of_plot == "f") |>
  dplyr::select(-outside_of_plot) |>
  dplyr::filter(!(taxon %in% c("Polytrichum",
                               "Unknown moss",
                               "Unknown",
                               "Unknown bryophytes",
                               "Non-sphagnum moss"))) |>
  dplyr::mutate("taxon" = stringr::str_remove_all(string = taxon, pattern = "\\ss.s.$|\\ss.l.$")) |>
  dplyr::mutate(
    "taxon" = dplyr::case_when(
      taxon == "Lonicera xbella" ~ "Lonicera x bella",
      TRUE ~ taxon
    )
  ) |>
  dplyr::mutate(
    "scov" = dplyr::case_when(
      scov == "x" ~ "+",
      TRUE ~ scov
    )
  )

mnnpc_example_data[["St. Croix State Forest"]] <- st_croix_processed
mnnpc_example_data[["Earthworm-Invaded Forests"]] <- earthworm_forests_processed

# check variables
str(mnnpc_example_data[["St. Croix State Forest"]]) # year is integer
str(mnnpc_example_data[["Earthworm-Invaded Forests"]])

# check that all taxa have accepted names
mnnpc_example_data[["St. Croix State Forest"]] |>
  dplyr::filter(!(taxon %in% mnnpc_taxa_lookup$taxon_name))

mnnpc_example_data[["Earthworm-Invaded Forests"]] |>
  dplyr::filter(!(taxon %in% mnnpc_taxa_lookup$taxon_name))

# check scov values
unique(mnnpc_example_data[["St. Croix State Forest"]]$scov)
unique(mnnpc_example_data[["Earthworm-Invaded Forests"]]$scov)

# save
usethis::use_data(mnnpc_example_data, internal = FALSE, overwrite = TRUE, compress = "xz")

# Floristic tables -------------------------------------------------------

# check
str(mnnpc_floristic_tables)
all(mnnpc_floristic_tables$npc_taxon_name %in% mnnpc_taxa_lookup$analysis_group)
setdiff(mnnpc_floristic_tables$npc_taxon_name, mnnpc_taxa_lookup$analysis_group) # all strata - fine!

# adjust

# save
usethis::use_data(mnnpc_floristic_tables, internal = FALSE, overwrite = TRUE, compress = "xz")

# Taxa lookup ------------------------------------------------------------

# check
str(mnnpc_taxa_lookup)

# save
usethis::use_data(mnnpc_taxa_lookup, internal = FALSE, overwrite = TRUE, compress = "xz")

# Taxonomic backbone -----------------------------------------------------

# check
str(mnnpc_taxonomic_backbone)
all(mnnpc_taxonomic_backbone$taxon_name %in% mnnpc_taxa_lookup$taxon_name)
setdiff(mnnpc_taxonomic_backbone$taxon_name, mnnpc_taxa_lookup$recommended_taxon_name)

# adust
mnnpc_taxonomic_backbone <- mnnpc_taxonomic_backbone |>
  dplyr::filter(taxon_name %in% mnnpc_taxa_lookup$recommended_taxon_name)

# save
usethis::use_data(mnnpc_taxonomic_backbone, internal = FALSE, overwrite = TRUE, compress = "xz")


# Hybrid crosswalk -------------------------------------------------------

# save
usethis::use_data(mnnpc_hybrid_crosswalk, internal = FALSE, overwrite = TRUE, compress = "xz")


# Floristic tables plot data ---------------------------------------------
mnnpc_floristic_table_data <- flor_dat

usethis::use_data(mnnpc_floristic_table_data, internal = FALSE, overwrite = TRUE, compress = "xz")
