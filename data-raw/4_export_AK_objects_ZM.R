# Establish filepaths
input_path <- file.path("data-raw", "data-out-ak")

# Load objects created by A.K.
load(file.path(input_path, "mnnpc_accepted_taxa.rds"))
load(file.path(input_path, "mnnpc_community_attributes.rds"))
load(file.path(input_path, "mnnpc_example_data.rds"))
load(file.path(input_path, "mnnpc_example_releve.rds"))
load(file.path(input_path, "mnnpc_floristic_table_data.rds"))
load(file.path(input_path, "mnnpc_floristic_tables.rds"))
load(file.path(input_path, "mnnpc_hybrid_crosswalk.rds"))
load(file.path(input_path, "mnnpc_taxa_lookup.rds"))
load(file.path(input_path, "mnnpc_taxonomic_backbone.rds"))

# Example releve ---------------------------------------------------------

# check 
colnames(mnnpc_example_releve)

# adjust
mnnpc_example_releve <- mnnpc_example_releve |>
  # dplyr::filter(outside_of_plot == "f") |>
  dplyr::select(-outside_of_plot)

# save
usethis::use_data(mnnpc_example_releve, internal = FALSE, overwrite = TRUE, compress = "xz")

# Community attributes --------------------------------------------------

# check
head(mnnpc_community_attributes)

# adjust

# save
usethis::use_data(mnnpc_community_attributes, internal = FALSE, overwrite = TRUE, compress = "xz")

# Example data ---------------------------------------------------------
st_croix_raw <- mnnpc_example_data[["St. Croix State Forest"]]
st_croix_processed <- st_croix_raw |>
  # dplyr::filter(outside_of_plot == "f") |>
  dplyr::select(-outside_of_plot) |>
  dplyr::filter(scov != "x") |>
  dplyr::mutate("taxon" = stringr::str_remove_all(string = taxon, pattern = "\\ss.s.$|\\ss.l.$")) |>
  dplyr::filter(!(taxon %in% c("Polytrichum",
                               "Unknown moss",
                               "Unknown",
                               "Unknown bryophytes",
                               "Non-sphagnum moss")))

earthworm_forests_raw <- mnnpc_example_data[["Earthworm-Invaded Forests"]]
earthworm_forests_processed <- earthworm_forests_raw  |>
  # dplyr::filter(outside_of_plot == "f") |>
  dplyr::select(-outside_of_plot) |>
  dplyr::filter(scov != "x") #|>
  # dplyr::filter(!(taxon %in% c("Polytrichum",
  #                              "Unknown moss",
  #                              "Unknown",
  #                              "Unknown bryophytes",
  #                              "Non-sphagnum moss"))) |>
  # dplyr::mutate("taxon" = stringr::str_remove_all(string = taxon, pattern = "\\ss.s.$|\\ss.l.$")) |>
  # dplyr::mutate(
  #   "taxon" = dplyr::case_when(
  #     taxon == "Lonicera xbella" ~ "Lonicera x bella",
  #     TRUE ~ taxon
  #   )
  # )

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

# Taxa lookup ------------------------------------------------------------

# check
str(mnnpc_taxa_lookup)

# adjust
mnnpc_taxa_lookup <- mnnpc_taxa_lookup |>
  dplyr::arrange(taxon_name)

# save
usethis::use_data(mnnpc_taxa_lookup, internal = FALSE, overwrite = TRUE, compress = "xz")

# Taxonomic backbone -----------------------------------------------------

# check
str(mnnpc_taxonomic_backbone)
all(mnnpc_taxonomic_backbone$taxon_name %in% mnnpc_taxa_lookup$taxon_name)
setdiff(mnnpc_taxonomic_backbone$taxon_name, mnnpc_taxa_lookup$recommended_taxon_name)

# adust
mnnpc_taxonomic_backbone <- mnnpc_taxonomic_backbone |>
  dplyr::filter(taxon_name %in% mnnpc_taxa_lookup$recommended_taxon_name)|>
  dplyr::arrange(taxon_name)

# save
usethis::use_data(mnnpc_taxonomic_backbone, internal = FALSE, overwrite = TRUE, compress = "xz")


# Hybrid crosswalk -------------------------------------------------------

# save
usethis::use_data(mnnpc_hybrid_crosswalk, internal = FALSE, overwrite = TRUE, compress = "xz")



# MN NPC development data -------------------------------------------------

# check
str(mnnpc_floristic_table_data)

# prepare
mnnpc_development_data <- mnnpc_floristic_table_data

# save
usethis::use_data(mnnpc_development_data, internal = FALSE, overwrite = TRUE, compress = "xz")

# Releves ----------------------------------------------------------------

# check


# prepare
mnnpc_releves_statewide_class <- mnnpc_floristic_table_data |>
  dplyr::mutate(ecs_section = "statewide") |>
  dplyr::select("npc_code" = "npc_class", "releve" = "Quadrat", "taxon_name" = "Species") |>
  dplyr::filter(!is.na(npc_code)) |>
  dplyr::mutate("releve" = paste0(npc_code, "_", releve))

mnnpc_releves_statewide_type <- mnnpc_floristic_table_data |>
  dplyr::mutate(ecs_section = "statewide") |>
  dplyr::select("npc_code" = "npc_type", "releve" = "Quadrat", "taxon_name" = "Species") |>
  dplyr::filter(!is.na(npc_code)) |>
  dplyr::mutate("releve" = paste0(npc_code, "_", releve))

mnnpc_releves_statewide_subtype <- mnnpc_floristic_table_data |>
  dplyr::mutate(ecs_section = "statewide") |>
  dplyr::select("npc_code" = "npc_subtype", "releve" = "Quadrat", "taxon_name" = "Species") |>
  dplyr::filter(!is.na(npc_code)) |>
  dplyr::mutate("releve" = paste0(npc_code, "_", releve))

mnnpc_releves_statewide <- dplyr::bind_rows(mnnpc_releves_statewide_class,
                                            mnnpc_releves_statewide_type,
                                            mnnpc_releves_statewide_subtype)

ecs_sections <- mnnpc_floristic_table_data$ecs_section |> unique()
ecs_sections <- ecs_sections[!is.na(ecs_sections)]

mnnpc_releves <- mnnpc_releves_statewide

for(section in ecs_sections){
  # section <- "MDL"
  
  mnnpc_releves_statewide_class_section <- mnnpc_floristic_table_data |>
    dplyr::filter(ecs_section == section) |>
    dplyr::select("npc_code" = "npc_class", "releve" = "Quadrat", "taxon_name" = "Species") |>
    dplyr::filter(!is.na(npc_code)) |>
    dplyr::mutate("releve" = paste0(npc_code, "_", section, "_", releve)) |>
    dplyr::mutate("npc_code" = paste0(npc_code, "_", section))
  
  mnnpc_releves_statewide_type_section <- mnnpc_floristic_table_data |>
    dplyr::filter(ecs_section == section) |>
    dplyr::select("npc_code" = "npc_type", "releve" = "Quadrat", "taxon_name" = "Species") |>
    dplyr::filter(!is.na(npc_code)) |>
    dplyr::mutate("releve" = paste0(npc_code, "_", section, "_", releve)) |>
    dplyr::mutate("npc_code" = paste0(npc_code, "_", section))
  
  mnnpc_releves_statewide_subtype_section <- mnnpc_floristic_table_data |>
    dplyr::filter(ecs_section == section) |>
    dplyr::select("npc_code" = "npc_subtype", "releve" = "Quadrat", "taxon_name" = "Species") |>
    dplyr::filter(!is.na(npc_code)) |>
    dplyr::mutate("releve" = paste0(npc_code, "_", section, "_", releve)) |>
    dplyr::mutate("npc_code" = paste0(npc_code, "_", section))
  
  mnnpc_releves_section <- dplyr::bind_rows(mnnpc_releves_statewide_class_section,
                                            mnnpc_releves_statewide_type_section,
                                            mnnpc_releves_statewide_subtype_section)
  
  mnnpc_releves <- mnnpc_releves |>
    dplyr::bind_rows(mnnpc_releves_section)
  
}

mnnpc_releves <- mnnpc_releves |>
  dplyr::select(npc_code, releve, taxon_name)

# check
all(unique(mnnpc_floristic_tables$npc_code) %in% unique(mnnpc_releves$npc_code))
table(mnnpc_releves$npc_code)

# save
usethis::use_data(mnnpc_releves, internal = FALSE, overwrite = TRUE, compress = "xz")
