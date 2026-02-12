#### import data ####

# import data
load("data-raw/data-out-ak/releve_species_grouped_data.rds")
load("data-raw/data-out-ak/releve_species_ungrouped_data.rds")
load("data-raw/data-out-ak/releve_plot_data.rds")
load("../npc-releve/data/originals-20260208/crosswalk.rds")

# remove everything except above
rm(list = ls()[!ls() %in% c("crosswalk",
                            "releve_plots", 
                            "releve_species_grouped",
                            "releve_species_ungrouped",
                            "resample")])

# load finalized objects
#devtools::load_all()

# load recent objects
load("data-raw/data-out-ak/mnnpc_hybrid_crosswalk.rds")
load("data-raw/data-out-ak/mnnpc_taxa_lookup.rds")
load("data-raw/data-out-ak/mnnpc_example_data.rds")
load("data/mnnpc_bb_conv.rda")
load("data/mnnpc_strata.rda")
load("data/mnnpc_ht_conv.rda")


#### test parameters ####

# number of releves to test
n_test <- 5

# randomly select releves
rels <- sample(releve_plots$relnumb, n_test)

# relnumb will match to crosswalk tables
# make up year and group
parms <- data.frame(relnumb = rels,
                    year = sample(2000:2025, n_test),
                    group = "A")


#### raw data ####

# select releve to test
# add columns
dat_raw <- crosswalk |>
  dplyr::select(relnumb, physcode, minht, maxht, pcov, taxon, relid, scov,
                outside_of_plot) |>
  dplyr::inner_join(parms) |>
  dplyr::rename(scov = scov)


#### process data ####

dat_proc <- process_dnr_releves(dat_raw, cover_scale = "braunBlanquet")


#### pre-processed data ####

# select releves
# add columns
# format to match processed data
dat_val <- releve_species_grouped |>
  dplyr::inner_join(parms) |>
  dplyr::select(year, group, relnumb, analysis_group_strata, scov) |>
  dplyr::arrange(year, group, relnumb, analysis_group_strata)|>
  dplyr::rename(Year = year, Group = group, Quadrat = relnumb,
                Species = analysis_group_strata, Cover = scov)


#### compare ####

# rows with mismatches
rows_mis <- which(rowSums(dat_proc[1:nrow(dat_val),] != dat_val) > 0)

# compare data
dat_proc[rows_mis, ]
dat_val[rows_mis, ]


#### example data ####

# process datasets
dat_ex_proc1 <- process_dnr_releves(mnnpc_example_data[[1]])
dat_ex_proc2 <- process_dnr_releves(mnnpc_example_data[[2]])

# format resample data
# format resample data
resample2 <- resample |>
  dplyr::filter(stringr::str_detect(npc, "MHs38|MHs39")) |> 
  dplyr::select(-relnumb) |>
  dplyr::rename(relnumb = orig_relnumb) |> 
  dplyr::full_join(resample |>
                     dplyr::filter(stringr::str_detect(npc, "MHs38|MHs39"))) |> 
  dplyr::mutate(group = ifelse(stringr::str_detect(npc, "MHs38"), 
                               "Oak-Basswood Group", 
                               "Maple-Basswood Group"))

# get releve numbers in the database
dat_ex_relnumb1 <- releve |>
  dplyr::filter(relnumb %in% dat_ex_proc1$Quadrat |
                  original_releve_nbr %in% dat_ex_proc1$Quadrat) |>
  dplyr::mutate(year = lubridate::year(date_),
                group = dplyr::case_when(is.na(place_name) ~ "Control",
                                         stringr::str_detect(place_name, 
                                                    "Outside Exclosure") ~ 
                                           "Outside Exclosure",
                                         stringr::str_detect(place_name, "Exclosure") ~ 
                                           "Inside Exclosure"),
                orig_relnumb = ifelse(relnumb %in% dat_ex_proc1$Quadrat,
                                      relnumb, original_releve_nbr)) |>
  dplyr::select(year, group, orig_relnumb, relnumb)

dat_ex_relnumb2 <- releve |>
  dplyr::filter(relnumb %in% dat_ex_proc2$Quadrat |
                  original_releve_nbr %in% dat_ex_proc2$Quadrat) |>
  dplyr::left_join(resample2 |>
                     dplyr::select(relnumb, group)) |>
  dplyr::mutate(year = lubridate::year(date_),
                year = ifelse(year < 2018, 1990, year),
                orig_relnumb = ifelse(relnumb %in% dat_ex_proc2$Quadrat,
                                      relnumb, original_releve_nbr)) |>
  dplyr::select(year, group, orig_relnumb, relnumb)
  
# previously processed example data
dat_ex_val1 <- crosswalk_pre |>
  dplyr::inner_join(dat_ex_relnumb1) |>
  dplyr::select(year, group, orig_relnumb, analysis_group_strata, scov_adj) |>
  dplyr::arrange(year, group, orig_relnumb, analysis_group_strata)|>
  dplyr::rename(Year = year, Group = group, Quadrat = orig_relnumb,
                Species = analysis_group_strata, Cover = scov_adj)

dat_ex_val2 <- crosswalk_pre |>
  dplyr::inner_join(dat_ex_relnumb2) |>
  dplyr::select(year, group, orig_relnumb, analysis_group_strata, scov_adj) |>
  dplyr::arrange(year, group, orig_relnumb, analysis_group_strata)|>
  dplyr::rename(Year = year, Group = group, Quadrat = orig_relnumb,
                Species = analysis_group_strata, Cover = scov_adj)

# rows with mismatches
rows_mis1 <- which(rowSums(dat_ex_proc1 != dat_ex_val1) > 0)
dat_ex_proc1[rows_mis1, ]
dat_ex_val1[rows_mis1, ]

rows_mis2 <- which(rowSums(dat_ex_proc2 != dat_ex_val2) > 0)
dat_ex_proc2[rows_mis2, ]
dat_ex_val2[rows_mis2, ]