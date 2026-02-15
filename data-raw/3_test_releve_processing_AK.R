#### import data ####

# import data
load("data-raw/data-out-ak/releve_species_grouped_data.rds")
load("data-raw/data-out-ak/releve_species_ungrouped_data.rds")
load("data-raw/data-out-ak/releve_plot_data.rds")

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
dat_raw <- releve_species_ungrouped |>
  dplyr::select(relnumb, physcode, minht, maxht, taxon, scov,
                outside_of_plot) |>
  dplyr::inner_join(parms) |>
  dplyr::arrange(year, group, relnumb, taxon, minht, maxht)


#### process data ####

dat_proc <- process_dnr_releves(dat_raw, cover_scale = "percentage")


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
dat_ex_proc1 <- process_dnr_releves(mnnpc_example_data[[1]] |>
                                      dplyr::filter(year == 1), 
                                    cover_scale = "braunBlanquet")
dat_ex_proc2 <- process_dnr_releves(mnnpc_example_data[[2]] |>
                                      dplyr::filter(year == 1), 
                                    cover_scale = "braunBlanquet")

# previously processed example data
dat_ex_val1 <- releve_species_grouped |>
  dplyr::inner_join(mnnpc_example_data[[1]] |>
                      dplyr::filter(year == 1) |>
                      dplyr::distinct(year, group, relnumb)) |>
  dplyr::select(year, group, relnumb, analysis_group_strata, scov) |>
  dplyr::arrange(year, group, relnumb, analysis_group_strata)|>
  dplyr::rename(Year = year, Group = group, Quadrat = relnumb,
                Species = analysis_group_strata, Cover = scov)

dat_ex_val2 <- releve_species_grouped |>
  dplyr::inner_join(mnnpc_example_data[[2]] |>
                      dplyr::filter(year == 1) |>
                      dplyr::distinct(year, group, relnumb)) |>
  dplyr::select(year, group, relnumb, analysis_group_strata, scov) |>
  dplyr::arrange(year, group, relnumb, analysis_group_strata)|>
  dplyr::rename(Year = year, Group = group, Quadrat = relnumb,
                Species = analysis_group_strata, Cover = scov)
# rows with mismatches
rows_mis1 <- which(rowSums(dat_ex_proc1 != dat_ex_val1) > 0)
dat_ex_proc1[rows_mis1, ]
dat_ex_val1[rows_mis1, ]

rows_mis2 <- which(rowSums(dat_ex_proc2 != dat_ex_val2) > 0)
dat_ex_proc2[rows_mis2, ]
dat_ex_val2[rows_mis2, ]
