#### import data ####

# import data
crosswalk_pre <- read.csv("../../intermediate-data/crosswalk_table_formatted_20251119a.csv")
crosswalk_raw <- read.csv("../../intermediate-data/crosswalk_raw_no_dups_20251119a.csv")
releve <- read.csv("../../intermediate-data/releve_table_formatted_20251119a.csv")

# remove everything except above
rm(list = ls()[!ls() %in% c("crosswalk_raw", "crosswalk_pre", "releve")])

# example data
load("data-raw/data-out-ak/mnnpc_example_data.rds")
resample <- readxl::read_excel("../../../releve-resample/data/releve_list_southern_20251120.xlsx")

#### test parameters ####

# number of releves to test
n_test <- 5

# randomly select releves
rels <- sample(crosswalk_pre$relnumb, n_test)

# relnumb will match to crosswalk tables
# make up year and group
parms <- data.frame(relnumb = rels,
                    year = sample(2000:2025, n_test),
                    group = "A")


#### raw data ####

# select releve to test
# add columns
dat_raw <- crosswalk_raw |>
  dplyr::inner_join(parms) |>
  dplyr::rename(scov = scov_mid)


#### process data ####

source("data-raw/releve_processing.R")

dat_proc <- rel_proc_fun(dat_raw)


#### pre-processed data ####

# select releves
# add columns
# format to match processed data
dat_val <- crosswalk_pre |>
  dplyr::inner_join(parms) |>
  dplyr::select(year, group, relnumb, analysis_group_strata, scov_adj) |>
  dplyr::arrange(year, group, relnumb, analysis_group_strata)|>
  dplyr::rename(Year = year, Group = group, Quadrat = relnumb,
                Species = analysis_group_strata, Cover = scov_adj)


#### compare ####

# rows with mismatches
rows_mis <- which(rowSums(dat_proc != dat_val) > 0)

# compare data
dat_proc[rows_mis, ]
dat_val[rows_mis, ]

# taxa crosswalk
load("data-raw/data-out-ak/taxa_conv.rds")

# explore issues
# dplyr::filter(taxa_conv, analysis_group == "Chamaenerion angustifolium")
# dplyr::filter(crosswalk_raw, relnumb == "4475" & 
#                 stringr::str_detect(taxon, "angustifolium"))
# dplyr::filter(crosswalk_raw, relnumb == "B950" & taxon == "Viola")
# mismatches left seem to be due to duplicates


#### example data ####

# process datasets
dat_ex_proc1 <- rel_proc_fun(mnnpc_example_data[[1]])
dat_ex_proc2 <- rel_proc_fun(mnnpc_example_data[[2]])

# format resample data
# format resample data
resample2 <- resample |>
  dplyr::filter(stringr::str_detect(npc, "MHs38|MHs39")) |> 
  dplyr::select(-relnumb) |>
  dplyr::rename(relnumb = orig_relnumb) |> 
  dplyr::full_join(resample |>
                     dplyr::filter(stringr::str_detect(npc, "MHs38|MHs39"))) |> 
  dplyr::mutate(group = ifelse(stringr::str_detect(npc, "MHs38"), "A", "B"))

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
                orig_relnumb = ifelse(relnumb %in% dat_ex_proc2$Quadrat,
                                      relnumb, original_releve_nbr)) |>
  dplyr::select(year, group, orig_relnumb, relnumb)
  
# pre-processed example data
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
