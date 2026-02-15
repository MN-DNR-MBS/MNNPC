#### overview ####

# prepare releve data for analyses within RMAVIS
# originals saved in npc-releve project in case this needs to be re-run

# to load
load("../npc-releve/data/originals-20260208/crosswalk.rds")
load("../npc-releve/data/originals-20260208/releve.rds")
load("../npc-releve/data/originals-20260208/spatial_releve.rds")
load("../npc-releve/data/originals-20260208/spatial_ecs_subsec.rds")

# data importing functions and calls below have been commented out

#### load packages ####

# install mntaxa (development)
pak::pak("MN-DNR-MBS/mntaxa", upgrade = T)

library(mntaxa)
library(RODBC)
library(collapse)
library(tidyverse)
library(janitor)
library(sf)
library(nngeo)


#### user options ####

# select scov midpoint methods
midpoint_method <- "scov_mid_te37"
# options:
# scov_mid_te37: Tuxen and Ellenberg (1937)
# scov_mid_bb64: Braun-Blanquet (1964)
# recommendation (AK): Tuxen and Ellenberg (1937) performed the best of the 
# midpoint methods in McNellie et al. 2019 Applied Vegetation Science

# exclude observations located outside of the plot, or include (actual scov for trees, r for everything else)?
op_exclude <- FALSE
# options: TRUE, FALSE
# recommendation (legacy, DW): Set to FALSE
# surveyors were not consistent with each other or the handbook for how they 
# recorded species outside of the plot

# exclude observations of dead/dying
death_exclude <- c()
# options: "DD", "DY", "DD/DY", c() for none
# recommendation (Norm): keep dead/dying since they were growing there
# somewhat recently

# exclude any surveyors?
surveyor_exclude <- c()

# life forms to include
physcode_include <- c("B", "C", "D", "E", "F", "G", "H", "K", "S", "X")
# options: "A", "B", "C", "D", "E", "F", "G", "H", "K", "L", "Li", "M", "S", "X"
# recommendation (legacy): people don't consistently collect or correctly
# identify A, L, Li, and M, so these should be removed
# A = algae
# L = lichens
# Li = lichens
# M = mosses & liverworts
# if including any of these groups, see note in "add analysis codes" section

# taxa to exclude
taxon_exclude <- c("Unknown", "cant find", "Unknown bryophytes",
                   "Unknown liverwort", "Non-sphagnum moss")

# maximum allowable reliability code
relid_thresh <- 7
# options: 0, 1, 2, 3, 4, 5, 6, 7

# stratify height classes?
stratify <- "nonpeat"
# options: "nonpeat", "peatland", "none"
# nonpeat: 1-3, 4-5, 5+
# peatland: 1-4, 5+
# applied to D and E that do not have stratacodes of "ground" or "shrub"

# include genera?
genera_include <- F
# options: T or F

# geographic filters (hierarchical)
prov_include <- c() # ECS province names
sec_include <- c() # ECS section names
subsec_include <- c() # ECS subsection names
# options:
# format names to match ecs_subsec (below)
# use <- c() for no filters

#### import data ####

# # connect to database
# db_con <- odbcConnect("Releve")
# 
# # load data
# releve <- sqlFetch(db_con, "vw_releve_export") %>% as_tibble()
# crosswalk <- sqlFetch(db_con, "vw_rel_data_crosswalk") %>% as_tibble()
# 
# # close database connection
# close(db_con)
# 
# # import supporting data
# ecs_subsec <- st_read("V:/gdrs/data/pub/us_mn_state_dnr/geos_ecological_class_system/fgdb/geos_ecological_class_system.gdb", 
#                       "ecs_subsections_of_mn_v99a")
# spat_releve <- st_read ("V:/gdrs/data/org/us_mn_state_dnr/biota_dnr_releve_sites/fgdb/biota_dnr_releve_sites.gdb",
#                         "dnr_releve_sites")
systems <- read_csv("../npc-releve/data/npc_systems.csv")


#### format supporting tables ####

# height conversion table
ht_conv <- tibble(ht = 1:8,
                  ht_min_m = c(0, 0.1, 0.5, 2, 5, 10, 20, 35),
                  ht_max_m = c(0.1, 0.5, 2, 5, 10, 20, 35, 50))

# create scov conversions
scov_conv <- tibble(scov = c(as.character(5:1), "+", "r"),
                    scov_ord = 7:1,
                    scov_mid_bb64 = c(87.5, 62.5, 37.5, 17.5, 5, 0.1, 0),
                    scov_mid_te37 = c(87.5, 62.5, 37.5, 15, 2.5, 0.1, 0.02))

# create pcov conversions
pcov_conv <- tibble(pcov = c("C", "I", "P", "R", "B", "A"),
                    pcov_ord = 6:1,
                    pcov_mid = c(87.5, 62.5, 37.5, 15, 3, 0.5))


#### format releve table ####

# require releve to be in both tables 
releve_include <- crosswalk %>%
  distinct(relnumb) %>%
  inner_join(releve %>%
               rename(relnumb = dnr_releve_nbr) %>%
               distinct(relnumb)) %>%
  pull(relnumb)

# NPC code corrections
npc_code_corrections <- tibble(npc_code_orig = c("LB", "MR_93a", "MR_93b",
                                                 "RB", "Opn", "WMn83a"),
                               npc_code_rep = c("LK", "MR", "MR", "RV", "OPn",
                                            "WMs83a"),
                               npc_class_name_rep = c(rep(NA_character_, 5),
                                                      "Southern Seepage Meadow/Carr"),
                               npc_type_name_rep = c(rep(NA_character_, 5),
                                                     "Seepage Meadow/Carr"))
  
# get plot area from remarks, when available
# replace 0 or 999 with NA plot area
# format survey names
# rename releve number column
# select releves also in crosswalk
# omit releve that's just a species list
releve2 <- releve %>%
  mutate(plot_area_rev = case_when(str_detect(remarks, "<0.1ha releve") ~ NA_real_,
                                   str_detect(remarks, "> 1ha releve") ~ NA_real_,
                                   str_detect(remarks, "0.01ha releve") ~ 100,
                                   str_detect(remarks, "0.5 acre releve|1/2 acre releve") ~ 2023.43,
                                   str_detect(remarks, "1 acre plot|1 acre releve|1-acre releve") ~ 4046.86,
                                   str_detect(remarks, "2 acre plot|2 acre releve|2 acre, plot|2 Acres") ~ 8093.71,
                                   str_detect(remarks, "3 acre plot|3 acre releve") ~ 12140.6,
                                   str_detect(remarks, "4 acre plot|4 acre releve") ~ 16187.4,
                                   str_detect(remarks, "5 acre releve|5-acre releve") ~ 20234.3,
                                   str_detect(remarks, "Releve generalized for entire 7-acre patch of woods") ~ 28328,
                                   str_detect(remarks, "Releve generalized or entire 8 acre tract") ~ 32374.9,
                                   str_detect(remarks, "Releve generalized for 9 acre stand") ~ 36421.7,
                                   str_detect(remarks, "10-acre releve|10 acre releve") ~ 40468.6,
                                   str_detect(remarks, "15-acre releve|Releve generalized for 15-acre stand") ~ 60702.8,
                                   str_detect(remarks, "60 acre releve") ~ 242811,
                                   str_detect(remarks, "80 acre plot") ~ 323749,
                                   str_detect(remarks, "0.01ha") ~ 100,
                                   str_detect(remarks, "0.1 ha releve|0.1 ha plot|0.1ha releve") ~ 1000,
                                   str_detect(remarks, "0.2 ha releve|0.2 ha plot") ~ 2000,
                                   str_detect(remarks, "0.3 ha plot|0.3 ha releve") ~ 3000,
                                   str_detect(remarks, "0.4 ha releve") ~ 4000,
                                   str_detect(remarks, "0.5 ha releve|0.5 ha plot|0.5ha releve") ~ 5000,
                                   str_detect(remarks, "1 ha releve|1 ha plot|1ha releve") ~ 10000,
                                   str_detect(remarks, "2 ha releve|Oversized plot (2 ha)") ~ 20000,
                                   str_detect(remarks, "20 ha releve") ~ 200000,
                                   str_detect(remarks, "1000 meters square plot") ~ 1000,
                                   str_detect(remarks, "8000+ meter squared plot") ~ 8000,
                                   plot_area %in% c(0, 999) ~ NA_real_,
                                   TRUE ~ plot_area),
         lead_surveyor = case_when(lead_surveyors_name %in% 
                                     c("  1 request (not on list) ",
                                       "  Other ",
                                       "  Unknown ") ~ "unknown",
                                   str_detect(lead_surveyors_name,
                                              "Chippewa NF") ~ "Chippewa NF",
                                   str_detect(lead_surveyors_name,
                                              "Superior National Forest") ~ 
                                     "Superior NF",
                                   lead_surveyors_name == "Karen Shik " ~
                                     "Karen G. Schik ", # this is a guess that they're the same person
                                   TRUE ~ lead_surveyors_name) %>%
           trimws() %>% # remove whitespace
           str_replace("  ", " "), # convert two spaces to one
         npc_code_orig = npc_code) %>% 
  left_join(npc_code_corrections) %>% 
  mutate(npc_code = if_else(!is.na(npc_code_rep), npc_code_rep, npc_code_orig),
         npc_system_id =  if_else(str_detect(npc_code, "_|\\/"),
                                  npc_code,
                                  str_sub(npc_code, 1, 2))) %>% 
  left_join(systems) %>% 
  mutate(npc_system = str_to_title(npc_system),
         npc_sys_flor = str_sub(npc_code, 1, 3),
         npc_class = str_sub(npc_code, 1, 5),
         npc_class_name = if_else(!is.na(npc_class_name_rep), 
                                  npc_class_name_rep, npc_class_name),
         npc_type = if_else(nchar(npc_code) > 5, str_sub(npc_code, 1, 6),
                            NA_character_),
         npc_type_name = if_else(!is.na(npc_type_name_rep), npc_type_name_rep, 
                                 npc_type_name),
         npc_subtype = if_else(nchar(npc_code) > 6, str_sub(npc_code, 1, 7),
                               NA_character_)
         ) %>%
  select(-c(npc_code_rep, npc_class_name_rep, npc_type_name_rep)) %>% 
  rename(relnumb = dnr_releve_nbr) %>%
  filter(relnumb %in% releve_include & relnumb != "5307")


#### add geographic info and filter by ECS ####

# convert releve table to spatial object
releve_sf <- st_as_sf(releve2, coords = c("utm_easting", "utm_northing"),
                      crs = st_crs(spat_releve)) # NAD83/UTM zone 15N

# rename and select columns for ECS
ecs_subsec2 <- ecs_subsec %>%
  rename(ecs_provname = PROVNAME,
         ecs_provnum = ECS_PROV,
         ecs_secname = SECNAME,
         ecs_secnum = ECS_SEC,
         ecs_subname = SUBSECNAME,
         ecs_subnum = ECS_SUBSEC) %>%
  select(starts_with("ecs_"), Shape)

# add ECS info and wetland regions to releves
releve_sf2 <- releve_sf %>%
  st_join(ecs_subsec2, join = st_nn, maxdist = 3000, k = 1, progress = F,
          parallel = 4)

# convert back to table
releve3 <- as_tibble(releve_sf2) %>%
  cbind(st_coordinates(releve_sf2))

# filter by province
if(length(prov_include) > 0){
  
  releve4 <- releve3 %>%
    filter(ecs_provname %in% prov_include)
  
} else {
  
  releve4 <- releve3
  
}

# filter by section
if(length(sec_include) > 0){
  
  releve5 <- releve4 %>%
    filter(ecs_secname %in% sec_include)
  
} else {
  
  releve5 <- releve4
  
}

# filter by sub-section
if(length(subsec_include) > 0){
  
  releve6 <- releve5 %>%
    filter(ecs_subname %in% subsec_include)
  
} else {
  
  releve6 <- releve5
  
}


#### clean crosswalk attributes ####

# format outside of plot
# select releves also in releve table
# remove excluded taxa
# rename ID to match with MNTaxa
# add pcov midpoints
crosswalk2 <- crosswalk %>%
  mutate(outside_of_plot = case_when((str_detect(remarks, "OP") | remarks == "op")
                                     & str_detect(remarks, "ORIG") == F &
                                       str_detect(remarks, "OPTION") == F & 
                                       outside_of_plot == 0 ~ 1,
                                     TRUE ~ outside_of_plot),
         vitality = case_when(vitality == "DD" & 
                                remarks %in% c("DD & DY", "and DY", 
                                               "some D, DY", "Dead/dying",
                                               "or dying, no needles") ~ "DD/DY",
                              vitality == "DY" & 
                                remarks %in% c("DD/DY", "and DD", "Dead & dying",
                                               "Dying and/or dead") ~ "DD/DY",
                              is.na(vitality) & 
                                remarks %in% c("1 DD", "DD", "treated, dead",
                                               "Dead") ~ "DD",
                              is.na(vitality) & remarks == "EX" ~ "EX",
                              TRUE ~ vitality),
         condition = case_when(remarks %in% c("1FL,", "\"two plants flowering\"",
                                              "7/1979 flowering", "fl", 
                                              "flowering", "Few flowers",
                                              "See seed heads-1 flower", 
                                              "Tiny Flower",
                                              "forb with opposite leaves, axillary flowers",
                                              "fr-flowering", "in flower!",
                                              "large yellow flowers", 
                                              "opposite, small flower. ##",
                                              "small flower",
                                              "small white flower, small",
                                              "white flowered", "white flowers",
                                              "BL", "Bl", "bl") |
                                 (str_detect(remarks, "bloom") & 
                                    str_detect(remarks, "none found blooming") == F) |
                                 str_detect(remarks, "Bloom") ~ "FL",
                               remarks %in% c("1FR,", "FR", "FR #", "Fr.",
                                              "7/1979 fruit ripe",
                                              "\"Bl/Fr\"") ~ "FR",
                               remarks %in% c("1SP,", ">1SP", "SP 2", 
                                              "SP = forked trunk",
                                              "clonal sprouting") ~ "SP",
                               remarks %in% c("GR", "milky juice, grazed") ~ "GR",
                               remarks %in% c("Narrow strip in MW portion") ~ "MW",
                               remarks %in% c("SD", "seedling (SD)",
                                              "Seedling") ~ "SD",
                               remarks %in% c("SE") ~ "SE",
                               remarks %in% c("Flower stalks browsed off") ~ "BR",
                               TRUE ~ NA_character_)) %>%
  inner_join(releve6 %>%
               select(relnumb, lead_surveyor)) %>%
  filter(!(taxon %in% taxon_exclude)) %>%
  rename(taxon_id = lm_id) %>%
  left_join(pcov_conv)


#### remove crosswalk taxon and strata if "not data" ####

# format scov conversions
scov_conv2 <- scov_conv %>%
  rename(scov_mid = !!sym(midpoint_method))

# exclude outside-of-plot?
if(op_exclude == T){
  
  crosswalk3 <- crosswalk2 %>%
    filter(outside_of_plot == 0) %>%
    left_join(scov_conv2)
  
} else {
  
  crosswalk3 <- crosswalk2 %>%
    mutate(scov = if_else(outside_of_plot == 1 & 
                            !(physcode %in% c("D", "E") & maxht >= 5), # allow canopy trees to have scov 
                             "r", scov)) %>%
    left_join(scov_conv2)
  
}

# exclude dead/dying?
if(length(death_exclude) > 0){
  
  crosswalk4 <- crosswalk3 %>%
    filter(!(vitality %in% dead_exclude) | is.na(vitality))
  
} else {
  
  crosswalk4 <- crosswalk3
  
}

# exclude surveyors?
if(length(surveyor_exclude) > 0){
  
  crosswalk5 <- crosswalk4 %>%
    filter(!(lead_surveyor %in% surveyor_exclude))
  
} else {
  
  crosswalk5 <- crosswalk4
  
}

#### clean up pcov values ####

# combine L and Li as one physcode (the two codes weren't used in same releve)
# take most common, highest value within a strata, removing pcov = "X" (missing)
# use sum of scov to fill in remaining missing pcov values
# convert missing min and max ht to NA
crosswalk6 <- crosswalk5 %>%
  mutate(physcode_orig = physcode,
         physcode = case_when(physcode_orig == "Li" ~ "L",
                              TRUE ~ physcode_orig)) %>%
  group_by(relnumb, physcode, minht, maxht, outside_of_plot) %>%
  mutate(mode_pcov_mid = fmode(pcov_mid, ties = "max"), # defaults to na.rm = T
         sum_scov_mid = sum(scov_mid, na.rm = T)) %>%
  ungroup() %>%
  mutate(pcov_mid = if_else(!is.na(mode_pcov_mid), mode_pcov_mid, 
                                sum_scov_mid)) %>%
  select(-c(mode_pcov_mid, sum_scov_mid, pcov, pcov_ord)) %>%
  mutate(minht = as.numeric(minht),
         maxht = as.numeric(maxht)) # causes warnings because of non-numeric


#### resolve duplicate taxa within releve strata ####

# mntaxa taxa
mntaxa_taxa <- taxa_mntaxa(taxonomy_levels = T,
                           sources = F,
                           releve = T)

# add rank
# round dates
crosswalk7 <- crosswalk6 %>%
  left_join(mntaxa_taxa %>% 
              distinct(taxon_id, rank)) %>% 
  mutate(releve_create_date = round_date(releve_create_date), # round to second
         created_at = round_date(created_at),
         updated_at = round_date(updated_at),
         rank = replace_na(rank, "group"))

# check that group rank is correct
filter(crosswalk7, rank == "group") %>%
  distinct(taxon)

# identify duplicates based on taxon entered into database
crosswalk_dups <- crosswalk7 %>%
  get_dupes(relnumb, physcode, minht, maxht, taxon_id)

# if any of the duplicates are outside of the plot, but there are some
# inside of the plot, remove outside of the plot if the cover is "r"
# add info about other causes of duplication
crosswalk_dups2 <- crosswalk_dups %>%
  group_by(relnumb, physcode, minht, maxht, taxon_id) %>%
  mutate(op_vals = n_distinct(outside_of_plot)) %>%
  ungroup() %>%
  filter(op_vals == 1 | outside_of_plot == 0 | 
           (outside_of_plot == 1 & scov != "r")) %>%
  select(-op_vals) %>%
  mutate(remarks_non_numeric = if_else(str_detect(remarks, "[0-9]") == F &
                                         str_detect(remarks, 
                                                    paste(letters, 
                                                          collapse = "|")) == T,
                                       remarks, NA_character_)) %>%
  group_by(relnumb, physcode, minht, maxht, taxon_id) %>%
  mutate(vitalities = n_distinct(vitality),
         remarks_combined = paste(na.omit(remarks_non_numeric), 
                                  collapse = ", "),
         created_ats = n_distinct(created_at),
         updated_ats = n_distinct(updated_at),
         max_created = max(created_at),
         max_updated = max(updated_at)) %>%
  ungroup() 

# examine remarks
crosswalk_dups2 %>% 
  distinct(remarks_combined) %>% 
  data.frame()

# identify and resolve duplicates associated with multiple entries
# omit multiple vitalities or remarks that might indicate different morphology
crosswalk_dups_ent <- crosswalk_dups2 %>%
  filter(vitalities == 1 & (remarks_combined %in% c("", "dup entry") |  # would be good to double check this remarks_combined filter 
                              str_detect(remarks_combined, "orig")) &
           (created_ats > 1 | updated_ats > 1))  %>%
  mutate(keep = case_when(created_ats > 1 & updated_ats == 1 & 
                            created_at == max_created ~ 1,
                          updated_ats > 1 & updated_at == max_updated ~ 1,
                          TRUE ~ 0)) %>%
  filter(keep == 1) %>%
  select(colnames(crosswalk7)) %>%
  group_by(relnumb, physcode, minht, maxht, taxon_id) %>%
  mutate(reps = n()) %>% # may still have duplicates
  ungroup()

# update crosswalk_dups
# remove duplicates resolved through entry errors
crosswalk_dups3 <- crosswalk_dups2 %>%
  anti_join(crosswalk_dups_ent %>%
              select(relnumb, physcode, minht, maxht, taxon_id)) %>%
  full_join(crosswalk_dups_ent %>%
              filter(reps > 1))

# identify and resolve duplicates associated with potentially distinct 
# observations:
# taxonomic levels above species
# complex or sensu lato (best indicated by taxon + s.l.)
# multiple vitalities, remarks that suggest distinctions, or 
crosswalk_dups_dist <- crosswalk_dups3 %>%
  filter(!(rank %in% c("subspecies", "variety", "species")) | 
             str_detect(taxon, "\\ s\\.l\\.") | vitalities > 1 | 
           !(remarks_combined %in% c("", "dup entry") | 
               str_detect(remarks_combined, "orig"))) %>%
  group_by(relnumb, physcode, minht, maxht, pcov_mid, taxon_id, taxon, spcode,  # multiple pcov values can cause duplicates at end
           lm_id_new, taxon_new, spcode_new, rank, outside_of_plot) %>%
  summarize(relid = fmin(relid),
            soc = fmax(soc),
            vitality = paste(unique(vitality), collapse = ", "),
            remarks = paste(unique(remarks), collapse = ", "),
            condition = paste(unique(condition), collapse = ", "),
            scov_mid = sum(scov_mid, na.rm = T),
            releve_create_date = paste(unique(releve_create_date), collapse = ", "), 
            created_by = paste(unique(created_by), collapse = ", "), 
            created_at = paste(unique(created_at), collapse = ", "), 
            updated_by = paste(unique(updated_by), collapse = ", "), 
            updated_at = paste(unique(updated_at), collapse = ", "),
            .groups = "drop")

# are any remaining due to multiple ID's, one taxon?
crosswalk_dups3 %>%
  anti_join(crosswalk_dups_dist %>%
              select(relnumb, physcode, minht, maxht, taxon)) %>%
  distinct(relnumb, physcode, minht, maxht, taxon, taxon_id) %>%
  get_dupes(relnumb, physcode, minht, maxht, taxon)
# no

# for all remaining, take row with reliability, then cover, then sociality
crosswalk_dups_rem <- crosswalk_dups3 %>%
  anti_join(crosswalk_dups_dist %>%
              select(relnumb, physcode, minht, maxht, taxon_id)) %>%
  group_by(relnumb, physcode, minht, maxht, taxon) %>%
  mutate(min_relid = min(relid)) %>% 
  ungroup() %>%
  filter(relid == min_relid) %>% # select the row that's the most reliable
  group_by(relnumb, physcode, minht, maxht, taxon_id) %>%
  mutate(max_scov = fmax(scov_mid)) %>%
  ungroup() %>%
  filter(scov_mid == max_scov) %>% # for remaining duplicates, select the cover that's the highest
  group_by(relnumb, physcode, minht, maxht, taxon_id) %>%
  mutate(max_soc = fmax(soc)) %>%
  ungroup() %>%
  filter(soc == max_soc | is.na(max_soc)) %>% # for remaining duplicates, select the most dense
  select(colnames(crosswalk7)) %>%
  group_by(relnumb, physcode, minht, maxht, pcov_mid, taxon_id, taxon, spcode, # collapse all remaining
           lm_id_new, taxon_new, spcode_new, rank, outside_of_plot) %>%
  summarize(relid = unique(relid),
            scov = unique(scov),
            soc = unique(soc),
            vitality = paste(unique(vitality), collapse = ", "),
            remarks = paste(unique(remarks), collapse = ", "),
            condition = paste(unique(condition), collapse = ", "),
            releve_create_date = paste(unique(releve_create_date), collapse = ", "), 
            created_by = paste(unique(created_by), collapse = ", "), 
            created_at = paste(unique(created_at), collapse = ", "), 
            updated_by = paste(unique(updated_by), collapse = ", "), 
            updated_at = paste(unique(updated_at), collapse = ", "),
            .groups = "drop") %>%
  left_join(scov_conv2)

# combine resolved datasets and check for duplicates
# dups_dist only has scov_mid -- remove all other forms
crosswalk_dups_res <- crosswalk_dups_ent %>%
  mutate(releve_create_date = as.character(releve_create_date),
         created_at = as.character(created_at),
         updated_at = as.character(updated_at)) %>%
  filter(reps == 1) %>%
  select(-reps) %>%
  full_join(crosswalk_dups_dist) %>%
  full_join(crosswalk_dups_rem) %>% 
  select(-c(scov, scov_ord, scov_mid_bb64))

# remaining duplicates?
crosswalk_dups_res %>%
  get_dupes(relnumb, physcode, minht, maxht, taxon_id) %>%
  select(relnumb, physcode, minht, taxon, scov_mid, outside_of_plot)
# all are trees that are both inside and outside of plot
# if needed, save this list for reference

# all duplicates addressed?
# manually enter remaining duplicates (line above)
nrow(crosswalk_dups_res) - 3 == crosswalk_dups %>%
  distinct(relnumb, physcode, minht, maxht, taxon) %>%
  nrow()

# crosswalk dupes not addressed
crosswalk_dups %>%
  distinct(relnumb, physcode, minht, maxht, taxon) %>% 
  anti_join(crosswalk_dups_res) %>% 
  inner_join(crosswalk7) %>% 
  data.frame()
  
# update duplicates
crosswalk8 <- crosswalk7 %>%
  anti_join(crosswalk_dups) %>%
  mutate(releve_create_date = as.character(releve_create_date),
         created_at = as.character(created_at),
         updated_at = as.character(updated_at)) %>% 
  select(colnames(crosswalk_dups_res)) %>%
  full_join(crosswalk_dups_res)


#### add taxonomic and analysis groups ####

# get taxonomic groups with strata codes
mntaxa_acc <- lookup_mntaxa(taxonomy_levels = F,
                            sources = F,
                            releve = T,
                            phys = F,
                            strata = T,
                            origin = F,
                            common = F,
                            cvals = F,
                            exclude = F,
                            replace_sub_var = T,
                            replace_family = T,
                            replace_genus = T,
                            drop_higher = T,
                            higher_include = c("Belonia",
                                               "Chara",
                                               "Lychnothamnus",
                                               "Nitella",
                                               "Nitellopsis",
                                               "Spirogyra",
                                               "Tolypella"),
                            excluded_duplicates = T,
                            clean_duplicates = F,
                            group_accepted = T,
                            group_analysis = T) %>%
  rename(taxon_name = taxon) %>%
  select(-rank) # already added with mntaxa_taxa

# add MNtaxa accepted names, analysis groups, and physcode
crosswalk9 <- crosswalk8 %>%
  select(-analysis_group) %>%
  inner_join(mntaxa_acc) %>%
  mutate(physcode_orig = physcode,
         physcode = if_else(!is.na(acc_physcode), acc_physcode, physcode))

# taxa removed
releve_taxa_removed <- crosswalk8 %>% 
  distinct(relnumb, taxon_id, taxon, rank) %>% 
  anti_join(crosswalk9) %>% 
  left_join(crosswalk8 %>%
              distinct(relnumb, taxon_id, taxon, physcode)) %>%
  arrange(taxon)

# taxa removed by rank
count(releve_taxa_removed, rank)

# species removed
releve_taxa_removed %>%
  filter(rank == "species") %>%
  count(physcode)
# all lichen and moss except one

# subspecies/varieties
releve_taxa_removed %>%
  filter(rank %in% c("subspecies", "variety")) %>%
  count(physcode)
# all mosses

# group
releve_taxa_removed %>%
  filter(rank == "group") %>%
  distinct(taxon, physcode)
# groups that are too broad

# genera
releve_taxa_removed %>%
  filter(rank == "genus") %>%
  count(physcode)


#### filter physcode and reliability ####

# pattern for combined physcodes
phys_pattern <- paste0("\\b(", paste(physcode_include, collapse = "|"), ")\\b")

# filter
crosswalk10 <- crosswalk9 %>%
  filter(str_detect(physcode, phys_pattern) &
           relid <= relid_thresh)

# see omitted physcodes
crosswalk9 %>%
  anti_join(crosswalk10) %>%
  count(physcode)
# A


#### divide height classes into strata ####

# use originally entered strata if none specified
if(stratify == "none"){
  
  # flip min and max ht if they're reversed
  # sum scov of analysis codes in same releves
  crosswalk11 <- crosswalk10 %>%
    mutate(minht_orig = minht,
           maxht_orig = maxht,
           minht = if_else(maxht_orig < minht_orig, maxht_orig, minht_orig),
           maxht = if_else(maxht_orig < minht_orig, minht_orig, maxht_orig)) %>%
    select(-c(minht_orig, maxht_orig)) %>% 
    group_by(relnumb, analysis_group) %>%
    summarize(scov = sum(scov_mid, na.rm = T), 
              .groups = "drop")
  
} else {
  
  # get physcodes in data
  crosswalk_phys <- crosswalk10 %>%
    distinct(physcode, acc_stratacode)
  
  # no stratification
  nostrata_phys <- crosswalk_phys %>%
    filter(str_detect(physcode, "D|E") == F | !is.na(acc_stratacode)) %>%
    mutate(strata_lower = 1,
           strata_upper = 8)
  
  # create strata tables
  if(stratify == "nonpeat"){
    
    strata_phys <- crosswalk_phys %>%
      filter(str_detect(physcode, "D|E") & is.na(acc_stratacode)) %>%
      expand_grid(tibble(strata_lower = c(1, 4, 6),
                         strata_upper = c(3, 5, 8)))
    
  } else {
    
    strata_phys <- crosswalk_phys %>%
      filter(str_detect(physcode, "D|E") & is.na(acc_stratacode)) %>%
      expand_grid(tibble(strata_lower = c(1, 5),
                         strata_upper = c(4, 8)))
    
  }
  
  # combine
  strata_include <- nostrata_phys %>%
    full_join(strata_phys)
  
  # flip min and max ht if they're reversed
  # manage missing values
  # match ht leves with strata to reduce dataset size before reframe
  # divide scov over its vertical extent
  # expand rows, one for each height level
  # select height levels within strata
  # calculate scov for that height level
  # sum across the same analysis codes and height levels within stratum
  crosswalk11a <- crosswalk10 %>%
    mutate(minht_orig = minht,
           maxht_orig = maxht,
           minht = if_else(maxht_orig < minht_orig, maxht_orig, minht_orig),
           maxht = if_else(maxht_orig < minht_orig, minht_orig, maxht_orig)) %>%
    select(-c(minht_orig, maxht_orig)) %>%
    left_join(strata_include, relationship = "many-to-many") %>% # add strata of interest to each physcode
    filter((strata_lower == 1 & strata_upper == 8) | # keep all entries, even NA if all height levels are in stratum
             (!is.na(minht) & !is.na(maxht))) %>% # for all remaining strata, require non-missing
    filter((minht >= strata_lower & minht <= strata_upper) | # select all overlap between observed and desired strata
             (strata_lower >= minht & strata_lower <= maxht) |
             (strata_lower == 1 & strata_upper == 8)) %>%
    mutate(minht = ifelse(is.na(minht) & strata_lower == 1 &
                            strata_upper == 8, 1, minht),
           maxht = ifelse(is.na(maxht) & strata_lower == 1 &
                            strata_upper == 8, 8, maxht)) %>%
    left_join(ht_conv %>%
                rename(minht = ht) %>%
                select(minht, ht_min_m)) %>%
    left_join(ht_conv %>%
                rename(maxht = ht) %>%
                select(maxht, ht_max_m)) %>%
    mutate(max_min_range = ht_max_m - ht_min_m,
           scov_per_m = scov_mid / max_min_range) %>% # divide scov over all meters in observed range
    group_by(relnumb, physcode, acc_stratacode, minht, maxht, scov_per_m,
             taxon_id, taxon, outside_of_plot, physcode_orig, # unique observation identifiers
             analysis_group, strata_lower, strata_upper) %>%
    reframe(ht = seq(minht, maxht)) %>% # expand rows to include every height level within range
    filter(ht >= strata_lower & ht <= strata_upper) %>% # select height levels within strata
    left_join(ht_conv %>%
                mutate(ht_range = ht_max_m - ht_min_m) %>%
                select(ht, ht_range)) %>%
    mutate(scov_ht = scov_per_m * ht_range) %>%  # scov proportional to m in ht level
    group_by(relnumb, analysis_group, strata_lower, strata_upper) %>% # sum across analysis codes in same ht level (overlapping height classes, different physcodes) and over all ht levels for stratum
    summarize(scov = sum(scov_ht, na.rm = T), # sum scov of analysis codes in same stratum
              .groups = "drop") %>%
    mutate(analysis_group_strata = paste0(analysis_group, strata_lower, strata_upper),
           code_strata = analysis_group_strata %>% 
             str_replace_all("\\ |\\/|-", "_") %>% 
             str_remove_all("\\(|\\)|\\.") %>%
             str_remove("18"))
  
  # replace strata numbers with names
  if(stratify == "nonpeat"){
    
    crosswalk11 <- crosswalk11a %>%
      mutate(analysis_group_strata = str_replace(analysis_group_strata, "13", " understory") %>% 
               str_replace("45", " subcanopy") %>%
               str_replace("68", " canopy") %>%
               str_remove("18"),
             code_strata = str_replace(code_strata, "13", "_understory") %>% 
               str_replace("45", "_subcanopy") %>%
               str_replace("68", "_canopy"))
    
  } else {
    
    crosswalk11 <- crosswalk11a %>%
      mutate(analysis_group_strata = str_replace(analysis_group_strata, "14", " understory") %>% 
               str_replace("58", " canopy") %>%
               str_remove("18"),
             code_strata = str_replace(code_strata, "14", "_understory") %>% 
               str_replace("58", "_canopy"))
    
    
  }

}

# check characters in code strata
crosswalk11 %>% 
  distinct(code_strata) %>% 
  pull(code_strata) %>% 
  regmatches(gregexpr("[^A-Za-z0-9]", .)) %>% 
  unlist() %>% 
  unique()


#### example data and releve ####

# make sure releve numbers only occur once in dataset
get_dupes(releve6, relnumb)

# # mask releve numbers
# releve_mask <- releve6 %>%
#   transmute(relnumb_orig = relnumb,
#             relnumb = sample(releve6$relnumb, nrow(releve6), replace = F) %>%
#               as.factor() %>%
#               as.numeric() %>%
#               as.character())
# 
# # save
# write_csv(releve_mask, "../npc-releve/data/releve_masking_crosswalk_20260208.csv")
releve_mask <- read.csv("../npc-releve/data/releve_masking_crosswalk_20260208.csv") %>%
  mutate(relnumb = as.character(relnumb))

# import releve info
stcroix <- readxl::read_excel("../npc-releve/data/duxbury_deer_releve_data_working.xlsx")
resample <- readxl::read_excel("../releve-resample/data/releve_list_southern_20251120.xlsx")

# get releves from database
rel_ex1 <- releve6 %>%
  filter(relnumb %in% stcroix$RELNO) %>%
  mutate(place_name = if_else(place_name == "", NA_character_, place_name),
         original_releve_nbr = if_else(original_releve_nbr == "",
                                       NA_character_, original_releve_nbr)) %>%
  transmute(group = case_when(is.na(place_name) ~ "Control",
                              str_detect(place_name, "Outside Exclosure") ~ 
                                "Outside Exclosure",
                              str_detect(place_name, "Exclosure") ~ 
                                "Inside Exclosure"),
            year = year(date_), # artificially set same initial year for all
            year = case_when(year <= 2004 ~ 1,
                             year <= 2010 ~ 2,
                             TRUE ~ 3) %>% 
              as.integer(),
            relnumb_orig = if_else(!is.na(original_releve_nbr), 
                                   original_releve_nbr,
                                   relnumb),
            relnumb = relnumb) %>% 
  relocate(year)

# format resample data
resample2 <- resample %>%
  filter(str_detect(npc, "MHs38|MHs39")) %>% 
  rename(relnumb_orig = orig_relnumb) %>% # releve number used for example (original)
  full_join(resample %>%
              filter(str_detect(npc, "MHs38|MHs39")) %>% 
              select(-relnumb) %>% 
              rename(relnumb = orig_relnumb) %>% 
              mutate(relnumb_orig = relnumb)) %>% # baseline data
  mutate(group = if_else(str_detect(npc, "MHs38"), "Oak-Basswood Group", 
                         "Maple-Basswood Group"))

rel_ex2 <- releve6 %>% 
  inner_join(resample2 %>% 
               select(relnumb_orig, relnumb, group)) %>% 
  transmute(year = year(date_),
            year = if_else(year < 2018, 1, 2) %>% 
              as.integer(),  
            group = group,
            relnumb_orig = relnumb_orig,
            relnumb = relnumb)

# get taxa from database
# convert hybrid names
rel_tax1 <- rel_ex1 %>% 
  inner_join(crosswalk %>%
               distinct(relnumb, physcode, minht, maxht, taxon, scov,
                        outside_of_plot)) %>% 
  select(-relnumb) %>% 
  left_join(releve_mask) %>%
  select(-relnumb_orig) %>%
  mutate(outside_of_plot = fct_recode(as.character(outside_of_plot),
                                       "t" = "1",
                                      "f" = "0") %>%
           as.character()) %>%
  relocate(relnumb, .after = "group")

rel_tax2 <- rel_ex2 %>% 
  inner_join(crosswalk %>%
               select(relnumb, physcode, minht, maxht, taxon, scov,
                      outside_of_plot)) %>% 
  select(-relnumb) %>% 
  left_join(releve_mask) %>%
  select(-relnumb_orig) %>%
  mutate(outside_of_plot = fct_recode(as.character(outside_of_plot),
                                      "t" = "1",
                                      "f" = "0") %>%
           as.character()) %>%
  relocate(relnumb, .after = "group")

# check
count(rel_tax1, year, group, relnumb)
count(rel_tax2, year, group, relnumb) %>% 
  arrange(relnumb, year, group) %>% 
  data.frame()

# add species
mnnpc_example_data <- list("St. Croix State Forest" = rel_tax1 %>%
                             as.data.frame(),
                           "Earthworm-Invaded Forests" = rel_tax2 %>% 
                             as.data.frame())

# check for NAs
mnnpc_example_data$`St. Croix State Forest` %>%
  filter(if_any(everything(), is.na))

mnnpc_example_data$`Earthworm-Invaded Forests` %>%
  filter(if_any(everything(), is.na))

# check structure
str(mnnpc_example_data$`St. Croix State Forest`)
str(mnnpc_example_data$`Earthworm-Invaded Forests`)

# save
save(mnnpc_example_data, file = "data-raw/data-out-ak/mnnpc_example_data.rds")
load("data-raw/data-out-ak/mnnpc_example_data.rds")

# select one releve for formatting example
# allow taxa without accepted names
mnnpc_example_releve <- rel_tax1 %>% 
  filter(relnumb == "582")

# save
save(mnnpc_example_releve, file = "data-raw/data-out-ak/mnnpc_example_releve.rds")
load("data-raw/data-out-ak/mnnpc_example_releve.rds")


#### prepare for saving in package ####

# grouped data
# mask releve numbers
crosswalk11_prepared <- crosswalk11 %>%
  rename(relnumb_orig = relnumb) %>%
  left_join(releve_mask) %>%
  select(-relnumb_orig) %>%
  relocate(relnumb)

# ungrouped data
# mask releve numbers
crosswalk10_prepared <- crosswalk10 %>%
  rename(relnumb_orig = relnumb) %>%
  left_join(releve_mask) %>%
  group_by(relnumb, physcode, minht, maxht, taxon, acc_assignment,
           outside_of_plot) %>%
  summarize(scov = sum(scov_mid, na.rm = T),
            .groups = "drop")

# mask releve numbers
# select required info
releve7 <- releve6 %>%
  rename(relnumb_orig = relnumb) %>%
  left_join(releve_mask) %>%
  select(-relnumb_orig) %>%
  select(relnumb, used_in_fieldguide, npc_code, npc_system_id, npc_system, 
         npc_sys_flor,npc_class, npc_class_name, npc_type, npc_type_name,
         npc_subtype, npc_subtype_name, ecs_secname)


#### save ####

# rename and save as data frame
releve_species_grouped <- crosswalk11_prepared %>%
  as.data.frame()
releve_species_ungrouped <- crosswalk10_prepared %>%
  as.data.frame()
releve_plots <- releve7 %>%
  as.data.frame()

save(releve_species_grouped, 
     file = "data-raw/data-out-ak/releve_species_grouped_data.rds")
save(releve_species_ungrouped, 
     file = "data-raw/data-out-ak/releve_species_ungrouped_data.rds")
save(releve_plots, file = "data-raw/data-out-ak/releve_plot_data.rds")

# # save original data in case needed
# save(crosswalk, file = "../npc-releve/data/originals-20260208/crosswalk.rds")
# save(releve, file = "../npc-releve/data/originals-20260208/releve.rds")
# save(spat_releve, file = "../npc-releve/data/originals-20260208/spatial_releve.rds")
# save(ecs_subsec, file = "../npc-releve/data/originals-20260208/spatial_ecs_subsec.rds")
