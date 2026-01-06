#### set-up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(janitor)
library(readxl)
library(sf)
library(khroma)
library(polylabelr)

# import data
crosswalk <- read_csv("../../intermediate-data/crosswalk_table_formatted_20251224a.csv",
                      guess_max = Inf) 
crosswalk_raw <- read_csv("../../intermediate-data/crosswalk_raw_no_dups_20251224a.csv",
                          guess_max = Inf)
releve <- read_csv("../../intermediate-data/releve_table_formatted_20251224a.csv")
stcroix <- read_excel("../../data/duxbury_deer_releve_data_working.xlsx")
resample <- read_excel("../../../releve-resample/data/releve_list_southern_20251120.xlsx")
mntaxa_lookup <- read_csv("../../intermediate-data/mntaxa_lookup_20251214.csv")
mntaxa <- read_csv("../../intermediate-data/mntaxa_taxa_20251214.csv")
mntaxa_dlist <- read_csv("../../intermediate-data/mntaxa_synonymies_20251214.csv")
analysis_codes <- read_csv("../../intermediate-data/analysis_codes_20251214.csv")
releve_taxa <- read_csv("../../intermediate-data/releve_taxa_formatted_20251214a.csv")
mntaxa_lookup_genus <- read_csv("../../intermediate-data/mntaxa_lookup_genera_20251214.csv")
mntaxa_dlist_pars <- read_csv("../../intermediate-data/mntaxa_dlist_parents_20251224.csv")
ecs_sec_sf <- st_read("V:/gdrs/data/pub/us_mn_state_dnr/geos_ecological_class_system/fgdb/geos_ecological_class_system.gdb", 
                      "ecs_sections_of_mn_v99a")

#### format releve data ####

# system info
npc_systems <- releve %>% 
  distinct(npc_system_id, npc_system) %>% 
  mutate(npc_system = str_to_title(npc_system))

# section info
ecs_sections <- tibble(ecs_secabb = c("LAP",
                                      "MIM",
                                      "MOP",
                                      "MDL",
                                      "CGP",
                                      "NSU",
                                      "PPL",
                                      "RRV",
                                      "SSU",
                                      "WSU"),
                       ecs_secname = c("Lake Agassiz, Aspen Parklands",
                                       "Minnesota & NE Iowa Morainal",
                                       "N. Minnesota & Ontario Peatlands",
                                       "N. Minnesota Drift & Lake Plains",
                                       "North Central Glaciated Plains",
                                       "Northern Superior Uplands",
                                       "Paleozoic Plateau",
                                       "Red River Valley",
                                       "Southern Superior Uplands",
                                       "Western Superior Uplands"))

# add NPC and section info
releve2 <- releve %>%
  select(-npc_system) %>% 
  mutate(npc_class = str_sub(npc_code, 1, 5),
         npc_type = if_else(nchar(npc_code) > 5, str_sub(npc_code, 1, 6),
                            NA_character_),
         npc_subtype = if_else(nchar(npc_code) > 6, str_sub(npc_code, 1, 7),
                               NA_character_),
         npc_system_id = if_else(str_detect(npc_system_id, "_|\\/"),
                                 npc_system_id,
                                 str_sub(npc_system_id, 1, 2)),
         npc_sys_flor = str_sub(npc_code, 1, 3)) %>% 
  left_join(npc_systems) %>% 
  left_join(ecs_sections)

# add classification
# format species names
crosswalk2 <- crosswalk %>%
  left_join(releve2 %>%
               select(relnumb, npc_system, npc_system_id, npc_class_name,
                      npc_class, npc_type_name, npc_type, npc_subtype_name, 
                      npc_subtype, ecs_secabb))

# select releves
releve3 <- releve2 %>%
  filter(used_in_fieldguide == "yes")

crosswalk3 <- crosswalk2 %>% 
  filter(relnumb %in% releve3$relnumb)

# are all classified to class?
sort(unique(releve3$npc_class)) # yes
         
# check that there aren't duplicates
get_dupes(releve3, relnumb)
get_dupes(crosswalk3, relnumb, code_strata)

# are the releve numbers equal?
nrow(releve3) == n_distinct(crosswalk3$relnumb)

# all ECS sections
all_sections <- releve3 %>% 
  distinct(ecs_secabb) %>% 
  pull(ecs_secabb)


#### floristic tables ####

# look at example
load("../RMAVIS/data/nvc_floristic_tables.rda")
head(nvc_floristic_tables)

flor_fun <- function(ecs_sec = all_sections){
  
  # select samples in section
  releve_sub <- releve3 %>% 
    filter(ecs_secabb %in% ecs_sec)
  
  crosswalk_sub <- crosswalk3 %>% 
    filter(relnumb %in% releve_sub$relnumb)

  # samples per group
  samps_class <- releve_sub %>%
    count(npc_class)

  samps_type <- releve_sub %>%
    filter(!is.na(npc_type)) %>%
    count(npc_type)

  samps_subtype <- releve_sub %>%
    filter(!is.na(npc_subtype)) %>%
    count(npc_subtype)

  # percentage of data classified to type
  print(paste("samples classified to type:",
              sum(samps_type$n) / nrow(releve_sub)))

  perc_rels_type <- releve_sub %>%
    group_by(npc_class) %>%
    summarize(rels_tot = n_distinct(relnumb),
              rels_type = sum(!is.na(npc_type)),
              .groups = "drop") %>%
    mutate(perc_type = rels_type / rels_tot)

  # percentage of data classified to subtype
  print(paste("samples classified to subtype:",
              sum(samps_subtype$n) / nrow(releve_sub)))

  perc_rels_subtype <- releve_sub %>%
    group_by(npc_class) %>%
    summarize(rels_tot = n_distinct(relnumb),
              rels_subtype = sum(!is.na(npc_subtype)),
              .groups = "drop") %>%
    mutate(perc_type = rels_subtype / rels_tot)

  # floristic tables
  flor_type <- crosswalk_sub %>%
    filter(!is.na(npc_type)) %>%
    group_by(npc_class, npc_type, analysis_group_strata) %>%
    summarize(absolute_frequency = n_distinct(relnumb),
              minimum_cover = min(scov_adj),
              mean_cover = mean(scov_adj),
              maximum_cover = max(scov_adj),
              .groups = "drop") %>%
    left_join(samps_type) %>%
    mutate(relative_frequency = absolute_frequency / n,
           constancy = case_when(relative_frequency >= 0.81 ~ 5,
                                 relative_frequency >= 0.61 ~ 4,
                                 relative_frequency >= 0.41 ~ 3,
                                 relative_frequency >= 0.21 ~ 2,
                                 TRUE ~ 1))

  # species with at least 5% frequency in any type, keep for class
  flor_type_spp <- flor_type %>%
    filter(relative_frequency >= 0.05) %>%
    distinct(npc_class, analysis_group_strata)

  # only keep more common species
  flor_type2 <- flor_type %>%
    inner_join(flor_type_spp) %>%
    select(npc_type, analysis_group_strata, constancy, absolute_frequency,
           relative_frequency, minimum_cover, mean_cover, maximum_cover)

  # class tables
  # keep species in at least 5% of a type's samples or 5% of the class (if no types)
  flor_class <- crosswalk_sub %>%
    group_by(npc_class, analysis_group_strata) %>%
    summarize(absolute_frequency = n_distinct(relnumb),
              minimum_cover = min(scov_adj),
              mean_cover = mean(scov_adj),
              maximum_cover = max(scov_adj),
              .groups = "drop") %>%
    left_join(samps_class) %>%
    mutate(relative_frequency = absolute_frequency / n,
           constancy = case_when(relative_frequency >= 0.81 ~ 5,
                                 relative_frequency >= 0.61 ~ 4,
                                 relative_frequency >= 0.41 ~ 3,
                                 relative_frequency >= 0.21 ~ 2,
                                 TRUE ~ 1)) %>%
    left_join(flor_type_spp %>%
                mutate(keep = 1)) %>%
    mutate(keep = if_else(!(npc_class %in% flor_type_spp$npc_class) &
                            relative_frequency >= 0.05, 1, keep)) %>%
    filter(keep == 1) %>%
    select(npc_class, analysis_group_strata, constancy, absolute_frequency,
           relative_frequency, minimum_cover, mean_cover, maximum_cover)

  # species kept
  flor_class_spp <- flor_class %>%
    distinct(npc_class, analysis_group_strata)

  flor_subtype <- crosswalk_sub %>%
    filter(!is.na(npc_subtype)) %>%
    group_by(npc_class, npc_type, npc_subtype, analysis_group_strata) %>%
    summarize(absolute_frequency = n_distinct(relnumb),
              minimum_cover = min(scov_adj),
              mean_cover = mean(scov_adj),
              maximum_cover = max(scov_adj),
              .groups = "drop") %>%
    left_join(samps_subtype) %>%
    mutate(relative_frequency = absolute_frequency / n,
           constancy = case_when(relative_frequency >= 0.81 ~ 5,
                                 relative_frequency >= 0.61 ~ 4,
                                 relative_frequency >= 0.41 ~ 3,
                                 relative_frequency >= 0.21 ~ 2,
                                 TRUE ~ 1)) %>%
    inner_join(flor_class_spp) %>%
    select(npc_subtype, analysis_group_strata, constancy, absolute_frequency,
           relative_frequency, minimum_cover, mean_cover, maximum_cover)

  # combine
  flor_comb <- flor_class %>%
    rename(npc_code = npc_class) %>%
    rbind(flor_type2 %>%
            rename(npc_code = npc_type)) %>%
    rbind(flor_subtype %>%
            rename(npc_code = npc_subtype)) %>%
    rename(npc_taxon_name = analysis_group_strata)

    # add ecs section
    if(length(ecs_sec) > 1){

     flor_comb2 <- flor_comb

    } else {

      flor_comb2 <- flor_comb %>%
        mutate(npc_code = paste(npc_code, ecs_sec, sep = "_"))

    }

    # output
    return(flor_comb2)
    
}

# floristic tables statewide and by region
flor_tab_state <- flor_fun()
flor_tab_LAP <- flor_fun("LAP")
flor_tab_MIM <- flor_fun("MIM")
flor_tab_MOP <- flor_fun("MOP")
flor_tab_MDL <- flor_fun("MDL")
flor_tab_CGP <- flor_fun("CGP")
flor_tab_NSU <- flor_fun("NSU")
flor_tab_PPL <- flor_fun("PPL")
flor_tab_RRV <- flor_fun("RRV")
flor_tab_SSU <- flor_fun("SSU")
flor_tab_WSU <- flor_fun("WSU")

# combine
mnnpc_floristic_tables <- flor_tab_state %>% 
  rbind(flor_tab_LAP) %>% 
  rbind(flor_tab_MIM) %>% 
  rbind(flor_tab_MOP) %>% 
  rbind(flor_tab_MDL) %>% 
  rbind(flor_tab_CGP) %>% 
  rbind(flor_tab_NSU) %>% 
  rbind(flor_tab_PPL) %>% 
  rbind(flor_tab_RRV) %>% 
  rbind(flor_tab_SSU) %>% 
  rbind(flor_tab_WSU)

# check for NAs
mnnpc_floristic_tables %>%
  filter(if_any(everything(), is.na))

# save
save(mnnpc_floristic_tables, file = "data-raw/data-out-ak/mnnpc_floristic_tables.rds")
load("data-raw/data-out-ak/mnnpc_floristic_tables.rds")


#### look-up table and hybrid crosswalk ####

# look at example
UKVegTB::taxa_lookup %>% 
  as_tibble()
# all taxa, recommended taxon_name = dlist, recommended TVK = dlist_id

# duplicate group due to different IDs
mntaxa_dlist %>% 
  distinct(taxon, dlist_assignment) %>% 
  get_dupes(taxon)

# remove taxa from mntaxa that are in analysis codes
# combine MNTaxa and analysis codes
# used Minnesota Wildflowers and other sites choose the analysis group that is 
# most likely in new releves when there are duplicate analysis groups for a
# taxon
taxa_crosswalk <- mntaxa_dlist %>% 
  filter(!(taxon_id %in% analysis_codes$taxon_id)) %>% 
  select(-c(rank)) %>% 
  mutate(analysis_group = dlist_assignment) %>% 
  full_join(analysis_codes %>% 
              rename(analysis_group = analysis_code)) %>% 
  distinct() %>% 
  left_join(mntaxa %>% 
              distinct(taxon_id, hybrid)) %>% 
  filter(!(taxon == "Arabis holboellii" & 
               analysis_group == "Boechera retrofracta") &
           !(taxon == "Botrychium lunaria" & 
               analysis_group == "Botrychium crenulatum") &
           !(taxon == "Cardamine pratensis" & 
               analysis_group == "Cardamine dentata") &
           !(taxon == "Cerastium brachypodum" &
               analysis_group == "Cerastium nutans") &
           !(taxon == "Chamerion angustifolium subsp. angustifolium" &
               analysis_group == "Eriophorum angustifolium") &
           !(taxon == "Cuscuta campestris" &
               analysis_group == "Cuscuta pentagona") &
           !(taxon == "Quercus x schuettei" &
               analysis_group == "Quercus x hillii") &
           !(taxon == "Scirpus pendulus" &
               analysis_group == "Scirpus atrocinctus/cyperinus/pedicellatus")) %>% 
  distinct() %>% 
  mutate(analysis_group_includes = case_when(str_detect(analysis_group,
                                                      "and genus") ~
                                           paste(word(dlist_assignment, 1),
                                                 dlist_assignment,
                                                 sep = "/"),
                                           str_detect(analysis_group,
                                                      "genus") ~
                                             paste(word(dlist_assignment, 1),
                                                   "genus and species"),
                                           TRUE ~ dlist_assignment))

# taxa added back in with analysis groups
# all are genus level
# some mapped to species, other to genus
analysis_taxa <- taxa_crosswalk %>% 
  anti_join(mntaxa_lookup %>% 
              distinct(taxon)) %>% 
  rename(c_value = dlist_c_value)

# use info for species-level when it's mapped to a species and genus level otherwise
# if missing higher orders, fill in with genus
analysis_taxa2 <- analysis_taxa %>% 
  filter(dlist_rank != "species") %>% 
  select(taxon_id, taxon, analysis_group, analysis_group_includes) %>% 
  left_join(mntaxa_lookup_genus) %>% 
  full_join(analysis_taxa %>% 
              filter(dlist_rank == "species") %>% 
              left_join(mntaxa %>% 
                          distinct(taxon_id, rank)) %>% 
              left_join(mntaxa_lookup %>% 
                          select(starts_with("dlist")) %>% 
                          distinct())) %>% 
  mutate(dlist_genus = if_else(is.na(dlist_genus), word(dlist_assignment, 1),
                               dlist_genus)) %>% 
  left_join(mntaxa_lookup %>% 
              select(dlist_genus:dlist_kingdom) %>% 
              distinct() %>% 
              rename_with(.cols = -dlist_genus,
                          .fn = ~paste0(.x, "_rep"))) %>% 
  mutate(dlist_family = if_else(is.na(dlist_family), dlist_family_rep,
                                dlist_family),
         dlist_order = if_else(is.na(dlist_order), dlist_order_rep,
                                dlist_order),
         dlist_class = if_else(is.na(dlist_class), dlist_class_rep,
                                dlist_class),
         dlist_phylum = if_else(is.na(dlist_phylum), dlist_phylum_rep,
                                dlist_phylum),
         dlist_kingdom = if_else(is.na(dlist_kingdom), dlist_kingdom_rep,
                                dlist_kingdom)) %>% 
  select(-ends_with("rep"))

# check for duplicates
taxa_crosswalk %>% 
  select(-taxon_id) %>% 
  distinct() %>% 
  get_dupes(taxon) %>% 
  data.frame()
# two hybrid statuses

analysis_taxa2 %>% 
  select(-taxon_id) %>% 
  distinct() %>% 
  get_dupes(taxon) %>% 
  data.frame()

# genera with no analysis group
# remove if they show up in the taxon or dlist_assignment columns
mntaxa_lookup_genus2 <- mntaxa_lookup_genus %>% 
  anti_join(taxa_crosswalk %>% 
              distinct(taxon)) %>% 
  anti_join(taxa_crosswalk %>% 
              distinct(dlist_assignment) %>% 
              rename(taxon = dlist_assignment))

# select column
# add analysis group taxa
# format hybrid columns
# add analysis groups
# add genera - okay to use lookup instead of dlist because these shouldn't be mapped to analysis groups
mntaxa_lookup2 <- mntaxa_lookup %>% 
  select(taxon, hybrid, rank, c_value, starts_with("dlist")) %>%
  distinct() %>% 
  left_join(taxa_crosswalk %>% 
              distinct(taxon, analysis_group, analysis_group_includes)) %>% 
  full_join(analysis_taxa2 %>% 
              select(taxon, hybrid, rank, c_value, starts_with("dlist"), 
                     analysis_group, analysis_group_includes) %>%
              distinct()) %>% 
  full_join(mntaxa_lookup_genus2 %>% 
              select(taxon, hybrid, rank, c_value, starts_with("dlist")) %>%
              distinct()) %>% 
  mutate(hybrid = if_else(!is.na(hybrid), 1, 0),
         dlist_hybrid = if_else(!is.na(dlist_hybrid), 1, 0))

# for hybrids, crosswalk various versions of the x
hybrid_crosswalk <- mntaxa_lookup2 %>% 
  filter(hybrid == 1 & (str_detect(taxon, " x ") | 
                          str_sub(taxon, 1, 2) == "x ")) %>% 
  distinct(taxon) %>% 
  rename(taxon_rep = taxon) %>% 
  full_join(mntaxa_lookup2 %>% 
              filter(dlist_hybrid == 1 & 
                       (str_detect(dlist_assignment, " x ") | 
                          str_sub(dlist_assignment, 1, 2) == "x ")) %>% 
              distinct(dlist_assignment) %>% 
              rename(taxon_rep = dlist_assignment)) %>% 
  mutate(taxon_1 = str_replace_all(taxon_rep, " x ", " x"),
         taxon_1 = if_else(str_sub(taxon_1, 1, 2) == "x ",
                         sub("^..", "x", taxon_1),
                         taxon_1),
         taxon_2 = str_replace_all(taxon_rep, " x ", " X "),
         taxon_2 = if_else(str_sub(taxon_2, 1, 2) == "x ",
                         sub("^.", "X", taxon_2),
                         taxon_2),
         taxon_3 = str_replace_all(taxon_rep, " x ", " X"),
         taxon_3 = if_else(str_sub(taxon_3, 1, 2) == "x ",
                         sub("^..", "X", taxon_3),
                         taxon_3)) %>% 
  pivot_longer(cols = -taxon_rep,
               names_to = "version",
               values_to = "taxon",
               names_prefix = "taxon_") %>% 
  select(-version)

# check for duplicates
get_dupes(hybrid_crosswalk)

# taxa missing phys
taxa_missing_phys <- filter(mntaxa_lookup2, is.na(dlist_physiognomy)) %>% 
  distinct(dlist_assignment) %>% 
  arrange(dlist_assignment) %>% 
  data.frame() %>% 
  mutate(dlist_physiognomy_rep = case_when(
    dlist_assignment == "Acer x freemanii" ~ "broadleaf deciduous",
    dlist_assignment == "Azolla caroliniana" ~ "floating aquatic",
    dlist_assignment == "Gentiana x pallidocyanea" ~ "forb",
    dlist_assignment == "Lathyrus sylvestris" ~ "climber/forb",
    dlist_assignment == "Lycopodium appalachianum x lucidulum" ~ "clubmoss",
    dlist_assignment == "Lycopodium appalachianum x selago" ~ "clubmoss",
    dlist_assignment == "Marsilea quadrifolia" ~ 
      "emergent aquatic/floating aquatic",
    dlist_assignment == "Pennisetum" ~ "graminoid",
    dlist_assignment == "Petunia axillaris" ~ "forb",
    dlist_assignment == "Polystichum lonchitis" ~ "forb"))

# repair physiognomy
# add strata to trees
mntaxa_lookup3 <- mntaxa_lookup2 %>% 
  left_join(taxa_missing_phys) %>% 
  mutate(dlist_physiognomy = if_else(is.na(dlist_physiognomy),
                                     dlist_physiognomy_rep,
                                     dlist_physiognomy)) %>% 
  select(-dlist_physiognomy_rep)

# taxa in floristic tables that aren't in lookup
mnnpc_floristic_tables %>%
  distinct(npc_taxon_name) %>%
  mutate(analysis_group = str_remove(npc_taxon_name,
                                     " understory| sub-canopy| canopy")) %>% 
  anti_join(mntaxa_lookup3)
# should be zero, follow steps below if missing

# taxa in releves that aren't in lookup
# note that joined datasets often contain overlapping info and selections
# were based on best available
releve_missing <- crosswalk_raw %>% 
  mutate(taxon = str_remove(taxon, " s\\.s\\.| s\\.l\\.| s\\.a\\.")) %>% 
  left_join(hybrid_crosswalk) %>% 
  mutate(taxon = if_else(!is.na(taxon_rep), taxon_rep, taxon)) %>% 
  distinct(taxon) %>% 
  anti_join(mntaxa_lookup3)

# add in manual dlist assignments (done in releve_processing)
releve_missing_groups <- releve_missing %>% 
  inner_join(releve_taxa %>% 
              select(taxon, starts_with("dlist"), analysis_group) %>% 
              distinct() %>% 
              mutate(taxon = str_remove(taxon, 
                                        " s\\.s\\.| s\\.l\\.| s\\.a\\."))) %>%
  left_join(mntaxa_lookup3 %>% 
              select(starts_with("analysis_group")) %>% 
              distinct()) %>% 
  rename(c_value = dlist_c_value) %>% 
  mutate(hybrid = 0,
         dlist_hybrid = hybrid, # all are NA
         rank = "species group",
         dlist_rank = "species group",
         dlist_species = NA_character_,
         dlist_genus = word(dlist_assignment, 1)) %>%
  left_join(mntaxa_lookup3 %>% 
              distinct(dlist_genus, dlist_family, dlist_order, dlist_class,
                       dlist_phylum, dlist_kingdom)) %>% 
  select(colnames(mntaxa_lookup3))

# add in dlist info based on genus
releve_missing_genus <- releve_missing %>% 
  anti_join(releve_missing_groups) %>% 
  filter(str_detect(taxon, "/|\\(") | str_count(taxon, "\\w+") == 1) %>% 
  mutate(taxon_rep = word(taxon, 1, 1)) %>% 
  inner_join(mntaxa_lookup3 %>% 
              rename(taxon_rep = taxon)) %>% 
  left_join(mntaxa_dlist_pars %>% 
              distinct(dlist_taxon, c_value) %>% 
              rename(dlist_assignment = dlist_taxon)) %>% 
  mutate(hybrid = 0, rank = "genus") %>% 
  select(colnames(mntaxa_lookup3))

# add in dlist info based on family
# use dlist_pars to get the dlist_id
releve_missing_family <- releve_missing %>% 
  anti_join(releve_missing_groups) %>% 
  anti_join(releve_missing_genus) %>% 
  filter(str_count(taxon, "\\w+") == 1) %>% 
  mutate(dlist_assignment = word(taxon, 1)) %>% 
  inner_join(mntaxa_dlist_pars %>% 
              select(starts_with("dlist"), c_value) %>% 
              distinct() %>% 
              rename(dlist_assignment = dlist_taxon)) %>% 
  left_join(mntaxa %>% 
              distinct(taxon_id, is_hybrid, rank, author, publication) %>% 
              rename(hybrid = is_hybrid,
                     dlist_id = taxon_id,
                     dlist_publication = publication)) %>% 
  mutate(dlist_hybrid = hybrid, # all dlist_hybrid were NA
         dlist_full_name = paste(dlist_assignment, author),
         analysis_group = NA_character_,
         analysis_group_includes = NA_character_,
         dlist_id = as.character(dlist_id)) %>% 
  select(colnames(mntaxa_lookup3))

# remaining
releve_missing %>% 
  anti_join(releve_missing_family) %>% 
  anti_join(releve_missing_genus) %>% 
  anti_join(releve_missing_groups)
# 434 remaining

# add missing taxa to lookup
mntaxa_lookup4 <- mntaxa_lookup3 %>% 
  full_join(releve_missing_groups) %>% 
  full_join(releve_missing_genus) %>% 
  full_join(releve_missing_family)

# add dlist to taxon list if it has an analysis group
  # otherwise it's a genus or family that was added just to put 
  # a recommended name on releve taxa
# remove duplicates with multiple hybrid values
# change rank of species groups
mntaxa_lookup5 <- mntaxa_lookup4 %>% 
  full_join(mntaxa_lookup4 %>% 
              filter(!is.na(analysis_group)) %>% 
              select(-c(taxon, hybrid, rank)) %>% 
              distinct() %>% 
              mutate(taxon = dlist_assignment,
                     hybrid = dlist_hybrid,
                     rank = dlist_rank)) %>%   
  group_by(taxon) %>% 
  mutate(hybrid = as.numeric(sum(hybrid) > 0)) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(rank = if_else(rank == "species" & str_detect(taxon, "/"),
                        "species group", rank),
         dlist_rank = if_else(dlist_rank == "species" & 
                                str_detect(dlist_assignment, "/"),
                        "species group", dlist_rank)) %>% 
  rename_with(.fn = ~str_replace(.x, "dlist_", "recommended_")) %>%
  rename(taxon_name = taxon,
         recommended_taxon_name = recommended_assignment,
         informal_group = recommended_physiognomy,
         recommended_scientific_name = recommended_full_name)

# format columns for look-up table
mnnpc_taxa_lookup <- mntaxa_lookup5 %>% 
  select(informal_group, taxon_name, recommended_taxon_name, recommended_rank,
         recommended_scientific_name,
         recommended_id, recommended_publication, analysis_group,
         analysis_group_includes) %>% 
  arrange(taxon_name) %>% 
  as.data.frame()

# check for duplicates
get_dupes(mnnpc_taxa_lookup, taxon_name)

# missing data
mnnpc_taxa_lookup %>% 
  select(-starts_with("analysis")) %>% 
  filter(if_any(everything(), is.na))
# informal group for genera and families

# save lookup
save(mnnpc_taxa_lookup, file = "data-raw/data-out-ak/mnnpc_taxa_lookup.rds")
load("data-raw/data-out-ak/mnnpc_taxa_lookup.rds")

# save hybrid crosswalk
mnnpc_hybrid_crosswalk <- hybrid_crosswalk %>% 
  as.data.frame()

save(mnnpc_hybrid_crosswalk, file = "data-raw/data-out-ak/mnnpc_hybrid_crosswalk.rds")
load("data-raw/data-out-ak/mnnpc_taxa_lookup.rds")


#### accepted taxa ####

# look at example
load("../RMAVIS/data/accepted_taxa.rda")
head(accepted_taxa)

# dlist ID and name
mnnpc_accepted_taxa <- mnnpc_taxa_lookup %>% 
  distinct(recommended_taxon_name) %>% 
  rename(taxon_name = recommended_taxon_name) %>% 
  as.data.frame()

# check for duplicates
get_dupes(mnnpc_accepted_taxa)

# save
save(mnnpc_accepted_taxa, file = "data-raw/data-out-ak/mnnpc_accepted_taxa.rds")
load("data-raw/data-out-ak/mnnpc_accepted_taxa.rds")


#### taxonomic backbone ####

# look at example
UKVegTB::taxonomic_backbone %>% 
  as_tibble()
# same list as accepted taxa, but with more info

# select columns
mnnpc_taxonomic_backbone <- mntaxa_lookup5 %>% 
  select(informal_group, starts_with("recommended")) %>% 
  distinct() %>% 
  rename_with(.fn = ~str_remove(.x, "recommended_")) %>% 
  select(id, informal_group, taxon_name, rank, scientific_name,
         common_name, species:kingdom, origin, publication)

# check for duplicates
get_dupes(mnnpc_taxonomic_backbone, taxon_name)

# missing data
mnnpc_taxonomic_backbone %>% 
  select(-c(common_name, species)) %>% 
  filter(if_any(everything(), is.na))
# just genus and family

mnnpc_taxonomic_backbone %>% 
  filter(is.na(species)) %>% 
  distinct(rank)

# check that all taxa are included
mnnpc_accepted_taxa %>% 
  anti_join(mnnpc_taxonomic_backbone)
# should return 0

# same taxa as accepted taxa
n_distinct(mnnpc_taxonomic_backbone$taxon_name) == nrow(mnnpc_accepted_taxa)

# save
save(mnnpc_taxonomic_backbone, file = "data-raw/data-out-ak/mnnpc_taxonomic_backbone.rds")
load("data-raw/data-out-ak/mnnpc_taxonomic_backbone.rds")


#### community attributes ####

# export above objects to data folder
# load package objects for releve processing
devtools::load_all()

# look at example
load("../RMAVIS/data/nvc_community_attributes.rda")
head(nvc_community_attributes)

# explore rank
unique(nvc_community_attributes$rank)
nvc_community_attributes %>%
  filter(rank == "type") %>%
  distinct(fullname)
# equivalent to systems
# don't have any species information

unique(nvc_community_attributes$rank)
nvc_community_attributes %>%
  filter(rank == "community") %>%
  distinct(fullname)
# equivalent to classes

# community attribute function
att_fun <- function(ecs_sec = all_sections){
  
  # select samples in section
  releve_sub <- releve3 %>% 
    filter(ecs_secabb %in% ecs_sec)
  
  crosswalk_sub <- crosswalk_raw %>% 
    filter(relnumb %in% releve_sub$relnumb) %>% 
    mutate(year = NA_real_,
           group = NA_real_,
           scov = scov_mid) %>% 
    process_dnr_releves(aggregate_into_analysis_groups = F) %>% 
    rename(relnumb = Quadrat) %>% 
    rename_with(str_to_lower) %>% 
    left_join(releve_sub %>% 
                select(relnumb, starts_with("npc"))) %>% 
    left_join(mnnpc_taxa_lookup %>% 
                rename(species = recommended_taxon_name) %>% 
                distinct(species, analysis_group)) %>% 
    filter(!is.na(analysis_group))
  
  # richness by level of hierarchy
  rich_sys <- crosswalk_sub %>% 
    group_by(npc_system_id) %>%
    summarize(species_count = n_distinct(species),
              .groups = "drop")

  rich_flor <- crosswalk_sub %>% 
    group_by(npc_sys_flor) %>%
    summarize(species_count = n_distinct(species),
              .groups = "drop")
  
  rich_class <- crosswalk_sub %>% 
    group_by(npc_class) %>%
    summarize(species_count = n_distinct(species),
              .groups = "drop")
  
  rich_type <- crosswalk_sub %>% 
    filter(!is.na(npc_type)) %>% 
    group_by(npc_class, npc_type) %>%
    summarize(species_count = n_distinct(species),
              .groups = "drop")
  
  rich_subtype <- crosswalk_sub %>% 
    filter(!is.na(npc_subtype)) %>% 
    group_by(npc_type, npc_subtype) %>%
    summarize(species_count = n_distinct(species),
              .groups = "drop")
  
  # attributes by system
  att_sys <- crosswalk_sub %>%
    group_by(relnumb, npc_system, npc_system_id) %>%
    summarize(n_species = n_distinct(species),
              .groups = "drop") %>% 
    group_by(npc_system, npc_system_id) %>% 
    summarize(num_samples = n_distinct(relnumb),
              min_species = min(n_species),
              max_species = max(n_species),
              mean_species = mean(n_species) %>% round_half_up(),
              .groups = "drop") %>%
    left_join(rich_sys) %>%
    mutate(fullname = npc_system,
           name = npc_system,
           npc_code = npc_system_id,
           npc_code_parent = NA_character_,
           basal = FALSE,
           rank = "system") %>%
    select(fullname, name, npc_code, npc_code_parent, basal, rank, num_samples,
           min_species, max_species, mean_species, species_count)
  
  # floristic regions
  flor_names <- rich_flor %>% 
    select(npc_sys_flor) %>% 
    mutate(floristic = str_sub(npc_sys_flor, 3, 3),
           flor_name = case_when(floristic == "c" ~ "central",
                                 floristic == "n" ~ "northern",
                                 floristic == "p" ~ "prairie",
                                 floristic == "s" ~ "southern",
                                 floristic == "u" ~ "Lake Superior",
                                 floristic == "w" ~ "notthwestern"))
  
  # attributes by system_floristic
  att_flor <- crosswalk_sub %>%
    group_by(relnumb, npc_system_id, npc_system, npc_sys_flor) %>%
    summarize(n_species = n_distinct(species),
              .groups = "drop") %>% 
    group_by(npc_system_id, npc_system, npc_sys_flor) %>% 
    summarize(num_samples = n_distinct(relnumb),
              min_species = min(n_species),
              max_species = max(n_species),
              mean_species = mean(n_species) %>% round_half_up(),
              .groups = "drop") %>%
    left_join(rich_flor) %>%
    left_join(flor_names) %>% 
    mutate(fullname = paste(npc_system, "-", flor_name, "floristic region"),
           name = fullname,
           npc_code = npc_sys_flor,
           npc_code_parent = npc_system_id,
           basal = FALSE,
           rank = "system + floristic region") %>%
    select(fullname, name, npc_code, npc_code_parent, basal, rank, num_samples,
           min_species, max_species, mean_species, species_count)
  
  # attributes by class
  # calculate richness by class
  # add system info
  att_class <- crosswalk_sub %>%
    group_by(relnumb, npc_sys_flor, npc_class_name, npc_class) %>%
    summarize(n_species = n_distinct(species),
              .groups = "drop") %>% 
    group_by(npc_sys_flor,, npc_class_name, npc_class) %>% 
    summarize(num_samples = n_distinct(relnumb),
              min_species = min(n_species),
              max_species = max(n_species),
              mean_species = mean(n_species) %>% round_half_up(),
              .groups = "drop") %>%
    left_join(rich_class) %>%
    mutate(fullname = npc_class_name,
           name = npc_class_name,
           npc_code = npc_class,
           npc_code_parent = npc_sys_flor,
           basal = if_else(npc_class %in% rich_type$npc_class, FALSE, TRUE),
           rank = "class") %>%
    select(fullname, name, npc_code, npc_code_parent, basal, rank, num_samples,
           min_species, max_species, mean_species, species_count)
  
  # attributes by type
  att_type <- crosswalk_sub %>%
    filter(!is.na(npc_type)) %>%
    group_by(relnumb, npc_class_name, npc_class, npc_type_name, npc_type) %>%
    summarize(n_species = n_distinct(species),
              .groups = "drop") %>% 
    group_by(npc_class_name, npc_class, npc_type_name, npc_type) %>% 
    summarize(num_samples = n_distinct(relnumb),
              min_species = min(n_species),
              max_species = max(n_species),
              mean_species = mean(n_species) %>% round_half_up(),
              .groups = "drop") %>%
    left_join(rich_type) %>%
    mutate(fullname = paste(npc_class_name, npc_type_name, sep = ", "),
           name = npc_type_name,
           npc_code = npc_type,
           npc_code_parent = npc_class,
           basal = if_else(npc_type %in% str_sub(rich_subtype$npc_subtype, 1, 6),
                           FALSE, TRUE),
           rank = "type") %>%
    select(fullname, name, npc_code, npc_code_parent, basal, rank, num_samples,
           min_species, max_species, mean_species, species_count)
  
  # attributes by sub-type
  att_subtype <- crosswalk_sub %>%
    filter(!is.na(npc_subtype)) %>%
    group_by(relnumb, npc_type_name, npc_type, npc_subtype_name, 
             npc_subtype) %>%
    summarize(n_species = n_distinct(species),
              .groups = "drop") %>% 
    group_by(npc_type_name, npc_type, npc_subtype_name, npc_subtype) %>% 
    summarize(num_samples = n_distinct(relnumb),
              min_species = min(n_species),
              max_species = max(n_species),
              mean_species = mean(n_species) %>% round_half_up(),
              .groups = "drop") %>%
    left_join(rich_subtype) %>%
    mutate(fullname = paste(npc_type_name, npc_subtype_name, sep = ", "),
           name = npc_subtype_name,
           npc_code = npc_subtype,
           npc_code_parent = npc_type,
           basal = TRUE,
           rank = "sub-type") %>%
    select(fullname, name, npc_code, npc_code_parent, basal, rank, num_samples,
           min_species, max_species, mean_species, species_count)
  
  # combine
  att_comb <- att_sys %>%
    rbind(att_flor) %>% 
    rbind(att_class) %>%
    rbind(att_type) %>% 
    rbind(att_subtype)
  
  # add ecs section
  if(length(ecs_sec)== 1){
    
    # get name
    ecs_name <- ecs_sections %>% 
      filter(ecs_secabb == ecs_sec) %>% 
      pull(ecs_secname)
    
    # add abbreviation and name
    att_comb <- att_comb %>%
      mutate(fullname = paste(fullname, ecs_name, sep = " - "),
             name = paste(name, ecs_name, sep = " - "),
             npc_code = paste(npc_code, ecs_sec, sep = "_"),
             npc_code_parent = if_else(!is.na(npc_code_parent),
                                       paste(npc_code_parent, ecs_sec, sep = "_"),
                                       npc_code_parent),
             ecs_section = ecs_name)
    
  }
  
  # output
  return(att_comb)
  
}

# floristic tables statewide and by region
att_tab_state <- att_fun() %>% 
  mutate(ecs_section = NA_character_)
att_tab_LAP <- att_fun("LAP")
att_tab_MIM <- att_fun("MIM")
att_tab_MOP <- att_fun("MOP")
att_tab_MDL <- att_fun("MDL")
att_tab_CGP <- att_fun("CGP")
att_tab_NSU <- att_fun("NSU")
att_tab_PPL <- att_fun("PPL")
att_tab_RRV <- att_fun("RRV")
att_tab_SSU <- att_fun("SSU")
att_tab_WSU <- att_fun("WSU")

# combine
mnnpc_community_attributes <- att_tab_state %>% 
  rbind(att_tab_LAP) %>% 
  rbind(att_tab_MIM) %>% 
  rbind(att_tab_MOP) %>% 
  rbind(att_tab_MDL) %>% 
  rbind(att_tab_CGP) %>% 
  rbind(att_tab_NSU) %>% 
  rbind(att_tab_PPL) %>% 
  rbind(att_tab_RRV) %>% 
  rbind(att_tab_SSU) %>% 
  rbind(att_tab_WSU)

# check for NAs
mnnpc_community_attributes %>%
  filter(if_any(-c(npc_code_parent, ecs_section), is.na))

mnnpc_community_attributes %>% 
  filter(rank != "system" & is.na(npc_code_parent))

mnnpc_community_attributes %>% 
  filter(str_detect(npc_code, "_") & is.na(ecs_section))

# save
save(mnnpc_community_attributes, 
     file = "data-raw/data-out-ak/mnnpc_community_attributes.rds")
load("data-raw/data-out-ak/mnnpc_community_attributes.rds")


#### example data and releve ####

# need to load MNNPC objects, see top of community attributes section

# look at example
load("../RMAVIS/data/example_data.rda")
head(example_data$`Newborough Warren`)

# get releves from database
rel_ex1 <- releve2 %>%
  filter(relnumb %in% stcroix$RELNO) %>%
  transmute(group = case_when(is.na(place_name) ~ "Control",
                              str_detect(place_name, "Outside Exclosure") ~ 
                                "Outside Exclosure",
                              str_detect(place_name, "Exclosure") ~ 
                                "Inside Exclosure"),
            year = if_else(group == "Control", 2004, year(date_)), # artificially set same initial year for all
            year = as.integer(year),
            quadrat = if_else(!is.na(original_releve_nbr), original_releve_nbr,
                              relnumb),
            relnumb = relnumb) %>% 
  relocate(year)

# format resample data
resample2 <- resample %>%
  filter(str_detect(npc, "MHs38|MHs39")) %>% 
  rename(quadrat = orig_relnumb) %>% 
  full_join(resample %>%
              filter(str_detect(npc, "MHs38|MHs39")) %>% 
              select(-relnumb) %>% 
              rename(relnumb = orig_relnumb) %>% 
              mutate(quadrat = relnumb)) %>% 
  mutate(group = if_else(str_detect(npc, "MHs38"), "Oak-Basswood Group", 
                         "Maple-Basswood Group"))

rel_ex2 <- releve2 %>% 
  inner_join(resample2 %>% 
               select(relnumb, group, quadrat)) %>% 
  transmute(year = year(date_),
            year = if_else(year < 2018, 1990, year),  # artificially set same initial year for all
            year = as.integer(year),
            group = group,
            quadrat = quadrat,
            relnumb = relnumb)

# get taxa from database
# convert hybrid names
# remove taxa that don't have accepted names
rel_tax1 <- rel_ex1 %>% 
  inner_join(crosswalk_raw %>%
               distinct(relnumb, physcode, minht, maxht, taxon, scov_mid)) %>% 
  select(-relnumb) %>% 
  rename(relnumb = quadrat,
         scov = scov_mid) %>% 
  filter(!is.na(scov)) %>%
  left_join(mnnpc_hybrid_crosswalk) %>%
  mutate(taxon = ifelse(!is.na(taxon_rep), taxon_rep, taxon)) %>%
  select(-taxon_rep) %>%
  inner_join(mnnpc_taxa_lookup %>%
               filter(!is.na(recommended_taxon_name)) %>% 
               select(taxon_name) %>%
               rename(taxon = taxon_name))

rel_tax2 <- rel_ex2 %>% 
  inner_join(crosswalk_raw %>%
               select(relnumb, physcode, minht, maxht, taxon, scov_mid)) %>% 
  select(-relnumb) %>% 
  rename(relnumb = quadrat,
         scov = scov_mid) %>% 
  filter(!is.na(scov)) %>%
  left_join(mnnpc_hybrid_crosswalk) %>%
  mutate(taxon = ifelse(!is.na(taxon_rep), taxon_rep, taxon)) %>%
  select(-taxon_rep) %>%
  inner_join(mnnpc_taxa_lookup %>%
               filter(!is.na(recommended_taxon_name)) %>% 
               select(taxon_name) %>%
               rename(taxon = taxon_name))

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
mnnpc_example_releve <- mnnpc_example_data$`St. Croix State Forest` %>% 
  filter(relnumb == "4710")

# save
save(mnnpc_example_releve, file = "data-raw/data-out-ak/mnnpc_example_releve.rds")
load("data-raw/data-out-ak/mnnpc_example_releve.rds")


#### data for testing ####

# data used to build floristic tables
test_dat <- crosswalk3 %>% 
  select(relnumb, analysis_group_strata, scov_adj,
         npc_class, npc_type, npc_subtype) %>% 
  rename(Quadrat = relnumb,
         Species = analysis_group_strata,
         Cover = scov_adj)

# save
write_csv(test_dat,
          "../../intermediate-data/floristic_table_development_data_20251224.csv")


#### section map ####

# color palette
pal <- color("light")(9)

# select necessary columns
# add abbreviations
ecs_sec_sf2 <- ecs_sec_sf %>% 
  select(SECNAME) %>% 
  rename(ecs_secname = SECNAME) %>% 
  left_join(ecs_sections)

# get label points
# nudge SSU label out
# remove lower SSU label (too small to see polygon)
ecs_points <- poi(x = ecs_sec_sf2, precision = 0.1) %>% 
  bind_rows() %>% 
  cbind(ecs_sec_sf2 %>% 
          select(ecs_secabb) %>% 
          st_drop_geometry()) %>% 
  mutate(x = if_else(ecs_secabb == "SSU", x + 31000, x)) %>% 
  group_by(ecs_secabb) %>% 
  mutate(max_y = max(y)) %>% 
  ungroup() %>% 
  filter(y == max_y) %>% 
  st_as_sf(coords = c("x", "y"),
           crs = st_crs(ecs_sec_sf2))

ecs_fig <- ggplot(ecs_sec_sf2) +
  geom_sf(aes(fill = ecs_secabb), size = 0.5, color = "black") +
  geom_sf_text(data = ecs_points,
               aes(label = ecs_secabb)) +
  scale_fill_manual(values = c(pal, "white")) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

ggsave(filename = "data-raw/data-out-ak/ecs_map.png", ecs_fig,
       width = 4)
