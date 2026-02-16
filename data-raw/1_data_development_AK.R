#### set-up ####

# clear environment
rm(list = ls())

# install mntaxa (development)
pak::pak("MN-DNR-MBS/mntaxa", upgrade = T)

# load packages
library(tidyverse)
library(janitor)
library(readxl)
library(sf)
library(khroma)
library(polylabelr)
library(mntaxa)

# import data
load("data-raw/data-out-ak/releve_species_grouped_data.rds")
load("data-raw/data-out-ak/releve_species_ungrouped_data.rds")
load("data-raw/data-out-ak/releve_plot_data.rds")
load("../npc-releve/data/originals-20260208/crosswalk.rds")
ecs_sec_sf <- st_read("V:/gdrs/data/pub/us_mn_state_dnr/geos_ecological_class_system/fgdb/geos_ecological_class_system.gdb", 
                      "ecs_sections_of_mn_v99a")


#### format releve data ####

# add abbreviations for sections
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

# add ecs abbreviations
# select plots used in field guide
releve <- releve_plots %>%
  left_join(ecs_sections) %>%
  filter(used_in_fieldguide == "yes")

# select field guide plots
# add plot attributes to species data
spec_grp <- releve_species_grouped %>%
  inner_join(releve)

spec_raw <- releve_species_ungrouped %>%
  inner_join(releve)

# are all classified to class?
sort(unique(releve$npc_class)) # yes
         
# check that there aren't duplicates
get_dupes(releve, relnumb)
get_dupes(spec_grp, relnumb, code_strata)

# are the releve numbers equal?
nrow(releve) == n_distinct(spec_grp$relnumb)

# all ECS sections in data
all_sections <- releve %>% 
  distinct(ecs_secabb) %>% 
  pull(ecs_secabb)


#### floristic tables ####

# look at example
load("../RMAVIS/data/nvc_floristic_tables.rda")
head(nvc_floristic_tables)

flor_fun <- function(ecs_sec = all_sections){
  
  # select samples in section
  releve_sub <- releve %>% 
    filter(ecs_secabb %in% ecs_sec)
  
  spec_sub <- spec_grp %>% 
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
  flor_type <- spec_sub %>%
    filter(!is.na(npc_type)) %>%
    group_by(npc_class, npc_type, analysis_group_strata) %>%
    summarize(absolute_frequency = n_distinct(relnumb),
              minimum_cover = min(scov),
              mean_cover = mean(scov),
              maximum_cover = max(scov),
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
  flor_class <- spec_sub %>%
    group_by(npc_class, analysis_group_strata) %>%
    summarize(absolute_frequency = n_distinct(relnumb),
              minimum_cover = min(scov),
              mean_cover = mean(scov),
              maximum_cover = max(scov),
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

  flor_subtype <- spec_sub %>%
    filter(!is.na(npc_subtype)) %>%
    group_by(npc_class, npc_type, npc_subtype, analysis_group_strata) %>%
    summarize(absolute_frequency = n_distinct(relnumb),
              minimum_cover = min(scov),
              mean_cover = mean(scov),
              maximum_cover = max(scov),
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


#### community attributes ####

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
  releve_sub <- releve %>% 
    filter(ecs_secabb %in% ecs_sec)
  
  spec_sub <- spec_raw %>% 
    filter(relnumb %in% releve_sub$relnumb) %>% 
    distinct(relnumb, acc_assignment, npc_system, npc_system_id, npc_sys_flor, 
             npc_class, npc_class_name, npc_type, npc_type_name, npc_subtype,
             npc_subtype_name)
  
  # richness by level of hierarchy
  rich_sys <- spec_sub %>% 
    group_by(npc_system_id) %>%
    summarize(species_count = n_distinct(acc_assignment),
              .groups = "drop")
  
  rich_flor <- spec_sub %>% 
    group_by(npc_sys_flor) %>%
    summarize(species_count = n_distinct(acc_assignment),
              .groups = "drop")
  
  rich_class <- spec_sub %>% 
    group_by(npc_class) %>%
    summarize(species_count = n_distinct(acc_assignment),
              .groups = "drop")
  
  rich_type <- spec_sub %>% 
    filter(!is.na(npc_type)) %>% 
    group_by(npc_class, npc_type) %>%
    summarize(species_count = n_distinct(acc_assignment),
              .groups = "drop")
  
  rich_subtype <- spec_sub %>% 
    filter(!is.na(npc_subtype)) %>% 
    group_by(npc_type, npc_subtype) %>%
    summarize(species_count = n_distinct(acc_assignment),
              .groups = "drop")
  
  # attributes by system
  att_sys <- spec_sub %>%
    group_by(relnumb, npc_system, npc_system_id) %>%
    summarize(n_species = n_distinct(acc_assignment),
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
  att_flor <- spec_sub %>%
    group_by(relnumb, npc_system_id, npc_system, npc_sys_flor) %>%
    summarize(n_species = n_distinct(acc_assignment),
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
  att_class <- spec_sub %>%
    group_by(relnumb, npc_sys_flor, npc_class_name, npc_class) %>%
    summarize(n_species = n_distinct(acc_assignment),
              .groups = "drop") %>% 
    group_by(npc_sys_flor, npc_class_name, npc_class) %>% 
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
  att_type <- spec_sub %>%
    filter(!is.na(npc_type)) %>%
    group_by(relnumb, npc_class_name, npc_class, npc_type_name, npc_type) %>%
    summarize(n_species = n_distinct(acc_assignment),
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
  att_subtype <- spec_sub %>%
    filter(!is.na(npc_subtype)) %>%
    group_by(relnumb, npc_type_name, npc_type, npc_subtype_name, 
             npc_subtype) %>%
    summarize(n_species = n_distinct(acc_assignment),
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
             ecs_section = ecs_sec)
    
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


#### hybrid crosswalk ####

# get all accepted names and their IDs
mntaxa_acc <- accepted_mntaxa(taxonomy_levels = TRUE,
                              sources = TRUE,
                              releve = TRUE,
                              phys = FALSE,
                              strata = TRUE,
                              origin = TRUE,
                              common = TRUE,
                              cvals = FALSE,
                              exclude = FALSE)

# get taxa list
mntaxa_taxa <- taxa_mntaxa(taxonomy_levels = FALSE,
                           sources = FALSE,
                           releve = TRUE)

# for hybrids, crosswalk various versions of the x
# remove cases where the name doesn't have an "x" (don't need to crosswalk)
hybrid_crosswalk <- mntaxa_taxa %>% 
  filter(!is.na(hybrid)) %>% 
  distinct(taxon, hybrid) %>% 
  full_join(mntaxa_acc %>% 
              filter(!is.na(hybrid)) %>% 
              distinct(taxon, hybrid)) %>% 
  rename(taxon_rep = taxon,
         taxon_1 = hybrid) %>% 
  mutate(taxon_2 = str_replace_all(taxon_rep, " x ", " X "),
         taxon_2 = if_else(str_sub(taxon_2, 1, 2) == "x ",
                           sub("^.", "X", taxon_2),
                           taxon_2),
         taxon_3 = str_replace_all(taxon_rep, " x ", " X"),
         taxon_3 = if_else(str_sub(taxon_3, 1, 2) == "x ",
                           sub("^..", "X", taxon_3),
                           taxon_3),
         taxon_4 = str_replace_all(taxon_rep, " x ", " x"),
         taxon_4 = if_else(str_sub(taxon_4, 1, 2) == "x ",
                           sub("^..", "x", taxon_4),
                           taxon_4),) %>% 
  pivot_longer(cols = -taxon_rep,
               names_to = "version",
               values_to = "taxon",
               names_prefix = "taxon_") %>% 
  select(-version) %>% 
  filter(taxon != taxon_rep)

# check for duplicates
get_dupes(hybrid_crosswalk, taxon)

# save hybrid crosswalk
mnnpc_hybrid_crosswalk <- hybrid_crosswalk %>%
  as.data.frame()

save(mnnpc_hybrid_crosswalk, file = "data-raw/data-out-ak/mnnpc_hybrid_crosswalk.rds")
load("data-raw/data-out-ak/mnnpc_hybrid_crosswalk.rds")


#### accepted assignments and analysis codes ####

# get lookup table
mntaxa_groups <- lookup_mntaxa(taxonomy_levels = FALSE,
                               sources = FALSE,
                               releve = TRUE,
                               phys = FALSE,
                               strata = FALSE,
                               origin = FALSE,
                               common = FALSE,
                               cvals = FALSE,
                               exclude = FALSE,
                               replace_sub_var = TRUE,
                               replace_family = TRUE,
                               replace_genus = TRUE,
                               drop_higher = TRUE,
                               higher_include = c("Belonia", 
                                                  "Chara", 
                                                  "Lychnothamnus", 
                                                  "Nitella", 
                                                  "Nitellopsis",
                                                  "Spirogyra", 
                                                  "Tolypella"),
                               excluded_duplicates = TRUE,
                               clean_duplicates = FALSE,
                               group_accepted = TRUE,
                               group_analysis = TRUE)

# unique taxa values
mntaxa_groups2 <- mntaxa_groups %>% 
  distinct(taxon, acc_assignment, analysis_code, analysis_group)

# duplicates?
mntaxa_groups_dups <- get_dupes(mntaxa_groups2, taxon) %>% 
  inner_join(mntaxa_groups %>% 
               select(taxon_id, colnames(mntaxa_groups2)))
# yes, but cleaning duplicates in function excludes groups that are in the 
# floristic tables (below)

# resolve duplicates
mntaxa_groups_dups2 <- mntaxa_groups_dups %>% 
  filter((taxon != acc_assignment | is.na(acc_assignment)) & 
           !(taxon == "Quercus x schuettei" & 
               acc_assignment == "Quercus bicolor x macrocarpa") &
           !(taxon == "Arabis holboellii" &
               acc_assignment == "Boechera collinsii"))

mntaxa_groups3 <- mntaxa_groups2 %>% 
  anti_join(mntaxa_groups_dups2)

# check that it worked
get_dupes(mntaxa_groups3, taxon)

# make sure all original taxa are there
mntaxa_groups %>% 
  distinct(taxon) %>% 
  anti_join(mntaxa_groups3)

# make sure all taxa from floristic tables are included
mnnpc_floristic_tables %>% 
  transmute(analysis_group = str_remove(npc_taxon_name, " understory") %>% 
              str_remove(" subcanopy") %>% 
              str_remove(" canopy")) %>% 
  distinct() %>% 
  anti_join(mntaxa_groups3)


#### accepted taxa lookup ####

# lookup table on RMAVIS: any name --> accepted name
# match-to-accepted function

# look at example
UKVegTB::taxa_lookup %>% 
  as_tibble()
# all taxa, recommended taxon_name = dlist, recommended TVK = dlist_id

# get accepted names from mntaxa
mntaxa_lookup <- lookup_mntaxa(taxonomy_levels = TRUE,
                            sources = TRUE,
                            releve = TRUE,
                            phys = FALSE,
                            strata = TRUE,
                            origin = FALSE,
                            common = FALSE,
                            cvals = FALSE,
                            exclude = FALSE,
                            replace_sub_var = TRUE,
                            replace_family = TRUE,
                            replace_genus = TRUE,
                            drop_higher = FALSE,
                            higher_include = c("Belonia", 
                                               "Chara", 
                                               "Lychnothamnus", 
                                               "Nitella", 
                                               "Nitellopsis",
                                               "Spirogyra", 
                                               "Tolypella"),
                            excluded_duplicates = TRUE,
                            clean_duplicates = TRUE,
                            group_accepted = FALSE,
                            group_analysis = FALSE)
# when taxa have different s.s. and s.l., they almost always have the same accepted assignment
# when they don't, one is a subset of the other
# therefore, taxon qualifiers aren't useful for this lookup table
# when taxon_id is used, there's a ton of repetition (different IDs, same taxon, same accepted assignment)

# get physcode names
load_mntaxa(
  synonymies = FALSE,
  all_taxa = FALSE,
  taxonomy_levels = FALSE,
  sources = FALSE,
  phys = TRUE,
  origin = FALSE,
  common = FALSE,
  cvals = FALSE,
  exclude = FALSE,
  source = "package",
  envir = .GlobalEnv
)

# add informal group
mntaxa_lookup2 <- mntaxa_lookup %>% 
  distinct(taxon_name = taxon,
         recommended_taxon_name = acc_assignment,
         recommended_rank = acc_rank,
         recommended_scientific_name = acc_full_name,
         recommended_id = acc_taxon_id,
         recommended_publication = acc_publication,
         recommended_stratacode = acc_stratacode,
         acc_physcode) %>% 
  separate_rows(acc_physcode, sep = "/") %>% 
  left_join(phys_codes_raw %>% 
              select(informal_group = physiognomy,
                     acc_physcode = physiognomy_code)) %>% 
  group_by(across(c(taxon_name, starts_with("recommended")))) %>% 
  summarize(informal_group = paste(sort(unique(informal_group)), 
                                   collapse = "/"),
            recommended_physcode = paste(sort(unique(acc_physcode)), 
                                         collapse = "/"),
            .groups = "drop") %>% 
  mutate(informal_group = if_else(informal_group == "", NA_character_,
                                  informal_group),
         recommended_physcode = if_else(recommended_physcode == "", 
                                        NA_character_,
                                        recommended_physcode))

# check that all releve taxa are included (strip suffixes, crosswalk hybrids)
crosswalk_taxa_notaccepted <- crosswalk %>% 
  transmute(taxon = str_remove(taxon, " s\\.s\\.") %>% 
           str_remove(" s\\.l\\.") %>% 
           str_remove(" s\\.a\\."),
           physcode = physcode) %>% 
  left_join(hybrid_crosswalk) %>% 
  mutate(taxon = if_else(!is.na(taxon_rep), taxon_rep, taxon)) %>% 
  distinct(taxon, physcode) %>% 
  anti_join(mntaxa_lookup2, by = c("taxon" = "taxon_name"))

# look at by physcode
count(crosswalk_taxa_notaccepted, physcode)

# look at non mosses/lichen
crosswalk_taxa_notaccepted %>% 
  filter(!(physcode %in% c("Li", "M")))
# all are intentionally omitted taxa

# missing informal group
filter(mntaxa_lookup2, is.na(informal_group)) %>% 
  count(recommended_rank)
# all are family

# combined accepted taxa
filter(mntaxa_lookup2, str_detect(recommended_taxon_name, "\\/")) %>% 
  distinct(recommended_taxon_name)

# format columns for look-up table
mnnpc_taxa_lookup <- mntaxa_lookup2 %>%
  left_join(mntaxa_groups3 %>% 
              select(taxon_name = taxon, 
                     recommended_assignment = acc_assignment, 
                     analysis_group)) %>% 
  arrange(taxon_name) %>%
  relocate(informal_group) %>% 
  as.data.frame()

# check for duplicates
get_dupes(mnnpc_taxa_lookup, taxon_name)

# missing data
mnnpc_taxa_lookup %>%
  filter(if_any(-recommended_stratacode, is.na)) %>% 
  count(recommended_rank)
# all above species

# save lookup
save(mnnpc_taxa_lookup, file = "data-raw/data-out-ak/mnnpc_taxa_lookup.rds")
load("data-raw/data-out-ak/mnnpc_taxa_lookup.rds")


#### accepted taxa ####

# look at example
load("../RMAVIS/data/accepted_taxa.rda")
head(accepted_taxa)

# recommended names that aren't MNTaxa accepted
mnnpc_taxa_lookup %>% 
  distinct(taxon = recommended_taxon_name) %>% 
  anti_join(mntaxa_acc)

# accepted taxa
# taxa groups created for lookup table
mnnpc_accepted_taxa <- mntaxa_acc %>% 
  distinct(taxon_id, taxon_name = taxon) %>%
  mutate(taxon_id = as.character(taxon_id)) %>% 
  full_join(mnnpc_taxa_lookup %>% 
              distinct(taxon_id = recommended_id,
                       taxon_name = recommended_taxon_name)) %>% 
  arrange(taxon_name) %>% 
  as.data.frame()

# check for duplicates
get_dupes(mnnpc_accepted_taxa, taxon_name)

# save
save(mnnpc_accepted_taxa, file = "data-raw/data-out-ak/mnnpc_accepted_taxa.rds")
load("data-raw/data-out-ak/mnnpc_accepted_taxa.rds")


#### taxonomic backbone ####

# look at example
UKVegTB::taxonomic_backbone %>% 
  as_tibble()
# same list as accepted taxa, but with more info

# add informal group
# select and rename columns
mntaxa_backbone <- mntaxa_acc %>% 
  separate_rows(physcode, sep = "/") %>% 
  left_join(phys_codes_raw %>% 
              select(informal_group = physiognomy,
                     physcode = physiognomy_code)) %>% 
  select(-physcode) %>% 
  group_by(across(-informal_group)) %>% 
  summarize(informal_group = paste(sort(unique(informal_group)), 
                                   collapse = "/"),
            .groups = "drop") %>% 
  transmute(id = taxon_id,
         taxon_name = taxon,
         rank,
         qualifier = ss_sl,
         authority = author,
         publication,
         common_name,
         origin = native_status,
         informal_group = if_else(informal_group == "", NA_character_,
                                  informal_group)) %>% 
  left_join(tax_levels %>% 
              rename_with(~str_remove(.x, "acc_")) %>% 
              rename(id = taxon_id) %>% 
              select(id, species, genus, family, order, class, phylum, kingdom))

# convert to data frame
mnnpc_taxonomic_backbone <- mntaxa_backbone %>% 
  as.data.frame()

# check for duplicates
get_dupes(mnnpc_taxonomic_backbone, taxon_name)
# higher levels that are combined together or omitted from analyses

# check that all taxa are included
mnnpc_accepted_taxa %>% 
  anti_join(mnnpc_taxonomic_backbone)
# all the grouped taxa

# save
save(mnnpc_taxonomic_backbone, file = "data-raw/data-out-ak/mnnpc_taxonomic_backbone.rds")
load("data-raw/data-out-ak/mnnpc_taxonomic_backbone.rds")


#### floristic table data ####

# data used to build floristic tables
flor_dat <- spec_grp %>% 
  rename(Quadrat = relnumb,
         Species = analysis_group_strata,
         Cover = scov,
         ecs_section = ecs_secabb) %>% 
  relocate(Quadrat, Species, Cover)

# save
save(flor_dat, file = "data-raw/data-out-ak/mnnpc_floristic_table_data.rds")


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
