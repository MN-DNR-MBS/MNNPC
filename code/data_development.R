#### set-up ####

# load packages
library(tidyverse)
library(janitor)

# import data
crosswalk <- read_csv("../../intermediate-data/crosswalk_table_formatted_20250423a.csv",
                      guess_max = Inf) 
releve <- read_csv("../../intermediate-data/releve_table_formatted_20250423a.csv")


#### format data ####

# select releves
releve2 <- releve %>%
  filter(used_in_fieldguide == "yes") %>%
  mutate(npc_class = str_sub(npc_code, 1, 5),
         npc_type = if_else(nchar(npc_code) > 5, str_sub(npc_code, 1, 6),
                            NA_character_),
         npc_subtype = if_else(nchar(npc_code) > 6, str_sub(npc_code, 1, 7),
                               NA_character_))

# add classification
# format species names
crosswalk2 <- crosswalk %>%
  inner_join(releve2 %>%
               select(relnumb, npc_system, npc_system_id, npc_class_name,
                      npc_class, npc_type_name, npc_type, npc_subtype_name, 
                      npc_subtype)) %>%
  mutate(code_strata = paste0(analysis_group, strata_lower, strata_upper),
         species_name =  str_remove(code_strata, "18") %>% 
           str_replace_all("_", " ") %>%
           str_replace("13", " understory") %>%
           str_replace("45", " sub-canopy") %>%
           str_replace("68", " canopy"))

# check that there aren't duplicates
get_dupes(releve2, relnumb)
get_dupes(crosswalk2, relnumb, species_name)

# are the releve numbers equal?
nrow(releve2) == n_distinct(crosswalk2$relnumb)


#### floristic tables ####

# look at example
load("../RMAVIS/data/nvc_floristic_tables.rda")
head(nvc_floristic_tables)

# explore rank
unique(nvc_floristic_tables$nvc_code)

# samples per group
samps_class <- releve2 %>% 
  count(npc_class)

samps_type <- releve2 %>% 
  filter(!is.na(npc_type)) %>% 
  count(npc_type)

samps_subtype <- releve2 %>% 
  filter(!is.na(npc_subtype)) %>% 
  count(npc_subtype)

# percentage of data classified to type
sum(samps_type$n) / nrow(releve2) # 97%

perc_rels_type <- releve2 %>%
  group_by(npc_class) %>%
  summarize(rels_tot = n_distinct(relnumb),
            rels_type = sum(!is.na(npc_type)),
            .groups = "drop") %>%
  mutate(perc_type = rels_type / rels_tot)
# ROs12, WFn74 don't have types in dataset
# remainder have enough releves representing types

# percentage of data classified to subtype
sum(samps_subtype$n) / nrow(releve2) # only 15%

perc_rels_subtype <- releve2 %>%
  group_by(npc_class) %>%
  summarize(rels_tot = n_distinct(relnumb),
            rels_subtype = sum(!is.na(npc_subtype)),
            .groups = "drop") %>%
  mutate(perc_type = rels_subtype / rels_tot)
# don't use to inform species inclusions

# floristic tables
# re-do these with BB scale instead of midpoint for scov_adj
flor_type <- crosswalk2 %>%
  group_by(npc_class, npc_type, species_name) %>% 
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
  distinct(npc_class, species_name)

# only keep more common species
flor_type2 <- flor_type %>%
  inner_join(flor_type_spp)

# class tables
flor_class <- crosswalk2 %>%
  group_by(npc_class, species_name) %>% 
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
  filter(keep == 1)

# species kept
flor_class_spp <- flor_class %>%
  distinct(npc_class, species_name)

flor_subtype <- crosswalk2 %>%
  group_by(npc_class, npc_type, npc_subtype, species_name) %>% 
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
  inner_join(flor_class_spp)


#### community attributes ####

# look at example
load("../RMAVIS/data/nvc_community_attributes.rda")
head(nvc_community_attributes)

# explore rank
unique(nvc_community_attributes$rank)
nvc_community_attributes %>%
  filter(rank == "type") %>%
  distinct(fullname)

unique(nvc_community_attributes$rank)
nvc_community_attributes %>%
  filter(rank == "community") %>%
  distinct(fullname)

# attributes by system
# come back to this, supposed to omit species in fewer than 5% of "sub-communities", I think
# system may not be worth doing, not included in floristic tables
unique(releve2$npc_system)

att_sys <- crosswalk2 %>% 
  group_by(npc_system, npc_system_id) %>% 
  mutate(species_count = n_distinct(code_strata)) %>%
  ungroup() %>% 
  group_by(relnumb, npc_system, npc_system_id, species_count) %>%
  summarize(n_species = n_distinct(code_strata),
            .groups = "drop") %>% 
  group_by(npc_system, npc_system_id, species_count) %>% 
  summarize(num_samples = n_distinct(relnumb),
            min_species = min(n_species),
            max_species = max(n_species),
            mean_species = mean(n_species),
            .groups = "drop")
  
