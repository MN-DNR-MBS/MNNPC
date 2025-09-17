#### set-up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(janitor)
library(RODBC)
library(readxl)

# import data
crosswalk <- read_csv("../../intermediate-data/crosswalk_table_formatted_20250916a.csv",
                      guess_max = Inf) 
releve <- read_csv("../../intermediate-data/releve_table_formatted_20250916a.csv")
stcroix <- read_excel("../../data/duxbury_deer_releve_data_working.xlsx")
mntaxa_dlist <- read_csv("../../intermediate-data/mntaxa_synonymies_20250916.csv")
mntaxa_lineage <- read_csv("../../intermediate-data/mntaxa_dlist_taxonomic_lineage_20250916.csv")


#### format data ####

# add NPC info
releve2 <- releve %>%
  mutate(npc_class = str_sub(npc_code, 1, 5),
         npc_type = if_else(nchar(npc_code) > 5, str_sub(npc_code, 1, 6),
                            NA_character_),
         npc_subtype = if_else(nchar(npc_code) > 6, str_sub(npc_code, 1, 7),
                               NA_character_))

# add classification
# format species names
crosswalk2 <- crosswalk %>%
  left_join(releve2 %>%
               select(relnumb, npc_system, npc_system_id, npc_class_name,
                      npc_class, npc_type_name, npc_type, npc_subtype_name, 
                      npc_subtype)) %>%
  mutate(code_strata = paste0(analysis_group, strata_lower, strata_upper),
         taxon_name_strata =  case_when(strata_lower == 1 & strata_upper == 8 ~
                                          taxon_name,
                                        strata_lower == 1 & strata_upper == 3 ~
                                          paste(taxon_name, "understory"),
                                        strata_lower == 4 & strata_upper == 5 ~
                                          paste(taxon_name, "sub-canopy"),
                                        strata_lower == 6 & strata_upper == 8 ~
                                          paste(taxon_name, "canopy")))

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


#### floristic tables ####

# look at example
load("../RMAVIS/data/nvc_floristic_tables.rda")
head(nvc_floristic_tables)

# samples per group
samps_class <- releve3 %>% 
  count(npc_class)

samps_type <- releve3 %>% 
  filter(!is.na(npc_type)) %>% 
  count(npc_type)

samps_subtype <- releve3 %>% 
  filter(!is.na(npc_subtype)) %>% 
  count(npc_subtype)

# percentage of data classified to type
sum(samps_type$n) / nrow(releve3) # 97%

perc_rels_type <- releve3 %>%
  group_by(npc_class) %>%
  summarize(rels_tot = n_distinct(relnumb),
            rels_type = sum(!is.na(npc_type)),
            .groups = "drop") %>%
  mutate(perc_type = rels_type / rels_tot)
# ROs12, WFn74 don't have types in dataset
# remainder have enough releves representing types

# percentage of data classified to subtype
sum(samps_subtype$n) / nrow(releve3) # only 15%

perc_rels_subtype <- releve3 %>%
  group_by(npc_class) %>%
  summarize(rels_tot = n_distinct(relnumb),
            rels_subtype = sum(!is.na(npc_subtype)),
            .groups = "drop") %>%
  mutate(perc_type = rels_subtype / rels_tot)
# don't use to inform species inclusions

# floristic tables
flor_type <- crosswalk3 %>%
  filter(!is.na(npc_type)) %>%
  group_by(npc_class, npc_type, taxon_name_strata) %>% 
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
  distinct(npc_class, taxon_name_strata)

# only keep more common species
flor_type2 <- flor_type %>%
  inner_join(flor_type_spp) %>%
  select(npc_type, taxon_name_strata, constancy, absolute_frequency, 
         relative_frequency, minimum_cover, mean_cover, maximum_cover)

# class tables
# keep species in at least 5% of a type's samples or 5% of the class (if no types)
flor_class <- crosswalk3 %>%
  group_by(npc_class, taxon_name_strata) %>% 
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
  select(npc_class, taxon_name_strata, constancy, absolute_frequency, 
         relative_frequency, minimum_cover, mean_cover, maximum_cover)

# species kept
flor_class_spp <- flor_class %>%
  distinct(npc_class, taxon_name_strata)

flor_subtype <- crosswalk3 %>%
  filter(!is.na(npc_subtype)) %>%
  group_by(npc_class, npc_type, npc_subtype, taxon_name_strata) %>% 
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
  select(npc_subtype, taxon_name_strata, constancy, absolute_frequency, 
         relative_frequency, minimum_cover, mean_cover, maximum_cover)

# combine
mnnpc_floristic_tables <- flor_class %>%
  rename(npc_code = npc_class) %>%
  rbind(flor_type2 %>%
          rename(npc_code = npc_type)) %>%
  rbind(flor_subtype %>%
          rename(npc_code = npc_subtype)) %>%
  rename(npc_taxon_name = taxon_name_strata)

# check for NAs
mnnpc_floristic_tables %>%
  filter(if_any(everything(), is.na))

# save
save(mnnpc_floristic_tables, file = "data/mnnpc_floristic_tables.rda")


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

# richness by system for species in floristic tables
rich_sys <- releve3 %>% 
  distinct(npc_system_id, npc_class) %>% 
  left_join(flor_class_spp) %>%
  group_by(npc_system_id) %>%
  summarize(species_count = n_distinct(taxon_name_strata),
            .groups = "drop")

# attributes by system
# species counts by releve
att_sys <- crosswalk3 %>%
  inner_join(flor_class_spp) %>%
  group_by(relnumb, npc_system, npc_system_id) %>%
  summarize(n_species = n_distinct(taxon_name_strata),
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

# attributes by class
# calculate richness by class
# add system info
att_class <- crosswalk3 %>%
  inner_join(flor_class_spp) %>%
  group_by(relnumb, npc_class_name, npc_class) %>%
  summarize(n_species = n_distinct(taxon_name_strata),
            .groups = "drop") %>% 
  group_by(npc_class_name, npc_class) %>% 
  summarize(num_samples = n_distinct(relnumb),
            min_species = min(n_species),
            max_species = max(n_species),
            mean_species = mean(n_species) %>% round_half_up(),
            .groups = "drop") %>%
  left_join(flor_class_spp %>%
              count(npc_class) %>%
              rename(species_count = n)) %>%
  left_join(releve3 %>%
              distinct(npc_system_id, npc_class)) %>%
  mutate(fullname = npc_class_name,
         name = npc_class_name,
         npc_code = npc_class,
         npc_code_parent = npc_system_id,
         basal = if_else(npc_class %in% flor_type_spp$npc_class, FALSE, TRUE),
         rank = "class") %>%
  select(fullname, name, npc_code, npc_code_parent, basal, rank, num_samples,
         min_species, max_species, mean_species, species_count)

# attributes by type
att_type <- crosswalk3 %>%
  filter(!is.na(npc_type)) %>%
  inner_join(flor_type_spp) %>%
  group_by(relnumb, npc_type_name, npc_type) %>%
  summarize(n_species = n_distinct(taxon_name_strata),
            .groups = "drop") %>% 
  group_by(npc_type_name, npc_type) %>% 
  summarize(num_samples = n_distinct(relnumb),
            min_species = min(n_species),
            max_species = max(n_species),
            mean_species = mean(n_species) %>% round_half_up(),
            .groups = "drop") %>%
  left_join(flor_type %>%
              count(npc_type) %>%
              rename(species_count = n)) %>%
  left_join(releve3 %>%
              filter(!is.na(npc_type)) %>% 
              distinct(npc_class_name, npc_class, npc_type)) %>%
  mutate(fullname = paste(npc_class_name, npc_type_name, sep = ", "),
         name = npc_type_name,
         npc_code = npc_type,
         npc_code_parent = npc_class,
         basal = if_else(npc_type %in% str_sub(flor_subtype$npc_subtype, 1, 6),
                         FALSE, TRUE),
         rank = "type") %>%
  select(fullname, name, npc_code, npc_code_parent, basal, rank, num_samples,
         min_species, max_species, mean_species, species_count)

# attributes by sub-type
att_subtype <- crosswalk3 %>%
  filter(!is.na(npc_subtype)) %>%
  inner_join(flor_class_spp) %>%
  group_by(relnumb, npc_subtype_name, npc_subtype) %>%
  summarize(n_species = n_distinct(taxon_name_strata),
            .groups = "drop") %>% 
  group_by(npc_subtype_name, npc_subtype) %>% 
  summarize(num_samples = n_distinct(relnumb),
            min_species = min(n_species),
            max_species = max(n_species),
            mean_species = mean(n_species) %>% round_half_up(),
            .groups = "drop") %>%
  left_join(flor_subtype %>%
              count(npc_subtype) %>%
              rename(species_count = n)) %>%
  left_join(releve3 %>%
              distinct(npc_type_name, npc_type, npc_subtype)) %>%
  mutate(fullname = paste(npc_type_name, npc_subtype_name, sep = ", "),
         name = npc_subtype_name,
         npc_code = npc_subtype,
         npc_code_parent = npc_type,
         basal = TRUE,
         rank = "sub-type") %>%
  select(fullname, name, npc_code, npc_code_parent, basal, rank, num_samples,
         min_species, max_species, mean_species, species_count)

# combine
mnnpc_community_attributes <- att_sys %>%
  rbind(att_class) %>%
  rbind(att_type) %>% 
  rbind(att_subtype)

# check for NAs
mnnpc_community_attributes %>%
  filter(if_any(-npc_code_parent, is.na))

mnnpc_community_attributes %>% 
  filter(rank != "system" & is.na(npc_code_parent))

# save
save(mnnpc_community_attributes, file = "data/mnnpc_community_attributes.rda")


#### example data ####

# look at example
load("../RMAVIS/data/example_data.rda")
head(example_data$`Newborough Warren`)

# get releves from database
rel_ex <- releve2 %>%
  filter(relnumb %in% stcroix$RELNO) %>%
  transmute(Site = "St. Croix State Forest",
            Year = year(date_),
            Group = case_when(is.na(place_name) ~ "Control",
                              str_detect(place_name, "Outside Exclosure") ~ 
                                "Outside Exclosure",
                              TRUE ~ "Inside Exclosure"),
            Quadrat = if_else(!is.na(original_releve_nbr), original_releve_nbr,
                              relnumb),
            relnumb = relnumb)

# add species
mnnpc_example_data <- list("St. Croix State Forest" = rel_ex %>%
                             inner_join(crosswalk2 %>%
                                          select(relnumb, taxon_name_strata, scov_adj) %>%
                                          rename(Species = taxon_name_strata,
                                                 Cover = scov_adj)) %>%
                             select(-relnumb))


# check for NAs
mnnpc_example_data$`St. Croix State Forest` %>%
  filter(if_any(everything(), is.na))

# save
save(mnnpc_example_data, file = "data/mnnpc_example_data.rda")


#### accepted taxa ####

# look at example
load("../RMAVIS/data/accepted_taxa.rda")
head(accepted_taxa)

# dlist ID and name
mnnpc_accepted_taxa <- crosswalk3 %>% 
  distinct(taxon_name, taxon_id)

# save
save(mnnpc_accepted_taxa, file = "data/mnnpc_accepted_taxa.rda")


#### taxonomic backbone ####

# look at example
UKVegTB::taxonomic_backbone %>% 
  as_tibble()
# same list as accepted taxa, but with more info

# dlist info
mnnpc_taxonomic_backbone <- mntaxa_dlist %>% 
  select(starts_with("dlist")) %>% 
  distinct() %>% 
  left_join(mntaxa_lineage %>% 
              mutate(dlist_id = as.character(dlist_id)) %>% 
              select(-c(dlist_taxon, dlist_rank))) %>% 
  rename_with(.fn = ~str_remove(.x, "dlist_")) %>% 
  rename(taxon_name = assignment,
         taxon_id = id)
  
# check for missing information
filter(mnnpc_taxonomic_backbone, is.na(publication))

# check for duplicates
get_dupes(mnnpc_taxonomic_backbone, taxon_id)

# save
save(mnnpc_taxonomic_backbone, file = "data/mnnpc_taxonomic_backbone.rda")


#### look-up table ####

# look at example
UKVegTB::taxa_lookup %>% 
  as_tibble()
# all taxa, recommended taxon_name = dlist, recommended TVK = dlist_id

# mntaxa crosswalk
mnnpc_taxa_lookup <- mntaxa_dlist %>% 
  select(taxon, taxon_id, rank, dlist_assignment, dlist_rank, dlist_full_name,
         dlist_id) %>% 
  rename_with(.fn = ~str_replace(.x, "dlist", "recommended") %>% 
                str_replace("assignment", "taxon"))

# check for duplicates
get_dupes(mnnpc_taxa_lookup, taxon_id)

# save
save(mnnpc_taxa_lookup, file = "data/mnnpc_taxa_lookup.rda")
