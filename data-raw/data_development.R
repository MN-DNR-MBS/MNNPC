#### set-up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(janitor)
library(readxl)

# import data
crosswalk <- read_csv("../../intermediate-data/crosswalk_table_formatted_20251119a.csv",
                      guess_max = Inf) 
crosswalk_raw <- read_csv("../../intermediate-data/crosswalk_raw_no_dups_20251119a.csv",
                          guess_max = Inf)
releve <- read_csv("../../intermediate-data/releve_table_formatted_20251119a.csv")
releve_taxa <- read_csv("../../intermediate-data/releve_taxa_formatted_20251119a.csv")
stcroix <- read_excel("../../data/duxbury_deer_releve_data_working.xlsx")
resample <- read_excel("../../../releve-resample/data/releve_list_southern_20251120.xlsx")
dlist_parents <- read_csv("../../intermediate-data/mntaxa_dlist_parents_20251119.csv",
                          guess_max = Inf)
mntaxa <- read_csv("../../intermediate-data/mntaxa_taxa_20251119.csv")
mntaxa_dlist <- read_csv("../../intermediate-data/mntaxa_synonymies_20251119.csv")
analysis_codes <- read_csv("../../intermediate-data/analysis_codes_20251119.csv")


#### format taxonomic crosswalk ####

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
  filter(!(taxon == "Acorus calamus" & 
             analysis_group == "Acorus calamus") &
           !(taxon == "Arabis holboellii" & 
               analysis_group == "Boechera retrofracta") &
           !(taxon == "Botrychium lunaria" & 
               analysis_group == "Botrychium crenulatum") &
           !(taxon == "Campanula glomerata" & 
               analysis_group == "Campanula cervicaria") &
           !(taxon == "Cardamine pratensis" & 
               analysis_group == "Cardamine dentata") &
           !(taxon == "Cerastium brachypodum" &
               analysis_group == "Cerastium nutans") &
           !(taxon == "Chamerion angustifolium subsp. angustifolium" &
               analysis_group == "Eriophorum angustifolium") &
           !(taxon == "Cornus amomum" &
               analysis_group == "Cornus obliqua") &
           !(taxon == "Quercus x schuettei" &
               analysis_group == "Quercus x hillii") &
           !(taxon == "Scirpus pendulus" &
               analysis_group == "Scirpus atrocinctus/cyperinus/pedicellatus") &
           !(taxon == "Veronica anagallis-aquatica" &
               analysis_group == "Veronica catenata")) %>% 
  distinct()

# add taxa-level info
taxa_crosswalk2 <- taxa_crosswalk %>% 
  left_join(mntaxa %>% 
              select(taxon_id, rank, author, publication))

# for hybrids, add various versions of the x
taxa_crosswalk3 <- taxa_crosswalk2 %>% 
  filter(!is.na(hybrid)) %>% 
  mutate(taxon = str_replace_all(taxon, " x ", " x"),
         taxon = if_else(str_sub(taxon, 1, 2) == "x ",
                         sub("^..", "x", taxon),
                         taxon)) %>% 
  full_join(taxa_crosswalk2 %>% 
              filter(!is.na(hybrid)) %>% 
              mutate(taxon = str_replace_all(taxon, " x ", " X "),
                     taxon = if_else(str_sub(taxon, 1, 2) == "x ",
                                     sub("^.", "X", taxon),
                                     taxon))) %>% 
  full_join(taxa_crosswalk2 %>% 
              filter(!is.na(hybrid)) %>% 
              mutate(taxon = str_replace_all(taxon, " x ", " X"),
                     taxon = if_else(str_sub(taxon, 1, 2) == "x ",
                                     sub("^..", "X", taxon),
                                     taxon))) %>% 
  full_join(taxa_crosswalk2) %>% 
  mutate(hybrid = if_else(!is.na(hybrid), 1, 0),
         dlist_hybrid = if_else(!is.na(dlist_hybrid), 1, 0)) %>% 
  distinct()

# possible tree/strata combos
tree_strata <- taxa_crosswalk3 %>% 
  distinct(dlist_physiognomy) %>% 
  filter(str_detect(dlist_physiognomy, 
                    "broadleaf deciduous|needleleaf evergreen")) %>% 
  cross_join(crosswalk %>% 
               distinct(strata)) # includes NA

# taxa missing phys
taxa_missing_phys <- filter(taxa_crosswalk3, is.na(dlist_physiognomy)) %>% 
  distinct(analysis_group) %>% 
  arrange(analysis_group) %>% 
  data.frame() %>% 
  mutate(dlist_physiognomy_rep = case_when(
    analysis_group == "Acer x freemanii" ~ "broadleaf deciduous",
    analysis_group == "Azolla caroliniana" ~ "floating aquatic",
    analysis_group == "Gentiana x pallidocyanea" ~ "forb",
    analysis_group == "Lathyrus sylvestris" ~ "climber/forb",
    analysis_group == "Lycopodium appalachianum x lucidulum" ~ "clubmoss",
    analysis_group == "Lycopodium appalachianum x selago" ~ "clubmoss",
    analysis_group == "Marsilea quadrifolia" ~ 
      "emergent aquatic/floating aquatic",
    analysis_group == "Pennisetum" ~ "graminoid",
    analysis_group == "Petunia axillaris" ~ "forb",
    analysis_group == "Polystichum lonchitis" ~ "forb"))

# add phys
taxa_crosswalk4 <- taxa_crosswalk3 %>% 
  left_join(taxa_missing_phys) %>% 
  mutate(dlist_physiognomy = if_else(is.na(dlist_physiognomy),
                                     dlist_physiognomy_rep,
                                     dlist_physiognomy)) %>% 
  select(-dlist_physiognomy_rep) %>% 
  distinct()

# add strata to trees
# remove attributes that are not comprehensive
taxa_crosswalk5 <- taxa_crosswalk4 %>% 
  left_join(tree_strata, relationship = "many-to-many") %>% 
  mutate(analysis_group_strata =  if_else(!is.na(strata), 
                                          paste(analysis_group, strata),
                                          analysis_group))

# check for taxa that were ID'd as trees in releves and add strata
nontree_strata <- releve_taxa %>% 
  distinct(analysis_group_strata) %>% 
  anti_join(taxa_crosswalk5) %>% 
  filter(str_detect(analysis_group_strata, "understory|sub-canopy|canopy")) %>% 
  mutate(analysis_group = str_remove(analysis_group_strata,
                                     " understory| sub-canopy| canopy")) %>% 
  distinct(analysis_group) %>% 
  inner_join(taxa_crosswalk4) %>% # info from before strata were added
  cross_join(crosswalk %>% 
               distinct(strata)) %>% 
  mutate(analysis_group_strata =  if_else(!is.na(strata), 
                                          paste(analysis_group, strata),
                                          analysis_group))

# add non-tree strata
taxa_crosswalk6 <- taxa_crosswalk5 %>% 
  full_join(nontree_strata) %>% 
  distinct()

# format releve taxa for check
releve_taxa2 <- releve_taxa %>% 
  mutate(taxon = gsub(" s\\.s\\.", "", taxon),
         taxon = gsub(" s\\.l\\.", "", taxon))

# isolate hard-coded releve groups
releve_groups <- releve_taxa2 %>% 
  distinct(taxon) %>% 
  anti_join(taxa_crosswalk6) %>% 
  inner_join(releve_taxa2) %>% 
  left_join(mntaxa %>% # doesn't really do anything -- they're not in MNTaxa
              select(taxon_id, rank, author, publication)) %>% 
  mutate(dlist_hybrid = if_else(!is.na(dlist_hybrid), 1, 0),
         hybrid = dlist_hybrid,
         rank = replace_na(rank, "group")) %>% 
  select(colnames(taxa_crosswalk6)) %>% 
  distinct()

# add hard-coded releve groups
# remove NA from full name
taxa_crosswalk7 <- taxa_crosswalk6 %>% 
  full_join(releve_groups)

# add strata to taxon name
taxa_strata <- taxa_crosswalk7 %>% 
  filter(!is.na(strata)) %>% 
  mutate(taxon = paste(taxon, strata))

# add analysis groups and taxa strata to taxa list
taxa_crosswalk8 <- taxa_crosswalk7 %>%  
  full_join(taxa_strata) %>% 
  mutate(full_name = taxon %>% 
           str_remove(" understory| sub-canopy| canopy"),
         full_name = if_else(!is.na(author), paste(full_name, author), 
                             full_name),
         taxon_id = as.character(taxon_id)) %>% 
  full_join(taxa_crosswalk7 %>% 
              select(-c(taxon, taxon_id, hybrid, rank, author, publication)) %>% 
              distinct() %>% 
              mutate(taxon = analysis_group_strata,
                     taxon_id = dlist_id,
                     hybrid = dlist_hybrid,
                     rank = dlist_rank,
                     publication = dlist_publication,
                     full_name = dlist_full_name))%>% 
  distinct()

# select necessary columns
taxa_conv <- taxa_crosswalk8 %>% 
  distinct(taxon, analysis_group) %>% 
  as.data.frame()

# duplicates for taxon?
get_dupes(taxa_conv, taxon)

# save
save(taxa_conv, file = "data-raw/data-out-ak/taxa_conv.rds")
load("data-raw/data-out-ak/taxa_conv.rds")


#### format releve data ####

# system info
npc_systems <- releve %>% 
  distinct(npc_system_id, npc_system) %>% 
  mutate(npc_system = str_to_title(npc_system))

# add NPC info
releve2 <- releve %>%
  select(-npc_system) %>% 
  mutate(npc_class = str_sub(npc_code, 1, 5),
         npc_type = if_else(nchar(npc_code) > 5, str_sub(npc_code, 1, 6),
                            NA_character_),
         npc_subtype = if_else(nchar(npc_code) > 6, str_sub(npc_code, 1, 7),
                               NA_character_),
         npc_system_id = if_else(str_detect(npc_system_id, "_|\\/"),
                                 npc_system_id,
                                 str_sub(npc_system_id, 1, 2))) %>% 
  left_join(npc_systems)

# add classification
# format species names
crosswalk2 <- crosswalk %>%
  left_join(releve2 %>%
               select(relnumb, npc_system, npc_system_id, npc_class_name,
                      npc_class, npc_type_name, npc_type, npc_subtype_name, 
                      npc_subtype))

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
flor_class <- crosswalk3 %>%
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

flor_subtype <- crosswalk3 %>%
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
mnnpc_floristic_tables <- flor_class %>%
  rename(npc_code = npc_class) %>%
  rbind(flor_type2 %>%
          rename(npc_code = npc_type)) %>%
  rbind(flor_subtype %>%
          rename(npc_code = npc_subtype)) %>%
  rename(npc_taxon_name = analysis_group_strata) %>% 
  as.data.frame()

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

# richness by system for species in floristic tables
rich_sys <- releve3 %>% 
  distinct(npc_system_id, npc_class) %>% 
  left_join(flor_class_spp) %>%
  group_by(npc_system_id) %>%
  summarize(species_count = n_distinct(analysis_group_strata),
            .groups = "drop")

# attributes by system
# species counts by releve
att_sys <- crosswalk3 %>%
  inner_join(flor_class_spp) %>%
  group_by(relnumb, npc_system, npc_system_id) %>%
  summarize(n_species = n_distinct(analysis_group_strata),
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
  summarize(n_species = n_distinct(analysis_group_strata),
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
  summarize(n_species = n_distinct(analysis_group_strata),
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
  summarize(n_species = n_distinct(analysis_group_strata),
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
  rbind(att_subtype) %>% 
  as.data.frame()

# check for NAs
mnnpc_community_attributes %>%
  filter(if_any(-npc_code_parent, is.na))

mnnpc_community_attributes %>% 
  filter(rank != "system" & is.na(npc_code_parent))

# save
save(mnnpc_community_attributes, 
     file = "data-raw/data-out-ak/mnnpc_community_attributes.rds")
load("data-raw/data-out-ak/mnnpc_community_attributes.rds")


#### example data ####

# look at example
load("../RMAVIS/data/example_data.rda")
head(example_data$`Newborough Warren`)

# get releves from database
rel_ex1 <- releve2 %>%
  filter(relnumb %in% stcroix$RELNO) %>%
  transmute(year = year(date_),
            group = case_when(is.na(place_name) ~ "Control",
                              str_detect(place_name, "Outside Exclosure") ~ 
                                "Outside Exclosure",
                              str_detect(place_name, "Exclosure") ~ 
                                "Inside Exclosure"),
            quadrat = if_else(!is.na(original_releve_nbr), original_releve_nbr,
                              relnumb),
            relnumb = relnumb)

# format resample data
resample2 <- resample %>%
  filter(str_detect(npc, "MHs38|MHs39")) %>% 
  rename(quadrat = orig_relnumb) %>% 
  full_join(resample %>%
              filter(str_detect(npc, "MHs38|MHs39")) %>% 
              select(-relnumb) %>% 
              rename(relnumb = orig_relnumb) %>% 
              mutate(quadrat = relnumb)) %>% 
  mutate(group = if_else(str_detect(npc, "MHs38"), "A", "B"))

rel_ex2 <- releve2 %>% 
  inner_join(resample2 %>% 
               select(relnumb, group, quadrat)) %>% 
  transmute(year = year(date_),
            group = group,
            quadrat = quadrat,
            relnumb = relnumb)

# get taxa from database
rel_tax1 <- rel_ex1 %>% 
  inner_join(crosswalk_raw %>%
               distinct(relnumb, physcode, minht, maxht, taxon, scov_mid)) %>% 
  select(-relnumb) %>% 
  rename(relnumb = quadrat,
         scov = scov_mid) %>% 
  filter(!is.na(scov))

rel_tax2 <- rel_ex2 %>% 
  inner_join(crosswalk_raw %>%
               select(relnumb, physcode, minht, maxht, taxon, scov_mid)) %>% 
  select(-relnumb) %>% 
  rename(relnumb = quadrat,
         scov = scov_mid) %>% 
  filter(!is.na(scov))

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

# save
save(mnnpc_example_data, file = "data-raw/data-out-ak/mnnpc_example_data.rds")
load("data-raw/data-out-ak/mnnpc_example_data.rds")


#### accepted taxa ####

# look at example
load("../RMAVIS/data/accepted_taxa.rda")
head(accepted_taxa)

# dlist ID and name
mnnpc_accepted_taxa <- taxa_crosswalk8 %>% 
  rename(taxon_name = taxon) %>% 
  distinct(taxon_name) %>% 
  arrange(taxon_name) %>% 
  as.data.frame()

# check for duplicates
get_dupes(mnnpc_accepted_taxa)

# make sure all floristic table taxa are included
mnnpc_floristic_tables %>% 
  distinct(npc_taxon_name) %>% 
  rename(taxon_name = npc_taxon_name) %>% 
  anti_join(mnnpc_accepted_taxa)

# save
save(mnnpc_accepted_taxa, file = "data-raw/data-out-ak/mnnpc_accepted_taxa.rds")
load("data-raw/data-out-ak/mnnpc_accepted_taxa.rds")


#### taxonomic backbone ####

# look at example
UKVegTB::taxonomic_backbone %>% 
  as_tibble()
# same list as accepted taxa, but with more info

# missing rank
taxa_crosswalk8 %>% 
  distinct(taxon, rank) %>% 
  filter(is.na(rank)) # ~genus

# add dlist info
# remove species unless taxa is lower
mnnpc_taxonomic_backbone <- taxa_crosswalk8 %>% 
  distinct(taxon, hybrid, rank, publication, full_name) %>% 
  mutate(rank = replace_na(rank, "genus")) %>% 
  rename(taxon_name = taxon) %>% 
  arrange(taxon_name, full_name) %>% 
  as.data.frame()

# check for duplicates
get_dupes(mnnpc_taxonomic_backbone)
  
# check for missing information
filter(mnnpc_taxonomic_backbone, is.na(rank))
filter(mnnpc_taxonomic_backbone, is.na(publication)) # hard coded releve groups

# check that all taxa are included
mnnpc_accepted_taxa %>% 
  anti_join(mnnpc_taxonomic_backbone)

# same taxa as accepted taxa
n_distinct(mnnpc_taxonomic_backbone$taxon_name) == nrow(mnnpc_accepted_taxa)

# save
save(mnnpc_taxonomic_backbone, file = "data-raw/data-out-ak/mnnpc_taxonomic_backbone.rds")
load("data-raw/data-out-ak/mnnpc_taxonomic_backbone.rds")


#### look-up table ####

# look at example
UKVegTB::taxa_lookup %>% 
  as_tibble()
# all taxa, recommended taxon_name = dlist, recommended TVK = dlist_id

# combine parent info for dlist
dlist_parents2 <- dlist_parents %>% 
  mutate(dlist_id_orig = as.character(dlist_id)) %>% 
  select(-c(dlist_id, dlist_taxon, dlist_rank, dlist_hybrid)) %>% 
  cross_join(taxa_crosswalk8 %>% 
               distinct(dlist_id)) %>% 
  filter(str_detect(dlist_id, dlist_id_orig)) %>% 
  group_by(dlist_id) %>% 
  summarize(species = paste(sort(unique(dlist_species)), collapse = "/"),
            genus = paste(sort(unique(dlist_genus)), collapse = "/"),
            family = paste(sort(unique(dlist_family)), collapse = "/"),
            order = paste(sort(unique(dlist_order)), collapse = "/"),
            class = paste(sort(unique(dlist_class)), collapse = "/"),
            phylum = paste(sort(unique(dlist_phylum)), collapse = "/"),
            kingdom = paste(sort(unique(dlist_kingdom)), collapse = "/"),
            lineage_source = paste(sort(unique(dlist_taxonomy_source)), 
                                   collapse = "/")) %>% 
  rename_with(.cols = -dlist_id,
              .fn = ~paste0("recommended_taxon_", .x))

mnnpc_taxa_lookup <- taxa_crosswalk8 %>% 
  rename(taxon_name = taxon,
         recommended_taxon_name = analysis_group) %>% 
  select(taxon_name, recommended_taxon_name, starts_with("dlist")) %>% 
  distinct() %>% 
  left_join(dlist_parents2) %>% 
  rename_with(.fn = ~str_replace(.x, "dlist_", "recommended_taxon_") %>% 
                str_replace("assignment", "included")) %>%
  rename(informal_group = "recommended_taxon_physiognomy") %>% 
  mutate(recommended_taxon_species = if_else(recommended_taxon_rank %in% 
                                               c("subspecies", "variety", 
                                                         "subspecies/vaeriety"),
                                       recommended_taxon_species, NA_character_),
         recommended_taxon_included = if_else(str_detect(recommended_taxon_name, 
                                                         "and genus"),
                                             paste(recommended_taxon_genus, 
                                                   recommended_taxon_included, 
                                                   sep = "/"),
                                             recommended_taxon_included)) %>% 
         #recommended_hybrid = if_else(!is.na(recommended_hybrid), 1, 0)) %>% 
  relocate(recommended_taxon_id, .after = "recommended_taxon_included") %>% 
  relocate(informal_group) %>% 
  arrange(recommended_taxon_name, taxon_name)

# check for duplicates
get_dupes(mnnpc_taxa_lookup, taxon_name)

# all accepted taxa included?
mnnpc_accepted_taxa %>% 
  anti_join(mnnpc_taxa_lookup)

# save
save(mnnpc_taxa_lookup, file = "data-raw/data-out-ak/mnnpc_taxa_lookup.rds")
load("data-raw/data-out-ak/mnnpc_taxa_lookup.rds")


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
          "../../intermediate-data/floristic_table_development_data_20251125.csv")
