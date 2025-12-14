#### set-up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(janitor)
library(readxl)

# import data
crosswalk <- read_csv("../../intermediate-data/crosswalk_table_formatted_20251214a.csv",
                      guess_max = Inf) 
crosswalk_raw <- read_csv("../../intermediate-data/crosswalk_raw_no_dups_20251214a.csv",
                          guess_max = Inf)
releve <- read_csv("../../intermediate-data/releve_table_formatted_20251214a.csv")
stcroix <- read_excel("../../data/duxbury_deer_releve_data_working.xlsx")
resample <- read_excel("../../../releve-resample/data/releve_list_southern_20251120.xlsx")
mntaxa_lookup <- read_csv("../../intermediate-data/mntaxa_lookup_20251214.csv")
mntaxa <- read_csv("../../intermediate-data/mntaxa_taxa_20251214.csv")
mntaxa_dlist <- read_csv("../../intermediate-data/mntaxa_synonymies_20251214.csv")
analysis_codes <- read_csv("../../intermediate-data/analysis_codes_20251214.csv")
releve_taxa <- read_csv("../../intermediate-data/releve_taxa_formatted_20251214a.csv")
mntaxa_lookup_genus <- read_csv("../../intermediate-data/mntaxa_lookup_genera_20251214.csv")


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

# select one releve for formatting example
example_releve <- mnnpc_example_data$`St. Croix State Forest` %>% 
  filter(relnumb == "4710")

# save
save(example_releve, file = "data-raw/data-out-ak/raw_releve_example.rds")
load("data-raw/data-out-ak/raw_releve_example.rds")


#### look-up table ####

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

# select column
# add analysis group taxa
# format hybrid columns
# add analysis groups
mntaxa_lookup2 <- mntaxa_lookup %>% 
  select(taxon, hybrid, rank, c_value, starts_with("dlist")) %>%
  distinct() %>% 
  left_join(taxa_crosswalk %>% 
              distinct(taxon, analysis_group, analysis_group_includes)) %>% 
  full_join(analysis_taxa2 %>% 
              select(taxon, hybrid, rank, c_value, starts_with("dlist"), 
                     analysis_group, analysis_group_includes) %>%
              distinct()) %>% 
  mutate(hybrid = if_else(!is.na(hybrid), 1, 0),
         dlist_hybrid = if_else(!is.na(dlist_hybrid), 1, 0))

# possible tree/strata combos
tree_strata <- mntaxa_lookup2 %>% 
  distinct(dlist_physiognomy) %>% 
  filter(dlist_physiognomy %in% 
           c("broadleaf deciduous", "needleleaf evergreen",
             "broadleaf deciduous/needleleaf evergreen")) %>% 
  cross_join(crosswalk %>% 
               distinct(strata)) # includes NA

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
  select(-dlist_physiognomy_rep) %>% 
  left_join(tree_strata, relationship = "many-to-many") %>% 
  mutate(analysis_group_strata = if_else(!is.na(strata), 
                                         paste(analysis_group, strata), 
                                         analysis_group))

# taxa in floristic tables that aren't in lookup
# will likely exclude these when we clean up physcode-strata relationship
# all are accepted names + strata
floristic_strata <- mnnpc_floristic_tables %>%
  distinct(npc_taxon_name) %>%
  rename(analysis_group_strata = npc_taxon_name) %>% 
  anti_join(mntaxa_lookup3) %>% 
  transmute(strata2 = word(analysis_group_strata, -1),
            analysis_group = word(analysis_group_strata, 1, -2))

# check that all are strata
floristic_strata %>% 
  filter(!(strata2) %in% tree_strata$strata)
# should return 0

# check that all species are in the data
floristic_strata %>% 
  distinct(analysis_group) %>% 
  anti_join(mntaxa_lookup2)
# should return 0

# add floristic strata to lookup
mntaxa_lookup4 <- mntaxa_lookup3 %>% 
  select(-analysis_group_strata) %>% 
  left_join(floristic_strata %>% 
              full_join(floristic_strata %>% 
                          distinct(analysis_group) %>% 
                          mutate(strata2 = NA_character_)), # add NA's too
            relationship = "many-to-many") %>% 
  mutate(strata = if_else(is.na(strata) & !is.na(strata2), strata2, strata),
         taxon =  if_else(!is.na(strata), paste(taxon, strata), taxon),
         dlist_assignment =  if_else(!is.na(strata), 
                                     paste(dlist_assignment, strata), 
                                     dlist_assignment),
         dlist_scientific_name = dlist_full_name,
         dlist_full_name = if_else(!is.na(strata), 
                                   paste(dlist_full_name, strata), 
                                   dlist_full_name),
         analysis_group_strata = if_else(!is.na(strata), 
                                         paste(analysis_group, strata), 
                                         analysis_group)) %>% 
  select(-c(strata, strata2))

# check that all analysis_groups are included
mnnpc_floristic_tables %>%
  distinct(npc_taxon_name) %>%
  rename(analysis_group_strata = npc_taxon_name) %>% 
  anti_join(mntaxa_lookup4)
# should return 0

# add dlist to taxon list
# add analysis group to taxon list
# remove duplicates with multiple hybrid values
# change rank of species groups
mntaxa_lookup5 <- mntaxa_lookup4 %>% 
  full_join(mntaxa_lookup4 %>% 
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
         informal_group = recommended_physiognomy)

# format columns for look-up table
mnnpc_taxa_lookup <- mntaxa_lookup5 %>% 
  select(-analysis_group) %>% 
  rename(analysis_group = analysis_group_strata) %>% 
  select(informal_group, taxon_name, recommended_taxon_name, recommended_rank,
         recommended_scientific_name, recommended_full_name, 
         recommended_id, recommended_publication, analysis_group,
         analysis_group_includes) %>% 
  arrange(taxon_name) %>% 
  as.data.frame()

# check for duplicates
get_dupes(mnnpc_taxa_lookup, taxon_name)

# check that all taxa (besides hard-coded groups) are included
crosswalk_raw_missing <- crosswalk_raw %>% 
  distinct(taxon, taxon_new) %>% 
  mutate(taxon_name = str_remove(taxon, " s\\.s\\.") %>% 
           str_remove(" s\\.l\\.") %>% 
           str_remove(" s\\.a\\."),
         taxon_name = if_else(str_detect(taxon_new, "×|Rubus rosa|Cystopteris laurentiana|Amelanchier intermedia"), 
                              str_replace(taxon_name, " x", " x ") %>% 
                                str_replace("x  ", "x ") %>% 
                                str_replace("xE", "x E"),
                              taxon_name),
         cw_rank = case_when(str_count(taxon_name, "\\w+") == 1 ~ "genus",
                             str_detect(taxon_name, "var\\.|subsp\\.") ~ "sub-species",
                             TRUE ~ "species")) %>% 
  anti_join(mnnpc_taxa_lookup)

# check that all are accounted for
crosswalk_raw_missing %>% 
  filter(cw_rank != "genus" & !is.na(taxon_new))
# should return 0

# missing data
filter(mnnpc_taxa_lookup, if_any(everything(), is.na))
# should return 0

# save
save(mnnpc_taxa_lookup, file = "data-raw/data-out-ak/mnnpc_taxa_lookup.rds")
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

# make sure all floristic table taxa are included (these have analysis groups)
mnnpc_floristic_tables %>% 
  distinct(npc_taxon_name) %>% 
  rename(taxon_name = npc_taxon_name) %>% 
  filter(str_detect(taxon_name, "genus|group|/|\\(") == F) %>% 
  anti_join(mnnpc_accepted_taxa)
# should return 0

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
  select(starts_with("recommended")) %>% 
  distinct() %>% 
  rename_with(.fn = ~str_remove(.x, "recommended_")) %>% 
  select(id, taxon_name, rank, scientific_name, full_name, publication,
         common_name, origin, species:kingdom)

# check for duplicates
get_dupes(mnnpc_taxonomic_backbone, taxon_name)

# missing data
mnnpc_taxonomic_backbone %>% 
  select(-c(common_name, species)) %>% 
  filter(if_any(everything(), is.na))

mnnpc_taxonomic_backbone %>% 
  filter(is.na(species)) %>% 
  distinct(rank)

# check that all taxa are included
mnnpc_accepted_taxa %>% 
  anti_join(mnnpc_taxonomic_backbone)
# shoudl return 0

# same taxa as accepted taxa
n_distinct(mnnpc_taxonomic_backbone$taxon_name) == nrow(mnnpc_accepted_taxa)

# save
save(mnnpc_taxonomic_backbone, file = "data-raw/data-out-ak/mnnpc_taxonomic_backbone.rds")
load("data-raw/data-out-ak/mnnpc_taxonomic_backbone.rds")


#### format taxonomic crosswalk ####

# rename taxon
taxa_conv1 <- mntaxa_lookup5 %>% 
  rename(taxon = taxon_name)

# for hybrids, add various versions of the x
taxa_conv2 <- taxa_conv1 %>% 
  filter(hybrid == 1) %>% 
  mutate(taxon = str_replace_all(taxon, " x ", " x"),
         taxon = if_else(str_sub(taxon, 1, 2) == "x ",
                         sub("^..", "x", taxon),
                         taxon)) %>% 
  full_join(taxa_conv1 %>% 
              filter(hybrid == 1) %>% 
              mutate(taxon = str_replace_all(taxon, " x ", " X "),
                     taxon = if_else(str_sub(taxon, 1, 2) == "x ",
                                     sub("^.", "X", taxon),
                                     taxon))) %>% 
  full_join(taxa_conv1 %>% 
              filter(hybrid == 1) %>% 
              mutate(taxon = str_replace_all(taxon, " x ", " X"),
                     taxon = if_else(str_sub(taxon, 1, 2) == "x ",
                                     sub("^..", "X", taxon),
                                     taxon))) %>% 
  full_join(taxa_conv1) %>% 
  distinct() %>% 
  distinct(taxon, analysis_group)

# format releve taxa
releve_taxa2 <- releve_taxa %>% 
  mutate(taxon = gsub(" s\\.s\\.", "", taxon),
         taxon = gsub(" s\\.l\\.", "", taxon))

# isolate hard-coded releve groups
releve_groups <- releve_taxa2 %>% 
  distinct(taxon) %>% 
  anti_join(taxa_conv2) %>% 
  inner_join(releve_taxa2 %>% 
               distinct(taxon, analysis_group))

# add hard-coded releve groups
# remove NA from full name
taxa_conv <- taxa_conv2 %>% 
  full_join(releve_groups)

# duplicates for taxon?
get_dupes(taxa_conv, taxon)

# save
save(taxa_conv, file = "data-raw/data-out-ak/taxa_conv.rds")
load("data-raw/data-out-ak/taxa_conv.rds")


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
          "../../intermediate-data/floristic_table_development_data_20251214.csv")
