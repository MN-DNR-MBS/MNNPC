# renv::install(file.path("..", "RMAVIS_1.2.0.tar.gz"), repos = NULL, type = "source")

base_fp <- file.path("C:", "Users", "zekmar", "OneDrive - UKCEH", "Projects", "MNNPC")
mnnpc_dd_fp <- file.path(base_fp, "floristic_table_development_data_2026-02-21.csv")
input_path <- file.path("data-raw", "data-out-ak")

load(file.path(input_path, "mnnpc_taxonomic_backbone.rds"))

higher_taxa <- mnnpc_taxonomic_backbone |>
  tibble::as_tibble() |>
  dplyr::select(taxon_name,
                "Kingdom" = "kingdom",
                "Phylum" = "phylum",
                "Class" = "class",
                "Order" = "order",
                "Family" = "family",
                "Genus" = "genus") |>
  dplyr::distinct()

strings_to_remove <- paste(x = c(" canopy",
                                 " subcanopy",
                                 " sub-canopy",
                                 " understory"), 
                           collapse = "|", sep = "")

mnnpc_dd <- read.csv(file = mnnpc_dd_fp) |>
  tibble::as_tibble() |>
  dplyr::select(npc_class, npc_type, npc_subtype, ecs_section, Quadrat, Species, Cover)|>
  dplyr::mutate("Species" = stringr::str_remove_all(string = Species, pattern = strings_to_remove)) |>
  dplyr::summarise("Cover" = sum(Cover), .by = c("npc_class", "npc_type", "npc_subtype", "ecs_section", "Quadrat", "Species")) |>
  dplyr::mutate("Cover" = ifelse(Cover > 100, 100, Cover))

# Check for duplicates
mnnpc_dd |>
  dplyr::group_by(npc_class, npc_type, npc_subtype, ecs_section, Quadrat, Species) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup()

# Check no cover values are above 100
summary(mnnpc_dd)

ecs_sections <- sort(setdiff(unique(mnnpc_dd$ecs_section), NA))

calc_mnnpc_div <- function(data, ecs_sections, output_name){
  
  data_use <- data |>
    dplyr::filter(ecs_section %in% ecs_sections) |>
    dplyr::select(-ecs_section)
  
  plot_data_class <- data_use |>
    tidyr::nest(data = c(npc_class, npc_type, npc_subtype, Quadrat, Species, Cover), .by = c("npc_class")) |>
    dplyr::summarise(
      "data" = list(dplyr::bind_rows(data)),
      .by = c(npc_class)
    )
  
  plot_data_type <- data_use |>
    tidyr::nest(data = c(npc_class, npc_type, npc_subtype, Quadrat, Species, Cover), .by = c("npc_class", "npc_type")) |>
    dplyr::summarise(
      "data" = list(dplyr::bind_rows(data)),
      .by = c(npc_class, npc_type)
    )
  
  plot_data_subtype <- data_use |>
    tidyr::nest(data = c(npc_class, npc_type, npc_subtype, Quadrat, Species, Cover), .by = c("npc_class", "npc_type", "npc_subtype")) |>
    dplyr::summarise(
      "data" = list(dplyr::bind_rows(data)),
      .by = c(npc_class, npc_type, npc_subtype)
    ) |>
    dplyr::filter(!is.na(npc_subtype))
  
  plot_data_quadrat <- data_use |>
    tidyr::nest(data = c(npc_class, npc_type, npc_subtype, Quadrat, Species, Cover), .by = c("npc_class", "npc_type", "npc_subtype", "Quadrat")) |>
    dplyr::summarise(
      "data" = list(dplyr::bind_rows(data)),
      .by = c(npc_class, npc_type, npc_subtype, Quadrat)
    )
  
  
  plot_data_class_results <- plot_data_class |>
    dplyr::rowwise() |>
    dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                        higher_taxa = higher_taxa, 
                                                                        phylo_tree = MNNPC::mnnpc_phylo_tree, # phylo -> phylo
                                                                        phylo_taxa_lookup = MNNPC::mnnpc_phylo_taxa_lookup,
                                                                        groups = c("npc_class", "npc_type", "npc_subtype", "Quadrat"))),
                  .keep = "unused") |>
    dplyr::cross_join(tibble::tibble("q" = c(0, 1, 2))) |>
    dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_meta(rdiv_objects = rdiv_objects,
                                                                        q = q))) |>
    dplyr::ungroup() |>
    dplyr::select(-rdiv_objects, -q) |>
    tidyr::unnest(results) |>
    suppressMessages() |>
    dplyr::mutate("ecs_section" = output_name,
                  "rank" = "class")
  
  plot_data_type_results <- plot_data_type |>
    dplyr::rowwise() |>
    dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                        higher_taxa = higher_taxa, 
                                                                        phylo_tree = MNNPC::mnnpc_phylo_tree, # phylo -> phylo
                                                                        phylo_taxa_lookup = MNNPC::mnnpc_phylo_taxa_lookup,
                                                                        groups = c("npc_class", "npc_type", "npc_subtype", "Quadrat"))),
                  .keep = "unused") |>
    dplyr::cross_join(tibble::tibble("q" = c(0, 1, 2))) |>
    dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_meta(rdiv_objects = rdiv_objects,
                                                                        q = q))) |>
    dplyr::ungroup() |>
    dplyr::select(-rdiv_objects, -q) |>
    tidyr::unnest(results) |>
    suppressMessages() |>
    dplyr::mutate("ecs_section" = output_name,
                  "rank" = "type")
  
  plot_data_subtype_results <- plot_data_subtype |>
    dplyr::rowwise() |>
    dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                        higher_taxa = higher_taxa, 
                                                                        phylo_tree = MNNPC::mnnpc_phylo_tree, # phylo -> phylo
                                                                        phylo_taxa_lookup = MNNPC::mnnpc_phylo_taxa_lookup,
                                                                        groups = c("npc_class", "npc_type", "npc_subtype", "Quadrat"))),
                  .keep = "unused") |>
    dplyr::cross_join(tibble::tibble("q" = c(0, 1, 2))) |>
    dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_meta(rdiv_objects = rdiv_objects,
                                                                        q = q))) |>
    dplyr::ungroup() |>
    dplyr::select(-rdiv_objects, -q) |>
    tidyr::unnest(results) |>
    suppressMessages() |>
    dplyr::mutate("ecs_section" = output_name,
                  "rank" = "subtype")
  
  plot_data_quadrat_results <- plot_data_quadrat |>
    dplyr::rowwise() |>
    dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                        higher_taxa = higher_taxa, 
                                                                        phylo_tree = MNNPC::mnnpc_phylo_tree, # phylo -> phylo
                                                                        phylo_taxa_lookup = MNNPC::mnnpc_phylo_taxa_lookup,
                                                                        groups = c("npc_class", "npc_type", "npc_subtype", "Quadrat"))),
                  .keep = "unused") |>
    dplyr::cross_join(tibble::tibble("q" = c(0, 1, 2))) |>
    dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_subcom(rdiv_objects = rdiv_objects,
                                                                          q = q))) |>
    dplyr::ungroup() |>
    dplyr::select(-rdiv_objects, -q) |>
    tidyr::unnest(results) |>
    suppressMessages() |>
    dplyr::mutate("ecs_section" = output_name,
                  "rank" = "quadrat")
  
  mnnpc_diversity_results <- dplyr::bind_rows(plot_data_class_results,
                                              plot_data_type_results,
                                              plot_data_subtype_results,
                                              plot_data_quadrat_results)
  
  return(mnnpc_diversity_results)
  
}

mnnpc_diversity_results_all <- calc_mnnpc_div(data = mnnpc_dd, ecs_sections = ecs_sections, output_name = "statewide")

for(section in ecs_sections){
  
  mnnpc_diversity_results_section <- calc_mnnpc_div(data = mnnpc_dd, ecs_sections = section, output_name = section)
  
  mnnpc_diversity_results_all <- mnnpc_diversity_results_all |>
    dplyr::bind_rows(mnnpc_diversity_results_section)
  
}

mnnpc_diversity_metrics <- mnnpc_diversity_results_all |>
  dplyr::select(ecs_section, rank, npc_class, npc_type, npc_subtype, Quadrat,
                measure, "metric" = "dat_id", partition_level, q, diversity)

write.csv(x = mnnpc_diversity_metrics, file = file.path(base_fp, paste0("mnnpc_diversity_metrics", ".csv")), row.names = FALSE)

usethis::use_data(mnnpc_diversity_metrics, internal = FALSE, overwrite = TRUE, compress = "xz")
