renv::install(file.path("..", "RMAVIS_1.2.0.tar.gz"), repos = NULL, type = "source")

base_fp <- file.path("C:", "Users", "zekmar", "OneDrive - UKCEH", "Projects", "MNNPC")
mnnpc_dd_fp <- file.path(base_fp, "floristic_table_development_data_20251125.csv")

mnnpc_dd <- read.csv(file = mnnpc_dd_fp) |>
  tibble::as_tibble() |>
  dplyr::select(npc_class, npc_type, npc_subtype, Quadrat, Species, Cover)


higher_taxa <- MNNPC::mnnpc_taxonomic_backbone |>
  tibble::as_tibble() |>
  dplyr::select("taxon_name" = "recommended_taxon_name",
                "Kingdom" = "kingdom",
                "Phylum" = "phylum",
                "Class" = "class",
                "Order" = "order",
                "Family" = "family",
                "Genus" = "genus") |>
  dplyr::distinct()


plot_data_class <- mnnpc_dd |>
  tidyr::nest(data = c(npc_class, npc_type, npc_subtype, Quadrat, Species, Cover), .by = c("npc_class")) |>
  dplyr::summarise(
    "data" = list(dplyr::bind_rows(data)),
    .by = c(npc_class)
  )

plot_data_type <- mnnpc_dd |>
  tidyr::nest(data = c(npc_class, npc_type, npc_subtype, Quadrat, Species, Cover), .by = c("npc_class", "npc_type")) |>
  dplyr::summarise(
    "data" = list(dplyr::bind_rows(data)),
    .by = c(npc_class, npc_type)
  )

plot_data_subtype <- mnnpc_dd |>
  tidyr::nest(data = c(npc_class, npc_type, npc_subtype, Quadrat, Species, Cover), .by = c("npc_class", "npc_type", "npc_subtype")) |>
  dplyr::summarise(
    "data" = list(dplyr::bind_rows(data)),
    .by = c(npc_class, npc_type, npc_subtype)
  ) |>
  dplyr::filter(!is.na(npc_subtype))

plot_data_quadrat <- mnnpc_dd |>
  tidyr::nest(data = c(npc_class, npc_type, npc_subtype, Quadrat, Species, Cover), .by = c("npc_class", "npc_type", "npc_subtype", "Quadrat")) |>
  dplyr::summarise(
    "data" = list(dplyr::bind_rows(data)),
    .by = c(npc_class, npc_type, npc_subtype, Quadrat)
  )


plot_data_class_results <- plot_data_class |>
  dplyr::rowwise() |>
  dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                      higher_taxa = higher_taxa, 
                                                                      phylo_tree = MNNPC::mnnpc_phlyo_tree, # phlyo -> phylo
                                                                      phylo_taxa_lookup = MNNPC::mnnpc_phylo_taxa_lookup,
                                                                      groups = c("npc_class", "npc_type", "npc_subtype", "Quadrat"))),
                .keep = "unused") |>
  dplyr::cross_join(tibble::tibble("q" = c(0, 1, 2))) |>
  dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_meta(rdiv_objects = rdiv_objects,
                                                                      q = q))) |>
  dplyr::ungroup() |>
  dplyr::select(-rdiv_objects, -q) |>
  tidyr::unnest(results) |>
  suppressMessages()

plot_data_type_results <- plot_data_type |>
  dplyr::rowwise() |>
  dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                      higher_taxa = higher_taxa, 
                                                                      phylo_tree = MNNPC::mnnpc_phlyo_tree, # phlyo -> phylo
                                                                      phylo_taxa_lookup = MNNPC::mnnpc_phylo_taxa_lookup,
                                                                      groups = c("npc_class", "npc_type", "npc_subtype", "Quadrat"))),
                .keep = "unused") |>
  dplyr::cross_join(tibble::tibble("q" = c(0, 1, 2))) |>
  dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_meta(rdiv_objects = rdiv_objects,
                                                                      q = q))) |>
  dplyr::ungroup() |>
  dplyr::select(-rdiv_objects, -q) |>
  tidyr::unnest(results) |>
  suppressMessages()

plot_data_subtype_results <- plot_data_subtype |>
  dplyr::rowwise() |>
  dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                      higher_taxa = higher_taxa, 
                                                                      phylo_tree = MNNPC::mnnpc_phlyo_tree, # phlyo -> phylo
                                                                      phylo_taxa_lookup = MNNPC::mnnpc_phylo_taxa_lookup,
                                                                      groups = c("npc_class", "npc_type", "npc_subtype", "Quadrat"))),
                .keep = "unused") |>
  dplyr::cross_join(tibble::tibble("q" = c(0, 1, 2))) |>
  dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_meta(rdiv_objects = rdiv_objects,
                                                                      q = q))) |>
  dplyr::ungroup() |>
  dplyr::select(-rdiv_objects, -q) |>
  tidyr::unnest(results) |>
  suppressMessages()

plot_data_quadrat_results <- plot_data_quadrat |>
  dplyr::rowwise() |>
  dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                      higher_taxa = higher_taxa, 
                                                                      phylo_tree = MNNPC::mnnpc_phlyo_tree, # phlyo -> phylo
                                                                      phylo_taxa_lookup = MNNPC::mnnpc_phylo_taxa_lookup,
                                                                      groups = c("npc_class", "npc_type", "npc_subtype", "Quadrat"))),
                .keep = "unused") |>
  dplyr::cross_join(tibble::tibble("q" = c(0, 1, 2))) |>
  dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_subcom(rdiv_objects = rdiv_objects,
                                                                        q = q))) |>
  dplyr::ungroup() |>
  dplyr::select(-rdiv_objects, -q) |>
  tidyr::unnest(results) |>
  suppressMessages()

write.csv(x = plot_data_class_results, file = file.path(base_fp, "plot_data_class_results.csv"), row.names = FALSE)
write.csv(x = plot_data_type_results, file = file.path(base_fp, "plot_data_type_results.csv"), row.names = FALSE)
write.csv(x = plot_data_subtype_results, file = file.path(base_fp, "plot_data_subtype_results.csv"), row.names = FALSE)
write.csv(x = plot_data_quadrat_results, file = file.path(base_fp, "plot_data_quadrat_results.csv"), row.names = FALSE)

