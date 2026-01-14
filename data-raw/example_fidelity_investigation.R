# This example used U!!!

# Select system and metric and filter fidelity values based on a cut-off value
cut_val <- 2
sys <- "AP"

foo <- MNNPC::mnnpc_fidelity_metrics |>
  dplyr::select(ecs_section, system, npc_class, species, u) |>
  dplyr::filter(system == sys) |>
  tidyr::drop_na() |>
  dplyr::mutate("u_abs" = abs(u)) |>
  dplyr::filter(u_abs >= cut_val) |>
  dplyr::select(-system, -u_abs) |>
  tidyr::pivot_wider(id_cols = c(ecs_section, species),
                     names_from = npc_class,
                     values_from = u) |>
  tidyr::drop_na()


# Join floristic tables and fidelity metrics then filter for high/low constancy
const <- 4

goo <- MNNPC::mnnpc_floristic_tables |>
  dplyr::rename("species" = "npc_taxon_name") |>
  collapse::join(MNNPC::mnnpc_fidelity_metrics |> dplyr::select(ecs_section, system, "npc_code" = "npc_class", species, u), on = c("species", "npc_code"), how = "inner") |>
  collapse::fsubset(constancy >= const)


# Compare two units
units <- c("FDn43", "FPn63")

hoo <- MNNPC::mnnpc_floristic_tables |>
  dplyr::rename("species" = "npc_taxon_name") |>
  collapse::join(MNNPC::mnnpc_fidelity_metrics |> dplyr::select(ecs_section, system, "npc_code" = "npc_class", species, u), on = c("species", "npc_code"), how = "inner") |>
  dplyr::filter(npc_code %in% units) |>
  dplyr::mutate("u_abs" = abs(u)) |>
  dplyr::filter(u_abs >= cut_val) |>
  dplyr::select(-u_abs) |>
  dplyr::select(npc_code, species, u) |>
  tidyr::pivot_wider(id_cols = c(species),
                     names_from = npc_code,
                     values_from = u)
