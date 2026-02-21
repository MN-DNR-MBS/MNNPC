# Establish filepaths
input_path <- file.path("data")
output_path <- file.path("C:", "Users", "zekmar", "OneDrive - UKCEH", "Projects", "MNNPC")

# Load MN NPC plots
load(file.path(input_path, "mnnpc_development_data.rda"))

# Inspect data
str(mnnpc_development_data)

# Prepare data for fidelity calculations
floristic_table_development_data <- mnnpc_development_data |>
  dplyr::select(Quadrat,
                Species,
                Cover,
                npc_class,
                npc_type,
                npc_subtype,
                ecs_section)

# Write data
write.csv(x = floristic_table_development_data, file = file.path(output_path, paste0("floristic_table_development_data_", Sys.Date(), ".csv")), row.names = FALSE)
