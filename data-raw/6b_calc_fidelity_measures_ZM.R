base_fp <- file.path("C:", "Users", "zekmar", "OneDrive - UKCEH", "Projects", "MNNPC")

fidelity_metrics <- read.csv(file = file.path(base_fp, "mnnpc_fidelity_metrics.csv"))

# usethis::use_data(fidelity_metrics, internal = FALSE, overwrite = TRUE)