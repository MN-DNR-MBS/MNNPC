base_fp <- file.path("C:", "Users", "zekmar", "OneDrive - UKCEH", "Projects", "MNNPC")

mnnpc_fidelity_metrics <- read.csv(file = file.path(base_fp, "mnnpc_fidelity_metrics.csv"))

usethis::use_data(mnnpc_fidelity_metrics, internal = FALSE, overwrite = TRUE, compress = "xz")
