# load(file = "./data/mnnpc_taxonomic_backbone.rda")

# mnnpc_taxa_aggregation <- mnnpc_taxonomic_backbone 

example_data_options <- c("None" = "none", "St. Croix State Forest" = "St. Croix State Forest")
usethis::use_data(example_data_options, internal = TRUE, overwrite = TRUE)

mnnpc_vc_types <- list("AP", "FD", "FF", "FP", "MH", "MR", "OP", "RO", "UP", "WF", "WM", "WP")
names(mnnpc_vc_types) <- mnnpc_vc_types
usethis::use_data(mnnpc_vc_types, internal = FALSE, overwrite = TRUE)
