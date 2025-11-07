# load(file = "./data/mnnpc_taxonomic_backbone.rda")

# mnnpc_taxa_aggregation <- mnnpc_taxonomic_backbone 

mnnpc_vc_types <- list("AP", "FD", "FF", "FP", "MH", "MR", "OP", "RO", "UP", "WF", "WM", "WP")
usethis::use_data(mnnpc_vc_types, internal = FALSE, overwrite = TRUE)

