# load(file = "./data/mnnpc_taxonomic_backbone.rda")

# mnnpc_taxa_aggregation <- mnnpc_taxonomic_backbone 

example_data_options <- c("None" = "none", "St. Croix State Forest" = "St. Croix State Forest")
usethis::use_data(example_data_options, internal = TRUE, overwrite = TRUE)

mnnpc_vc_types <- list("AP", "FD", "FF", "FP", "MH", "MR", "OP", "RO", "UP", "WF", "WM", "WP")
usethis::use_data(mnnpc_vc_types, internal = FALSE, overwrite = TRUE)

mnnpc_vc_types_named <- c("Acid Peatland (AP)" = "AP",
                          "Fire-Dependent Forest (FD)" = "FD", 
                          "Floodplain Forest (FF)" = "FF", 
                          "Forested Rich Peatland (FP)" = "FP", 
                          "Mesic Hardwood Forest (MH)" = "MH", 
                          "Marsh (MR)" = "MR", 
                          "Open Rich Peatland (OP)" = "OP", 
                          "Rock Outcrop (RO)" = "RO", 
                          "Upland Prarie (UP)" = "UP", 
                          "Wet Forest (WF)" = "WF", 
                          "Wet Meadow/Carr (WM)" = "WM", 
                          "Wet Prarie (WP)" = "WP")
usethis::use_data(mnnpc_vc_types_named, internal = FALSE, overwrite = TRUE)
