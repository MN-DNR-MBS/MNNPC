example_data_options <- c("None" = "none", 
                          "St. Croix State Forest" = "St. Croix State Forest",
                          "Earthworm-Invaded Forests" = "Earthworm-Invaded Forests")
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

# Scov midpoint conversion
mnnpc_scov_conv <- data.frame(scov = c(as.character(5:1), "+", "r"),
                              scov_mid = c(87.5, 62.5, 37.5, 15, 2.5, 0.5, 0.1))

usethis::use_data(mnnpc_scov_conv, internal = FALSE, overwrite = TRUE)

# Strata for physcodes
mnnpc_strata <- data.frame(physcode = c(rep(c("D", "E"), each = 3), "B", "C", "F", "G", "H", "K", "S", "X"),
                           strata_lower = c(rep(c(1, 4, 6), 2), rep(1, 8)),
                           strata_upper = c(rep(c(3, 5, 8), 2), rep(8, 8)))

usethis::use_data(mnnpc_strata, internal = FALSE, overwrite = TRUE)

# Height conversion table
mnnpc_ht_conv <- data.frame(ht = 1:8,
                            ht_min_m = c(0, 0.1, 0.5, 2, 5, 10, 20, 35),
                            ht_max_m = c(0.1, 0.5, 2, 5, 10, 20, 35, 50))

usethis::use_data(mnnpc_ht_conv, internal = FALSE, overwrite = TRUE)

