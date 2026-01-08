# Load objects ------------------------------------------------------------
input_path <- file.path("data-raw", "data-out-ak")

# Internal Objects --------------------------------------------------------
example_data_options <- c("None" = "none", 
                          "St. Croix State Forest" = "St. Croix State Forest",
                          "Earthworm-Invaded Forests" = "Earthworm-Invaded Forests")

usethis::use_data(example_data_options,
                  internal = TRUE, overwrite = TRUE)

# External Objects --------------------------------------------------------
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

mnnpc_vc_types <- unname(mnnpc_vc_types_named)
usethis::use_data(mnnpc_vc_types, internal = FALSE, overwrite = TRUE)

mnnpc_vc_types_flreg_named <- c("Acid Peatland, Northern (APn)" = "APn",
                                "Fire-Dependent Forest, Central (FDc)" = "FDc",
                                "Fire-Dependent Forest, Northern (FDn)" = "FDn",
                                "Fire-Dependent Forest, Southern (FDs)" = "FDs",
                                "Fire-Dependent Forest, Northwestern (FDw)" = "FDw",
                                "Floodplain Forest, Northern (FFn)" = "FFn",
                                "Floodplain Forest, Southern (FFs)" = "FFs",
                                "Forested Rich Peatland, Northern (FPn)" = "FPn",
                                "Forested Rich Peatland, Southern  (FPs)" = "FPs",
                                "Forested Rich Peatland, Northwestern  (FPw)" = "FPw",
                                "Mesic Hardwood Forest, Central (MHc)" = "MHc",
                                "Mesic Hardwood Forest, Northern (MHn)" = "MHn",
                                "Mesic Hardwood Forest, Southern (MHs)" = "MHs",
                                "Mesic Hardwood Forest, Northwestern (MHw)" = "MHw",
                                "Marsh, Northern (MRn)" = "MRn",
                                "Marsh, Prairie (MRp)" = "MRp",
                                "Marsh, Lake Superior (MRu)" = "MRu",
                                "Open Rich Peatland, Northern (OPn)" = "OPn",
                                "Open Rich Peatland, Prairie (OPp)" = "OPp",
                                "Rock Outcrop, Southern (ROs)" = "ROs",
                                "Upland Prarie, Northern (UPn)" = "UPn",
                                "Upland Prarie, Southern (UPs)" = "UPs",
                                "Wet Forest, Northern (WFn)" = "WFn",
                                "Wet Forest, Southern (WFs)" = "WFs",
                                "Wet Forest, Northwestern (WFw)" = "WFw",
                                "Wet Meadow/Carr, Northern (WMn)" = "WMn",
                                "Wet Meadow/Carr, Prairie (WMp)" = "WMp",
                                "Wet Meadow/Carr, Southern (WMs)" = "WMs",
                                "Wet Prarie, Northern (WPn)" = "WPn",
                                "Wet Prarie, Southern (WPs)" = "WPs")
usethis::use_data(mnnpc_vc_types_flreg_named, internal = FALSE, overwrite = TRUE)

mnnpc_vc_types_flreg <- unname(mnnpc_vc_types_flreg_named)
usethis::use_data(mnnpc_vc_types_flreg, internal = FALSE, overwrite = TRUE)

mnnpc_vc_types_flreg_nested <- list("AP" = c("AP", "APn"),
                                    "FD" = c("FD", "FDc", "FDn", "FDs", "FDw"), 
                                    "FF" = c("FF", "FFn", "FFs"),
                                    "FP" = c("FF", "FPn", "FPs", "FPw"), 
                                    "MH" = c("MH", "MHc", "MHn", "MHs", "MHw"), 
                                    "MR" = c("MR", "MRn", "MRp", "MRu"), 
                                    "OP" = c("OP", "OPn", "OPp"), 
                                    "RO" = c("RO", "ROs"), 
                                    "UP" = c("UP", "UPn", "UPs"), 
                                    "WF" = c("WF", "WFn", "WFs", "WFw"), 
                                    "WM" = c("WM", "WMn", "WMp", "WMs"), 
                                    "WP" = c("WP", "WPn", "WPs"),
                                    "APn" = "APn",
                                    "FDc" = "FDc",
                                    "FDn" = "FDn",
                                    "FDs" = "FDs",
                                    "FDw" = "FDw",
                                    "FFn" = "FFn",
                                    "FFs" = "FFs",
                                    "FPn" = "FPn",
                                    "FPs" = "FPs",
                                    "FPw" = "FPw",
                                    "MHc" = "MHc",
                                    "MHn" = "MHn",
                                    "MHs" = "MHs",
                                    "MHw" = "MHw",
                                    "MRn" = "MRn",
                                    "MRp" = "MRp",
                                    "MRu" = "MRu",
                                    "OPn" = "OPn",
                                    "OPp" = "OPp",
                                    "ROs" = "ROs",
                                    "UPn" = "UPn",
                                    "UPs" = "UPs",
                                    "WFn" = "WFn",
                                    "WFs" = "WFs",
                                    "WFw" = "WFw",
                                    "WMn" = "WMn",
                                    "WMp" = "WMp",
                                    "WMs" = "WMs",
                                    "WPn" = "WPn",
                                    "WPs" = "WPs")
usethis::use_data(mnnpc_vc_types_flreg_nested, internal = FALSE, overwrite = TRUE)

mnnpc_ecs_sections <- c("Statewide" = "statewide", 
                        "Lake Agassiz, Aspen Parklands (LAP)" = "LAP", 
                        "Minnesota and Northeast Iowa Morainal (MIM)" = "MIM", 
                        "Northern Minnesota and Ontario Peatlands (MOP)" = "MOP", 
                        "Northern Minnesota Drift and Lake Plains (MDL)" = "MDL",
                        "North Central Glaciated Plains (CGP)" = "CGP", 
                        "Northern Superior Uplands (NSU)" = "NSU", 
                        "Paleozoic Plateau (PPL)" = "PPL", 
                        "Red River Valley (RRV)" = "RRV", 
                        "Southern Superior Uplands (SSU)" = "SSU", 
                        "Western Superior Uplands (WSU)" = "WSU")
usethis::use_data(mnnpc_ecs_sections, internal = FALSE, overwrite = TRUE)

# Scov midpoint conversion for Braun-Blanquet (van der Maarel 2007)
mnnpc_bb_conv <- data.frame(scov = c(as.character(5:1), "+", "r"),
                              scov_mid = c(87.5, 62.5, 37.5, 15, 2.5, 0.1, 
                                           0.02))

usethis::use_data(mnnpc_bb_conv, internal = FALSE, overwrite = TRUE)

# Scov midpoint conversion for Domin (RMAVIS)
mnnpc_dom_conv <- data.frame(scov = c(as.character(10:1), "+"),
                            scov_mid = c(95.5, 83, 63, 42, 30, 18, 8, 3, 5, 3,
                                         0.1))

usethis::use_data(mnnpc_dom_conv, internal = FALSE, overwrite = TRUE)

# Strata for physcodes
mnnpc_strata <- data.frame(physcode = c(rep(c("D", "E"), each = 3), 
                                        "B", "C", "F", "G", "H", "K", "S", "X"),
                           strata_lower = c(rep(c(1, 4, 6), 2), rep(1, 8)),
                           strata_upper = c(rep(c(3, 5, 8), 2), rep(8, 8)))

usethis::use_data(mnnpc_strata, internal = FALSE, overwrite = TRUE)

# Height conversion table
mnnpc_ht_conv <- data.frame(ht = 1:8,
                            ht_min_m = c(0, 0.1, 0.5, 2, 5, 10, 20, 35),
                            ht_max_m = c(0.1, 0.5, 2, 5, 10, 20, 35, 50))

usethis::use_data(mnnpc_ht_conv, internal = FALSE, overwrite = TRUE)

