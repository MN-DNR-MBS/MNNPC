testthat::test_that("process_dnr_releves works", {
  
  actual <- MNNPC::process_dnr_releves(releve_data = MNNPC::mnnpc_example_releve,
                                       cover_scale = "braunBlanquet")
  
  expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  testthat::expect_equal(colnames(actual), expected_colnames)
  
})

testthat::test_that("process_dnr_releves works on malformed data", {
  
  test_data_malformed <- MNNPC::mnnpc_example_data$`St. Croix State Forest` |>
    dplyr::filter(relnumb == "2" & year == 2010)
  
  test_data_malformed[1, 8] <- NA
  test_data_malformed[2, 7] <- NA
  test_data_malformed[3, 6] <- NA
  test_data_malformed[4, 5] <- NA
  test_data_malformed[5, 4] <- NA
  
  actual <- MNNPC::process_dnr_releves(releve_data = test_data_malformed, 
                                       process_malformed_data = TRUE,
                                       cover_scale = "percentage")
  
  expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  testthat::expect_equal(colnames(actual), expected_colnames)
  
})

# testthat::test_that("process_dnr_releves works on empty data", {
#   
#   test_data_empty <- MNNPC::mnnpc_example_data$`St. Croix State Forest` |>
#     dplyr::filter(relnumb == "2" & year == 2010) |>
#     dplyr::mutate("taxon" = NA, "scov" = NA)
#   
#   actual <- MNNPC::process_dnr_releves(releve_data = test_data_empty, 
#                                        process_malformed_data = TRUE,
#                                        cover_scale = "percentage")
#   
#   expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
#   
#   testthat::expect_equal(colnames(actual), expected_colnames)
#   
# })

testthat::test_that("process_dnr_releves works with name-matching argument variation", {
  
  test_data <- MNNPC::mnnpc_example_releve
  
  actual_unacc_unagg <- MNNPC::process_dnr_releves(releve_data = test_data,
                                                   match_to_accepted = FALSE,
                                                   aggregate_into_analysis_groups = FALSE,
                                                   cover_scale = "percentage")

  actual_acc_unagg <- MNNPC::process_dnr_releves(releve_data = test_data,
                                                 match_to_accepted = TRUE,
                                                 aggregate_into_analysis_groups = FALSE,
                                                 cover_scale = "percentage")

  actual_unacc_agg <- MNNPC::process_dnr_releves(releve_data = test_data,
                                                 match_to_accepted = FALSE,
                                                 aggregate_into_analysis_groups = TRUE,
                                                 cover_scale = "percentage")
  
  actual_acc_agg <- MNNPC::process_dnr_releves(releve_data = test_data,
                                               match_to_accepted = TRUE,
                                               aggregate_into_analysis_groups = TRUE,
                                               cover_scale = "percentage")
  
  expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  testthat::expect_equal(colnames(actual_unacc_unagg), expected_colnames)
  testthat::expect_equal(colnames(actual_acc_unagg), expected_colnames)
  testthat::expect_equal(colnames(actual_unacc_agg), expected_colnames)
  testthat::expect_equal(colnames(actual_acc_agg), expected_colnames)
  
  actual_unacc_agg$Species <- gsub("\\scanopy|\\sunderstory|\\ssub-canopy", "",
                                   actual_unacc_agg$Species)
  actual_acc_agg$Species <- gsub("\\scanopy|\\sunderstory|\\ssub-canopy", "",
                                 actual_acc_agg$Species)

  testthat::expect_true(setdiff(actual_unacc_unagg$Species,
                                MNNPC::mnnpc_taxa_lookup$taxon_name) == "Polytrichum")
  
  testthat::expect_true(all(actual_acc_unagg$Species %in% MNNPC::mnnpc_taxa_lookup$recommended_taxon_name))
  
  testthat::expect_true(setdiff(actual_unacc_agg$Species,
                                MNNPC::mnnpc_taxa_lookup$analysis_group) |>
                          length() == 22)
  
  testthat::expect_true(all(actual_acc_agg$Species %in%
                              MNNPC::mnnpc_taxa_lookup$analysis_group))
  
})

testthat::test_that("process_dnr_releves stops if fields are missing", {
  
  test_dat <- subset(MNNPC::mnnpc_example_releve, select = -taxon)
  
  testthat::expect_error(MNNPC::process_dnr_releves(test_dat,
                                                    cover_scale = "percentage"), 
                         NULL)
  
})

testthat::test_that("process_dnr_releves gives warning if data are missing", {
  
  test_dat <- MNNPC::mnnpc_example_releve
  test_dat$scov[3] <- NA
  
  testthat::expect_warning(MNNPC::process_dnr_releves(test_dat,
                                                      cover_scale = "percentage"), 
                         NULL)
  
})

testthat::test_that("process_dnr_releves stops if data are missing and malformed F", {
  
  test_dat <- MNNPC::mnnpc_example_releve
  test_dat$scov[3] <- NA
  
  testthat::expect_error(MNNPC::process_dnr_releves(test_dat,
                                                    process_malformed_data = F,
                                                    cover_scale = "percentage"), 
                         NULL)
  
})

testthat::test_that("process_dnr_releves gives warning if suffixes aren't fixed", {
  
  testthat::expect_warning(MNNPC::process_dnr_releves(releve_data = MNNPC::mnnpc_example_releve,
                                                      strip_suffixes = F,
                                                      cover_scale = "percentage"), 
                           NULL)
  
})

testthat::test_that("process_dnr_releves gives warning if taxa aren't matched", {
  
  testthat::expect_warning(MNNPC::process_dnr_releves(releve_data = MNNPC::mnnpc_example_releve,
                                                      match_to_accepted = F,
                                                      cover_scale = "percentage"), 
                           NULL)
  
})

testthat::test_that("process_dnr_releves gives warning if taxa aren't aggregated", {
  
  testthat::expect_warning(MNNPC::process_dnr_releves(releve_data = MNNPC::mnnpc_example_releve,
                                                      aggregate_into_analysis_groups = F,
                                                      cover_scale = "percentage"), 
                           NULL)
  
})
