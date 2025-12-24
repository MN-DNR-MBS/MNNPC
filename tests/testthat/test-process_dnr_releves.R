testthat::test_that("process_dnr_releves works", {
  
  actual <- MNNPC::process_dnr_releves(releve_data = MNNPC::example_releve)
  
  expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  testthat::expect_equal(colnames(actual), expected_colnames)
  
})

testthat::test_that("process_dnr_releves stops if fields are missing", {
  
  test_dat <- subset(MNNPC::example_releve, select = -taxon)
  
  testthat::expect_error(MNNPC::process_dnr_releves(test_dat), NULL)
  
})

testthat::test_that("process_dnr_releves gives warning if data are missing", {
  
  test_dat <- MNNPC::example_releve
  test_dat$scov[3] <- NA
  
  testthat::expect_warning(MNNPC::process_dnr_releves(test_dat), 
                         NULL)
  
})

testthat::test_that("process_dnr_releves stops if data are missing and malformed F", {
  
  test_dat <- MNNPC::example_releve
  test_dat$scov[3] <- NA
  
  testthat::expect_error(MNNPC::process_dnr_releves(test_dat,
                                                    process_malformed_data = F), 
                         NULL)
  
})

testthat::test_that("process_dnr_releves gives warning if suffixes aren't fixed", {
  
  testthat::expect_warning(MNNPC::process_dnr_releves(releve_data = MNNPC::example_releve,
                                                      strip_suffixes = F), 
                           NULL)
  
})

testthat::test_that("process_dnr_releves gives warning if taxa aren't matched", {
  
  testthat::expect_warning(MNNPC::process_dnr_releves(releve_data = MNNPC::example_releve,
                                                      match_to_accepted = F), 
                           NULL)
  
})

testthat::test_that("process_dnr_releves gives warning if taxa aren't aggregated", {
  
  testthat::expect_warning(MNNPC::process_dnr_releves(releve_data = MNNPC::example_releve,
                                                      aggregate_into_analysis_groups = F), 
                           NULL)
  
})
