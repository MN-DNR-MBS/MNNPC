testthat::test_that("process_dnr_releves works", {
  
  actual <- process_dnr_releves(releve_data = MNNPC::example_releve)
  
  expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  testthat::expect_equal(colnames(actual), expected_colnames)
  
})
