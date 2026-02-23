# example dataset
test_data <- MNNPC::mnnpc_example_data$`Earthworm-Invaded Forests`

testthat::test_that("process_dnr_releves works with default arguments", {
  
  actual <- MNNPC::process_dnr_releves(releve_data = test_data,
                                       cover_scale = "braunBlanquet")
  
  expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  testthat::expect_equal(colnames(actual), expected_colnames)
  
})

testthat::test_that("process_dnr_releves works on malformed data", {
  
  test_data_malformed <- test_data |>
    dplyr::filter(relnumb == first(relnumb) & year == first(year))
  
  test_data_malformed[1, 8] <- NA
  test_data_malformed[2, 7] <- NA
  test_data_malformed[3, 6] <- NA
  test_data_malformed[4, 5] <- NA
  test_data_malformed[5, 4] <- NA
  
  actual <- MNNPC::process_dnr_releves(releve_data = test_data_malformed, 
                                       cover_scale = "braunBlanquet")
  
  expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  testthat::expect_equal(colnames(actual), expected_colnames)
  
})

testthat::test_that("process_dnr_releves returns pre-processed data unchanged", {
  
  preprocessed <- MNNPC::process_dnr_releves(releve_data = test_data,
                                             cover_scale = "braunBlanquet")
  
  actual <- MNNPC::process_dnr_releves(releve_data = preprocessed,
                                       cover_scale = "braunBlanquet")
  
  testthat::expect_equal(actual, preprocessed)
  
})

# ---- name-matching and aggregation combinations (include_strata = FALSE) ----
# Tests only valid parameter combinations based on function logic:
# - match_to_accepted = FALSE always returns early on raw taxon names
# - aggregate_into_assigned and aggregate_into_analysis_groups are only
#   evaluated when match_to_accepted = TRUE
# - aggregate_into_analysis_groups takes precedence over aggregate_into_assigned

testthat::test_that("process_dnr_releves output columns are correct across valid aggregation combinations", {
  
  expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  # match_to_accepted = FALSE: always returns raw taxon names
  actual_no_match <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  # matched to accepted names only
  actual_match_only <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = FALSE,
    aggregate_into_analysis_groups = FALSE,
    include_strata = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  # matched and aggregated into assigned
  actual_assigned <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = TRUE,
    aggregate_into_analysis_groups = FALSE,
    include_strata = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  # matched and aggregated into analysis groups (aggregate_into_assigned ignored)
  actual_analysis_groups <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = FALSE,
    aggregate_into_analysis_groups = TRUE,
    include_strata = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  # analysis groups take precedence when both aggregation flags are TRUE
  actual_both_agg <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = TRUE,
    aggregate_into_analysis_groups = TRUE,
    include_strata = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  testthat::expect_equal(colnames(actual_no_match), expected_colnames)
  testthat::expect_equal(colnames(actual_match_only), expected_colnames)
  testthat::expect_equal(colnames(actual_assigned), expected_colnames)
  testthat::expect_equal(colnames(actual_analysis_groups), expected_colnames)
  testthat::expect_equal(colnames(actual_both_agg), expected_colnames)
  
})

testthat::test_that("process_dnr_releves species names are correct for each aggregation level", {
  
  actual_no_match <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  actual_match_only <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = FALSE,
    aggregate_into_analysis_groups = FALSE,
    include_strata = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  actual_assigned <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = TRUE,
    aggregate_into_analysis_groups = FALSE,
    include_strata = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  actual_analysis_groups <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = FALSE,
    aggregate_into_analysis_groups = TRUE,
    include_strata = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  # unmatched species should only differ from lookup by "Unknown" taxa
  testthat::expect_true(
    all(grepl("Unknown", setdiff(actual_no_match$Species,
                                 MNNPC::mnnpc_taxa_lookup$taxon_name)))
  )
  
  # matched species should all be in the recommended taxon list
  testthat::expect_true(
    all(actual_match_only$Species %in% MNNPC::mnnpc_taxa_lookup$recommended_taxon_name)
  )
  
  # assigned species should all be in recommended_assignment
  testthat::expect_true(
    all(actual_assigned$Species %in% MNNPC::mnnpc_taxa_lookup$recommended_assignment)
  )
  
  # analysis group species (stripping strata suffixes) should all be in analysis_group
  testthat::expect_true(
    all(gsub("\\s(canopy|subcanopy|understory)$", "", actual_analysis_groups$Species) %in%
          MNNPC::mnnpc_taxa_lookup$analysis_group)
  )
  
})

testthat::test_that("aggregate_into_assigned and aggregate_into_analysis_groups both TRUE produces same output as analysis_groups alone", {
  
  actual_ag_only <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = FALSE,
    aggregate_into_analysis_groups = TRUE,
    include_strata = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  actual_both_agg <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = TRUE,
    aggregate_into_analysis_groups = TRUE,
    include_strata = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  testthat::expect_equal(actual_ag_only, actual_both_agg)
  
})

# ---- include_strata combinations ----

testthat::test_that("process_dnr_releves include_strata = TRUE produces strata suffixes for trees", {
  
  strata_suffixes <- c("canopy", "subcanopy", "understory")
  
  actual_match_strata <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = FALSE,
    aggregate_into_analysis_groups = FALSE,
    include_strata = TRUE,
    cover_scale = "braunBlanquet"
  )
  
  actual_assigned_strata <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = TRUE,
    aggregate_into_analysis_groups = FALSE,
    include_strata = TRUE,
    cover_scale = "braunBlanquet"
  )
  
  actual_ag_strata <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = FALSE,
    aggregate_into_analysis_groups = TRUE,
    include_strata = TRUE,
    cover_scale = "braunBlanquet"
  )
  
  # strata suffixes should be present in at least one species name for each output
  testthat::expect_true(
    any(grepl(paste(strata_suffixes, collapse = "|"), actual_match_strata$Species))
  )
  
  testthat::expect_true(
    any(grepl(paste(strata_suffixes, collapse = "|"), actual_assigned_strata$Species))
  )
  
  testthat::expect_true(
    any(grepl(paste(strata_suffixes, collapse = "|"), actual_ag_strata$Species))
  )
  
})

testthat::test_that("include_strata = TRUE produces more rows than include_strata = FALSE for tree-containing data", {
  
  actual_no_strata <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_analysis_groups = TRUE,
    include_strata = FALSE,
    cover_scale = "braunBlanquet"
  )
  
  actual_with_strata <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_analysis_groups = TRUE,
    include_strata = TRUE,
    cover_scale = "braunBlanquet"
  )
  
  testthat::expect_gt(nrow(actual_with_strata), nrow(actual_no_strata))
  
})

testthat::test_that("include_strata = TRUE adds columns consistent with final field spec", {
  
  expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  actual_match_strata <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = FALSE,
    aggregate_into_analysis_groups = FALSE,
    include_strata = TRUE,
    cover_scale = "braunBlanquet"
  )
  
  actual_assigned_strata <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = TRUE,
    aggregate_into_analysis_groups = FALSE,
    include_strata = TRUE,
    cover_scale = "braunBlanquet"
  )
  
  actual_ag_strata <- MNNPC::process_dnr_releves(
    releve_data = test_data,
    match_to_accepted = TRUE,
    aggregate_into_assigned = FALSE,
    aggregate_into_analysis_groups = TRUE,
    include_strata = TRUE,
    cover_scale = "braunBlanquet"
  )
  
  testthat::expect_equal(colnames(actual_match_strata), expected_colnames)
  testthat::expect_equal(colnames(actual_assigned_strata), expected_colnames)
  testthat::expect_equal(colnames(actual_ag_strata), expected_colnames)
  
})

# ---- cover scale tests ----

testthat::test_that("process_dnr_releves returns zero cover when cover_scale = 'none'", {
  
  test_dat <- test_data
  test_dat$scov <- NA_real_
  
  actual_none <- MNNPC::process_dnr_releves(releve_data = test_dat,
                                            cover_scale = "none")
  
  testthat::expect_true(unique(actual_none$Cover) == 0)
  
})

testthat::test_that("process_dnr_releves cover values are non-negative for all cover scales", {
  
  # format scov
  test_data_perc <- test_data |>
    dplyr::left_join(MNNPC::mnnpc_bb_conv) |>
    dplyr::select(-scov) |>
    dplyr::rename(scov = scov_mid)
  
  test_data_prop <- test_data_perc |>
    dplyr::mutate(scov = scov / 100)
  
  test_data_dom <- test_data |>
    dplyr::left_join(MNNPC::mnnpc_dom_conv) |>
    dplyr::select(-scov) |>
    dplyr::rename(scov = scov_mid)
  
  # process
  actual_perc <- MNNPC::process_dnr_releves(releve_data = test_data_perc,
                                            cover_scale = "percentage")
  actual_prop <- MNNPC::process_dnr_releves(releve_data = test_data_prop,
                                            cover_scale = "proportional")
  actual_dom <- MNNPC::process_dnr_releves(releve_data = test_data,
                                           cover_scale = "domin")
  actual_bb <- MNNPC::process_dnr_releves(releve_data = test_data,
                                          cover_scale = "braunBlanquet")
  
  # tests    
  testthat::expect_true(all(actual_perc$Cover >= 0),
                        label = "Cover values non-negative for scale: percentage")
  testthat::expect_true(all(actual_prop$Cover >= 0),
                        label = "Cover values non-negative for scale: proportion")
  testthat::expect_true(all(actual_dom$Cover >= 0),
                        label = "Cover values non-negative for scale: Domin")
  testthat::expect_true(all(actual_bb$Cover >= 0),
                        label = "Cover values non-negative for scale: Braun Blanquet")
  
})

testthat::test_that("process_dnr_releves stops on invalid cover_scale", {
  
  testthat::expect_error(
    MNNPC::process_dnr_releves(releve_data = test_data,
                               cover_scale = "invalid_scale")
  )
  
})

# ---- warning tests ----

testthat::test_that("process_dnr_releves gives warning if suffixes aren't stripped", {
  
  testthat::expect_warning(
    MNNPC::process_dnr_releves(releve_data = test_data,
                               strip_suffixes = FALSE,
                               cover_scale = "braunBlanquet")
  )
  
})

testthat::test_that("process_dnr_releves gives warning if taxa aren't matched", {
  
  testthat::expect_warning(
    MNNPC::process_dnr_releves(releve_data = test_data,
                               match_to_accepted = FALSE,
                               cover_scale = "braunBlanquet")
  )
  
})

testthat::test_that("process_dnr_releves gives warning if taxa aren't aggregated", {
  
  testthat::expect_warning(
    MNNPC::process_dnr_releves(releve_data = test_data,
                               aggregate_into_analysis_groups = FALSE,
                               cover_scale = "braunBlanquet")
  )
  
})

# ---- pseudo-column addition ----

testthat::test_that("process_dnr_releves adds pseudo-columns when year, group, relnumb are missing", {
  
  test_dat_minimal <- test_data |>
    dplyr::select(-year, -group, -relnumb)
  
  testthat::expect_message(
    actual <- MNNPC::process_dnr_releves(releve_data = test_dat_minimal,
                                         cover_scale = "braunBlanquet"),
    regexp = NULL
  )
  
  testthat::expect_equal(colnames(actual), c("Year", "Group", "Quadrat", "Species", "Cover"))
  testthat::expect_true(all(actual$Year == 1))
  testthat::expect_true(all(actual$Group == "A"))
  testthat::expect_true(all(actual$Quadrat == "1"))
  
})
