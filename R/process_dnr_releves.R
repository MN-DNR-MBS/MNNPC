#' Process releve data into the format required by RMAVIS
#'
#' Process releve data into the format required by RMAVIS, specifically a data frame
#' in long format containing "Year", "Group", "Quadrat", "Species", and "Cover" columns.
#'
#' @param releve_data See `MNNPC::mnnpc_example_releve`.
#' @param strip_suffixes Specify whether to strip suffixes (e.g. s.l., s.s., and s.a.) from taxon names, a boolean (TRUE/FALSE).
#' @param match_to_accepted Specify whether to convert raw taxon names to accepted/recommended taxon names, a boolean (TRUE/FALSE).
#' @param aggregate_into_analysis_groups Specify whether to aggregate taxa into analysis groups, a boolean (TRUE/FALSE).
#' @param cover_scale Specify the scale of species cover values, one of: "percentage", "proportional", "domin", "braunBlanquet", or "none".
#'
#' @returns A data frame containing the releve data with five columns: "Year", "Group", "Quadrat", "Species", "Cover"
#' @export
#'
#' @examples
#' MNNPC::process_dnr_releves(releve_data = MNNPC::mnnpc_example_releve)
process_dnr_releves <- function(releve_data,
                                strip_suffixes = TRUE,
                                match_to_accepted = TRUE,
                                aggregate_into_accepted = FALSE,
                                aggregate_into_analysis_groups = TRUE,
                                cover_scale = "percentage") {
  #### check for processed releve ####
  
  # final fields
  final_fields <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  # check for processed releve
  if (all(final_fields %in% names(releve_data)) == T) {
    message(
      "It looks like you've uploaded processed data. No further processing will be applied before analysis."
    )
    
    return(releve_data)
    
    stop()
    
  }
  
  #### add pseudo columns if missing ####
  
  if (!("year" %in% names(releve_data))) {
    releve_data$year <- 1
    
    message("Year column was added with value '1'.")
    
  }
  
  if (!("group" %in% names(releve_data))) {
    releve_data$group <- "A"
    
    message("Group column was added with value 'A'.")
    
  }
  
  if (!("relnumb" %in% names(releve_data))) {
    releve_data$relnumb <- "1"
    
    message("Releve number column was added with value '1'.")
    
  }
  
  
  #### convert cover ####
  
  if (!(cover_scale %in% c(
    "percentage",
    "proportional",
    "domin",
    "braunBlanquet",
    "none"
  ))) {
    stop("Please specify a valid cover scale.")
    
  } else if (cover_scale == "proportional") {
    releve_data$scov <- releve_data$scov * 100
    
  } else if (cover_scale == "domin") {
    releve_data <- releve_data |>
      dplyr::left_join(MNNPC::mnnpc_dom_conv, by = "scov") |>
      dplyr::select(-scov) |>
      dplyr::rename(scov = scov_mid)
    
  } else if (cover_scale == "braunBlanquet") {
    releve_data <- releve_data |>
      dplyr::left_join(MNNPC::mnnpc_bb_conv, by = "scov") |>
      dplyr::select(-scov) |>
      dplyr::rename(scov = scov_mid)
    
  } else if (cover_scale == "none") {
    releve_data$scov <- NA_real_
    
  }
  
  
  #### return with suffixes ####
  
  # check for suffixes
  suffixes_present <- sum(grepl("(s\\.s\\.|s\\.l\\.|s\\.a\\.)", releve_data$taxon))
  
  # stop if suffixes present and removing not selected
  if (suffixes_present > 0 & strip_suffixes == F) {
    # sum cover for each taxon (over strata, outside of plot, etc.)
    dat_out_suff <- releve_data |>
      dplyr::group_by(year, group, relnumb, taxon) |>
      dplyr::summarize(Cover = sum(scov, na.rm = T), .groups = "drop") |>
      dplyr::rename(
        Year = year,
        Group = group,
        Quadrat = relnumb,
        Species = taxon
      )
    
    # warning
    warning(
      "Taxa were not matched to accepted names or aggregated into analysis groups. Cover was summed for each taxon. Please remove suffixes (s.s., s.l.) from taxa names or set strip_suffixes = TRUE to continue processing data."
    )
    
    # return
    return(dat_out_suff)
    
    # exit
    stop()
    
  }
  
  #### remove suffixes ####
  
  if (suffixes_present > 0 & strip_suffixes == T) {
    releve_data <- releve_data |>
      dplyr::mutate(
        taxon = gsub(" s\\.s\\.", "", taxon),
        taxon = gsub(" s\\.l\\.", "", taxon),
        taxon = gsub(" s\\.a\\.", "", taxon)
      )
    
  }
  
  # stop if taxa aren't matched to accepted names
  if (match_to_accepted == F) {
    # sum cover for each taxon (over strata, outside of plot, etc.)
    dat_out_no_match <- releve_data |>
      dplyr::group_by(year, group, relnumb, taxon) |>
      dplyr::summarize(Cover = sum(scov, na.rm = T), .groups = "drop") |>
      dplyr::rename(
        Year = year,
        Group = group,
        Quadrat = relnumb,
        Species = taxon
      )
    
    # message
    warning(
      "Taxa were not matched to accepted names. Cover was summed for each taxon. Please set match_to_accepted = TRUE to continue processing data."
    )
    
    # return
    return(dat_out_no_match)
    
    # stop
    stop()
    
  }
  
  
  #### lookup table ####
  
  lookup <- MNNPC::mnnpc_taxa_lookup |>
    dplyr::select(
      taxon_name,
      recommended_taxon_name,
      recommended_assignment,
      analysis_group,
      recommended_physcode,
      stratacode = recommended_stratacode
    )
  
  
  #### format for matching and aggregating ####
  
  # remove columns that are in lookup
  releve_data <- releve_data[, !names(releve_data) %in% names(lookup)]
  
  # crosswalk hybrids
  # add match names and analysis group by taxon name
  # replace physcode with standardized physcode
  releve_data <- releve_data |>
    dplyr::left_join(MNNPC::mnnpc_hybrid_crosswalk) |>
    dplyr::mutate(taxon = ifelse(!is.na(taxon_rep), taxon_rep, taxon)) |>
    dplyr::select(-taxon_rep) |>
    dplyr::rename(taxon_name = taxon) |>
    dplyr::left_join(lookup) |>
    dplyr::mutate(physcode = dplyr::if_else(
      !is.na(recommended_physcode),
      recommended_physcode,
      physcode
    ))
  
  
  #### output matched taxa ####
  
  # stop if not aggregating
  if (aggregate_into_accepted == F &
      aggregate_into_analysis_groups == F) {
    # taxa that don't have recommended names
    taxa_removed <- releve_data |>
      dplyr::filter(is.na(recommended_taxon_name)) |>
      dplyr::pull(taxon_name) |>
      unique() |>
      sort()
    
    # warn user about taxa being removed and no aggregation
    if (length(taxa_removed) > 0) {
      warning(
        paste(
          "Taxa were not aggregated and cover was summed for each accepted name. The following taxa were be removed because they do not have accepted names in the taxa lookup table:",
          paste(taxa_removed, collapse = ", ")
        )
      )
      
    } else {
      warning("Taxa were not aggregated and cover was summed for each accepted name.")
      
    }
    
    # sum cover for each taxon (over strata, outside of plot, etc.)
    dat_out_match <- releve_data |>
      dplyr::filter(!is.na(recommended_taxon_name)) |>
      dplyr::group_by(year, group, relnumb, recommended_taxon_name) |>
      dplyr::summarize(Cover = sum(scov, na.rm = T), .groups = "drop") |>
      dplyr::rename(
        Year = year,
        Group = group,
        Quadrat = relnumb,
        Species = recommended_taxon_name
      )
    
    # return
    return(dat_out_match)
    
    # exit
    stop()
    
  }
  
  
  #### output accepted name groups ####
  
  # aggregate into accepted names (grouped due to taxonomy changes)
  if (aggregate_into_accepted == T &
      aggregate_into_analysis_groups == F) {
    # taxa that don't have analysis groups
    taxa_removed <- releve_data |>
      dplyr::filter(is.na(recommended_assignment)) |>
      dplyr::pull(taxon_name) |>
      unique() |>
      sort()
    
    # warn user about taxa being removed
    if (length(taxa_removed) > 0) {
      warning(
        paste0(
          "The following taxa were removed because they do not have accepted names or cannot be uniquely identified: ",
          paste(taxa_removed, collapse = ", "),
          ". See taxa lookup table for details."
        )
      )
      
    }
    
    # sum cover for each accepted name (over strata, outside of plot, etc.)
    dat_out_acc <- releve_data |>
      dplyr::filter(!is.na(recommended_assignment)) |>
      dplyr::group_by(year, group, relnumb, recommended_assignment) |>
      dplyr::summarize(scov = sum(scov, na.rm = T), .groups = "drop") |>
      dplyr::rename(
        Year = year,
        Group = group,
        Quadrat = relnumb,
        Species = recommended_taxon_name
      )
    
    # return
    return(dat_out_acc)
    
    # exit
    stop()
    
  }
  
  #### missing height values ####
  
  # convert heights to numbers if they aren't already
  # this will cause a warning if there's "X" in these columns
  releve_data <- releve_data |>
    dplyr::mutate(minht = as.numeric(minht), maxht = as.numeric(maxht))
  
  # trees that don't have strata
  strata_missing <- releve_data |>
    dplyr::filter(stringr::str_detect(physcode, "D|E") &
                    is.na(stratacode) &
                    (is.na(minht) | is.na(maxht))) |>
    dplyr::pull(taxon_name) |>
    unique() |>
    sort()
  
  # warn user that trees will be stratified across levels
  if (length(strata_missing) > 0) {
    warning(
      paste0(
        "The following taxa are missing height values and will be stratified across understory, subcanopy, and canopy: ",
        paste(strata_missing, collapse = ", ")
      )
    )
    
  }
  
  # replace any missing with lowest and highest possible
  releve_data <- releve_data |>
    dplyr::mutate(
      minht = tidyr::replace_na(minht, 1),
      maxht = tidyr::replace_na(maxht, 8)
    )
  
  
  #### mis-ordered heights ####
  
  # check that min and max heights are in order
  n_misordered_heights <- releve_data |>
    dplyr::filter(maxht < minht) |>
    nrow()
  
  # warn and flip min and max heights if out of order
  if (n_misordered_heights > 0) {
    warning(
      paste(
        "There are",
        n_misordered_heights,
        "rows with maxht < minht. These rows will be corrected by swapping minht and maxht values."
      )
    )
    
    releve_data <- releve_data |>
      dplyr::mutate(
        minht_orig = minht,
        maxht_orig = maxht,
        minht = ifelse(maxht_orig < minht_orig, maxht_orig, minht_orig),
        maxht = ifelse(maxht_orig < minht_orig, minht_orig, maxht_orig)
      ) |>
      dplyr::select(-c(minht_orig, maxht_orig))
    
  }
  
  #### outside-of-plot ####
  
  # convert outside-of-plot cover to "r" unless it's a canopy tree
  # add outside of plot if missing
  if ("outside_of_plot" %in% names(releve_data)) {
    releve_data <- releve_data |>
      dplyr::mutate(scov = ifelse(
        outside_of_plot %in% c(1, "t", "T", TRUE) &
          !(physcode %in% c("D", "E") &
              maxht >= 5),
        min(MNNPC::mnnpc_bb_conv$scov_mid),
        scov
      ))
    
  } else {
    releve_data$outside_of_plot <- 0
    
  }
  
  #### taxa removed from analysis groups ####
  
  # physcodes to include
  physcode_include <- c("B", "C", "D", "E", "F", "G", "H", "K", "S", "X")
  phys_pattern <- paste0("\\b(", paste(physcode_include, collapse = "|"), ")\\b")
  
  # taxa that don't have analysis groups
  taxa_removed <- releve_data |>
    dplyr::filter(is.na(analysis_group) |
                    stringr::str_detect(physcode, phys_pattern) == F) |>
    dplyr::pull(taxon_name) |>
    unique() |>
    sort()
  
  # warn user about taxa being removed
  if (length(taxa_removed) > 0) {
    warning(
      paste0(
        "The following taxa were removed because they don't belong to analysis groups or have included physiognomy codes: ",
        paste(taxa_removed, collapse = ", "),
        ". See taxa lookup table for details."
      )
    )
    
  }
  
  
  #### stratification ####
  
  # remove taxa that don't have analysis groups
  # select taxa with appropriate physcodes
  releve_data <- releve_data |>
    dplyr::filter(!is.na(analysis_group) &
                    stringr::str_detect(physcode, phys_pattern))
  
  # code combinations
  dat_codes <- releve_data |>
    dplyr::distinct(physcode, stratacode)
  
  # create physcode crosswalk
  strata_include <- dat_codes |>
    dplyr::filter(stringr::str_detect(physcode, "D|E") == F |
                    !is.na(stratacode)) |>
    dplyr::mutate(strata_lower = 1, strata_upper = 8) |>
    dplyr::full_join(
      dat_codes |>
        dplyr::filter(stringr::str_detect(physcode, "D|E") &
                        is.na(stratacode)) |>
        tidyr::expand_grid(data.frame(
          strata_lower = c(1, 4, 6),
          strata_upper = c(3, 5, 8)
        ))
    )
  
  # add strata
  # select overlapping height levels and strata
  # add height in meters
  # divide scov over its vertical extent
  # expand rows, one for each height level
  # select height levels within strata
  # calculate scov for that height level
  # sum across the same analysis codes and height levels within stratum
  # add strata to tree names
  # rename columns
  releve_data <- releve_data |>
    dplyr::left_join(strata_include, by = c("physcode", "stratacode"),
                     relationship = "many-to-many") |>
    dplyr::filter((minht >= strata_lower &
                     minht <= strata_upper) |
                    (strata_lower >= minht &
                       strata_lower <= maxht) |
                    (strata_lower == 1 & strata_upper == 8)
    ) |>
    dplyr::left_join(MNNPC::mnnpc_ht_conv |>
                       dplyr::rename(minht = ht) |>
                       dplyr::select(minht, ht_min_m)) |>
    dplyr::left_join(MNNPC::mnnpc_ht_conv |>
                       dplyr::rename(maxht = ht) |>
                       dplyr::select(maxht, ht_max_m)) |>
    dplyr::mutate(max_min_range = ht_max_m - ht_min_m,
                  scov_per_m = scov / max_min_range) |>
    dplyr::select(-c(ht_max_m, ht_min_m, max_min_range, scov)) |>
    dplyr::group_by(across(everything())) |>
    dplyr::reframe(ht = seq(minht, maxht)) |>
    dplyr::filter(ht >= strata_lower & ht <= strata_upper) |>
    dplyr::left_join(
      MNNPC::mnnpc_ht_conv |>
        dplyr::mutate(ht_range = ht_max_m - ht_min_m) |>
        dplyr::select(ht, ht_range)
    ) |>
    dplyr::mutate(scov_ht = scov_per_m * ht_range) |>
    dplyr::group_by(year,
                    group,
                    relnumb,
                    analysis_group,
                    strata_lower,
                    strata_upper) |>
    dplyr::summarize(scov = sum(scov_ht, na.rm = T), .groups = "drop") |>
    dplyr::mutate(
      strata = dplyr::case_when(
        strata_lower == 1 &
          strata_upper == 8 ~ NA_character_,
        strata_lower == 1 &
          strata_upper == 3 ~ "understory",
        strata_lower == 4 &
          strata_upper == 5 ~ "subcanopy",
        strata_lower == 6 &
          strata_upper == 8 ~ "canopy"
      ),
      analysis_group_strata =  ifelse(
        !is.na(strata),
        paste(analysis_group, strata),
        analysis_group
      )
    ) |>
    dplyr::select(year, group, relnumb, analysis_group_strata, scov) |>
    dplyr::arrange(year, group, relnumb, analysis_group_strata) |>
    dplyr::rename(
      Year = year,
      Group = group,
      Quadrat = relnumb,
      Species = analysis_group_strata,
      Cover = scov
    ) |>
    as.data.frame()
  
  # return
  return(releve_data)
  
}
