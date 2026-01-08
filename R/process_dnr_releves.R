#' Process releve data into the format required by RMAVIS
#' 
#' Process releve data into the format required by RMAVIS, specifically a data frame
#' in long format containing "Year", "Group", "Quadrat", "Species", and "Cover" columns.
#'
#' @param releve_data See `MNNPC::mnnpc_example_releve`.
#' @param process_malformed_data Specify whether to process data with missing values, a boolean (TRUE/FALSE).
#' @param strip_suffixes Specify whether to strip suffixes (e.g. s.l., s.s., and s.a.) from taxon names, a boolean (TRUE/FALSE).
#' @param match_to_accepted Specify whether to convert raw taxon names to accepted/recommended taxon names, a boolean (TRUE/FALSE).
#' @param aggregate_into_analysis_groups Specify whether to aggregate taxa into analysis groups, a boolean (TRUE/FALSE).
#'
#' @returns A data frame containing the releve data with five columns: "Year", "Group", "Quadrat", "Species", "Cover"
#' @export
#'
#' @examples
#' MNNPC::process_dnr_releves(releve_data = MNNPC::mnnpc_example_releve)
process_dnr_releves <- function(releve_data,
                                process_malformed_data = TRUE,
                                strip_suffixes = TRUE,
                                match_to_accepted = TRUE,
                                aggregate_into_analysis_groups = TRUE,
                                cover_scale = "braunBlanquet"){
  
  #### check for processed releve ####
  
  # final fields
  final_fields <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  # check for processed releve
  if(all(final_fields %in% names(releve_data)) == T){
    
    message("It looks like you've uploaded processed data. No further processing will be applied before analysis.")
    
    return(releve_data)
    
  } else {
    
    #### checks and replies ####
    
    # check for required fields
    fields_required <- c("year", "group", "relnumb", "physcode", "minht", "maxht", 
                         "taxon", "scov")
    
    if(all(fields_required %in% names(releve_data)) == F){
      
      missing_fields <- fields_required[!(fields_required %in% names(releve_data))]
      stop(paste("Missing required fields:", 
                 paste(missing_fields, collapse = ", ")))
      
    }
    
    # convert heights to numbers if they aren't already
    # this will cause a warning if there's "X" in these columns
    dat2 <- releve_data |>
      dplyr::mutate(minht = as.numeric(minht),
                    maxht = as.numeric(maxht))
    
    # check for missing data
    n_missing_data <- dat2 |>
      dplyr::filter(is.na(physcode) | is.na(taxon) | is.na(scov) |
                      (physcode %in% c("D", "E") & is.na(minht)) | 
                      (physcode %in% c("D", "E") & is.na(maxht))) |>
      nrow()
    
    # warn and remove rows with missing data
    if(n_missing_data > 0 & process_malformed_data == T){
      
      warning(paste("There are", n_missing_data, "rows with missing data (physcode, taxon, scov, or trees missing minht or maxht). These rows will be excluded from analysis."))
      
      dat2 <- dat2 |>
        dplyr::filter(!is.na(physcode) & !is.na(taxon) & !is.na(scov)) # minht and maxht fixed below
      
    }

    # stop if data can't be processed if missing
    if(n_missing_data > 0 & process_malformed_data == F){
      
      stop(paste("There are", n_missing_data, "rows with missing required data (physcode, taxon, scov, or trees missing minht or maxht)."))
      
    }
    
    # check that min and max heights are in order
    n_misordered_heights <- dat2 |>
      dplyr::filter(maxht < minht) |>
      nrow()
    
    # warn and flip min and max heights if out of order
    if(n_misordered_heights > 0){
      
      warning(paste("There are", n_misordered_heights, "rows with maxht < minht. These rows will be corrected by swapping minht and maxht values."))
      
      dat2 <- dat2 |>
        dplyr::mutate(minht_orig = minht,
                      maxht_orig = maxht,
                      minht = ifelse(maxht_orig < minht_orig, maxht_orig, 
                                     minht_orig),
                      maxht = ifelse(maxht_orig < minht_orig, minht_orig, 
                                     maxht_orig)) |>
        dplyr::select(-c(minht_orig, maxht_orig))
      
    }
    
    
    #### convert cover to percentage ####
    
    if(!(cover_scale %in% c("percentage", "proportional", "domin",
                            "braunBlanquet"))){
      
      stop("Please specify a valid cover scale.")
      
    } else if(cover_scale == "proportional"){
      
      dat2$scov <- dat2$scov * 100
      
    } else if(cover_scale == "domin"){
      
      dat2 <- dat2 |>
        dplyr::left_join(MNNPC::mnnpc_dom_conv) |>
        dplyr::select(-scov) |>
        dplyr::rename(scov = scov_mid)
      
    } else if(cover_scale == "braunBlanquet"){
      
      dat2 <- dat2 |>
        dplyr::left_join(MNNPC::mnnpc_bb_conv) |>
        dplyr::select(-scov) |>
        dplyr::rename(scov = scov_mid)
      
    }
    
    #### process data ####
    
    # convert outside-of-plot cover to "r" unless it's a canopy tree
    if("outside_of_plot" %in% names(dat2)){
      
      dat2 <- dat2 |>
        dplyr::mutate(scov = ifelse(outside_of_plot == 1 & 
                                      !(physcode %in% c("D", "E") & 
                                          maxht >= 5), 
                                    0.1, scov))
      
    }
    
    # remove analysis group if included
    if("analysis_group" %in% names(dat2)){
      
      dat2 <- dat2 |>
        dplyr::select(-analysis_group)
      
    }
    
    # check for suffixes
    suffixes_present <- sum(grepl("(s\\.s\\.|s\\.l\\.|s\\.a\\.)", dat2$taxon))
    
    # remove suffixes
    if(suffixes_present > 0 & strip_suffixes == T){
      
      dat2 <- dat2 |>
        dplyr::mutate(taxon = gsub(" s\\.s\\.", "", taxon),
                      taxon = gsub(" s\\.l\\.", "", taxon),
                      taxon = gsub(" s\\.a\\.", "", taxon))
      
    }
    
    # stop if suffixes present and removing not selected
    if(suffixes_present > 0 & strip_suffixes == F){
      
      # sum cover for each taxon (over strata, outside of plot, etc.)
      dat_out_suff <- dat2 |>
        dplyr::group_by(year, group, relnumb, taxon) |>
        dplyr::summarize(Cover = sum(scov, na.rm = T),
                         .groups = "drop") |>
        dplyr::rename(Year = year, 
                     Group = group,
                     Quadrat = relnumb,
                     Species = taxon)
      
      # warning
      warning("Taxa were not matched to accepted names or aggregated into analysis groups. Cover was summed for each taxon. Please remove suffixes (s.s., s.l.) from taxa names or set strip_suffixes = TRUE to continue processing data.")
      
      # return
      return(dat_out_suff)
      
      # exit
      stop()
      
    }
    
    # stop if taxa aren't matched to accepted names
    if(match_to_accepted == F){
      
      # sum cover for each taxon (over strata, outside of plot, etc.)
      dat_out_no_match <- dat2 |>
        dplyr::group_by(year, group, relnumb, taxon) |>
        dplyr::summarize(Cover = sum(scov, na.rm = T),
                         .groups = "drop") |>
        dplyr::rename(Year = year, 
                     Group = group,
                     Quadrat = relnumb,
                     Species = taxon)
      
      # message
      warning("Taxa were not matched to accepted names. Cover was summed for each taxon. Please set match_to_accepted = TRUE to continue processing data.")
      
      # return
      return(dat_out_no_match)
      
      # stop
      stop()
      
    }
    
    # crosswalk hybrids
    # add match names and analysis group by taxon name
    dat3 <- dat2 |>
      dplyr::left_join(MNNPC::mnnpc_hybrid_crosswalk) |>
      dplyr::mutate(taxon = ifelse(!is.na(taxon_rep), taxon_rep, taxon)) |>
      dplyr::select(-taxon_rep) |>
      dplyr::rename(taxon_name = taxon) |>
      dplyr::left_join(MNNPC::mnnpc_taxa_lookup |>
                         dplyr::select(taxon_name, recommended_taxon_name,
                                       analysis_group))
    
    # stop if not aggregating into analysis groups
    if(aggregate_into_analysis_groups == F){
      
      # taxa that don't have recommended names
      taxa_removed <- dat3 |>
        dplyr::filter(is.na(recommended_taxon_name)) |>
        dplyr::pull(taxon_name) |>
        unique() |>
        sort()
      
      # warn user about taxa being removed and no aggregation
      if(length(taxa_removed) > 0){
        
        warning(paste("Taxa were not aggregated and cover was summed for each accepted name. Please set aggregate_into_analysis_groups = TRUE to continue processing data. The following taxa were be removed because they do not have accepted names in the taxa lookup table:", 
                      paste(taxa_removed, collapse = ", ")))
        
      } else {
        
        warning("Taxa were not aggregated and cover was summed for each accepted name. Please set aggregate_into_analysis_groups = TRUE to continue processing data.")
        
      }
      
      # sum cover for each taxon (over strata, outside of plot, etc.)
      dat_out_match <- dat3 |>
        dplyr::filter(!is.na(recommended_taxon_name))|>
        dplyr::group_by(year, group, relnumb, recommended_taxon_name) |>
        dplyr::summarize(Cover = sum(scov, na.rm = T),
                         .groups = "drop") |>
        dplyr::rename(Year = year, 
                      Group = group,
                      Quadrat = relnumb,
                      Species = recommended_taxon_name)
      
      # return
      return(dat_out_match)
      
      # exit
      stop()
      
    }
    
    # taxa that don't have analysis groups
    taxa_removed <- dat3 |>
      dplyr::filter(is.na(analysis_group)) |>
      dplyr::pull(taxon_name) |>
      unique() |>
      sort()
    
    # warn user about taxa being removed
    if(length(taxa_removed) > 0){
      
      warning(paste0("The following taxa were removed because they don't belong to analysis groups: ", 
                     paste(taxa_removed, collapse = ", "), 
                     ". See taxa lookup table for details."))
      
    }
    
    # remove taxa that aren't in the taxa crosswalk (e.g., genus IDs, moss and lichens)
    # add midpoint cover
    # sum scov across species and inside/outside of plot for each analysis group
    dat4 <- dat3 |>
      dplyr::filter(!is.na(analysis_group))|>
      dplyr::group_by(year, group, relnumb, physcode, minht, maxht, 
                      analysis_group) |>
      dplyr::summarize(scov = sum(scov, na.rm = T),
                       .groups = "drop")
    
    # add strata
    # remove rows missing heights if they're trees
    # add heights for missing values
    # select overlapping height levels and strata
    # add height in meters
    # divide scov over its vertical extent
    # expand rows, one for each height level
    # select height levels within strata
    # calculate scov for that height level
    # sum across the same analysis codes and height levels within stratum
    # add strata to tree names
    # rename columns
    dat5 <- dat4 |>
      dplyr::inner_join(MNNPC::mnnpc_strata, relationship = "many-to-many") |>
      dplyr::filter((strata_lower == 1 & strata_upper == 8) |
                      (!is.na(minht) & !is.na(maxht))) |> 
      dplyr::mutate(minht = ifelse(is.na(minht) & strata_lower == 1 &
                                     strata_upper == 8, 1, minht),
                    maxht = ifelse(is.na(maxht) & strata_lower == 1 &
                                     strata_upper == 8, 8, maxht)) |>
      dplyr::filter((minht >= strata_lower & minht <= strata_upper) | 
                      (strata_lower >= minht & strata_lower <= maxht) |
                      (strata_lower == 1 & strata_upper == 8)) |>
      dplyr::left_join(MNNPC::mnnpc_ht_conv |>
                         dplyr::rename(minht = ht) |>
                         dplyr::select(minht, ht_min_m)) |>
      dplyr::left_join(MNNPC::mnnpc_ht_conv |>
                         dplyr::rename(maxht = ht) |>
                         dplyr::select(maxht, ht_max_m)) |>
      dplyr::mutate(max_min_range = ht_max_m - ht_min_m,
                    scov_per_m = scov / max_min_range) |> 
      dplyr::group_by(year, group, relnumb, physcode, minht, maxht, 
                      analysis_group, scov_per_m, strata_lower, strata_upper) |>
      dplyr::reframe(ht = seq(minht, maxht)) |> 
      dplyr::ungroup() |>
      dplyr::filter(ht >= strata_lower & ht <= strata_upper) |> 
      dplyr::left_join(MNNPC::mnnpc_ht_conv |>
                         dplyr::mutate(ht_range = ht_max_m - ht_min_m) |>
                         dplyr::select(ht, ht_range)) |>
      dplyr::mutate(scov_ht = scov_per_m * ht_range) |>  
      dplyr::group_by(year, group, relnumb, analysis_group, strata_lower, 
                      strata_upper) |> 
      dplyr::summarize(scov_adj = sum(scov_ht, na.rm = T), 
                       .groups = "drop") |>
      dplyr::mutate(strata = dplyr::case_when(strata_lower == 1 & 
                                                strata_upper == 8 ~ NA_character_,
                                              strata_lower == 1 & 
                                                strata_upper == 3 ~ "understory",
                                              strata_lower == 4 & 
                                                strata_upper == 5 ~ "sub-canopy",
                                              strata_lower == 6 & 
                                                strata_upper == 8 ~ "canopy"),
                    analysis_group_strata =  ifelse(!is.na(strata), 
                                                    paste(analysis_group, strata),
                                                    analysis_group)) |>
      dplyr::select(year, group, relnumb, analysis_group_strata, scov_adj) |>
      dplyr::arrange(year, group, relnumb, analysis_group_strata)|>
      dplyr::rename(Year = year, Group = group, Quadrat = relnumb,
                    Species = analysis_group_strata, Cover = scov_adj) |>
      as.data.frame()
    
    # return
    return(dat5)
    
  }
  
}
