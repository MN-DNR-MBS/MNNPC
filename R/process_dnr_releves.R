#' Process releve data into the format required by RMAVIS
#' 
#' Process releve data into the format required by RMAVIS, specifically a data frame
#' in long format containing "Year", "Group", "Quadrat", "Species", and "Cover" columns.
#'
#' @param releve_data See `MNNPC::example_releve`.
#'
#' @returns A data frame containing the releve data with five columns: "Year", "Group", "Quadrat", "Species", "Cover"
#' @export
#'
#' @examples
#' MNNPC::process_dnr_releves(releve_data = MNNPC::example_releve)
process_dnr_releves <- function(releve_data){
  
  #### check for processed releve ####
  
  dat <- releve_data
  
  # final fields
  final_fields <- c("Year", "Group", "Quadrat", "Species", "Cover")
  
  # check for processed releve
  if(all(final_fields %in% names(dat)) == T){
    
    message("It looks like you've uploaded processed data. No further processing will be applied before analysis.")
    
    return(dat)
    
  } else {
    
    #### checks and replies ####
    
    # check for required fields
    fields_required <- c("year", "group", "relnumb", "physcode", "minht", "maxht", 
                         "taxon", "scov")
    
    if(all(fields_required %in% names(dat)) == F){
      
      missing_fields <- fields_required[!(fields_required %in% names(dat))]
      stop(paste("Missing required fields:", 
                 paste(missing_fields, collapse = ", ")))
      
    }
    
    # convert heights to numbers if they aren't already
    # this will cause a warning if there's "X" in these columns
    dat2 <- dat |>
      dplyr::mutate(minht = as.numeric(minht),
                    maxht = as.numeric(maxht))
    
    # check for missing data
    n_missing_data <- dat2 |>
      dplyr::filter(is.na(physcode) | is.na(taxon) | is.na(scov) |
                      (physcode %in% c("D", "E") & is.na(minht)) | 
                      (physcode %in% c("D", "E") & is.na(maxht))) |>
      nrow()
    
    # warn and remove rows with missing required data
    if(n_missing_data > 0){
      warning(paste("There are", n_missing_data, "rows with missing required data (physcode, taxon, scov, or trees missing minht or maxht). These rows will be excluded from analysis."))
      
      dat2 <- dat2 |>
        dplyr::filter(!is.na(physcode) & !is.na(taxon) & !is.na(scov)) # minht and maxht fixed below
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
    
    
    #### process data ####
    
    # convert scov to numeric if it's not already
    if(is.numeric(dat2$scov) == F){
      
      dat2 <- dat2 |>
        dplyr::left_join(MNNPC::mnnpc_scov_conv) |>
        dplyr::select(-scov) |>
        dplyr::rename(scov = scov_mid)
      
    }
    
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
    
    # remove s.s. and s.l.
    # add analysis group by taxon name
    dat3 <- dat2 |>
      dplyr::mutate(taxon = gsub(" s\\.s\\.", "", taxon),
                    taxon = gsub(" s\\.l\\.", "", taxon))|>
      dplyr::left_join(MNNPC::mnnpc_taxa_conv)
    
    # taxa that will be removed
    taxa_removed <- dat3 |>
      dplyr::filter(is.na(analysis_group)) |>
      dplyr::pull(taxon) |>
      unique() |>
      sort()
    
    # warn user about taxa being removed
    if(length(taxa_removed) > 0){
      
      warning(paste("The following taxa will be removed from the analysis. Please see the taxonomic backbone and taxa lookup for taxa that can be included in the analysis:", paste(taxa_removed, collapse = ", ")))
      
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