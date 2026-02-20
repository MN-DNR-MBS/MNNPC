#' Accepted taxon names
#'
#' A data frame containing the accepted taxon names following MNTaxa: The State of Minnesota Vascular Plant Checklist
#' as present in the `MNNPC::mnnpc_taxonomic_backbone` object.
#'
#' \code{mnnpc_accepted_taxa} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_accepted_taxa)` rows and `r ncol(MNNPC::mnnpc_accepted_taxa)` columns, the definitions of which are:
#' \describe{
#'   \item{taxon_id}{Taxon ID in MNTaxa, if available.}
#'   \item{taxon_name}{Taxon names.}
#' }
"mnnpc_accepted_taxa"

#' Diversity statistics
#'
#' Diversity statistics for the MNNPC calculated using the R package `rdiversity`.
#'
#' \code{mnnpc_diversity_metrics}
#' 
#' @references Reeve, R., Leinster, T., Cobbold, C.A., Thompson, J., Brummitt, N., Mitchell, S.N., Matthews, L., 2016. How to partition diversity. https://doi.org/10.48550/arXiv.1404.6520
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_diversity_metrics)` rows and `r ncol(MNNPC::mnnpc_diversity_metrics)` columns, the definitions of which are:
#' \describe{
#'   \item{ecs_section}{The abbreviation of the ECS Section in which the plots occurred, one of: Lake Agassiz, Aspen Parklands (LAP), Minnesota & NE Iowa Morainal (MIM), N. Minnesota & Ontario Peatlands(MOP), N. Minnesota Drift & Lake Plains (MDL), North Central Glaciated Plains (CGP), Northern Superior Uplands (NSU), Paleozoic Plateau (PPL), Red River Valley (RRV), Southern Superior Upland (SSU), Western Superior Uplands (WSU), or Statewide (statewide).}
#'   \item{rank}{The MNNPC unit rank associated with the npc_class, npc_type, or npc_subtype.}
#'   \item{npc_class}{The names of the MNNPC unit classes, the highest rank.}
#'   \item{npc_type}{The names of the MNNPC unit types.}
#'   \item{npc_subtype}{The names of the MNNPC unit subtypes.}
#'   \item{Quadrat}{The name of the quadrat/releve.}
#'   \item{measure}{The name of the diversity measure, one of: normalised alpha, normalised beta, and gamma.}
#'   \item{metric}{The name of the diversity metric, one of: naive, taxonomic, and phylogenetic.}
#'   \item{partition_level}{The diversity partition level, which for this object is equal to metacommunity for rows containing data for MNNPC class, type, or subtypes; for quadrats/releves the partition level is equal to subcommunity.}
#'   \item{q}{The Hill number, for this dataset three Hill numbers were used: 0, 1, and 2.}
#'   \item{diversity}{The diversity value.}
#' }
"mnnpc_diversity_metrics"

#' Attributes for each MN NPC unit.
#'
#' Selected attributes for each MN NPC unit, including statewide attributes and attributes specific to each Ecological Classification System Section.
#'
#' \code{mnnpc_community_attributes} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_community_attributes)` rows and `r ncol(MNNPC::mnnpc_community_attributes)` columns, the definitions of which are:
#' \describe{
#'   \item{fullname}{The full name of the MN NPC unit, which may include the ECS Section, if applicable.}
#'   \item{name}{The name of the MN NPC unit, which may include the ECS Section, if applicable.}
#'   \item{npc_code}{The code for the MN NPC unit, which may have the ECS Section abbreviated appended with an underscore, if applicable.}
#'   \item{npc_code_parent}{The parent unit for the MN NPC unit, if applicable.}
#'   \item{basal}{A boolean (TRUE/FALSE) indicating whether the MN NPC unit is at the lowest rank and has no sub-units.}
#'   \item{rank}{The rank of the unit, one of: system, system + floristic region, class, type, subtype.}
#'   \item{num_samples}{The number of plots/samples which constitute the MN NPC unit.}
#'   \item{min_species}{The minimum number of species recorded in the plots which constitute the MN NPC unit.}
#'   \item{max_species}{The maximum number of species recorded in the plots which constitute the MN NPC unit.}
#'   \item{mean_species}{The mean number of species in the plots which constitute the MN NPC unit.}
#'   \item{species_count}{The total number of species recorded in the plots which constitute the MN NPC unit.}
#'   \item{ecs_section}{The abbreviation of the ECS Section in which the plots occurred, one of: Lake Agassiz, Aspen Parklands (LAP), Minnesota & NE Iowa Morainal (MIM), N. Minnesota & Ontario Peatlands(MOP), N. Minnesota Drift & Lake Plains (MDL), North Central Glaciated Plains (CGP), Northern Superior Uplands (NSU), Paleozoic Plateau (PPL), Red River Valley (RRV), Southern Superior Upland (SSU), Western Superior Uplands (WSU), or statewide.}
#' }
"mnnpc_community_attributes"

#' Example vegetation plot data
#'
#' A selection of example vegetation plot datasets, currently included are:
#' 
#' - St. Croix State Forest (https://silvlib.cfans.umn.edu/deer-exclosure-effects-mesic-oak-forest-st-croix-state-forest-mn-dnr)
#' - Earthworm-Invaded Forests
#'
#'For more information on physiognomy codes and height levels, see A handbook for collecting vegetation plot data in Minnesota: The relevé method.
#'
#' \code{mnnpc_example_data} 
#'
#' @format A list of `r length(MNNPC::mnnpc_example_data)` data frames with `r nrow(MNNPC::mnnpc_example_data[[1]])` and `r nrow(MNNPC::mnnpc_example_data[[2]])` rows, respectively, and `r ncol(MNNPC::mnnpc_example_data[[1]])` columns, the definitions of which are:
#' \describe{
#'   \item{year}{The year the plot was sampled. Here, numbered from one on, but can be actual year.}
#'   \item{group}{The group containing the plot.}
#'   \item{relnumb}{The ID/releve number of the plot (true numbers have been masked for data security).}
#'   \item{physcode}{The physiognomy code of the taxon.}
#'   \item{minht}{The minimum height class in which the taxon was recorded.}
#'   \item{maxht}{The maximum height class in which the taxon was recorded.}
#'   \item{taxon}{The taxon name, see `MNNPC::mnnpc_taxa_lookup`.}
#'   \item{scov}{The cover of the taxon in the plot within the strata on the Braun-Blanquet scale..}
#'   \item{outside_of_plot}{Indicator of whether the observation occurred outside of the plot ("t" for true, "f" for false).}
#' }
"mnnpc_example_data"

#' Fidelity measures
#' 
#' Statistical measures of fidelity for taxa present in the MNNPC classification,
#' calculated following Chytrý et al (2002).
#'
#' \code{mnnpc_fidelity_metrics}
#' 
#' @references Chytrý, M., Tichý, L., Holt, J., Botta-Dukát, Z., 2002. Determination of diagnostic species with statistical fidelity measures. Journal of Vegetation Science 13, 79–90. https://doi.org/10.1111/j.1654-1103.2002.tb02025.x
#'
#' @format A list of `r length(MNNPC::mnnpc_fidelity_metrics)` data frames with `r nrow(MNNPC::mnnpc_fidelity_metrics)` rows and `r ncol(MNNPC::mnnpc_example_data)` columns, the definitions of which are:
#' \describe{
#'   \item{ecs_section}{The abbreviation of the ECS Section in which the plots occurred, one of: Lake Agassiz, Aspen Parklands (LAP), Minnesota & NE Iowa Morainal (MIM), N. Minnesota & Ontario Peatlands(MOP), N. Minnesota Drift & Lake Plains (MDL), North Central Glaciated Plains (CGP), Northern Superior Uplands (NSU), Paleozoic Plateau (PPL), Red River Valley (RRV), Southern Superior Upland (SSU), Western Superior Uplands (WSU), or Statewide (statewide).}
#'   \item{system}{The MNNPC unit system code.}
#'   \item{npc_class}{The MNNPC unit class code.}
#'   \item{species}{The name of the species/taxon.}
#'   \item{u}{The fidelity measure u, more specifically Bruelheide's u.}
#'   \item{phi}{The fidelity measure phi.}
#'   \item{chisq}{The fidelity measure chisq.}
#'   \item{g}{The fidelity measure g.}
#'   \item{indval}{The fidelity measure indval.}
#' }
"mnnpc_fidelity_metrics"

#' Floristic Table Data
#'
#' The data used to build floristic tables for each MN NPC unit.
#'
#' \code{mnnpc_development_data} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_development_data)` rows and `r ncol(MNNPC::mnnpc_development_data)` columns, the definitions of which are:
#' \describe{
#'   \item{Quadrat}{The ID/releve number of the plot (true numbers have been masked for data security).}
#'   \item{Species}{The formatted taxon name (analysis group and strata, if applicable), see `MNNPC::mnnpc_taxa_lookup`.}
#'   \item{Cover}{The percent cover of the taxon in the plot within the strata.}
#'   \item{analysis_group}{The analysis group, see `MNNPC::mnnpc_taxa_lookup`.}
#'   \item{strata_lower}{The minimum height class of the strata.}
#'   \item{strata_upper}{The maximum height class of the strata.}
#'   \item{code_strata}{Equivalent to Species, but with spaces replaced by underscores.}
#'   \item{used_in_fieldguide}{Indicator for whether the plot was used to inform MN NPC field guides published in 2003 and 2005.}
#'   \item{npc_code}{The most specific MN NPC unit code that the plot was identified to.}
#'   \item{npc_system_id}{Code for the system level of the MN NPC classification system.}
#'   \item{npc_system}{Name of the system level of the MN NPC classification system.}
#'   \item{npc_sys_flor}{Code for the system level and floristic region of the MN NPC classification system.}
#'   \item{npc_class}{Code for the class level of the MN NPC classification system.}
#'   \item{npc_class_name}{Name of the class level of the MN NPC classification system.}
#'   \item{npc_type}{Code for the type level of the MN NPC classification system, if available.}
#'   \item{npc_type_name}{Name of the type level of the MN NPC classification system, if available.}
#'   \item{npc_subtype}{Code for the subtype level of the MN NPC classification system, if available.}
#'   \item{npc_subtype_name}{Name of the subtype level of the MN NPC classification system, if available.
#'   \item{ecs_secname}{Name of the ECS section where the plot occurs.}
#'   \item{ecs_section}{Code of the ECS section where the plot occurs.}
#' }
"mnnpc_development_data"

#' Floristic Tables
#'
#' The floristic tables for each MN NPC unit.
#'
#' \code{mnnpc_floristic_tables} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_floristic_tables)` rows and `r ncol(MNNPC::mnnpc_floristic_tables)` columns, the definitions of which are:
#' \describe{
#'   \item{npc_code}{The code for the MN NPC unit.}
#'   \item{npc_taxon_name}{The taxon name, see `MNNPC::mnnpc_accepted_taxa` and `MNNPC::mnnpc_taxonomic_backbone`.}
#'   \item{constancy}{The constancy of occurrence across the plots constituting the MN NPC unit.}
#'   \item{absolute_frequency}{The number of occurrences in the plots constituting the MN NPC unit.}
#'   \item{relative_frequency}{The proportion of plots constituting the MN NPC unit that the taxon is present in.}
#'   \item{minimum_cover}{The minimum cover of the taxon in the plots constituting the MN NPC unit.}
#'   \item{mean_cover}{The mean cover of the taxon in the plots constituting the MN NPC unit.}
#'   \item{maximum_cover}{The maximum cover of the taxon in the plots constituting the MN NPC unit.}
#' }
"mnnpc_floristic_tables"

#' Pseudo-quadrats
#'
#' Pseudo quadrats for each MNNPC unit.
#'
#' \code{mnnpc_pquads} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_pquads)` rows and `r ncol(MNNPC::mnnpc_pquads)` columns, the definitions of which are:
#' \describe{
#'   \item{npc_code}{The code for the MN NPC unit.}
#'   \item{psq_id}{The pseudo-quadrat ID.}
#'   \item{taxon_name}{The taxon name, see `MNNPC::mnnpc_accepted_taxa` and `MNNPC::mnnpc_taxonomic_backbone`.}
#' }
"mnnpc_pquads"

#' MN NPC Releves
#'
#' The releves used to define each MNNPC unit, by ECS section, produced using the 
#' `MNNPC::mnnpc_development_data` object.
#'
#' \code{mnnpc_releves} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_releves)` rows and `r ncol(MNNPC::mnnpc_releves)` columns, the definitions of which are:
#' \describe{
#'   \item{npc_code}{The code for the MN NPC unit.}
#'   \item{releve}{An anonymised ID for each releve.}
#'   \item{taxon_name}{The taxon name, see `MNNPC::mnnpc_accepted_taxa` and `MNNPC::mnnpc_taxonomic_backbone`.}
#' }
"mnnpc_releves"

#' A lookup between the accepted MNTaxa taxa and their synonyms
#'
#' A lookup between the accepted MNTaxa taxa and their synonyms, with additional information.
#'
#' \code{mnnpc_taxa_lookup} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_taxa_lookup)` rows and `r ncol(MNNPC::mnnpc_taxa_lookup)` columns, the definitions of which are:
#' \describe{
#'   \item{informal_group}{The informal taxonomic group (physiognomy) in which the taxon belongs.}
#'   \item{taxon_name}{The taxon concept name, includes both accepted/recommended taxa and their synonyms.}
#'   \item{recommended_taxon_name}{The accepted/recommended taxon name, see `MNNPC::mnnpc_accepted_taxa` and `MNNPC::mnnpc_taxonomic_backbone`. Note that in some cases, taxa cannot be matched to a single accepted/recommended taxon, so these have been grouped together.}
#'   \item{recommended_rank}{The taxonomic rank of the accepted/recommended taxon.}
#'   \item{recommended_scientific_name}{The scientific name of the accepted/recommended taxon concept, which is equivalent to a concatentation of the taxon_name and authority, if available, and a qualifier, if applicable.}
#'   \item{recommended_id}{The ID of the accepted/recommended taxon in MNTaxa: The State of Minnesota Vascular Plant Checklist.}
#'   \item{recommended_publication}{The parent publication containing the accepted/recommended taxon description.}
#'   \item{recommended_stratcode}{One of "ground", "shrube", or NA, where ground or shrub, when applied to a broadleaf deciduous or needleleaf evergreen, indicate that the accepted/recommended taxon should not be stratified.}
#'   \item{recommended_physcode}{Physiognomy code for the accepted/recommended taxon.}
#'   \item{recommended_assignment}{Combinations of accepted/recommended taxa names that cannot be distinguished when working with historical data due to taxonomic name changes.}
#'   \item{analysis_group}{The group in which the accepted/recommended taxon was placed for MN NPC classification analyses.}
#' }
"mnnpc_taxa_lookup"

#' The taxonomic backbone for the MNNPC
#'
#' The taxonomic backbone for the MNNPC, containing the recommended/accepted taxon names and associated taxonomic information.
#'
#' \code{mnnpc_taxonomic_backbone} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_taxonomic_backbone)` rows and `r ncol(MNNPC::mnnpc_taxonomic_backbone)` columns, the definitions of which are:
#' \describe{
#'   \item{id}{The ID of the taxon in MNTaxa: The State of Minnesota Vascular Plant Checklist.}
#'   \item{taxon_name}{The taxon concept name, includes accepted or recommended taxa only.}
#'   \item{rank}{The taxonomic rank of the taxon recorded in taxon_name.}
#'   \item{qualifier}{Indicator of taxonomic uncertainty, if applicable (s.l. = sensu lato, s.s. = sensu stricto).}
#'   \item{authority}{Author(s) responsible for describing taxon, if applicable.}
#'   \item{publication}{The parent publication containing the taxon description.}
#'   \item{common_name}{The common name associated with the taxon.}
#'   \item{origin}{The origin relative to Minnesota, USA (Introduced, Native, Unknown).}
#'   \item{informal_group}{The informal taxonomic group (physiognomy) in which the taxa belongs.}
#'   \item{species}{The name of the species taxon associated with the taxon_name, if applicable.}
#'   \item{genus}{The name of the genus taxon associated with the taxon_name.}
#'   \item{family}{The name of the family taxon associated with the taxon_name.}
#'   \item{order}{The name of the order taxon associated with the taxon_name.}
#'   \item{class}{The name of the class taxon associated with the taxon_name.}
#'   \item{phylum}{The name of the phylum taxon associated with the taxon_name.}
#'   \item{kingdom}{The name of the kingdom taxon associated with the taxon_name.}
#' }
"mnnpc_taxonomic_backbone"

#' mnnpc_example_releve
#'
#' A releve plot from the St. Croix State Forest study (https://silvlib.cfans.umn.edu/deer-exclosure-effects-mesic-oak-forest-st-croix-state-forest-mn-dnr) to serve as an example for formatting. For more information on physiognomy codes and height levels, see A handbook for collecting vegetation plot data in Minnesota: The relevé method.
#'
#' \code{mnnpc_example_releve} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_example_releve)` rows and `r ncol(MNNPC::mnnpc_example_releve)` columns, the definitions of which are:
#' \describe{
#'   \item{year}{The year the plot was sampled. Here, numbered from one on, but can be actual year.}
#'   \item{group}{The group containing the plot.}
#'   \item{relnumb}{The ID/releve number of the plot (true numbers have been masked for data security).}
#'   \item{physcode}{The physiognomy code of the taxon.}
#'   \item{minht}{The minimum height class in which the taxon was recorded.}
#'   \item{maxht}{The maximum height class in which the taxon was recorded.}
#'   \item{taxon}{The taxon name, see `MNNPC::mnnpc_taxa_lookup`.}
#'   \item{scov}{The cover of the taxon in the plot within the strata on the Braun-Blanquet scale.}
#'   \item{outside_of_plot}{Indicator of whether the observation occurred outside of the plot ("t" for true, "f" for false).}
#' }
"mnnpc_example_releve"

#' mnnpc_bb_conv
#'
#' A dataframe used to convert cover values from the Braun-Blanquet cover/abundance scale to percent cover based on approximate mid-points (Tüxen and Ellenberg 1937, van der Maarel 2007).
#'
#' \code{mnnpc_bb_conv} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_bb_conv)` rows and `r ncol(MNNPC::mnnpc_bb_conv)` columns, the definitions of which are:
#' \describe{
#'   \item{scov}{Braun-Blanquet cover/abundance scale values.}
#'   \item{scov_mid}{Percent cover values.}
#' }
"mnnpc_bb_conv"

#' mnnpc_dom_conv
#'
#' A dataframe used to convert cover values from the Domin cover/abundance scale to percent cover based on approximate mid-points.
#'
#' \code{mnnpc_dom_conv} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_dom_conv)` rows and `r ncol(MNNPC::mnnpc_dom_conv)` columns, the definitions of which are:
#' \describe{
#'   \item{scov}{Domin cover/abundance scale values.}
#'   \item{scov_mid}{Percent cover values.}
#' }
"mnnpc_dom_conv"

#' mnnpc_strata
#'
#' A dataframe to specify height strata based on physiognomy. Trees are stratified into levels 1-3, 4-5, and 6-8. Other physiognomy types are not stratified.
#'
#' \code{mnnpc_strata} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_strata)` rows and `r ncol(MNNPC::mnnpc_strata)` columns, the definitions of which are:
#' \describe{
#'   \item{physcode}{Physiognomy code. For more information, see A handbook for collecting vegetation plot data in Minnesota: The relevé method.}
#'   \item{strata_lower}{The lower height level of the stratum.}
#'   \item{strata_upper}{The upper height level of the stratum.}
#' }
"mnnpc_strata"

#' mnnpc_ht_conv
#'
#' A dataframe to convert height levels to meters based on A handbook for collecting vegetation plot data in Minnesota: The relevé method.
#' 
#' \code{mnnpc_ht_conv} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_ht_conv)` rows and `r ncol(MNNPC::mnnpc_ht_conv)` columns, the definitions of which are:
#' \describe{
#'   \item{ht}{The height level to assign for strata between ht_min_m and ht_max_m.}
#'   \item{ht_min_m}{The minimum height of the level, in meters.}
#'   \item{ht_max_m}{The maximum height of the level, in meters.}
#' }
"mnnpc_ht_conv"

#' A lookup between accepted taxon names and the taxon names present in the phylogenetic tree
#'
#' A lookup between accepted taxon names as present in `MNNPC::mnnpc_accepted_taxa` and the taxon names present in the `MNNPC::mnnpc_phylo_tree` phylogenetic tree object.
#' At present `r t(as.matrix(table(MNNPC::mnnpc_phylo_taxa_lookup$phylo)))[[1, "TRUE"]]` taxa out of `r nrow(MNNPC::mnnpc_accepted_taxa)` are present in the tree.
#' The phylo_taxon_name are produced by reducing varieties and subspecies to the species rank, and removing strata and ecological group suffixes.
#' 
#' \code{mnnpc_phylo_taxa_lookup} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_phylo_taxa_lookup)` rows and `r ncol(MNNPC::mnnpc_phylo_taxa_lookup)` columns, the definitions of which are:
#' \describe{
#'   \item{taxon_name}{Accepted taxon names as present in `MNNPC::mnnpc_accepted_taxa`.}
#'   \item{search_name}{The taxon name used to search for OTL data, and as present in the `MNNPC::mnnpc_phylo_tree` object.}
#'   \item{matched_name}{The OTL name matched to the search_name.}
#'   \item{phylo}{A boolean representing whether each taxon is present in the `MNNPC::mnnpc_phylo_tree` object.}
#'   \item{ottid}{The OTL id associated with the matched name.}
#' }
"mnnpc_phylo_taxa_lookup"

#' A phylogenetic tree for the MNNPC taxa
#'
#' A phylogenetic tree in the Newick format, formed from taxa present in the search_name column in `MNNPC::mnnpc_phylo_taxa_lookup`, where available.
#' Branch lengths were calculated using the 'Grafen' method implemented in the `ape::compute.brlen` function.
#' 
#' \code{mnnpc_phylo_tree} 
#'
#' @format A string containing the phylogentic tree in the Newick format.
"mnnpc_phylo_tree"

#' Hybrid name crosswalk
#'
#' A data frame containing different formats of hybrid taxon names.
#'
#' \code{mnnpc_hybrid_crosswalk} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_hybrid_crosswalk)` rows and `r ncol(MNNPC::mnnpc_hybrid_crosswalk)` columns, the definitions of which are:
#' \describe{
#'   \item{taxon_rep}{The taxon name as it appears in other taxonomic data objects, such as `MNNPC::mnnpc_taxa_lookup`.}
#'   \item{taxon}{Three alternative formats of the hybrid taxon name that include capitalized and lowercase "x" and spaces between "x" and other words.}
#' }
"mnnpc_hybrid_crosswalk"


#' MNNPC ECS Sections
#'
#' A vector of named ECS section codes with full names.
#' 
#' \code{mnnpc_ecs_sections} 
#'
#' @format A named vector of strings of length `r length(MNNPC::mnnpc_ecs_sections)`.
"mnnpc_ecs_sections"

#' MNNPC system codes
#' 
#' A vector of codes for the MNNPC community systems which are prefixes for all child community classes, types, and sub-types.
#'
#' \code{mnnpc_vc_systems} 
#'
#' @format A vector of strings of length `r length(MNNPC::mnnpc_vc_systems)`.
"mnnpc_vc_systems"

#' MNNPC system codes with full names
#'
#' A vector of named codes for the MNNPC community system which are prefixes for all child community classes, types, and sub-types.
#'
#' \code{mnnpc_vc_systems_named} 
#'
#' @format A named vector of strings of length `r length(MNNPC::mnnpc_vc_systems_named)`.
"mnnpc_vc_systems_named"

#' MNNPC systems with floristic regions
#'
#' A vector of codes consisting of the MNNPC community systems and floristic regions.
#' 
#' \code{mnnpc_vc_systems_flreg} 
#'
#' @format A vector of strings of length `r length(MNNPC::mnnpc_vc_systems_flreg)`.
"mnnpc_vc_systems_flreg"

#' MNNPC systems with floristic regions with full names
#'
#' A named vector of MNNPC systems and floristic regions with full names.
#' 
#' \code{mnnpc_vc_systems_flreg_named} 
#'
#' @format A named vector of strings of length `r length(MNNPC::mnnpc_vc_systems_flreg_named)`.
"mnnpc_vc_systems_flreg_named"

#' MNNPC systems by region
#'
#' A list of MNNPC systems and system-regions with list entried equal to all child system-regions.
#' 
#' \code{mnnpc_vc_systems_flreg_nested} 
#'
#' @format A list of length `r length(MNNPC::mnnpc_vc_systems_flreg_nested)`
"mnnpc_vc_systems_flreg_nested"
