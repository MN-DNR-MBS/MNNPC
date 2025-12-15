#' Accepted taxon names
#'
#' A data frame containing the accepted taxon names following MNTaxa: The State of Minnesota Vascular Plant Checklist
#' as present in the `MNNPC::mnnpc_taxonomic_backbone` object.
#'
#' \code{mnnpc_accepted_taxa} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_accepted_taxa)` rows and `r ncol(MNNPC::mnnpc_accepted_taxa)` columns, the definitions of which are:
#' \describe{
#'   \item{taxon_name}{Taxon names.}
#' }
"mnnpc_accepted_taxa"

#' Attributes for each MNNPC community.
#'
#' Selected attributes for each MNNPC community.
#'
#' \code{mnnpc_community_attributes} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_community_attributes)` rows and `r ncol(MNNPC::mnnpc_community_attributes)` columns, the definitions of which are:
#' \describe{
#'   \item{fullname}{The full name of the MN NPC unit.}
#'   \item{name}{The name of the MN NPC unit.}
#'   \item{npc_code}{The code for the MN NPC unit.}
#'   \item{npc_code_parent}{The parent unit for the MN NPC unit.}
#'   \item{basal}{A boolean (TRUE/FALSE) indicating whether the MN NPC unit is at the lowest rank and has no sub-units.}
#'   \item{rank}{The rank of the unit, one of: system, class, type, subtype.}
#'   \item{num_samples}{The number of plots/samples which constitute the MN NPC unit.}
#'   \item{min_species}{The minimum number of species recorded in the plots which constitute the MN NPC unit.}
#'   \item{max_species}{The maximum number of species recorded in the plots which constitute the MN NPC unit.}
#'   \item{mean_species}{The mean number of species in the plots which constitute the MN NPC unit.}
#'   \item{species_count}{The total number of species recorded in the plots which constitute the MN NPC unit.}
#' }
"mnnpc_community_attributes"

#' Example vegetation plot data
#'
#' A selection of example vegetation plot datasets, currently included are:
#' 
#' - St. Croix State Forest
#' - Earthworm-Invaded Forests
#'
#' \code{mnnpc_example_data} 
#'
#' @format A list of `r length(MNNPC::mnnpc_example_data)` data frames with `r nrow(MNNPC::mnnpc_example_data)` rows and `r ncol(MNNPC::mnnpc_example_data)` columns, the definitions of which are:
#' \describe{
#'   \item{year}{The year the plot was sampled.}
#'   \item{group}{The group containing the plot.}
#'   \item{relnumb}{The ID/releve number of the plot.}
#'   \item{physcode}{The physiognomy code of the taxon.}
#'   \item{minht}{The minimum height class in which the taxon was recorded.}
#'   \item{maxht}{The maximum height class in which the taxon was recorded.}
#'   \item{taxon}{The taxon name, see `MNNPC::mnnpc_taxa_lookup`.}
#'   \item{scov}{The percent cover of the taxon in the plot within the strata.}
#' }
"mnnpc_example_data"

#' Floristic Tables
#'
#' The floristic tables for each MNNPC unit.
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

#' A lookup between the accepted MNTaxa taxa and their synonyms
#'
#' A lookup between the accepted MNTaxa taxa and their synonyms, with additional information.
#'
#' \code{mnnpc_taxa_lookup} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_taxa_lookup)` rows and `r ncol(MNNPC::mnnpc_taxa_lookup)` columns, the definitions of which are:
#' \describe{
#'   \item{informal_group}{The informal taxonomic group in which the taxa belongs.}
#'   \item{taxon_name}{The taxon concept name, includes both accepted/recommended taxa and their synonyms.}
#'   \item{recommended_taxon_name}{The accepted/recommended taxon name, see `MNNPC::mnnpc_accepted_taxa` and `MNNPC::mnnpc_taxonomic_backbone`.}
#'   \item{recommended_rank}{The taxonomic rank of the accepted/recommended taxon.}
#'   \item{recommended_scientific_name}{The scientific name of the accepted/recommended taxon concept, which is equivalent to a concatentation of the taxon_name and authority.}
#'   \item{recommended_full_name}{The scientific name of the accepted/recommended taxon concept, plus the height strata, if applicable.}
#'   \item{recommended_id}{The ID of the accepted/recommended taxon in MNTaxa: The State of Minnesota Vascular Plant Checklist.}
#'   \item{recommended_publication}{The parent publication containing the accepted/recommended taxon description.}
#'   \item{analysis_group}{The group in which the accepted/recommended taxon was placed for MN NPC classification analyses.}
#'   \item{analysis_group_includes}{The list of taxa in the group (separated by slashes) in which the accepted/recommended taxon was placed for MN NPC classification analyses.}
#' }
"mnnpc_taxa_lookup"

#' Taxon name conversion
#'
#' A data frame containing all taxon names present in `MNNPC::taxa_lookup` and 
#' the taxa and taxon groups that should be used for MNNPC analysis. For example,
#' Oxalis stricta, Oxalis dillenii, Oxalis montana, and Oxalis acetosella are all
#' lumped into the 'Oxalis ecological group' analysis group. 
#' The `MNNPC::mnnpc_pquads`, `MNNPC::mnnpc_floristic_tables`, and `MNNPC::example_data`
#' objects only include taxa and taxon groups from the 'analysis_group' column 
#' in this object and `RMAVIS` will only match based on these groups.
#'
#' \code{mnnpc_taxa_conv} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_taxa_conv)` rows and `r ncol(MNNPC::mnnpc_taxa_conv)` columns, the definitions of which are:
#' \describe{
#'   \item{taxon_name}{The taxon name, see `MNNPC::taxa_lookup`}
#'   \item{analysis_group}{The group in which the accepted/recommended taxon was placed for MN NPC classification analyses, with height strata omitted.}
#' }
"mnnpc_taxa_conv"

#' The taxonomic backbone for the MNNPC
#'
#' The taxonomic backbone for the MNNPC, containing the recommended/accepted taxon names and associated taxonomic information.
#'
#' \code{mnnpc_taxonomic_backbone} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_taxonomic_backbone)` rows and `r ncol(MNNPC::mnnpc_taxonomic_backbone)` columns, the definitions of which are:
#' \describe{
#'   \item{informal_group}{The informal taxonomic group in which the taxa belongs.}
#'   \item{id}{The ID of the taxon in MNTaxa: The State of Minnesota Vascular Plant Checklist.}
#'   \item{taxon_name}{The taxon concept name, includes accepted or recommended taxa only.}
#'   \item{rank}{The taxonomic rank of the taxon recorded in taxon_name.}
#'   \item{scientific_name}{The scientific name of the taxon concept, which is equivalent to a concatentation of the taxon_name and authority.}
#'   \item{full_name}{The scientific name of the taxon concept, plus the height strata, if applicable.}
#'   \item{publication}{The parent publication containing the taxon_name description.}
#'   \item{common_name}{The common name associated with the taxon_name.}
#'   \item{origin}{The origin relative to Minnesota, USA (Introduced, Native, Unknown) of the taxon_name.}
#'   \item{species}{The name of the species taxon associated with the taxon_name, if applicable.}
#'   \item{genus}{The name of the genus taxon associated with the taxon_name.}
#'   \item{family}{The name of the family taxon associated with the taxon_name.}
#'   \item{order}{The name of the order taxon associated with the taxon_name.}
#'   \item{class}{The name of the class taxon associated with the taxon_name.}
#'   \item{phylum}{The name of the phylum taxon associated with the taxon_name.}
#'   \item{kingdom}{The name of the kingdom taxon associated with the taxon_name.}
#' }
"mnnpc_taxonomic_backbone"

#' MNNPC system code types/prefixes
#'
#' A vector of codes for the MNNPC community system which are prefixes for all child community classes, types, and sub-types.
#'
#' \code{mnnpc_vc_types} 
#'
#' @format A data frame with `r length(MNNPC::mnnpc_vc_types)` which contains ...
"mnnpc_vc_types"

#' MNNPC system code types/prefixes with full names
#'
#' A vector of named codes for the MNNPC community system which are prefixes for all child community classes, types, and sub-types.
#'
#' \code{mnnpc_vc_types_named} 
#'
#' @format A data frame with `r length(MNNPC::mnnpc_vc_types_named)` which contains ...
"mnnpc_vc_types_named"

#' example_releve
#'
#' A releve plot from the St. Croix State Forest study to serve as an example for formatting. For more information on height levels, see A handbook for collecting vegetation plot data in Minnesota: The relevé method.
#'
#' \code{example_releve} 
#'
#' @format A data frame with `r nrow(MNNPC::example_releve)` rows and `r ncol(MNNPC::example_releve)` columns, the definitions of which are:
#' \describe{
#'   \item{year}{The year the plot was sampled.}
#'   \item{group}{The group containing the plot.}
#'   \item{relnumb}{The ID/releve number of the plot.}
#'   \item{physcode}{The physiognomy code of the taxon.}
#'   \item{minht}{The minimum height class in which the taxon was recorded.}
#'   \item{maxht}{The maximum height class in which the taxon was recorded.}
#'   \item{taxon}{The taxon name, see `MNNPC::mnnpc_taxa_lookup`.}
#'   \item{scov}{The percent cover of the taxon in the plot within the strata.}
#' }
"example_releve"

#' mnnpc_scov_conv
#'
#' A dataframe used to convert cover values from the Braun-Blanquet cover/abundance scale to percent cover based on approximate mid-points (Tüxen and Ellenberg 1937).
#'
#' \code{mnnpc_scov_conv} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_scov_conv)` rows and `r ncol(MNNPC::mnnpc_scov_conv)` columns, the definitions of which are:
#' \describe{
#'   \item{scov}{Braun-Blanquet cover/abundance scale values.}
#'   \item{scov_mid}{Percent cover values.}
#' }
"mnnpc_scov_conv"

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
