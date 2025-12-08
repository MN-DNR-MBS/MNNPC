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
#'   \item{fullname}{The full name of the MNNPC unit.}
#'   \item{name}{The name of the MNNPC unit.}
#'   \item{npc_code}{The code for the MNNPC unit.}
#'   \item{npc_code_parent}{The parent unit for the MNNPC unit.}
#'   \item{basal}{A boolean (TRUE/FALSE) indicating whether the MNNPC unit. is at the lowest rank and has no }
#'   \item{rank}{The rank of the community, one of: system, class, type, subtype.}
#'   \item{num_samples}{The number of plots/samples which constitute the MNNPC unit.}
#'   \item{min_species}{The minimum number of species recorded in the plots which constitute the MNNPC unit.}
#'   \item{max_species}{The maximum number of species recorded in the plots which constitute the MNNPC unit.}
#'   \item{mean_species}{The mean number of species in the plots which constitute the MNNPC unit.}
#'   \item{species_count}{The total number of species recorded in the plots which constitute the MNNPC unit.}
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
#'   \item{Site}{The name of the site.}
#'   \item{Year}{The year the quadrat was sampled in.}
#'   \item{Group}{The group containing the quadrat.}
#'   \item{Quadrat}{The quadrat name.}
#'   \item{Species}{The taxon name, see `MNNPC::mnnpc_accepted_taxa` and `MNNPC::mnnpc_taxonomic_backbone`.}
#'   \item{Cover}{The cover of the taxon in the quadrat.}
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
#'   \item{npc_code}{The code for the MNNPC unit.}
#'   \item{npc_taxon_name}{The taxon name, see `MNNPC::mnnpc_accepted_taxa` and `MNNPC::mnnpc_taxonomic_backbone`.}
#'   \item{constancy}{The constancy of occurrence across the plots constituting the MNNPC unit.}
#'   \item{absolute_frequency}{The number of occurrences in the plots constituting the MNNPC unit.}
#'   \item{relative_frequency}{The proportion of plots constituting the MNNPC unit that the taxon is present in.}
#'   \item{minimum_cover}{The minimum cover of the taxon in the plots constituting the MNNPC unit.}
#'   \item{mean_cover}{The mean cover of the taxon in the plots constituting the MNNPC unit.}
#'   \item{maximum_cover}{The maximum cover of the taxon in the plots constituting the MNNPC unit.}
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
#'   \item{npc_code}{The code for the MNNPC unit.}
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
#'   \item{taxon_name}{The taxon concept name, includes both accepted/recommended taxa and their synonyms.}
#'   \item{recommended_taxon_name}{The accepted/recommended taxon name, see `MNNPC::mnnpc_accepted_taxa` and `MNNPC::mnnpc_taxonomic_backbone`.}
#' }
"mnnpc_taxa_lookup"

#' Taxon name conversion
#'
#' A data frame containing all taxon names present in `MNNPC::taxa_lookup` and 
#' the taxa and taxon groups that should be used for MNNPC analysis. For example,
#' Oxalis stricta, Oxalis dillenii, Oxalis montana, and Oxalis acetosella are all
#' lumped into the 'Oxalis ecological group' analysis group. 
#' The `MNNPC::mnnpc_pquads`, `MNNPC::mnnpc_floristic_tables`, and `MNNPC::example_data`
#' objects only include taxa and taxon froups from the 'analysis_group' column 
#' in this object and `RMAVIS` will only match based on these groups.
#'
#' \code{mnnpc_taxa_conv} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_taxa_conv)` rows and `r ncol(MNNPC::mnnpc_taxa_conv)` columns, the definitions of which are:
#' \describe{
#'   \item{taxon}{The taxon name, see `MNNPC::taxa_lookup`}
#'   \item{analysis_group}{The taxon or group of taxa used for analysis in the MNNPC and RMAVIS.}
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
#'   \item{recommended_taxon_name}{The taxon concept name, includes accepted or recommended taxa only.}
#'   \item{taxa_included}{The taxon names of the taxa included in taxon_name.}
#'   \item{taxa_ids}{The ids of the taxa present in the taxa_included column.}
#'   \item{rank}{The taxonomic rank of the taxon recorded in taxon_name.}
#'   \item{hybrid}{The hybrid status of the taxon_name: 0 = non-hybrid, 1 = hybrid.}
#'   \item{full_name}{The full name of the taxon concept, which is equivalent to a concatentation of the taxon_name, qualifier, and authority.}
#'   \item{publication}{The parent publication containing the taxon_name description.}
#'   \item{common_name}{The common name associated with the taxa_included.}
#'   \item{origin}{The nativeness status of the taxon_name.}
#'   \item{species}{The name of the species taxon associated with the taxon_name.}
#'   \item{genus}{The name of the genus taxon associated with the taxon_name.}
#'   \item{family}{The name of the family taxon associated with the taxon_name.}
#'   \item{order}{The name of the order taxon associated with the taxon_name.}
#'   \item{class}{The name of the class taxon associated with the taxon_name.}
#'   \item{phylum}{The name of the phylum taxon associated with the taxon_name.}
#'   \item{kingdom}{The name of the kingdom taxon associated with the taxon_name.}
#'   \item{lineage_source}{The source of the parent taxa.}
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
#' ...
#'
#' \code{example_releve} 
#'
#' @format A data frame with `r nrow(MNNPC::example_releve)` rows and `r ncol(MNNPC::example_releve)` columns, the definitions of which are:
#' \describe{
#'   \item{year}{...}
#'   \item{group}{...}
#'   \item{relnumb}{...}
#'   \item{physcode}{...}
#'   \item{minht}{...}
#'   \item{maxht}{...}
#'   \item{taxon}{...}
#'   \item{scov}{...}
#' }
"example_releve"

#' mnnpc_scov_conv
#'
#' ...
#'
#' \code{mnnpc_scov_conv} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_scov_conv)` rows and `r ncol(MNNPC::mnnpc_scov_conv)` columns, the definitions of which are:
#' \describe{
#'   \item{scov}{...}
#'   \item{scov_mid}{...}
#' }
"mnnpc_scov_conv"

#' mnnpc_strata
#'
#' ...
#'
#' \code{mnnpc_strata} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_strata)` rows and `r ncol(MNNPC::mnnpc_strata)` columns, the definitions of which are:
#' \describe{
#'   \item{physcode}{...}
#'   \item{strata_lower}{...}
#'   \item{strata_upper}{...}
#' }
"mnnpc_strata"

#' mnnpc_ht_conv
#'
#' ...
#' 
#' \code{mnnpc_ht_conv} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_ht_conv)` rows and `r ncol(MNNPC::mnnpc_ht_conv)` columns, the definitions of which are:
#' \describe{
#'   \item{ht}{The height category to assign for strata between ht_min_m and ht_max_m.}
#'   \item{ht_min_m}{...}
#'   \item{ht_max_m}{...}
#' }
"mnnpc_ht_conv"

#' A lookup between accepted taxon names and the taxon names present in the phylogenetic tree
#'
#' A lookup between accepted taxon names as present in `MNNPC::mnnpc_accepted_taxa` and the taxon names present in the `MNNPC::mnnpc_phlyo_tree` phylogenetic tree object.
#' At present `r t(as.matrix(table(MNNPC::mnnpc_accepted_phylo_taxa_lookup$phylo)))[[1, "TRUE"]]` taxa out of `r nrow(MNNPC::mnnpc_accepted_taxa)` are present in the tree.
#' The phylo_taxon_name are produced by reducing varieties and subspecies to the species rank, and removing strata and ecological group suffixes.
#' 
#' \code{mnnpc_accepted_phylo_taxa_lookup} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_accepted_phylo_taxa_lookup)` rows and `r ncol(MNNPC::mnnpc_accepted_phylo_taxa_lookup)` columns, the definitions of which are:
#' \describe{
#'   \item{taxon_name}{Accepted taxon names as present in `MNNPC::mnnpc_accepted_taxa`.}
#'   \item{phylo_taxon_name}{The taxon name present in the `MNNPC::mnnpc_phlyo_tree` object.}
#'   \item{phylo}{A boolean representing whether each taxon is present in the `MNNPC::mnnpc_phlyo_tree` object.}
#' }
"mnnpc_accepted_phylo_taxa_lookup"

#' A phylogenetic tree for the MNNPC taxa
#'
#' A phylogenetic tree in the Newick format, formed from taxa present in the phylo_taxon_name column in `MNNPC::mnnpc_accepted_phylo_taxa_lookup`, where available.
#' 
#' \code{mnnpc_phlyo_tree} 
#'
#' @format A string containing the phylogentic tree in the Newick format.
"mnnpc_phlyo_tree"