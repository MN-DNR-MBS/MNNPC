#' ...
#'
#' ...
#'
#' \code{...} 
#'
#' @format A data frame with `r nrow(MNNPC::...)` rows and `r ncol(MNNPC::...)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#' }
""

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
#' A lookup between the accepted MNTaxa taxa and their synonyms
#'
#' \code{mnnpc_taxa_lookup} 
#'
#' @format A data frame with `r nrow(MNNPC::mnnpc_taxa_lookup)` rows and `r ncol(MNNPC::mnnpc_taxa_lookup)` columns, the definitions of which are:
#' \describe{
#'   \item{informal_group}{The informal taxonomic group in which the taxa belongs.}
#'   \item{taxon_name}{Taxon concept names, includes both accepted/recommended taxa and their synonyms.}
#'   \item{rank}{The taxonomic rank of the taxon recorded in taxon_name.}
#'   \item{qualifier}{}
#'   \item{authority}{}
#'   \item{strata}{}
#'   \item{full_name}{}
#'   \item{publication}{}
#'   \item{taxon_id}{}
#'   \item{recommended_taxon_name}{The accepted/recommended taxon name, see `MNNPC::mnnpc_accepted_taxa` and `MNNPC::mnnpc_taxonomic_backbone`.}
#'   \item{recommended_taxa_included}{}
#'   \item{recommended_publication}{}
#'   \item{recommended_common_name}{}
#'   \item{recommended_c_value}{...}
#'   \item{recommended_origin}{}
#' }
"mnnpc_taxa_lookup"