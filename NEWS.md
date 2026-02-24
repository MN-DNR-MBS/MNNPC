# MNNPC 1.1.5 (24-02-2026)

* Adding the 'include_strata' argument to `MNNPC::process_dnr_releves` which controls whether taxa are aggregated into strata or not.

# MNNPC 1.1.4 (22-02-2026)

* Re-exporting `MNNPC::mnnpc_releves` with section names in between unit codes and releve numbers.

# MNNPC 1.1.3 (22-02-2026)

* Re-exporting `MNNPC::mnnpc_releves` ensuring that all classes, types, and subtypes for all sections and statewide are included.

# MNNPC 1.1.2 (21-02-2026)

* Re-calculating `MNNPC::mnnpc_diversity_metrics`, ensuring that all strata are correctly removed and all cover values are less than 100.

# MNNPC 1.1.1 (21-02-2026)

* Re-calculating `MNNPC::mnnpc_fidelity_metrics` and `MNNPC::mnnpc_diversity_metrics`.

# MNNPC 1.1.0 (20-02-2026)

* Adding `MNNPC::mnnpc_development_data` object which contains the releves used to define the MN NPC communities.
* Adding `MNNPC::mnnpc_releves` which contains the releve data from `MNNPC::mnnpc_development_data` for each ecs_section and statewide.
* Finalising the floristic tables and taxonomic backbone information.

# MNNPC 1.0.0 (04-02-2026)

* Adding `MNNPC::mnnpc_fidelity_metrics` and `MNNPC::mnnpc_diversity_metrics`.
* Ensuring data compression is xz.
* Adding the cover_scale option to `MNNPC::process_dnr_releves`.

# MNNPC 0.9.9 (02-01-2026)

* Removing `MNNPC::mnnpc_taxa_conv`, all lookup information now contained in `MNNPC::mnnpc_taxa_lookup` and a new object `MNNPC::mnnpc_hybrid_crosswalk`.
* Adding four arguments to `MNNPC::process_dnr_releves` which 
* Adding section-specific floristic tables, pseudo-quadrats, and community attributes.
* Adding constant objects `MNNPC::mnnpc_vc_types_flreg_named` and `MNNPC::mnnpc_ecs_sections`.
* Re-naming `MNNPC::example_releve` to `MNNPC::mnnpc_example_releve`.
* Removing unwanted and indeterminate taxa from `MNNPC::mnnpc_example_data`.

# MNNPC 0.9.8 (15-12-2025)

* Ensuring all analysis_group aggregates (see `MNNPC::mnnpc_taxa_lookup`) are included in `MNNPC::mnnpc_accepted_taxa`.

# MNNPC 0.9.7 (15-12-2025)

* Ensuring all data documentation is complete.
* Finalising `MNNPC::mnnpc_taxa_conv` and adding an internal object `MNNPC::mnnpc_taxa_conv_raw` for use by `MNNPC::process_dnr_releves`.

# MNNPC 0.9.6 (12-12-2025)

* Fixing instances where "phylo" was incorrectly spelt as phlyo.
* Adding code to calculate diversity measures for the MNNPC in ./data-raw/9_calc_diversity.R

# MNNPC 0.9.5 (09-12-2025)

* Adding branch lengths to `MNNPC::mnnpc_phylo_tree`.
* Changing the name of `MNNPC::mnnpc_accepted_phylo_taxa_lookup` to `MNNPC::mnnpc_phylo_taxa_lookup` 
  and ensuring that the matched OTL name and OTL OTT codes are included in this lookup.

# MNNPC 0.9.4 (08-12-2025)

* Adding function `MNNPC::process_dnr_releves`.
* Adding objects: `MNNPC::mnnpc_phylo_tree`, `MNNPC::mnnpc_accepted_phylo_taxa_lookup`, `MNNPC::mnnpc_ht_conv`, `MNNPC::mnnpc_strata`, `MNNPC::mnnpc_scov_conv`, and `MNNPC::mnnpc_example_releve`.

# MNNPC 0.9.3 (03-12-2025)

* Adding temporary `MNNPC::mnnpc_taxa_conv` object.
* `MNNPC::mnnpc_taxa_conv`, `MNNPC::mnnpc_taxonomic_backbone`, and `MNNPC::mnnpc_taxa_lookup` still need finalising.

# MNNPC 0.9.2 (26-12-2025)

* Ensuring version in DESCRIPTION is correct.
* Adding README version badge.

# MNNPC 0.9.1 (26-12-2025)

* Ensuring example data Year column values are of type integer.
* Ensuring Earthworm-Invaded Forests example dataset has complete cover values.

# MNNPC 0.9.0 (26-12-2025)

Initial prototype version.
