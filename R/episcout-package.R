#' episcout: cleaning, exploration and reviewed data workflows
#'
#' @description Helpers for cleaning, exploring and visualising data, including specification-first EDA and an audit-first PostgreSQL workflow for stable longitudinal pseudonymisation.
#'
#' @details Start with `vignette("introduction_episcout")` for package helpers, `vignette("specification-first-eda")` for reviewed EDA, or `vignette("longitudinal-pseudonymisation")` for one stable pseudonymous identity across related PostgreSQL tables. The longitudinal guide covers database prerequisites, value-free linkage metadata, read-only audits, blockers, apply, recovery and EDA handoff. The installed `examples/db-to-report/walkthrough.R` script provides a complete interactive synthetic exercise from duplicate review and database setup through pseudonymisation, EDA, Table 1 and HTML report output.
#'
#' Pseudonymised data remain restricted personal data. They are not anonymous or automatically disclosure-controlled.
#'
#' @seealso [epi_sec_linkage_scaffold()], [epi_sec_identity_registry_init()], [epi_sec_pseudonymise_db()], [epi_eda_intake_run()]
#' @keywords internal
"_PACKAGE"
