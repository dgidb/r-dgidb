#' Get ClinicalTrials.gov studies for drug terms
#'
#' @param terms Character vector of drug names.
#' @param clinicalTrialsUrl ClinicalTrials.gov v2 studies endpoint.
#'
#' @return A data frame with one row per matching study per supplied drug term.
#' Multi-value fields are represented as list-columns.
#' @export
#'
#' @examples
#' \dontrun{
#' getClinicalTrials(c("zolgensma", "imatinib"))
#' }
getClinicalTrials <- function(
    terms,
    clinicalTrialsUrl = "https://clinicaltrials.gov/api/v2/studies"
) {
    if (!is.character(terms) || !length(terms) || anyNA(terms) ||
        any(!nzchar(terms))) {
        stop("`terms` must be a non-empty character vector.", call. = FALSE)
    }

    output <- .emptyClinicalTrials()

    for (term in terms) {
        response <- httr2::request(clinicalTrialsUrl) |>
            httr2::req_url_query(
                `query.intr` = term,
                pageSize = 1000,
                format = "json"
            ) |>
            httr2::req_perform() |>
            httr2::resp_body_json(simplifyVector = FALSE)

        studies <- response$studies %||% list()

        for (study in studies) {
            output <- .addClinicalTrial(output, term, study)
        }
    }

    output
}

.emptyClinicalTrials <- function() {
    structure(
        list(
            drug_name = character(),
            trial_id = character(),
            brief = character(),
            study_type = character(),
            min_age = character(),
            max_age = character(),
            age_groups = list(),
            pediatric = logical(),
            conditions = list(),
            interventions = list(),
            incl_excl_criteria = character(),
            population_sex = character(),
            population_description = character(),
            potential_sites = list()
        ),
        class = "data.frame",
        row.names = integer()
    )
}

.addClinicalTrial <- function(output, drugName, study) {
    protocol <- study$protocolSection %||% list()

    identification <- protocol$identificationModule %||% list()
    design <- protocol$designModule %||% list()
    eligibility <- protocol$eligibilityModule %||% list()
    conditionsModule <- protocol$conditionsModule %||% list()
    arms <- protocol$armsInterventionsModule %||% list()
    contacts <- protocol$contactsLocationsModule %||% list()

    ageGroups <- eligibility$stdAges %||% NULL
    locations <- contacts$locations %||% list()

    potentialSites <- lapply(
        Filter(
            function(location) {
                (location$status %||% NA_character_) %in% c(
                    "RECRUITING",
                    "NOT_YET_RECRUITING",
                    "AVAILABLE",
                    "TEMPORARILY_NOT_AVAILABLE",
                    "UNKNOWN"
                )
            },
            locations
        ),
        function(location) {
            list(
                name = location$facility %||% NA_character_,
                status = location$status %||% NA_character_,
                city = location$city %||% NA_character_,
                country = location$country %||% NA_character_,
                coordinates = location$geoPoint %||% NULL
            )
        }
    )

    newRow <- structure(
        list(
            drug_name = toupper(drugName),
            trial_id = identification$nctId %||% NA_character_,
            brief = identification$briefTitle %||% NA_character_,
            study_type = design$studyType %||% NA_character_,
            min_age = eligibility$minimumAge %||% NA_character_,
            max_age = eligibility$maximumAge %||% NA_character_,
            age_groups = list(ageGroups),
            pediatric = if (is.null(ageGroups)) {
                NA
            } else {
                "CHILD" %in% ageGroups
            },
            conditions = list(conditionsModule$conditions %||% NULL),
            interventions = list(arms$interventions %||% NULL),
            incl_excl_criteria = eligibility$eligibilityCriteria %||% NA_character_,
            population_sex = eligibility$sex %||% NA_character_,
            population_description = eligibility$population %||% NA_character_,
            potential_sites = list(potentialSites)
        ),
        class = "data.frame",
        row.names = 1L
    )

    rbind(output, newRow)
}

`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L) y else x
}