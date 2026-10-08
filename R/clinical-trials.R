#' Get ClinicalTrials.gov studies for drug terms
#'
#' Retrieves ClinicalTrials.gov studies matching each supplied drug term. Results
#' are returned as one row per matching study per term. This function queries
#' ClinicalTrials.gov directly and does not use the DGIdb GraphQL API.
#'
#' Sites in \code{potential_sites} are limited to locations with a status of
#' \code{RECRUITING}, \code{NOT_YET_RECRUITING}, \code{AVAILABLE},
#' \code{TEMPORARILY_NOT_AVAILABLE}, or \code{UNKNOWN}.
#'
#' @param terms Character vector of drug names to search.
#' @param clinicalTrialsUrl ClinicalTrials.gov v2 studies endpoint. Primarily
#'   intended for testing with a mocked endpoint.
#'
#' @return A data frame with one row per clinical trial. The returned columns are:
#' \describe{
#'   \item{drug_name}{Uppercase version of the queried drug term.}
#'   \item{trial_id}{ClinicalTrials.gov NCT identifier.}
#'   \item{brief}{Brief study title.}
#'   \item{study_type}{ClinicalTrials.gov study type.}
#'   \item{min_age}{Minimum participant age, when reported.}
#'   \item{max_age}{Maximum participant age, when reported.}
#'   \item{age_groups}{List-column of ClinicalTrials.gov standard age groups.}
#'   \item{pediatric}{Whether the study includes the \code{CHILD} age group.}
#'   \item{conditions}{List-column of reported conditions.}
#'   \item{interventions}{List-column of intervention records.}
#'   \item{incl_excl_criteria}{Eligibility criteria text.}
#'   \item{population_sex}{Eligible participant sex.}
#'   \item{population_description}{Study population description, when reported.}
#'   \item{potential_sites}{List-column of potentially enrolling locations.}
#' }
#'
#' @details
#' Fields with multiple values are stored as list-columns, consistent with other
#' dgiR query functions. Missing scalar fields are returned as \code{NA}; missing
#' multi-value fields are retained as \code{NULL} list-column entries.
#'
#' @examples
#' \dontrun{
#' trials <- getClinicalTrials(c("zolgensma", "GDC-0199"))
#' trials[, c("drug_name", "trial_id", "brief", "pediatric")]
#'
#' # Examine locations that may be recruiting.
#' trials$potential_sites[[1]]
#' }
#'
#' @export
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
            httr2::req_timeout(30) |>
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