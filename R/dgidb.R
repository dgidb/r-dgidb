api_endpoint_url <- Sys.getenv(
  "DGIDB_API_URL",
  unset = "https://dgidb.org/api/graphql"
)

group_attributes <- function(row) {
  grouped_dict <- list()
  for (attr in row) {
    if (is.null(attr$value)) {
      next
    } else if (attr$name %in% names(grouped_dict)) {
      grouped_dict[[attr$name]] <- append(grouped_dict[[attr$name]], attr$value)
    } else {
      grouped_dict[[attr$name]] <- list(attr$value)
    }
  }
  return(grouped_dict)
}

backfill_dicts <- function(col) {
  keys <- unique(unlist(lapply(col, names)))
  result <- lapply(col, function(cell) sapply(keys, function(key) cell[[key]]))
  return(result)
}

get_drugs <- function(
    terms,
    immunotherapy = NULL,
    antineoplastic = NULL,
    api_url = NULL) {
  params <- list(names = terms)
  if (!is.null(immunotherapy)) {
    params$immunotherapy <- immunotherapy
  }
  if (!is.null(antineoplastic)) {
    params$antineoplastic <- antineoplastic
  }

  api_url <- if (!is.null(api_url)) api_url else api_endpoint_url
  query <- readr::read_file("queries/get_drugs.graphql")
  response <- httr::POST(
    api_url,
    body = list(query = query, variables = params),
    encode = "json"
  )
  result <- httr::content(response)$data

  output <- list(
    drug_name = list(),
    drug_concept_id = list(),
    drug_aliases = list(),
    drug_attributes = list(),
    drug_is_antineoplastic = list(),
    drug_is_immunotherapy = list(),
    drug_is_approved = list(),
    drug_approval_ratings = list(),
    drug_fda_applications = list()
  )

  for (match in result$drugs$nodes) {
    output$drug_name <- append(
      output$drug_name, match$name
    )
    output$drug_concept_id <- append(
      output$drug_concept_id, match$conceptId
    )
    output$drug_aliases <- append(
      output$drug_aliases, list(lapply(match$drugAliases, function(a) a$alias))
    )
    output$drug_attributes <- append(
      output$drug_attributes, list(group_attributes(match$drugAttributes))
    )
    output$drug_is_antineoplastic <- append(
      output$drug_is_antineoplastic, match$antiNeoplastic
    )
    output$drug_is_immunotherapy <- append(
      output$drug_is_immunotherapy, match$immunotherapy
    )
    output$drug_is_approved <- append(
      output$drug_is_approved, match$approved
    )
    output$drug_approval_ratings <- append(
      output$drug_approval_ratings,
      list(lapply(match$drugApprovalRatings, function(r) {
        list(rating = r$rating, source = r$source$sourceDbName)
      }))
    )
    output$drug_fda_applications <- append(
      output$drug_fda_applications,
      list(lapply(match$drugApplications, function(app) app$appNo))
    )
  }
  output$drug_attributes <- backfill_dicts(output$drug_attributes)
  return(output)
}

get_genes <- function(terms, api_url = NULL) {
  api_url <- if (!is.null(api_url)) api_url else api_endpoint_url
  query <- readr::read_file("queries/get_genes.graphql")
  response <- httr::POST(
    api_url,
    body = list(query = query, variables = list(names = terms)),
    encode = "json"
  )
  result <- httr::content(response)$data

  output <- list(
    gene_name = list(),
    gene_concept_id = list(),
    gene_aliases = list(),
    gene_attributes = list()
  )

  for (match in result$genes$nodes) {
    output$gene_name <- append(
      output$gene_name, match$name
    )
    output$gene_concept_id <- append(
      output$gene_concept_id, match$conceptId
    )
    output$gene_aliases <- append(
      output$gene_aliases, list(lapply(match$geneAliases, function(a) a$alias))
    )
    output$gene_attributes <- append(
      output$gene_attributes, list(group_attributes(match$geneAttributes))
    )
    View(output$gene_attributes)
  }
  output$gene_attributes <- backfill_dicts(output$gene_attributes)
  return(output)
}

get_interactions <- function(
    terms,
    search = "genes",
    immunotherapy = NULL,
    antineoplastic = NULL,
    source = NULL,
    pmid = NULL,
    interaction_type = NULL,
    approved = NULL,
    api_url = NULL) {
  params <- list(names = terms)
  if (!is.null(immunotherapy)) {
    params$immunotherapy <- immunotherapy
  }
  if (!is.null(antineoplastic)) {
    params$antiNeoplastic <- antineoplastic
  }
  if (!is.null(source)) {
    params$sourceDbName <- source
  }
  if (!is.null(pmid)) {
    params$pmid <- pmid
  }
  if (!is.null(interaction_type)) {
    params$interactionType <- interaction_type
  }
  if (!is.null(approved)) {
    params$approved <- approved
  }

  api_url <- if (!is.null(api_url)) api_url else api_endpoint_url

  if (search == "genes") {
    query <- readr::read_file("queries/get_interactions_by_gene.graphql")
    response <- httr::POST(
      api_url,
      body = list(query = query, variables = list(names = terms)),
      encode = "json"
    )
    results <- httr::content(response)$data$genes$nodes
  } else if (search == "drugs") {
    query <- readr::read_file("queries/get_interactions_by_drug.graphql")
    response <- httr::POST(
      api_url,
      body = list(query = query, variables = list(names = terms)),
      encode = "json"
    )
    results <- httr::content(response)$data$drugs$nodes
  } else {
    msg <- "Search type must be specified using: search='drugs' or search='genes'" # nolint: line_length_linter.
    stop(msg)
  }

  output <- list(
    gene_name = list(),
    gene_concept_id = list(),
    gene_long_name = list(),
    drug_name = list(),
    drug_concept_id = list(),
    drug_approved = list(),
    interaction_score = list(),
    interaction_attributes = list(),
    interaction_sources = list(),
    interaction_pmids = list()
  )

  for (result in results) {
    for (interaction in result$interactions) {
      output$gene_name <- append(
        output$gene_name, match$name
      )
      output$gene_concept_id <- append(
        output$gene_concept_id, match$conceptId
      )
      output$gene_long_name <- list(append(
        output$gene_long_name,
        lapply(match$drugAliases, function(a) a$alias)
      ))
      output$drug_attributes <- list(append(
        output$drug_attributes, group_attributes(match$drugAttributes)
      ))
      output$drug_is_antineoplastic <- append(
        output$drug_is_antineoplastic, match$antiNeoplastic
      )
      output$drug_is_immunotherapy <- append(
        output$drug_is_immunotherapy, match$immunotherapy
      )
      output$drug_is_approved <- append(
        output$drug_is_approved, match$approved
      )
      output$drug_approval_ratings <- list(append(
        output$drug_approval_ratings,
        lapply(match$drugApprovalRatings, function(r) {
          list(rating = r$rating, source = r$source$sourceDbName)
        })
      ))
      output$drug_fda_applications <- list(append(
        output$drug_fda_applications,
        lapply(match$drugApplications, function(app) app$appNo)
      ))
    }
  }
  output$drug_attributes <- backfill_dicts(output$drug_attributes)
  return(output)
}

#' Get all gene names present in DGIdb
#'
#' @returns full list of all genes
#' @export
#'
#' @examples
#' get_gene_list()
get_gene_list <- function() {
  query <- "{\ngenes {\nnodes {\nname\n}\n}\n}"
  r <- httr::POST(api_endpoint_url, body = list(query = query), encode = "json")
  gene_list <- list()
  raw_nodes <- httr::content(r)$data$genes$nodes
  for (i in seq_along(raw_nodes)) {
    gene_name <- raw_nodes[[i]]$name
    gene_list <- append(gene_list, gene_name)
  }
  gene_list <- sort(unlist(gene_list))
  return(gene_list)
}

#' Perform a look up for ANDA/NDA applications for drug or drugs of interest
#'
#' @param terms drugs of interest
#' @param use_processing placeholder
#'
#' @returns all ANDA/NDA applications for drugs of interest
#' @export
#'
#' @examples
#' terms <- "DOVITINIB"
#' get_drug_applications(terms)
get_drug_applications <- function(terms, use_processing = TRUE) {
  terms <- paste0("[\"", paste(toupper(terms), collapse = "\",\""), "\"]")
  query <- paste0("{\ndrugs(names: ", terms, ") {\nnodes{\nname \ndrugApplications {\nappNo\n}\n}\n}\n}\n") # nolint: line_length_linter.

  r <- httr::POST(api_endpoint_url, body = list(query = query), encode = "json")
  data <- httr::content(r)

  if (use_processing == TRUE) {
    data <- process_drug_applications(data)
    data <- openfda_data(data)
  }
  return(data)
}

#' Process Drug Applications
#'
#' @param data drug applications to process
#'
#' @returns processed drug applications
#' @export
#'
process_drug_applications <- function(data) {
  0
  drug_list <- c()
  application_list <- c()

  for (node in data$data$drugs$nodes) {
    current_drug <- node$name

    for (application in node$drugApplications) {
      drug_list <- c(drug_list, current_drug)
      application <- toupper(gsub(
        ":",
        "",
        strsplit(application$appNo, "\\.")[[1]][2]
      ))
      application_list <- c(application_list, application)
    }
  }

  dataframe <- data.frame(drug = drug_list, application = application_list)
  return(dataframe)
}

#' OpenFDA Data
#'
#' @param dataframe placeholder
#'
#' @returns placeholder
#' @export
#'
openfda_data <- function(dataframe) {
  openfda_base_url <- "https://api.fda.gov/drug/drugsfda.json?search=openfda.application_number:" # nolint: line_length_linter.
  terms <- as.list(dataframe$application)
  descriptions <- vector("list", length(terms))

  for (i in seq_along(terms)) {
    term <- terms[[i]]
    r <- httr::GET(
      paste0(
        openfda_base_url,
        "\"", term, "\""
      ),
      httr::add_headers("User-Agent" = "Custom")
    )

    tryCatch(
      {
        results <- httr::content(r, "parsed")$results
        if (length(results) > 0 && !is.null(results[[1]]$products)) {
          products <- results[[1]]$products
          f <- vector("character", length(products))

          for (j in seq_along(products)) {
            product <- products[[j]]
            brand_name <- product$brand_name
            marketing_status <- product$marketing_status
            dosage_form <- product$dosage_form
            active_ingredient <- product$active_ingredients[[1]]$name # nolint: object_usage_linter, line_length_linter.
            dosage_strength <- product$active_ingredients[[1]]$strength
            f[j] <- paste0(
              brand_name,
              ": ",
              dosage_strength,
              " ",
              marketing_status,
              " ",
              dosage_form
            )
          }

          descriptions[[i]] <- paste(f, collapse = " | ")
        } else {
          descriptions[[i]] <- "none"
        }
      },
      error = function(e) {
        descriptions[[i]] <- "none"
      }
    )
  }

  dataframe$description <- descriptions
  return(dataframe)
}
