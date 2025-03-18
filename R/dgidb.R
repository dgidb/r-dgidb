api_endpoint_url <- Sys.getenv(
  "DGIDB_API_URL",
  unset = "https://dgidb.org/api/graphql"
)

#' Group Attributes
#'
#' Groups attributes by name and stores their values in a list
#'
#' @param row
#' A list of dictionaries of attributes
#' @return
#' A dictionary which maps each attribute to a list of associated values
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
  grouped_dict
}

#' Backfill Dicts
#'
#' Fills missing values in a list of dictionaries
#'
#' @param col
#' A list of dictionaries, where each dictionary might have missing keys
#' @return
#' A list of dictionaries, where each dictionary contains all possible keys
backfill_dicts <- function(col) {
  keys <- unique(unlist(lapply(col, names)))
  result <- lapply(col, function(cell) sapply(keys, function(key) cell[[key]]))
  result
}

#' Get Drugs
#'
#' Perform a record look up in DGIdb for a drug of interest
#'
#' @param terms
#' drugs for record lookup
#' @param immunotherapy
#' filter option for results that are only immunotherapy, Default: NULL
#' @param antineoplastic
#' filter option for results that see antineoplastic use, Default: NULL
#' @param api_url
#' API endpoint for GraphQL request, Default: NULL
#' @return
#' drug data
#'
#' @examples
#' get_drugs(c("Imatinib"))
#' @export
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
  query_file_path <- system.file(
    "queries/get_drugs.graphql",
    package = "rdgidb"
  )
  query <- readr::read_file(query_file_path)
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

#' Get Genes
#'
#' Perform a record look up in DGIdb for genes of interest
#'
#' @param terms genes for record lookup
#' @param api_url API endpoint for GraphQL request, Default: NULL
#' @return gene data
#'
#' @examples
#' get_genes(c("BRAF", "PDGFRA"))
#' @export
get_genes <- function(terms, api_url = NULL) {
  api_url <- if (!is.null(api_url)) api_url else api_endpoint_url
  query_file_path <- system.file(
    "queries/get_genes.graphql",
    package = "rdgidb"
  )
  query <- readr::read_file(query_file_path)
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
  }
  output$gene_attributes <- backfill_dicts(output$gene_attributes)
  return(output)
}

#' Get Interactions
#'
#' Perform an interaction look up for drugs or genes of interest
#'
#' @param terms
#' drugs or genes for interaction look up
#' @param search
#' interaction search type. valid types are "drugs" or "genes", Default: 'genes'
#' @param immunotherapy
#' filter option for results that are used in immunotherapy, Default: NULL
#' @param antineoplastic
#' filter option for results that are part of antineoplastic regimens,
#' Default: NULL
#' @param source
#' filter option for specific database of interest, Default: NULL
#' @param pmid
#' filter option for specific PMID, Default: NULL
#' @param interaction_type
#' filter option for specific interaction types, Default: NULL
#' @param approved
#' filter option for approved interactions, Default: NULL
#' @param api_url
#' API endpoint for GraphQL request, Default: NULL
#' @return
#' interaction results for terms
#'
#' @examples
#' get_interactions(c("BRAF", "PDGFRA"))
#' @export
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
    query_file_path <- system.file(
      "queries/get_interactions_by_gene.graphql",
      package = "rdgidb"
    )
    query <- readr::read_file(query_file_path)
    response <- httr::POST(
      api_url,
      body = list(query = query, variables = list(names = terms)),
      encode = "json"
    )
    results <- httr::content(response)$data$genes$nodes
  } else if (search == "drugs") {
    query_file_path <- system.file(
      "queries/get_interactions_by_drug.graphql",
      package = "rdgidb"
    )
    query <- readr::read_file(query_file_path)
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
        output$gene_name, interaction$gene$name
      )
      output$gene_concept_id <- append(
        output$gene_concept_id, interaction$gene$conceptId
      )
      output$gene_long_name <- append(
        output$gene_long_name, interaction$gene$longName
      )
      output$drug_name <- append(
        output$drug_name, interaction$drug$name
      )
      output$drug_concept_id <- append(
        output$drug_concept_id, interaction$drug$conceptId
      )
      output$drug_approved <- append(
        output$drug_approved, interaction$drug$approved
      )
      output$interaction_score <- append(
        output$interaction_score, interaction$interactionScore
      )
      output$interaction_attributes <- append(
        output$interaction_attributes,
        list(group_attributes(interaction$interactionAttributes))
      )
      pubs <- list()
      sources <- list()
      for (claim in interaction$interactionClaims) {
        sources <- append(sources, claim$source$sourceDbName)
        pubs <- append(pubs, lapply(claim$publications, function(p) p$pmid))
      }
      output$interaction_pmids <- append(
        output$interaction_pmids, list(pubs)
      )
      output$interaction_sources <- append(
        output$interaction_sources, list(sources)
      )
    }
  }
  output$interaction_attributes <- backfill_dicts(output$interaction_attributes)
  return(output)
}

#' Get Categories
#'
#' Perform a category annotation lookup for genes of interest
#'
#' @param terms Genes of interest for annotations
#' @param api_url API endpoint for GraphQL request, Default: NULL
#' @return category annotation results for genes
#'
#' @examples
#' get_categories(c("BRAF", "PDGFRA"))
#' @export
get_categories <- function(terms, api_url = NULL) {
  api_url <- if (!is.null(api_url)) api_url else api_endpoint_url
  query_file_path <- system.file(
    "queries/get_gene_categories.graphql",
    package = "rdgidb"
  )
  query <- readr::read_file(query_file_path)
  response <- httr::POST(
    api_url,
    body = list(query = query, variables = list(names = terms)),
    encode = "json"
  )
  results <- httr::content(response)$data

  output <- list(
    gene_name = list(),
    gene_concept_id = list(),
    gene_full_name = list(),
    gene_category = list(),
    gene_category_sources = list()
  )

  for (result in results$genes$nodes) {
    name <- result$name
    long_name <- result$longName
    concept_id <- result$conceptId
    for (cat in result$geneCategoriesWithSources) {
      output$gene_name <- append(
        output$gene_name, name
      )
      output$gene_concept_id <- append(
        output$gene_concept_id, concept_id
      )
      output$gene_full_name <- append(
        output$gene_full_name, long_name
      )
      output$gene_category <- append(
        output$gene_category, cat$name
      )
      output$gene_category_sources <- append(
        output$gene_category_sources, cat$sourceNames
      )
    }
  }
  return(output)
}

source_type <- list(
  DRUG = "drug",
  GENE = "gene",
  INTERACTION = "interaction",
  POTENTIALLY_DRUGGABLE = "potentially_druggable"
)

#' Get Sources
#'
#' Perform a source lookup for relevant aggregate sources
#'
#' @param source_type
#' type of source to look up. Fetches all sources otherwise, Default: NULL
#' @param api_url
#' API endpoint for GraphQL request, Default: NULL
#' @return
#' all sources of relevant type in a json object
#'
#' @examples
#' source_type <- list(
#'   DRUG = "drug",
#'   GENE = "gene",
#'   INTERACTION = "interaction",
#'   POTENTIALLY_DRUGGABLE = "potentially_druggable"
#' )
#' sources <- get_sources(source_type$POTENTIALLY_DRUGGABLE)
#' @export
get_sources <- function(source_type = NULL, api_url = NULL) {
  source_param <- if (!is.null(source_type)) {
    toupper(source_type)
  } else {
    NULL
  }
  api_url <- if (!is.null(api_url)) api_url else api_endpoint_url
  query_file_path <- system.file(
    "queries/get_sources.graphql",
    package = "rdgidb"
  )
  query <- readr::read_file(query_file_path)
  params <- if (is.null(source_type)) {
    list()
  } else {
    list(sourceType = source_param)
  }
  response <- httr::POST(
    api_url,
    body = list(query = query, variables = params),
    encode = "json"
  )
  results <- httr::content(response)$data

  output <- list(
    source_name = list(),
    source_short_name = list(),
    source_version = list(),
    source_drug_claims = list(),
    source_gene_claims = list(),
    source_interaction_claims = list(),
    source_license = list(),
    source_license_url = list()
  )

  for (result in results$sources$nodes) {
    output$source_name <- append(
      output$source_name, result$fullName
    )
    output$source_short_name <- append(
      output$source_short_name, result$sourceDbName
    )
    output$source_version <- append(
      output$source_version, result$sourceDbVersion
    )
    output$source_drug_claims <- append(
      output$source_drug_claims, result$drugClaimsCount
    )
    output$source_gene_claims <- append(
      output$source_gene_claims, result$geneClaimsCount
    )
    output$source_interaction_claims <- append(
      output$source_interaction_claims, result$interactionClaimsCount
    )
    output$source_license <- append(
      output$source_license, result$license
    )
    output$source_license_url <- append(
      output$source_license_url, result$licenseLink
    )
  }
  return(output)
}

#' Get All Genes
#'
#' Get all gene names present in DGIdb
#'
#' @param api_url API endpoint for GraphQL request, Default: NULL
#' @return a full list of genes present in dgidb
#'
#' @examples
#' get_all_genes()
#' @export
get_all_genes <- function(api_url = NULL) {
  api_url <- if (!is.null(api_url)) api_url else api_endpoint_url
  query_file_path <- system.file(
    "queries/get_all_genes.graphql",
    package = "rdgidb"
  )
  query <- readr::read_file(query_file_path)
  response <- httr::POST(
    api_url,
    body = list(query = query),
    encode = "json"
  )
  results <- httr::content(response)$data

  genes <- list(
    gene_name = list(),
    gene_concept_id = list()
  )

  for (result in results$genes$nodes) {
    genes$gene_name <- append(
      genes$gene_name, result$name
    )
    genes$gene_concept_id <- append(
      genes$gene_concept_id, result$conceptId
    )
  }
  return(genes)
}

#' Get All Drugs
#'
#' Get all drug names present in DGIdb
#'
#' @param api_url API endpoint for GraphQL request, Default: NULL
#' @return a full list of drugs present in dgidb
#'
#' @examples
#' get_all_drugs()
#' @export
get_all_drugs <- function(api_url = NULL) {
  api_url <- if (!is.null(api_url)) api_url else api_endpoint_url
  query_file_path <- system.file(
    "queries/get_all_drugs.graphql",
    package = "rdgidb"
  )
  query <- readr::read_file(query_file_path)
  response <- httr::POST(
    api_url,
    body = list(query = query),
    encode = "json"
  )
  results <- httr::content(response)$data

  drugs <- list(
    drug_name = list(),
    drug_concept_id = list()
  )

  for (result in results$drugs$nodes) {
    drugs$drug_name <- append(
      drugs$drug_name, result$name
    )
    drugs$drug_concept_id <- append(
      drugs$drug_concept_id, result$conceptId
    )
  }
  return(drugs)
}
