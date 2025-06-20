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

#' Post Query
#'
#' Sends a POST request to DGIdb GraphQL API with the specified paramters
#'
#' @param api_url
#' API endpoint for GraphQL request
#' @param query_file
#' path to GraphQL query file
#' @param variables
#' variables to be passed to GraphQL query
#' @return
#' data from GraphQL API response
post_query <- function(api_url, query_file, variables) {
  api_url <- if (!is.null(api_url)) api_url else api_endpoint_url
  query_file_path <- system.file(query_file, package = "rdgidb")
  query <- readr::read_file(query_file_path)
  response <- httr::POST(
    api_url,
    body = list(query = query, variables = variables),
    encode = "json"
  )
  httr::content(response)$data
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
  if (!is.null(immunotherapy)) params$immunotherapy <- immunotherapy
  if (!is.null(antineoplastic)) params$antineoplastic <- antineoplastic
  results <- post_query(api_url, "queries/get_drugs.graphql", params)

  nodes <- results$drugs$nodes
  output <- list(
    drug_name = vapply(nodes, function(x) x$name, character(1)),
    drug_concept_id = vapply(nodes, function(x) x$conceptId, character(1)),
    drug_aliases = lapply(nodes, function(x) vapply(x$drugAliases, function(a) a$alias, character(1))),
    drug_attributes = lapply(nodes, function(x) group_attributes(x$drugAttributes)),
    drug_is_antineoplastic = vapply(nodes, function(x) x$antiNeoplastic, logical(1)),
    drug_is_immunotherapy = vapply(nodes, function(x) x$immunotherapy, logical(1)),
    drug_is_approved = vapply(nodes, function(x) x$approved, logical(1)),
    drug_approval_ratings = lapply(nodes, function(x) {
      lapply(x$drugApprovalRatings, function(r) list(rating = r$rating, source = r$source$sourceDbName))
    }),
    drug_fda_applications = lapply(nodes, function(x) vapply(x$drugApplications, function(a) a$appNo, character(1)))
  )
  output$drug_attributes <- backfill_dicts(output$drug_attributes)
  output
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
  params <- list(names = terms)
  results <- post_query(api_url, "queries/get_genes.graphql", params)

  nodes <- results$genes$nodes
  output <- list(
    gene_name = vapply(nodes, function(x) x$name, character(1)),
    gene_concept_id = vapply(nodes, function(x) x$conceptId, character(1)),
    gene_aliases = lapply(nodes, function(x) vapply(x$geneAliases, function(a) a$alias, character(1))),
    gene_attributes = lapply(nodes, function(x) group_attributes(x$geneAttributes))
  )
  output$gene_attributes <- backfill_dicts(output$gene_attributes)
  output
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
  if (!is.null(immunotherapy)) params$immunotherapy <- immunotherapy
  if (!is.null(antineoplastic)) params$antiNeoplastic <- antineoplastic
  if (!is.null(source)) params$sourceDbName <- source
  if (!is.null(pmid)) params$pmid <- pmid
  if (!is.null(interaction_type)) params$interactionType <- interaction_type
  if (!is.null(approved)) params$approved <- approved

  if (search == "genes") {
    results <- post_query(
      api_url,
      "queries/get_interactions_by_gene.graphql",
      params
    )$genes$nodes
  } else if (search == "drugs") {
    results <- post_query(
      api_url,
      "queries/get_interactions_by_drug.graphql",
      params
    )$drugs$nodes
  } else {
    msg <- "Search type must be specified using: search='drugs' or search='genes'"
    stop(msg)
  }

  nodes <- unlist(lapply(results, function(x) x$interactions), recursive = FALSE)
  output <- list(
    gene_name = vapply(nodes, function(x) x$gene$name, character(1)),
    gene_concept_id = vapply(nodes, function(x) x$gene$conceptId, character(1)),
    gene_long_name = vapply(nodes, function(x) x$gene$longName, character(1)),
    drug_name = vapply(nodes, function(x) x$drug$name, character(1)),
    drug_concept_id = vapply(nodes, function(x) x$drug$conceptId, character(1)),
    drug_approved = vapply(nodes, function(x) x$drug$approved, logical(1)),
    interaction_score = vapply(nodes, function(x) x$interactionScore, numeric(1)),
    interaction_attributes = lapply(nodes, function(x) group_attributes(x$interactionAttributes)),
    interaction_pmids = lapply(nodes, function(x) {
      unlist(lapply(x$interactionClaims, function(y) {
        vapply(y$publications, function(z) z$pmid, numeric(1))
      }))
    }),
    interaction_sources = lapply(nodes, function(x) {
      vapply(x$interactionClaims, function(y) y$source$sourceDbName, character(1))
    })
  )
  output$interaction_attributes <- backfill_dicts(output$interaction_attributes)
  output
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
  params <- list(names = terms)
  results <- post_query(api_url, "queries/get_gene_categories.graphql", params)

  nodes <- results$genes$nodes
  output <- list(
    gene_name = unlist(lapply(nodes, function(x) {
      rep(x$name, length(x$geneCategoriesWithSources))
    })),
    gene_concept_id = unlist(lapply(nodes, function(x) {
      rep(x$conceptId, length(x$geneCategoriesWithSources))
    })),
    gene_full_name = unlist(lapply(nodes, function(x) {
      rep(x$longName, length(x$geneCategoriesWithSources))
    })),
    gene_category = unlist(lapply(nodes, function(x) {
      vapply(x$geneCategoriesWithSources, function(y) y$name, character(1))
    })),
    gene_category_sources = unlist(lapply(nodes, function(x) {
      lapply(x$geneCategoriesWithSources, function(y) y$sourceNames)
    }), recursive = FALSE)
  )
  output
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
  params <- if (!is.null(source_type)) list(sourceType = toupper(source_type)) else list()
  results <- post_query(api_url, "queries/get_sources.graphql", params)

  nodes <- results$sources$nodes
  output <- list(
    source_name = vapply(nodes, function(x) x$fullName, character(1)),
    source_short_name = vapply(nodes, function(x) x$sourceDbName, character(1)),
    source_version = vapply(nodes, function(x) x$sourceDbVersion, character(1)),
    source_drug_claims = vapply(nodes, function(x) x$drugClaimsCount, numeric(1)),
    source_gene_claims = vapply(nodes, function(x) x$geneClaimsCount, numeric(1)),
    source_interaction_claims = vapply(nodes, function(x) x$interactionClaimsCount, numeric(1)),
    source_license = vapply(nodes, function(x) x$license, character(1)),
    source_license_url = vapply(nodes, function(x) x$licenseLink, character(1))
  )
  output
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
  results <- post_query(api_url, "queries/get_all_genes.graphql", list())

  nodes <- results$genes$nodes
  output <- list(
    gene_name = vapply(nodes, function(x) x$name, character(1)),
    gene_concept_id = vapply(nodes, function(x) x$conceptId, character(1))
  )
  output
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
  results <- post_query(api_url, "queries/get_all_drugs.graphql", list())

  nodes <- results$drugs$nodes
  output <- list(
    drug_name = vapply(nodes, function(x) x$name, character(1)),
    drug_concept_id = vapply(nodes, function(x) x$conceptId, character(1))
  )
  output
}
