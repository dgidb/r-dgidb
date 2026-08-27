# Migrating from the legacy rDGIdb package

## Overview

The original `rDGIdb` Bioconductor package has been deprecated and removed from active Bioconductor releases. It was developed as a community-maintained wrapper around an earlier version of the DGIdb API and is no longer compatible with the current DGIdb platform.

A new R client is now maintained by the DGIdb development team:

* Legacy package: https://bioconductor.posit.co/packages/3.19/bioc/html/rDGIdb.html
* New package: https://github.com/dgidb/r-dgidb

The new package connects directly to the current DGIdb GraphQL API and provides dedicated functions for retrieving interactions, drugs, genes, gene categories, and source metadata.

The new package is a replacement for the legacy client, but it is not a drop-in replacement. Function names, arguments, return types, and installation procedures have changed.

## Installation

The legacy package was installed through Bioconductor:

```r
BiocManager::install("rDGIdb")
library(rDGIdb)
```

The new package can currently be installed from the DGIdb GitHub repository:

```r
install.packages("remotes")
remotes::install_github("dgidb/r-dgidb")

library(rdgidb)
```

If the legacy package remains installed, it can be removed before installing the new client:

```r
remove.packages("rDGIdb")
```

Because R package names are case-sensitive, note that the current package name is `rdgidb`, rather than the legacy `rDGIdb`.

## Major changes

| Legacy `rDGIdb`              | New `rdgidb`                                 | Migration notes                                                                                                        |
| ---------------------------- | -------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `queryDGIdb()`               | `get_interactions()`                         | Interaction searches now use a dedicated function and may be performed by gene or drug.                                |
| `sourceDatabases()`          | `get_sources()`                              | Returns current DGIdb source metadata rather than only a vector of valid filters.                                      |
| `geneCategories()`           | `get_categories()`                           | Retrieves category annotations for specified genes. It is not a direct replacement for listing category filter values. |
| `interactionTypes()`         | No direct equivalent                         | Supply a known interaction type through `get_interactions(interaction_type = ...)`.                                    |
| `resourceVersions()`         | `get_sources()`                              | Source names, versions, licenses, and claim counts are returned together.                                              |
| `resultSummary()`            | Process the result from `get_interactions()` | The new client returns a named list instead of an S4 result object.                                                    |
| `detailedResults()`          | `get_interactions()`                         | Interaction-level information is returned directly.                                                                    |
| `byGene()`                   | Group the returned interaction data in R     | Use tools such as `data.frame()`, `tibble::as_tibble()`, or `dplyr`.                                                   |
| `searchTermSummary()`        | `get_genes()` or `get_interactions()`        | Gene records and interaction results are retrieved separately.                                                         |
| `plotInteractionsBySource()` | No direct equivalent                         | Create plots from the returned source information using base R or a plotting package.                                  |
| No equivalent                | `get_drugs()`                                | Retrieves normalized drug records and supporting metadata.                                                             |
| No equivalent                | `get_genes()`                                | Retrieves normalized gene records, aliases, and attributes.                                                            |
| No equivalent                | `get_all_drugs()`                            | Retrieves the names and concept identifiers of all DGIdb drugs.                                                        |
| No equivalent                | `get_all_genes()`                            | Retrieves the names and concept identifiers of all DGIdb genes.                                                        |

## Migrating interaction queries

### Basic gene query

Legacy code:

```r
library(rDGIdb)

result <- queryDGIdb(
  genes = c("BRAF", "EGFR")
)

interactions <- detailedResults(result)
```

New code:

```r
library(rdgidb)

interactions <- get_interactions(
  terms = c("BRAF", "EGFR"),
  search = "genes"
)
```

The `search` argument determines whether the supplied terms are interpreted as genes or drugs. Supported values are `"genes"` and `"drugs"`.

### Querying by drug

The new package also supports interaction searches starting from drug names:

```r
interactions <- get_interactions(
  terms = c("IMATINIB", "OSIMERTINIB"),
  search = "drugs"
)
```

### Filtering by source

Legacy code:

```r
result <- queryDGIdb(
  genes = "BRAF",
  sourceDatabases = "CIViC"
)
```

New code:

```r
interactions <- get_interactions(
  terms = "BRAF",
  search = "genes",
  source = "CIViC"
)
```

Current source names and versions can be inspected with:

```r
sources <- get_sources()
```

### Filtering by interaction type

Legacy code:

```r
result <- queryDGIdb(
  genes = "BRAF",
  interactionTypes = "inhibitor"
)
```

New code:

```r
interactions <- get_interactions(
  terms = "BRAF",
  search = "genes",
  interaction_type = "inhibitor"
)
```

### Additional filters

The new client supports several filters that were not available through the legacy `queryDGIdb()` interface:

```r
interactions <- get_interactions(
  terms = "EGFR",
  search = "genes",
  approved = TRUE,
  antineoplastic = TRUE,
  source = "CIViC",
  pmid = 12345678,
  interaction_type = "inhibitor"
)
```

Available filters include:

* `approved`
* `antineoplastic`
* `immunotherapy`
* `source`
* `pmid`
* `interaction_type`

Filters may be combined. Results must satisfy the filters supported by the corresponding DGIdb GraphQL query.

## Working with returned results

The legacy package returned an S4 `rDGIdbResult` object. Information was extracted using accessor functions such as:

```r
resultSummary(result)
detailedResults(result)
byGene(result)
searchTermSummary(result)
```

The new package returns a named list whose elements represent result columns. For interaction queries, these currently include:

* `gene_name`
* `gene_concept_id`
* `gene_long_name`
* `drug_name`
* `drug_concept_id`
* `drug_approved`
* `interaction_score`
* `interaction_attributes`
* `interaction_pmids`
* `interaction_sources`

The result can be inspected directly:

```r
interactions <- get_interactions("BRAF")

names(interactions)
str(interactions)
```

It can also be converted into a base R data frame:

```r
interaction_df <- data.frame(
  gene_name = interactions$gene_name,
  gene_concept_id = interactions$gene_concept_id,
  drug_name = interactions$drug_name,
  drug_concept_id = interactions$drug_concept_id,
  drug_approved = interactions$drug_approved,
  interaction_score = interactions$interaction_score
)
```

Some fields, including publications, sources, aliases, and attributes, may contain multiple values per record and are therefore returned as list-columns. These fields should generally remain as lists when constructing a tibble:

```r
interaction_tbl <- tibble::tibble(
  gene_name = interactions$gene_name,
  gene_concept_id = interactions$gene_concept_id,
  drug_name = interactions$drug_name,
  drug_concept_id = interactions$drug_concept_id,
  drug_approved = interactions$drug_approved,
  interaction_score = interactions$interaction_score,
  interaction_pmids = interactions$interaction_pmids,
  interaction_sources = interactions$interaction_sources,
  interaction_attributes = interactions$interaction_attributes
)
```

List-columns can be expanded with packages such as `tidyr` when one row per source or publication is required:

```r
interaction_sources <- interaction_tbl |>
  tidyr::unnest_longer(interaction_sources)
```

## Replacing legacy summaries

### Summarize interactions by gene

The legacy `byGene()` accessor can be approximated using `dplyr`:

```r
interaction_tbl |>
  dplyr::group_by(gene_name) |>
  dplyr::summarise(
    interaction_count = dplyr::n(),
    drug_count = dplyr::n_distinct(drug_concept_id),
    .groups = "drop"
  )
```

### Summarize interactions by source

To replace `resultSummary()` or prepare data similar to the legacy source summary:

```r
source_summary <- interaction_tbl |>
  tidyr::unnest_longer(interaction_sources) |>
  dplyr::count(interaction_sources, name = "interaction_count") |>
  dplyr::arrange(dplyr::desc(interaction_count))
```

### Plot interactions by source

The legacy package provided `plotInteractionsBySource()`. Equivalent plots can be created from the expanded results:

```r
barplot(
  height = source_summary$interaction_count,
  names.arg = source_summary$interaction_sources,
  las = 2,
  ylab = "Number of interactions"
)
```

Alternatively:

```r
ggplot2::ggplot(
  source_summary,
  ggplot2::aes(
    x = stats::reorder(
      interaction_sources,
      interaction_count
    ),
    y = interaction_count
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = "DGIdb source",
    y = "Number of interactions"
  )
```

## Retrieving gene records

The new `get_genes()` function retrieves normalized DGIdb gene records independently of interaction searches:

```r
genes <- get_genes(
  c("BRAF", "EGFR", "PDGFRA")
)
```

Results include:

* Normalized gene name
* DGIdb gene concept identifier
* Gene aliases
* Gene attributes

Because aliases and attributes may contain multiple values, they are returned as nested lists.

## Retrieving drug records

The new `get_drugs()` function retrieves normalized drug records:

```r
drugs <- get_drugs(
  c("IMATINIB", "OSIMERTINIB")
)
```

Optional drug-level filters are also available:

```r
drugs <- get_drugs(
  terms = c("IMATINIB", "OSIMERTINIB"),
  antineoplastic = TRUE
)
```

Returned information includes:

* Normalized drug name
* DGIdb drug concept identifier
* Drug aliases and attributes
* Approval status
* Antineoplastic and immunotherapy annotations
* Approval ratings
* FDA application numbers

## Retrieving gene categories

Legacy `queryDGIdb()` calls could use `geneCategories` as a query filter. The new package separates category retrieval from interaction retrieval:

```r
categories <- get_categories(
  c("BRAF", "EGFR")
)
```

The result includes gene names, concept identifiers, category annotations, and the sources supporting each category.

At present, `get_categories()` retrieves annotations for genes; it does not apply a gene-category filter to `get_interactions()`. If category-filtered interactions are required, retrieve both datasets and join or filter them locally.

## Retrieving source versions

Replace the legacy `resourceVersions()` function with:

```r
sources <- get_sources()
```

The result includes:

* Full and abbreviated source names
* Source versions
* Drug, gene, and interaction claim counts
* License names
* License URLs

A source type can optionally be supplied:

```r
interaction_sources <- get_sources("interaction")
drug_sources <- get_sources("drug")
gene_sources <- get_sources("gene")
category_sources <- get_sources("potentially_druggable")
```

## Retrieving all DGIdb genes or drugs

The new client provides lightweight functions for obtaining the complete set of normalized records:

```r
all_genes <- get_all_genes()
all_drugs <- get_all_drugs()
```

Each result contains the normalized name and DGIdb concept identifier.

## Using another DGIdb instance

All public query functions accept an optional `api_url` argument:

```r
interactions <- get_interactions(
  terms = "BRAF",
  api_url = "https://example.org/api/graphql"
)
```

A default endpoint can also be set through the `DGIDB_API_URL` environment variable before loading or using the package:

```r
Sys.setenv(
  DGIDB_API_URL = "https://example.org/api/graphql"
)

library(rdgidb)
```

If no custom endpoint is supplied, the client uses:

```text
https://dgidb.org/api/graphql
```

## Migration checklist

When updating an existing analysis:

1. Replace the legacy `rDGIdb` installation with the DGIdb-maintained `rdgidb` package.
2. Change `library(rDGIdb)` to `library(rdgidb)`.
3. Replace `queryDGIdb()` with `get_interactions()`.
4. Specify whether query terms are genes or drugs using `search`.
5. Replace S4 accessor functions with direct access to the returned named list.
6. Preserve nested source, publication, alias, and attribute fields as list-columns where appropriate.
7. Replace `resourceVersions()` with `get_sources()`.
8. Replace legacy gene-category filtering with `get_categories()` followed by local filtering or joining.
9. Recreate legacy summary tables and plots using standard R data-manipulation and plotting tools.
10. Review saved scripts for assumptions about legacy column names and result-object classes.
11. Validate migrated workflows against a small set of known genes or drugs before running large analyses.

## Troubleshooting

### `queryDGIdb()` cannot be found

`queryDGIdb()` belongs to the legacy package. Use:

```r
get_interactions(
  terms = "BRAF",
  search = "genes"
)
```

### `detailedResults()` or `resultSummary()` cannot be found

The new client does not return an `rDGIdbResult` S4 object. Access fields directly from the result:

```r
results <- get_interactions("BRAF")

results$gene_name
results$drug_name
results$interaction_score
```

### `library(rDGIdb)` fails after installation

The new package’s current package name is lowercase:

```r
library(rdgidb)
```

### Results contain nested lists

Fields such as aliases, attributes, PMIDs, and sources can have multiple values per record. Store them as list-columns or expand them with `tidyr::unnest_longer()`.

### A legacy gene-category query cannot be reproduced directly

Retrieve category annotations using `get_categories()`, identify genes assigned to the desired category, and then submit those genes to `get_interactions()`.

## Getting help

Questions, bug reports, and feature requests for the new package should be submitted through the DGIdb-maintained GitHub repository:

https://github.com/dgidb/r-dgidb/issues

When reporting a problem, include the package version, R version, a reproducible example, and the complete error message.
