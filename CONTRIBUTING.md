# Contributing

Common development commands are:

```{r development, eval = FALSE}
devtools::load_all()
devtools::document()
devtools::test()
devtools::check()
```

The package uses [`testthat`](https://testthat.r-lib.org/) for testing and
[`httptest2`](https://enpiar.com/httptest2/) to mock external HTTP requests.

Code can be formatted using the Bioconductor-oriented style supplied by
[`biocthis`](https://github.com/lcolladotor/biocthis):

```{r style, eval = FALSE}
styler::style_pkg(
    transformers = biocthis::bioc_style()
)
```

## GraphQL queries

GraphQL queries used to access the DGIdb API are stored in
`inst/queries/`. Files under `inst/` are included in the installed
package and are located at runtime using `system.file()`.

- `get_all_drugs.graphql` — Retrieve all drugs.
- `get_all_genes.graphql` — Retrieve all genes.
- `get_drugs.graphql` — Retrieve specified drugs.
- `get_gene_categories.graphql` — Retrieve gene categories.
- `get_genes.graphql` — Retrieve specified genes.
- `get_interactions_by_drug.graphql` — Retrieve interactions for specified drugs.
- `get_interactions_by_gene.graphql` — Retrieve interactions for specified genes.
- `get_sources.graphql` — Retrieve DGIdb sources.

Queries should be kept in separate `.graphql` files rather than
embedded in R source.

When adding or modifying an API operation, update the corresponding
query in `inst/queries/` and add or update its tests.
