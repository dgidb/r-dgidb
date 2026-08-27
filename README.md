
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rDGIdb

<!-- badges: start -->

[![GitHub
issues](https://img.shields.io/github/issues/dgidb/r-dgidb)](https://github.com/dgidb/r-dgidb/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/dgidb/r-dgidb)](https://github.com/dgidb/r-dgidb/pulls)
<!-- badges: end -->

`rDGIdb` provides an R interface to the [Drug-Gene Interaction Database
(DGIdb)](https://dgidb.org/) GraphQL API. It supports queries for drugs,
genes, drug-gene interactions, gene categories, DGIdb data sources, and
Drugs@FDA application data.

## Installation

`rDGIdb` is currently under development as version `0.99.0`.

The development version can be installed from GitHub with:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("dgidb/r-dgidb")
```

Alternatively, after cloning the repository, install the local checkout
with:

``` r
devtools::install(build_vignettes = TRUE)
```

## Quick start

Load the package:

``` r
library(rDGIdb)
```

Look up genes:

``` r
genes <- getGenes(c("BRAF", "PDGFRA"))
genes
```

Retrieve drug-gene interactions for genes of interest:

``` r
interactions <- getInteractions(c("BRAF", "PDGFRA"))
interactions
```

Look up a drug:

``` r
drugs <- getDrugs("Imatinib")
drugs
```

Interactions can also be queried starting from a drug:

``` r
getInteractions(
    "Imatinib",
    search = "drugs",
    approved = TRUE
)
```

Other package functions provide access to DGIdb gene categories, source
databases, complete gene and drug lists, and Drugs@FDA application data.

``` r
getCategories("BRAF")
getSources(sourceTypes$INTERACTION)
getDrugApplications("Imatinib")
getAllGenes()
getAllDrugs()
```

For a complete walkthrough, see:

``` r
vignette("rDGIdb")
```

## Getting help

If you encounter a problem with `rDGIdb`, first check the package
documentation and vignette.

``` r
help(package = "rDGIdb")
vignette("rDGIdb")
```

Bug reports and feature requests can be submitted through the [GitHub
issue tracker](https://github.com/dgidb/r-dgidb/issues).

## Citation

To obtain the preferred citation for `rDGIdb`, run:

``` r
citation("rDGIdb")
```

Please also cite the Drug-Gene Interaction Database (DGIdb) when
appropriate.

## Code of Conduct

Please note that the `rDGIdb` project follows the [Bioconductor Code of
Conduct](https://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development

Common development commands are:

``` r
devtools::load_all()
devtools::document()
devtools::test()
devtools::check()
```

The package uses [`testthat`](https://testthat.r-lib.org/) for testing
and [`httptest2`](https://enpiar.com/httptest2/) to mock external HTTP
requests.

Code can be formatted using the Bioconductor-oriented style supplied by
[`biocthis`](https://github.com/lcolladotor/biocthis):

``` r
styler::style_pkg(
    transformers = biocthis::bioc_style()
)
```
