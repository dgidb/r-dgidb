# r-dgidb
In-progress R wrapper for DGIdb

# Development
Use the renv package to install packages recorded in the renv lockfile 
 ```r
library(renv)

# Restore packages
renv::restore()
```

Use the devtools package to develop r-dgidb
```r
library(devtools)

# Load package
devtools::load_all()

# Build docs
devtools::document()

# Run tests
devtools::test()

# Build & Install vignettes
devtools::install(build_vignettes = TRUE)

# Check package
devtools::check()
```
Use the styler package to format r-dgidb
```r
library(styler)

# Style package
styler::style_pkg()
```