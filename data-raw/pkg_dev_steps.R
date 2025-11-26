# 1) Create constants
# source("./data-raw/create_external_constants.R")
# source("./data-raw/create_internal_constants.R")

# 2) Ensure all external constants are documented in ./R/data.R and and functions are documented in their individual files in the ./R directory.

# 3) Write documentation for functions and objects
devtools::document()

# 4) Test package
# devtools::test()

# 5) Compute code coverage
# covr::codecov()

# 6) check the package
devtools::check(document = FALSE)

# It is recommended that you do not proceed if all checks do not pass!
# i.e. no errors or warnings, only notes.

# 7) build package
devtools::build(vignettes = FALSE)

# 8) install package
devtools::install(pkg = ".")

# 9) restart R
.rs.restartR()

# 10) load package
library(MNNPC)

# 11) build readme
devtools::build_readme()

# 12) build vignettes
# devtools::build_vignettes()

# 13) Build package manual
devtools::build_manual(pkg = ".", path = ".")

# 14) Build pkgdown website, this builds the site locally, 
#     which is important if building your site takes a long time or reads data outside of the repository.
# devtools::build_site()

# 15) Manually deploy website
pkgdown::deploy_to_branch()

# 16) Release!


