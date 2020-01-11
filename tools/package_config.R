
## ===== Project Setup ====

# Set name
options(usethis.full_name = "Adam H. James")

# Ignore folders on build
usethis::use_build_ignore("tools")

# Import badges for use in documentation
usethis::use_lifecycle()

# License
usethis::use_gpl3_license("costverse")

## ===== DESCRIPTION =====

# Description list
description <- list(Description = "Tools for Cost and Software Data Reporting (CSDR) data",
                    Title = "Tools for CSDR data",
                    `Authors@R` = list(person(given = "Daniel", family = "Germony",
                                              email = "daniel.r.germony.civ@mail.mil", role = c("cre","aut")),
                                       person(given = "Adam H.", family = "James",
                                              email = "ajames@technomics.net", role = c("aut")),
                                       person(given = "Justin", family = "Cooper",
                                              email = "jcooper@technomics.net", role = c("aut"))))

# Run this to set description. It will replace whatever is there! Keep in mind the version before doing this.
# usethis::use_description(description)

# Package dependencies
usethis::use_pipe()
#usethis::use_package("dplyr", min_version = "0.8.3")
#usethis::use_package("tidyr", min_version = "1.0.0")
#usethis::use_package("tibble", min_version = "2.0.0")
#usethis::use_package("purrr", min_version = "0.3.3")
#usethis::use_package("rlang", min_version = "0.4.2")
#usethis::use_package("pillar", min_version = "1.4.0")
#usethis::use_package("stringr", min_version = "1.4.0")
#usethis::use_package("crayon", min_version = "1.3.4")
#usethis::use_package("lifecycle")

## ===== README & NEWS =====

# Readme setup with RMarkdown
usethis::use_readme_rmd()
usethis::use_news_md()

usethis::use_lifecycle_badge("questioning")
#usethis::use_badge("Build: passing", "https://gitlab.technomics.net/ajames/ff2db", "https://img.shields.io/badge/build-passing-green.svg")
usethis::use_badge("License: GPLv3", "https://opensource.org/licenses/GPL-3.0", "https://img.shields.io/badge/License-GPLv3-blue.svg")


## ===== Developmental Tools =====

devtools::document()
devtools::check()

usethis::use_version()

devtools::load_all()

devtools::build(binary = TRUE)
devtools::build()

detach("package:wbstools", unload = TRUE)

## ===== Scratch Work =====

x <- c("1", "1.1", "1.1.1", "1.10", "1.A", "1.1.3", "1.1.10", "1.1.28", "1.3.4")

x <- c("1", "1.1", "1.1.1", "1.10", "1.1.3", "1.1.10", "1.1.28", "1.3.4")
y <- as_wbs(x)

as_wbs(x, .force = TRUE)

fun <- function() {
  cli_process_start("Calculating")
  if (interactive()) Sys.sleep(1)
  if (runif(1) < 0.5) stop("Failed")
  cli_process_done()
}

tryCatch(fun(), error = function(err) err)




library(wbstools)




