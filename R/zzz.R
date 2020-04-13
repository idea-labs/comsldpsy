pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {

  pkgenv[["pkg_name"]] <- getPackageName()[[1]]
  pkgenv[["source_dir"]] <- "analysis_dir"
  pkgenv[["source_path"]] <- paste(
    system.file(package = pkgenv$pkg_name), pkgenv$source_dir, sep = "/"
  )
  pkgenv[["authors"]] <- paste0(
    "Visser, L., Kalmar, J., Linkersdörfer, J., Görgen, R., Rothe, J., ",
    "Hasselhorn, M., & Schulte-Körne, G."
  )
  pkgenv[["title"]] <- paste0(
    "Comorbidities between specific learning disorders and psychopathology in ",
    "elementary school children in Germany"
  )
  pkgenv[["product"]] <- "paper"
  pkgenv[["year"]] <- "2020"
  pkgenv[["journal"]] <- "Frontiers in Psychiatry"
  pkgenv[["doi_published"]] <- "http://doi.org/10.3389/fpsyt.2020.00292"
  pkgenv[["doi_preprint"]] <- ""
  pkgenv[["doi_osf"]] <- "http://doi.org/10.17605/OSF.IO/9MXP2"
  pkgenv[["url_osf"]] <- "https://osf.io/9mxp2/"
  pkgenv[["url_github"]] <- "idea-labs/comsldpsy"
  pkgenv[["url_docker"]] <- "idealabsffm/comsldpsy"

  pkgconfig::set_config("drake::strings_in_dots" = "literals")
}

#' Export package environment
#'
#' Exports the package environment, which holds several variables like the
#'   name of the package, authors, title, etc. for use in other functions.
#'
#' @return The package environment
#' @export
export_pkgenv <- function() {
  pkgenv
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Attaching package: '", pkgenv$pkg_name, "'\n\n",
    "The package provides data and analysis code for our ", pkgenv$product,
    ":\n\n",
    pkgenv$authors, " (", pkgenv$year, "). ", pkgenv$title, ". ",
    pkgenv$journal, ". ", pkgenv$doi_published,
    "\n\n",
    "To reproduce the analysis, run `reproduce_analysis()`",
    appendLF = TRUE
  )
}
