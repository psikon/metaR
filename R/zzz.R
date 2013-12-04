.onLoad <- function(libname, pkgname) {
  ## set global options
  options(verbose = FALSE)
  options(reutils.verbose.queries = FALSE)
  op <- options()
  op.metaR <- list(
    ncbi.taxonomy.path = normalizePath("~/local/db/taxonomy", mustWork=FALSE)
  )
  toset <- !(names(op.metaR) %in% names(op))
  if (any(toset)) {
    options(op.metaR[toset])
  }
  invisible()
}
