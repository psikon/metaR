# getReadName============================
#
#' Getter methods for MetaCVReport
#' 
#' @param x A \code{\linkS4class{metaCVReport}] object.
#' @param i index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getReadName", function(x, ...) standardGeneric("getReadName"))

#' @export
setMethod("getReadName", signature='metaCVReport',
          function(x, i = NULL){
            if (is.null(i)) {
              x[, "readName"]
            } else {
              x[i, "readName"]
            }
          })

# getScore============================
#
#' Getter methods for MetaCVReport
#' 
#' @param x A \code{\linkS4class{metaCVReport}] object.
#' @param i index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getScore", function(x, ...) standardGeneric("getScore"))

#' @export
setMethod("getScore",signature='metaCVReport',
          function(x, i = NULL) {
            if (is.null(i)) {
              x[, "score"]
            } else {
              x[i, "score"]
            }
})

# getGeneID============================
#
#' Getter methods for MetaCVReport
#' 
#' @param x A \code{\linkS4class{metaCVReport}] object.
#' @param i index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getGeneID", function(x, ...) standardGeneric("getGeneID"))

#' @export
setMethod("getGeneID",signature='metaCVReport',
          function(x, i = NULL) {
            if (is.null(i)) {
              x[, "geneID"]
            } else {
              x[i, "geneID"]
            }
})

# getTaxID============================
#
#' Getter methods for MetaCVReport
#' 
#' @param x A \code{\linkS4class{metaCVReport}] object.
#' @param i index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getTaxID", function(x,...) standardGeneric("getTaxID"))

#' @export
setMethod("getTaxID",signature='metaCVReport',
          function(x, i = NULL) {
            if (is.null(i)) {
              x[, "taxID"]
            } else {
              x[i, "taxID"]
            }
})


# getTaxName============================
#
#' Getter methods for MetaCVReport
#' 
#' @param x A \code{\linkS4class{metaCVReport}] object.
#' @param i index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getTaxName", function(x, ...) standardGeneric("getTaxName"))
#' @export
setMethod("getTaxName",signature='metaCVReport',
          function(x, i = NULL) {
            if (is.null(i)) {
              x[, "taxName"]
            } else {
              x[i, "taxName"]
            }
})

# getGeneIDbyTaxID============================
#
#' get for a specifig taxID all geneID(s)
#' 
#' @param x A \code{\linkS4class{metaCVReport}] object.
#' @param i taxID(s) for searching
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getGeneIDbyTaxID", function(x, i) standardGeneric("getGeneIDbyTaxID"))
#' @export
setMethod("getGeneIDbyTaxID",signature='metaCVReport',
          function(x, i) {
            x[which(x[,"taxID"] == i), "geneID"]
          })

# getByScore============================
#
#' get only the entries with a score greater than x, to prevent 
#' wrongly classified entries
#' 
#' @param x A \code{\linkS4class{metaCVReport}] object.
#' @param i score
#' 
#' @details The score refers to the composition identity between 
#' the query read and the best target gene, and ranges from 0 
#' to 100. In our analysis, reads with very low scores are highly 
#' possible to be wrongly classified and should be filtered. 
#' Recommended is  a set of cutoff values 
#' (minimal scores to pass) according to different read lengths: 
#' \describe{
#'   \item{Read length}{Minimal score}
#'   \item{100 bp }{20}
#'   \item{200 bp}{10}
#'   \item{400 bp}{5}
#'   \item{600 bp}{4}
#'   \item{800 bp}{3}
#'   \item{1000 bp}{3}
#' } 
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getByScore", function(x,i) standardGeneric("getByScore"))

#' @export
setMethod("getByScore",signature='metaCVReport',
          function(x, i) {
            x[which(x[,"score"] >= i),]
          })