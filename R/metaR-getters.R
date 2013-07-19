
## Getters for metsCVReport objects

#' @export
setMethod("getReadName", 'metaCVReport',
          function (x, i = NULL) {
            if (is.null(i)) {
              x[, "readName"]
            } else {
              x[i, "readName"]
            }
          })


#' @export
setMethod("getScore", 'metaCVReport',
          function(x, i = NULL) {
            if (is.null(i)) {
              x[, "score"]
            } else {
              x[i, "score"]
            }
})


#' @export
setMethod("getGeneID", 'metaCVReport',
          function(x, i = NULL) {
            if (is.null(i)) {
              x[, "geneID"]
            } else {
              x[i, "geneID"]
            }
})


#' @export
setMethod("getTaxID", 'metaCVReport',
          function(x, i = NULL) {
            if (is.null(i)) {
              x[, "taxID"]
            } else {
              x[i, "taxID"]
            }
})


#' @export
setMethod("getTaxName", 'metaCVReport',
          function(x, i = NULL) {
            if (is.null(i)) {
              x[, "taxName"]
            } else {
              x[i, "taxName"]
            }
})


#' @export
setMethod("getGeneIDbyTaxID", 'metaCVReport',
          function(x, i) {
            x[which(x[,"taxID"] == i), "geneID"]
          })


#' @export
setMethod("getByScore", 'metaCVReport',
          function(x, i) {
            x[which(x[,"score"] >= i),]
          })
