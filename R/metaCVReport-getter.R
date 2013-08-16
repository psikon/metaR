#'Getter for metaCVReport
#'
#'@param metaCVReport  a \code{metaCVReport} object
#'@param id  tax_id
#'
#' @rdname metaCVReport-getter
#' @export
setMethod("getQueryDef", 'metaCVReport', function (x, id) {
  x[['query_def']][which(metaCV[['tax_id']] == id)]
          })

#' @rdname metaCVReport-getter
#' @export
setMethod("getScore", 'metaCVReport', function(x, id) {
  x[['score']][which(metaCV[['tax_id']] == id)]
})


#' @rdname metaCVReport-getter
#' @export
setMethod("getGeneID", 'metaCVReport', function(x, id) {
  x[['gene_id']][which(metaCV[['tax_id']] == id)]
})

#' @rdname metaCVReport-getter
#' @export
setMethod("getKeggID", 'metaCVReport', function(x, id) {
  x[['kegg_id']][which(metaCV[['tax_id']] == id)]
})

#' @rdname metaCVReport-getter
#' @export
setMethod("getCogID", 'metaCVReport', function(x, id) {
  x[['cog_id']][which(metaCV[['tax_id']] == id)]
})

#' @rdname metaCVReport-getter
#' @export
setMethod("getTaxID", 'metaCVReport', function(x, id) {
  x[['tax_id']][which(metaCV[['tax_id']] == id)]
})


#' @rdname metaCVReport-getter
#' @export
setMethod("getScientificName", 'metaCVReport', function(x, id) {
  x[['scientific_name']][which(metaCV[['tax_id']] == id)]
})
