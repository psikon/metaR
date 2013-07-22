
#'Getter for metaCVReport
#'
#'@param x  \code{metaCVReport} object
#'@param id  tax_id
#'
#' @rdname metaCVReport-getter.rd
#' @export
setMethod("getQueryDef", 'metaCVReport', function (x, id) {
  x[['query_def']][which(metaCV[['tax_id']] == id)]
          })

#' @rdname metaCVReport-getter.rd
#' @export
setMethod("getScore", 'metaCVReport', function(x, id) {
  x[['score']][which(metaCV[['tax_id']] == id)]
})


#' @rdname metaCVReport-getter.rd
#' @export
setMethod("getGeneId", 'metaCVReport', function(x, id) {
  x[['gene_id']][which(metaCV[['tax_id']] == id)]
})

#' @rdname metaCVReport-getter.rd
#' @export
setMethod("getKeggId", 'metaCVReport', function(x, id) {
  x[['kegg_id']][which(metaCV[['tax_id']] == id)]
})

#' @rdname metaCVReport-getter.rd
#' @export
setMethod("getCogId", 'metaCVReport', function(x, id) {
  x[['cog_id']][which(metaCV[['tax_id']] == id)]
})

#' @rdname metaCVReport-getter.rd
#' @export
setMethod("getTaxId", 'metaCVReport', function(x, id) {
  x[['tax_id']][which(metaCV[['tax_id']] == id)]
})


#' @rdname metaCVReport-getter.rd
#' @export
setMethod("getScientificName", 'metaCVReport', function(x, id) {
  x[['scientific_name']][which(metaCV[['tax_id']] == id)]
})
