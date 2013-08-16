#' check ranks of taxon(s) against reference
#' 
#'@param x taxon object
#'@param ranks character vector of taxonomical ranks
#' 
#'@return A logical.
#' 
#'@rdname has_ranks
#'@export
setGeneric("has_ranks", function (x, ranks, ...) standardGeneric("has_ranks"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryID", function (x, ...) standardGeneric("getQueryID"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitID", function (x, ...) standardGeneric("getHitID"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHspID", function (x, ...) standardGeneric("getHspID"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getGeneID", function (x, ...) standardGeneric("getGeneID"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHspID", function (x, ...) standardGeneric("getHspID"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getTaxon", function (x, ...) standardGeneric("getTaxon"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getDefinition", function (x, ...) standardGeneric("getDefinition"))

setGeneric("getKeggID", function (x, ...) standardGeneric("getKeggID"))

setGeneric("getCogID", function (x, ...) standardGeneric("getCogID"))
# internals --------------------------------------------------------------

#'@keywords internal
# recursive walk through the taxonomy tree until taxon has a valid rank
setGeneric('.resolveNoRank', function(taxon, taxon_db, ...) standardGeneric('.resolveNoRank'))

#' count the occurences of ßcode{tax_id(s)}
#' 
#' @decsription count the occurences of tax_id(s) in a given metaCVReport or taxonomyReportDB object and 
#' return a dataframe consisting of the tax_id(s) itself and their frequencies in the object. it is also possible 
#' to only count one special tax_id 
#' 
#' @param metaCVReport a \code{metaCVReport} object
#' @param taxonomyReportDB a \code{taxonomyReportDB} object
#' @param id tax_id (if only one tax_id have to be count)
#'
#'@return data.frame
#'
#'@rdname countTaxa
#'@export
setGeneric('countTaxa',function(x, ...) standardGeneric('countTaxa'))