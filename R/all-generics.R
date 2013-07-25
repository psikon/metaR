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
setGeneric('.resolveNoRank', function(taxon, taxonDB, ...) standardGeneric('.resolveNoRank'))

#'count he number of occurences of \code{tax_id(s)} 
#'
#'@description count the number of occureneces of \code{tax_id(s)} and
#'return a data.frame containing the \code{tax_id(s)} and their frequencies
#'
#'@param x metaCVReport or taxonomyReportDB
#'@param id \code{tax_id}
#'
#'@return data.frame
#'
#'@rdname countTaxa
#'@export
setGeneric('countTaxa',function(x, ...) standardGeneric('countTaxa'))