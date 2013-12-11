#' Check ranks of taxa against a reference
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
setGeneric("getTaxon", function (x, ...) standardGeneric("getTaxon"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getScientificName")

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getTaxID")

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getParentTaxID")

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getRank")

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getOtherName")

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getLineage")

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getDefinition", function (x, ...) standardGeneric("getDefinition"))

setGeneric("getKeggID", function (x, ...) standardGeneric("getKeggID"))

setGeneric("getCogID", function (x, ...) standardGeneric("getCogID"))

#' @rdname blastReportStreamer
#' @export
setGeneric("yield", function(x, ...) standardGeneric("yield"))

# internals --------------------------------------------------------------

#'@keywords internal
# recursively walk through the taxonomy tree until a taxon has a valid rank
setGeneric('.resolveNoRank', function(taxon, ...) standardGeneric('.resolveNoRank'))

#' Count the occurences of \code{tax_id}s
#' 
#' @description count the occurences of tax_id(s) in a given metaCVReport or taxonomyReportDB object and 
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
setGeneric('countTaxa', function(x, ...) standardGeneric('countTaxa'))
