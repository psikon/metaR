#' Check ranks of taxa against a reference
#' 
#' @param x taxon object
#' @param ranks character vector of taxonomic ranks
#' 
#' @return A logical.
#' 
#' @rdname has_ranks
#' @export
#' @docType methods
setGeneric("has_ranks", function (x, ranks, ...) standardGeneric("has_ranks"))


#' Count the occurences of \code{tax_id}s
#' 
#' @description count the occurences of tax_id(s) in a given metaCVReport or taxonomyReportDB object and 
#' return a dataframe consisting of the tax_id(s) itself and their frequencies in the object. it is also possible 
#' to only count one special tax_id 
#' 
#' @param metaCVReport a \code{metaCVReport} object
#' @param taxonomyReportDB a \code{taxonomyReportDB} object
#' @param id tax_id (if only one tax_id have to be count)
#' @return data.frame
#'
#' @rdname countTaxa-methods
#' @export
#' @docType methods
setGeneric('countTaxa', function(x, ...) standardGeneric('countTaxa'))


#' Access fields of a taxonomyReportDB object.
#'
#' @param x a \code{\linkS4class{taxonomyReportDB}} object.
#' @param id an identifier
#' @param type type of identifier. One of \sQuote{tax_id}, \sQuote{query_id},
#' or \sQuote{hit_id}.
#'
#' @rdname taxonomyReportDB-methods
#' @export
#' @docType methods
setGeneric("getTaxon", function (x, ...) standardGeneric("getTaxon"))

#' @rdname taxonomyReportDB-methods
#' @export
#' @docType methods
setGeneric("getScientificName")

#' @rdname taxonomyReportDB-methods
#' @export
#' @docType methods
setGeneric("getTaxID")

#' @rdname taxonomyReportDB-methods
#' @export
#' @docType methods
setGeneric("getParentTaxID")

#' @rdname taxonomyReportDB-methods
#' @export
#' @docType methods
setGeneric("getRank")

#' @rdname taxonomyReportDB-methods
#' @export
#' @docType methods
setGeneric("getOtherName")

#' @rdname taxonomyReportDB-methods
#' @export
#' @docType methods
setGeneric("getLineage")

#' @rdname taxonomyReportDB-methods
#' @export
#' @docType methods
setGeneric("getDefinition", function (x, ...) standardGeneric("getDefinition"))

#' Access fields of a metaCVReport object.
#'
#' @param x a \code{\linkS4class{metaCVReport}} object.
#' @param ...
#'
#' @rdname metaCVReport-methods
#' @export
#' @docType methods
setGeneric("getKeggID", function (x, ...) standardGeneric("getKeggID"))

#' @rdname metaCVReport-methods
#' @export
#' @docType methods
setGeneric("getCogID", function (x, ...) standardGeneric("getCogID"))

#' @rdname blastReportStreamer
#' @export
#' @docType methods
setGeneric("yield", function(x, ...) standardGeneric("yield"))

# internals --------------------------------------------------------------

#' @keywords internal
# recursively walk through the taxonomy tree until a taxon has a valid rank
setGeneric('.resolveNoRank', function(taxon, ...) standardGeneric('.resolveNoRank'))


