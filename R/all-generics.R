# taxonCount============================
# 
# ' Count taxa of MetaCV
# '
# ' @description creates a code{data.frame}, that contains the counts ordered by taxIds and the 
# ' corresponding taxName
# ' 
# ' @param x A metaCVReport object.
# ' 
# ' @rdname MetaCVReport-methods
# ' @export
# setGeneric("taxonCount", function(x, ...) standardGeneric("taxonCount"))
# 
# 

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
setGeneric("getQueryId", function (x, ...) standardGeneric("getQueryId"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitId", function (x, ...) standardGeneric("getHitId"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHspId", function (x, ...) standardGeneric("getHspId"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getGeneId", function (x, ...) standardGeneric("getGeneId"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHspId", function (x, ...) standardGeneric("getHspId"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getTaxon", function (x, ...) standardGeneric("getTaxon"))

#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getDefinition", function (x, ...) standardGeneric("getDefinition"))

setGeneric("getKeggId", function (x, ...) standardGeneric("getKeggId"))

setGeneric("getCogId", function (x, ...) standardGeneric("getCogId"))
# internals --------------------------------------------------------------


#'@keywords internal
# recursive walk through the taxonomy tree until taxon has a valid rank
setGeneric('.resolveNoRank', function(taxon, taxonDB, ...) standardGeneric('.resolveNoRank'))
