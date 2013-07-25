# taxonCount============================
# 
# ' Count taxa of MetaCV
# '
# ' @description creates a code{data.frame}, that contains the counts ordered by taxId(s) and the 
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
