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
# getReadName============================
# 
## ' Getter methods for MetaCVReport
## ' 
## ' @param x A metaCVReport object.
## ' @param i index of the Object
## ' 
## ' @rdname MetaCVReport-class
## ' @export
# setGeneric("getReadName", function(x, ...) standardGeneric("getReadName"))
# 
# 
# getScore============================
# 
# The generic defined in package 'blastr' 
# 
##' Getter methods for MetaCVReport
##' 
## ' @param x A metaCVReport object.
## ' @param i index of the Object
##'  
## ' @rdname MetaCVReport-class
## ' @export
# setGeneric("getScore", function(x, ...) standardGeneric("getScore"))
# 
# getGeneID============================
# 
# The generic if defined in package 'blastr' 
# 
## ' Getter methods for MetaCVReport
## ' 
## ' @param x A metaCVReport object.
## ' @param i index of the Object
## ' 
## ' @rdname MetaCVReport-class
## ' @export
# setGeneric("getGeneID", function(x, ...) standardGeneric("getGeneID"))
# 
# 
# getTaxID============================
# 
# This generic is defined in package 'ncbi'
# 
## ' Getter methods for MetaCVReport
## ' 
## ' @param x A metaCVReport object.
## ' @param i index of the Object
## ' 
## ' @rdname MetaCVReport-class
## ' @export
# setGeneric("getTaxID",function(x, ...) standardGeneric("getTaxID"))
# 
# 
# getTaxName============================
# 
##' Getter methods for MetaCVReport
##' 
## '@param x A metaCVReport object.
## '@param i index of the Object
## ' 
## '@rdname MetaCVReport-class
## '@export
# setGeneric("getTaxName", function(x, ...) standardGeneric("getTaxName"))
# 
# 
# getGeneIDbyTaxID============================
# 
## ' get for a specifi taxID all geneID(s)
## ' 
## ' @param x A metaCVReport object.
## ' @param i taxID(s) for searching
## ' 
## ' @rdname MetaCVReport-class
## ' @export
# setGeneric("getGeneIDbyTaxID", function(x, i) standardGeneric("getGeneIDbyTaxID"))
# 
# 
# getByScore============================
# 
## ' get only the entries with a score greater than x, to prevent 
## ' wrongly classified entries
## ' 
## ' @param x A metaCVReport object.
## ' @param i score
## ' 
## ' @details The score refers to the composition identity between 
## ' the query read and the best target gene, and ranges from 0 
## ' to 100. In our analysis, reads with very low scores are highly 
## ' possible to be wrongly classified and should be filtered. 
## ' Recommended is  a set of cutoff values 
## ' (minimal scores to pass) according to different read lengths: 
## ' \itemize{
## '   \item{Read length}{Minimal score}
## '   \item{100 bp }{20}
## '   \item{200 bp}{10}
## '   \item{400 bp}{5}
## '   \item{600 bp}{4}
## '   \item{800 bp}{3}
## '   \item{1000 bp}{3}
## ' } 
## ' 
## ' @rdname MetaCVReport-class
## ' @export
# setGeneric("getByScore", function(x,i) standardGeneric("getByScore"))


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
