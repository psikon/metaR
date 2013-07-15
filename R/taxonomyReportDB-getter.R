#'@importFrom ncbi taxonDB
#'@importFrom ncbi getLineage
NULL
# Taxonomy Table

#' getQueryId
#' 
#'@description getter for taxonomyReportDB
#'
#'@param x   taxonomyReportDB object
#'@param id  tax_id
#'@param typ 
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryId", function(x, ...) standardGeneric("getQueryId"))

.getQueryId <- .getterConstructor('query_id', 'taxonomy', as = 'integer')
setMethod("getQueryId", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQueryId(x, id, typ))
})

#' getHitId
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitId", function(x, ...) standardGeneric("getHitId"))

.getHitId <- .getterConstructor ('hit_id', 'taxonomy', as = 'integer')
setMethod("getHitId", signature='taxonomyReportDB', function (x, id, typ) {
  unlist(.getHitId(x, id, typ))
})

#' getGeneId
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getGeneId", function(x, ...) standardGeneric("getGeneId"))

.getGeneId <- .getterConstructor ('gene_id', 'taxonomy', as = 'character')
setMethod("getGeneId", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getGeneId(x, id, typ))
})

#' getAccession
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getAccession", function(x, ...) standardGeneric("getAccession"))

.getAccession <- .getterConstructor ('accession', 'taxonomy', as = 'character')
setMethod("getAccession", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getAccession(x, id, typ))
})

#' getTaxId
#' 
#'@rdname taxonomyReportDB
#'@export
setGeneric("getTaxId", function(x, ...) standardGeneric("getTaxId"))

.getTaxId <- .getterConstructor ('tax_id', 'taxonomy', as = 'integer')
setMethod("getTaxId", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getTaxId(x, id, typ))
})

#' getScientificName
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getScientificName", function(x, ...) standardGeneric("getScientificName"))

.getScientificName <- .getterConstructor ('scientific_name', 'taxonomy', as = 'character')
setMethod("getScientificName", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getScientificName(x, id, typ))
})

#' getRank
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getRank", function(x, ...) standardGeneric("getRank"))

.getRank <- .getterConstructor ('rank', 'taxonomy', as = 'character')
setMethod("getRank", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getRank(x, id, typ))
})

#'getTaxon
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric('getTaxon', function(x, ...) standardGeneric('getTaxon'))

setMethod('getTaxon','taxonomyReportDB', function (x, id, typ, taxon_db) {
  taxonDB(getTaxId(x, id, typ), taxon_db[[1]])
})

#'getLineage
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric('getLineage', function(x, ...) standardGeneric('getLineage'))

setMethod('getLineage','taxonomyReportDB', function (x, id, typ, taxon_db) {
  getLineage(taxonDB(getTaxId(x, id, typ), taxon_db[[1]]))
})

# Query Table

#' getQueryDef
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryDef", function(x, ...) standardGeneric("getQueryDef"))

.getQueryDef <- .getterConstructor ('query_def', 'query', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getQueryDef", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQueryDef(x, id, typ))
})

#' getQueryLen
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryLen", function(x, ...) standardGeneric("getQueryLen"))

.getQueryLen <- .getterConstructor ('query_len', 'query', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getQueryLen", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQueryLen(x, id, typ))
})

# hit Table

#' getHitNum
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitNum", function(x, ...) standardGeneric("getHitNum"))

.getHitNum <- .getterConstructor ('hit_num', 'hit', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getHitNum", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitNum(x, id, typ))
})

#' getDefinition
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getDefinition", function(x, ...) standardGeneric("getDefinition"))

.getDefinition <- .getterConstructor ('definition', 'hit', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getDefinition", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getDefinition(x, id, typ))
})

#' getHitLen
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitLen", function(x, ...) standardGeneric("getHitLen"))

.getHitLen <- .getterConstructor ('length', 'hit', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getHitLen", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitLen(x, id, typ))
})

# hsp table

#' getHspId
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHspId", function(x, ...) standardGeneric("getHspId"))

.getHspId <- .getterConstructor ('hsp_id', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getHspId", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHspId(x, id, typ))
})

#' getHspNum
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHspNum", function(x, ...) standardGeneric("getHspNum"))

.getHspNum <- .getterConstructor ('hsp_num', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getHspNum", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHspNum(x, id, typ))
})

#' getBitScore
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getBitScore", function(x, ...) standardGeneric("getBitScore"))

.getBitScore <- .getterConstructor ('bit_score', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getBitScore", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getBitScore(x, id, typ))
})

#' getScore
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getScore", function(x, ...) standardGeneric("getScore"))

.getScore <- .getterConstructor ('score', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getScore", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getScore(x, id, typ))
})

#' getEvalue
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getEvalue", function(x, ...) standardGeneric("getEvalue"))

.getEvalue <- .getterConstructor ('evalue', 'hsp', WHERE = 'query_id',
                                 VAL = 'query_id', TABLE = 'taxonomy', 
                                 as = 'character')
setMethod("getEvalue", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getEvalue(x, id, typ))
})

#' getQueryFrom
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryFrom", function(x, ...) standardGeneric("getQueryFrom"))

.getQueryFrom <- .getterConstructor ('query_from', 'hsp', WHERE = 'query_id',
                                 VAL = 'query_id', TABLE = 'taxonomy', 
                                 as = 'character')
setMethod("getQueryFrom", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQueryFrom(x, id, typ))
})

#' getQueryTo
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryTo", function(x, ...) standardGeneric("getQueryTo"))

.getQueryTo <- .getterConstructor ('query_to', 'hsp', WHERE = 'query_id',
                                 VAL = 'query_id', TABLE = 'taxonomy', 
                                 as = 'character')
setMethod("getQueryTo", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQueryTo(x, id, typ))
})

#' getHitFrom
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitFrom", function(x, ...) standardGeneric("getHitFrom"))

.getHitFrom <- .getterConstructor ('hit_from', 'hsp', WHERE = 'query_id',
                                 VAL = 'query_id', TABLE = 'taxonomy', 
                                 as = 'character')
setMethod("getHitFrom", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitFrom(x, id, typ))
})

#' getHitTo
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitTo", function(x, ...) standardGeneric("getHitTo"))

.getHitTo <- .getterConstructor ('hit_to', 'hsp', WHERE = 'query_id',
                                 VAL = 'query_id', TABLE = 'taxonomy', 
                                 as = 'character')
setMethod("getHitTo", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitTo(x, id, typ))
})

#' getQueryFrame
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryFrame", function(x, ...) standardGeneric("getQueryFrame"))

.getQueryFrame <- .getterConstructor ('query_frame', 'hsp', WHERE = 'query_id',
                                 VAL = 'query_id', TABLE = 'taxonomy', 
                                 as = 'character')
setMethod("getQueryFrame", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQueryFrame(x, id, typ))
})

#' getHitFrame
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitFrame", function(x, ...) standardGeneric("getHitFrame"))

.getHitFrame <- .getterConstructor ('hit_frame', 'hsp', WHERE = 'query_id',
                                      VAL = 'query_id', TABLE = 'taxonomy', 
                                      as = 'character')
setMethod("getHitFrame", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitFrame(x, id, typ))
})

#' getIdentity
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getIdentity", function(x, ...) standardGeneric("getIdentity"))

.getIdentity <- .getterConstructor ('identity', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getIdentity", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getIdentity(x, id, typ))
})

#' getPositive
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getPositive", function(x, ...) standardGeneric("getPositive"))

.getPositive <- .getterConstructor ('positive', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getPositive", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getPositive(x, id, typ))
})

#' getGaps
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getGaps", function(x, ...) standardGeneric("getGaps"))

.getGaps <- .getterConstructor ('gaps', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getGaps", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getGaps(x, id, typ))
})

#' getAlignLen
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getAlignLen", function(x, ...) standardGeneric("getAlignLen"))

.getAlignLen <- .getterConstructor ('align_len', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getAlignLen", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getAlignLen(x, id, typ))
})

#' getQuerySeq
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQuerySeq", function(x, ...) standardGeneric("getQuerySeq"))

.getQuerySeq <- .getterConstructor ('qseq', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getQuerySeq", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQuerySeq(x, id, typ))
})

#' getHitSeq
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitSeq", function(x, ...) standardGeneric("getHitSeq"))

.getHitSeq <- .getterConstructor ('hseq', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getHitSeq", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitSeq(x, id, typ))
})

#' getMidline
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getMidline", function(x, ...) standardGeneric("getMidline"))

.getMidline <- .getterConstructor ('midline', 'hsp', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getMidline", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getMidline(x, id, typ))
})