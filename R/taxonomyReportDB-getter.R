#'@importFrom ncbi taxonDB
#'@importClassesFrom ncbi Taxon
#'@importClassesFrom ncbi TaxonList
#'@importFrom ncbi getRank
#'@importFrom ncbi getLineage
#'@importFrom ncbi getOtherName
#'@importFrom ncbi getParentTaxId
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
#'@rdname taxonomyReportDB-getter
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
  taxonDB(getTaxId(x, id, typ), taxon_db[['taxon_db']])
})

#'getLineage
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric('getLineage', function(x, ...) standardGeneric('getLineage'))

setMethod('getLineage','taxonomyReportDB', function (x, id, typ, taxon_db) {
  getLineage(getTaxon(x, id, typ , taxon_db))
})

#'getOtherName
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric('getOtherName', function(x, ...) standardGeneric('getOtherName'))

setMethod('getOtherName','taxonomyReportDB', function (x, id, typ, taxon_db) {
  getOtherName(getTaxon(x, id, typ , taxon_db))
})

#'getParentTaxId
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric('getParentTaxId', function(x, ...) standardGeneric('getParentTaxId'))

setMethod('getParentTaxId','taxonomyReportDB', function (x, id, typ, taxon_db) {
  getParentTaxId(getTaxon(x, id, typ , taxon_db))
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
  unlist(.getQueryDef(x, getQueryId(x,id,typ), 'query_id'))
})

#' getQueryLen
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryLen", function(x, ...) standardGeneric("getQueryLen"))

.getQueryLen <- .getterConstructor ('query_len', 'query', WHERE = 'query_id',
                                    VAL = 'query_id', TABLE = 'taxonomy', 
                                    as = 'integer')
setMethod("getQueryLen", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQueryLen(x, getQueryId(x,id,typ), 'query_id'))
})

# hit Table

#' getHitNum
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitNum", function(x, ...) standardGeneric("getHitNum"))

.getHitNum <- .getterConstructor ('hit_num', 'hit', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
setMethod("getHitNum", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitNum(x, getHitId(x,id,typ), 'hit_id'))
})

#' getDefinition
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getDefinition", function(x, ...) standardGeneric("getDefinition"))

.getDefinition <- .getterConstructor ('definition', 'hit', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getDefinition", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getDefinition(x, getHitId(x,id,typ), 'hit_id'))
})

#' getHitLen
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitLen", function(x, ...) standardGeneric("getHitLen"))

.getHitLen <- .getterConstructor ('length', 'hit', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
setMethod("getHitLen", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitLen(x, getHitId(x,id,typ), 'hit_id'))
})

# hsp table

#' getHspId
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHspId", function(x, ...) standardGeneric("getHspId"))

.getHspId <- .getterConstructor ('hsp_id', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
setMethod("getHspId", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHspId(x, getHitId(x,id,typ), 'hit_id'))
})

#' getHspNum
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHspNum", function(x, ...) standardGeneric("getHspNum"))

.getHspNum <- .getterConstructor ('hsp_num', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
setMethod("getHspNum", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHspNum(x, getHitId(x,id,typ), 'hit_id'))
})

#' getBitScore
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getBitScore", function(x, ...) standardGeneric("getBitScore"))

.getBitScore <- .getterConstructor ('bit_score', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'numeric')
setMethod("getBitScore", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getBitScore(x, getHitId(x,id,typ), 'hit_id'))
})

#' getScore
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getScore", function(x, ...) standardGeneric("getScore"))

.getScore <- .getterConstructor ('score', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'numeric')
setMethod("getScore", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getScore(x, getHitId(x,id,typ), 'hit_id'))
})

#' getEvalue
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getEvalue", function(x, ...) standardGeneric("getEvalue"))

.getEvalue <- .getterConstructor ('evalue', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'numeric')
setMethod("getEvalue", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getEvalue(x, getHitId(x,id,typ), 'hit_id'))
})

#' getQueryFrom
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryFrom", function(x, ...) standardGeneric("getQueryFrom"))

.getQueryFrom <- .getterConstructor ('query_from', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'integer')
setMethod("getQueryFrom", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQueryFrom(x, getHitId(x,id,typ), 'hit_id'))
})

#' getQueryTo
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryTo", function(x, ...) standardGeneric("getQueryTo"))

.getQueryTo <- .getterConstructor ('query_to', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'integer')
setMethod("getQueryTo", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQueryTo(x, getHitId(x,id,typ), 'hit_id'))
})

#' getHitFrom
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitFrom", function(x, ...) standardGeneric("getHitFrom"))

.getHitFrom <- .getterConstructor ('hit_from', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'integer')
setMethod("getHitFrom", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitFrom(x, getHitId(x,id,typ), 'hit_id'))
})

#' getHitTo
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitTo", function(x, ...) standardGeneric("getHitTo"))

.getHitTo <- .getterConstructor ('hit_to', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'integer')
setMethod("getHitTo", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitTo(x, getHitId(x,id,typ), 'hit_id'))
})

#' getQueryFrame
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQueryFrame", function(x, ...) standardGeneric("getQueryFrame"))

.getQueryFrame <- .getterConstructor ('query_frame', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'character')
setMethod("getQueryFrame", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQueryFrame(x, getHitId(x,id,typ), 'hit_id'))
})

#' getHitFrame
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitFrame", function(x, ...) standardGeneric("getHitFrame"))

.getHitFrame <- .getterConstructor ('hit_frame', 'hsp', WHERE = 'hit_id',
                                      VAL = 'hit_id', TABLE = 'taxonomy', 
                                      as = 'character')
setMethod("getHitFrame", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitFrame(x, getHitId(x,id,typ), 'hit_id'))
})

#' getIdentity
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getIdentity", function(x, ...) standardGeneric("getIdentity"))

.getIdentity <- .getterConstructor ('identity', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
setMethod("getIdentity", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getIdentity(x, getHitId(x,id,typ), 'hit_id'))
})

#' getPositive
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getPositive", function(x, ...) standardGeneric("getPositive"))

.getPositive <- .getterConstructor ('positive', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
setMethod("getPositive", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getPositive(x, getHitId(x,id,typ), 'hit_id'))
})

#' getGaps
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getGaps", function(x, ...) standardGeneric("getGaps"))

.getGaps <- .getterConstructor ('gaps', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
setMethod("getGaps", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getGaps(x, getHitId(x,id,typ), 'hit_id'))
})

#' getAlignLen
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getAlignLen", function(x, ...) standardGeneric("getAlignLen"))

.getAlignLen <- .getterConstructor ('align_len', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
setMethod("getAlignLen", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getAlignLen(x, getHitId(x,id,typ), 'hit_id'))
})

#' getQuerySeq
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getQuerySeq", function(x, ...) standardGeneric("getQuerySeq"))

.getQuerySeq <- .getterConstructor ('qseq', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getQuerySeq", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getQuerySeq(x, getHitId(x,id,typ), 'hit_id'))
})

#' getHitSeq
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getHitSeq", function(x, ...) standardGeneric("getHitSeq"))

.getHitSeq <- .getterConstructor ('hseq', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getHitSeq", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getHitSeq(x, getHitId(x,id,typ), 'hit_id'))
})

#' getMidline
#'
#'@rdname taxonomyReportDB-getter
#'@export
setGeneric("getMidline", function(x, ...) standardGeneric("getMidline"))

.getMidline <- .getterConstructor ('midline', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'character')
setMethod("getMidline", "taxonomyReportDB", function (x, id, typ) {
  unlist(.getMidline(x, getHitId(x,id,typ), 'hit_id'))
})