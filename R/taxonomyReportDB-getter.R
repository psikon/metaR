#'@importClassesFrom ncbi Taxon
#'@importClassesFrom ncbi TaxonList
#'@importFrom ncbi taxonDB
#'@importFrom ncbi getTaxId
#'@importFrom ncbi getParentTaxId
#'@importFrom ncbi getScientificName
#'@importFrom ncbi getRank
#'@importFrom ncbi getLineage
#'@importFrom ncbi getOtherName
#'@importFrom blastr getGeneID
#'@importFrom blastr getScore
NULL


.getTaxId <- .getterConstructor ('tax_id', 'taxonomy', as = 'integer')
#' Getter for taxonomyReportDB
#'
#' @param x A \code{taxonomyReportDB} object.
#' @param id An identifier
#' @param type Type of identifier. One of \sQuote{tax_id}, \sQuote{query_id},
#' or \sQuote{hit_id}.
#'
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getTaxId", "taxonomyReportDB", function (x, id, type) {
  unlist(.getTaxId(x, id, type))
})


#'@rdname taxonomyReportDB-getter
#'@export
setMethod('getParentTaxId','taxonomyReportDB', function (x, id, type, taxon_db) {
  getParentTaxId(getTaxon(x, id, type, taxon_db))
})


.getScientificName <- .getterConstructor ('scientific_name', 'taxonomy', as = 'character')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getScientificName", "taxonomyReportDB", function (x, id, type) {
  unlist(.getScientificName(x, id, type))
})


.getRank <- .getterConstructor ('rank', 'taxonomy', as = 'character')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getRank", "taxonomyReportDB", function (x, id, type) {
  unlist(.getRank(x, id, type))
})


#'@rdname taxonomyReportDB-getter
#'@export
setMethod('getTaxon','taxonomyReportDB', function (x, id, type, taxon_db) {
  taxonDB(getTaxId(x, id, type), taxon_db[['taxon_db']])
})


#'@rdname taxonomyReportDB-getter
#'@export
setMethod('getLineage','taxonomyReportDB', function (x, id, type, taxon_db) {
  getLineage(getTaxon(x, id, type, taxon_db))
})


#'@rdname taxonomyReportDB-getter
#'@export
setMethod('getOtherName','taxonomyReportDB', function (x, id, type, taxon_db) {
  getOtherName(getTaxon(x, id, type, taxon_db))
})



.getQueryId <- .getterConstructor('query_id', 'taxonomy', as = 'integer')
#' @rdname taxonomyReportDB-getter
#' @export
setMethod("getQueryId", "taxonomyReportDB", function (x, id, type) {
  unlist(.getQueryId(x, id, type))
})


.getHitId <- .getterConstructor ('hit_id', 'taxonomy', as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getHitId", signature='taxonomyReportDB', function (x, id, type) {
  unlist(.getHitId(x, id, type))
})


.getGeneId <- .getterConstructor ('gene_id', 'taxonomy', as = 'character')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getGeneId", "taxonomyReportDB", function (x, id, type) {
  unlist(.getGeneId(x, id, type))
})


.getAccession <- .getterConstructor ('accession', 'taxonomy', as = 'character')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getAccession", "taxonomyReportDB", function (x, id, type) {
  unlist(.getAccession(x, id, type))
})


.getQueryDef <- .getterConstructor('query_def', 'query', WHERE = 'query_id',
                                   VAL = 'query_id', TABLE = 'taxonomy', 
                                   as = 'character')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getQueryDef", "taxonomyReportDB", function (x, id, type) {
  unlist(.getQueryDef(x, getQueryId(x, id, type ), 'query_id'))
})


.getQueryLen <- .getterConstructor('query_len', 'query', WHERE = 'query_id',
                                   VAL = 'query_id', TABLE = 'taxonomy', 
                                   as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getQueryLen", "taxonomyReportDB", function (x, id, type) {
  unlist(.getQueryLen(x, getQueryId(x, id, type), 'query_id'))
})


.getHitNum <- .getterConstructor ('hit_num', 'hit', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getHitNum", "taxonomyReportDB", function (x, id, type) {
  unlist(.getHitNum(x, getHitId(x, id, type), 'hit_id'))
})


.getDefinition <- .getterConstructor ('definition', 'hit', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'character')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getDefinition", "taxonomyReportDB", function (x, id, type) {
  unlist(.getDefinition(x, getHitId(x, id, type), 'hit_id'))
})


.getHitLen <- .getterConstructor ('length', 'hit', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getHitLen", "taxonomyReportDB", function (x, id, type) {
  unlist(.getHitLen(x, getHitId(x, id, type), 'hit_id'))
})

.getHspId <- .getterConstructor('hsp_id', 'hsp', WHERE = 'hit_id',
                                VAL = 'hit_id', TABLE = 'taxonomy', 
                                as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getHspId", "taxonomyReportDB", function (x, id, type) {
  unlist(.getHspId(x, getHitId(x, id, type), 'hit_id'))
})


.getHspNum <- .getterConstructor('hsp_num', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getHspNum", "taxonomyReportDB", function (x, id, type) {
  unlist(.getHspNum(x, getHitId(x, id, type), 'hit_id'))
})


.getBitscore <- .getterConstructor('bit_score', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'numeric')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getBitscore", "taxonomyReportDB", function (x, id, type) {
  unlist(.getBitscore(x, getHitId(x,id,type), 'hit_id'))
})


.getScore <- .getterConstructor('score', 'hsp', WHERE = 'hit_id',
                                VAL = 'hit_id', TABLE = 'taxonomy', 
                                as = 'numeric')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getScore", "taxonomyReportDB", function (x, id, type) {
  unlist(.getScore(x, getHitId(x, id, type), 'hit_id'))
})


.getEvalue <- .getterConstructor('evalue', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'numeric')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getEvalue", "taxonomyReportDB", function (x, id, type) {
  unlist(.getEvalue(x, getHitId(x, id, type), 'hit_id'))
})

.getQueryFrom <- .getterConstructor('query_from', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getQueryFrom", "taxonomyReportDB", function (x, id, type) {
  unlist(.getQueryFrom(x, getHitId(x, id, type), 'hit_id'))
})


.getQueryTo <- .getterConstructor('query_to', 'hsp', WHERE = 'hit_id',
                                  VAL = 'hit_id', TABLE = 'taxonomy', 
                                  as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getQueryTo", "taxonomyReportDB", function (x, id, type) {
  unlist(.getQueryTo(x, getHitId(x, id, type), 'hit_id'))
})


.getHitFrom <- .getterConstructor('hit_from', 'hsp', WHERE = 'hit_id',
                                  VAL = 'hit_id', TABLE = 'taxonomy', 
                                  as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getHitFrom", "taxonomyReportDB", function (x, id, type) {
  unlist(.getHitFrom(x, getHitId(x, id, type), 'hit_id'))
})


.getHitTo <- .getterConstructor('hit_to', 'hsp', WHERE = 'hit_id',
                                VAL = 'hit_id', TABLE = 'taxonomy', 
                                as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getHitTo", "taxonomyReportDB", function (x, id, type) {
  unlist(.getHitTo(x, getHitId(x, id, type), 'hit_id'))
})


.getQueryFrame <- .getterConstructor('query_frame', 'hsp', WHERE = 'hit_id',
                                     VAL = 'hit_id', TABLE = 'taxonomy', 
                                     as = 'character')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getQueryFrame", "taxonomyReportDB", function (x, id, type) {
  unlist(.getQueryFrame(x, getHitId(x, id, type), 'hit_id'))
})


.getHitFrame <- .getterConstructor('hit_frame', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'character')

#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getHitFrame", "taxonomyReportDB", function (x, id, type) {
  unlist(.getHitFrame(x, getHitId(x, id, type), 'hit_id'))
})


.getIdentity <- .getterConstructor('identity', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getIdentity", "taxonomyReportDB", function (x, id, type) {
  unlist(.getIdentity(x, getHitId(x, id, type), 'hit_id'))
})


.getPositive <- .getterConstructor('positive', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getPositive", "taxonomyReportDB", function (x, id, type) {
  unlist(.getPositive(x, getHitId(x, id, type), 'hit_id'))
})


.getGaps <- .getterConstructor('gaps', 'hsp', WHERE = 'hit_id',
                               VAL = 'hit_id', TABLE = 'taxonomy', 
                               as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getGaps", "taxonomyReportDB", function (x, id, type) {
  unlist(.getGaps(x, getHitId(x, id, type), 'hit_id'))
})


.getAlignLen <- .getterConstructor('align_len', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'integer')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getAlignLen", "taxonomyReportDB", function (x, id, type) {
  unlist(.getAlignLen(x, getHitId(x, id, type), 'hit_id'))
})


.getQuerySeq <- .getterConstructor('qseq', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'character')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getQuerySeq", "taxonomyReportDB", function (x, id, type) {
  unlist(.getQuerySeq(x, getHitId(x, id, type), 'hit_id'))
})


.getHitSeq <- .getterConstructor('hseq', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'character')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getHitSeq", "taxonomyReportDB", function (x, id, type) {
  unlist(.getHitSeq(x, getHitId(x, id, type), 'hit_id'))
})


.getMatch <- .getterConstructor('midline', 'hsp', WHERE = 'hit_id',
                                VAL = 'hit_id', TABLE = 'taxonomy', 
                                as = 'character')
#'@rdname taxonomyReportDB-getter
#'@export
setMethod("getMatch", "taxonomyReportDB", function (x, id, type) {
  unlist(.getMatch(x, getHitId(x, id, type), 'hit_id'))
})
