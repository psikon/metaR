#' @include taxonomyReportDB-class.R
#' @importClassesFrom ncbi Taxon TaxonList
#' @importFrom ncbi taxonDB getTaxID getParentTaxID getScientificName getRank getLineage getOtherName
NULL

#' Generate an observation metadata table
#' 
#' @param txdb A \code{\linkS4class{taxonomyReportDB}} object.
#' @param otu_prefix String before numeric OTU indices.
#' @param include_qseq Include the query sequence (if available) into the output.
#' @return A \code{data.frame}.
#' @keywords internal
#' @export
taxonomy_table <- function(txdb, otu_prefix="OTU_", include_qseq=FALSE) {
  
  stmt <- paste0("
  SELECT q.query_def AS OTUID,
         t.tax_id AS taxonomy,
         h.rel_score,
         h.perc_ident",
         if (include_qseq) ", h.otu_seq" else " ",
  "FROM (query AS q LEFT OUTER JOIN taxonomy AS t USING(query_id))
       LEFT OUTER JOIN
          (SELECT query_id,
                  ROUND(CAST(MAX(score) AS FLOAT)/(SELECT MAX(score) FROM hsp), 3) AS rel_score,
                  ROUND(MAX(CAST(identity AS FLOAT)/align_len), 3) AS perc_ident,
                  REPLACE(qseq, '-', '') AS otu_seq
           FROM hsp
           GROUP BY query_id) AS h USING(query_id)
  GROUP BY query_id")
  otu_pat <- paste0('^', otu_prefix, '\\d+$')
  otu_cache <- metaR:::new_otu_cache()
  generate.row <- metaR:::row.generator()
  tbl <- db_query(txdb, stmt)
  ## Attempt to sort OTUs
  if (all(grepl(otu_pat, tbl$OTUID))) {
    tbl <- tbl[order(as.integer(sub(otu_prefix, "", tbl$OTUID))), ]
  } else {
    warning("OTU prefix '", otu_prefix, "' does not match. Will return OTUs unsorted.")
  }
  tbl$taxonomy <- foreach(taxid = iter(tbl$taxonomy), .combine="c") %do% {
    if (!otu_cache$has_key(taxid)) {
      otu_row <- paste0(c('Root', generate.row(taxid)), collapse=';')
      otu_cache$set(taxid, otu_row)
    } else {
      otu_row <- otu_cache$get(taxid)
    }
    otu_row
  }
  names(tbl)[1] <- "#OTUID"
  tbl
}

#' @export
#' @rdname countTaxa-methods
setMethod('countTaxa', 'taxonomyReportDB', function(x, id = NULL) {
  if (is.null(id)) {
    stmt <- "SELECT tax_id, COUNT(tax_id) FROM taxonomy GROUP by tax_id"
  } else {
    r <- range(id)
    assert_that(r[1] > 0)
    tax_range <- paste("tax_id >=", r[1], "AND tax_id <=", r[2])
    stmt <- paste("SELECT tax_id, COUNT(tax_id) FROM taxonomy WHERE", tax_range, "GROUP BY tax_id")                  
  }
  df <- db_query(x, stmt)
  colnames(df) <- c('tax_id', 'count')
  df
})

.getTaxID <- .getterConstructor('tax_id', 'taxonomy', as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getTaxID", "taxonomyReportDB", function(x, id, type) {
  unlist(.getTaxID(x, id, type))
})

#' @export
#' @rdname taxonomyReportDB-methods
setMethod('getParentTaxID','taxonomyReportDB', function(x, id, type) {
  getParentTaxID(getTaxon(x, id, type))
})

.getScientificName <- .getterConstructor ('scientific_name', 'taxonomy', as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getScientificName", "taxonomyReportDB", function(x, id, type) {
  unlist(.getScientificName(x, id, type))
})

.getRank <- .getterConstructor ('rank', 'taxonomy', as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getRank", "taxonomyReportDB", function(x, id, type) {
  unlist(.getRank(x, id, type))
})

#' @export
#' @rdname taxonomyReportDB-methods
setMethod('getTaxon','taxonomyReportDB', function(x, id, type) {
  taxonDB(getTaxID(x, id, type))
})

#' @export
#' @rdname taxonomyReportDB-methods
setMethod('getLineage','taxonomyReportDB', function(x, id, type) {
  getLineage(getTaxon(x, id, type))
})

#' @export
#' @rdname taxonomyReportDB-methods
setMethod('getOtherName','taxonomyReportDB', function(x, id, type) {
  getOtherName(getTaxon(x, id, type))
})

.getQueryID <- .getterConstructor('query_id', 'taxonomy', as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getQueryID", "taxonomyReportDB", function(x, id, type) {
  unlist(.getQueryID(x, id, type))
})

.getHitID <- .getterConstructor ('hit_id', 'taxonomy', as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getHitID", 'taxonomyReportDB', function(x, id, type) {
  unlist(.getHitID(x, id, type))
})

.getGeneID <- .getterConstructor ('gene_id', 'taxonomy', as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getGeneID", "taxonomyReportDB", function(x, id, type) {
  unlist(.getGeneID(x, id, type))
})


.getAccession <- .getterConstructor('accession', 'taxonomy', as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getAccession", "taxonomyReportDB", function(x, id, type) {
  unlist(.getAccession(x, id, type))
})


.getQueryDef <- .getterConstructor('query_def', 'query', WHERE = 'query_id',
                                   VAL = 'query_id', TABLE = 'taxonomy', 
                                   as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getQueryDef", "taxonomyReportDB", function(x, id, type) {
  unlist(.getQueryDef(x, getQueryID(x, id, type ), 'query_id'))
})


.getQueryLen <- .getterConstructor('query_len', 'query', WHERE = 'query_id',
                                   VAL = 'query_id', TABLE = 'taxonomy', 
                                   as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getQueryLen", "taxonomyReportDB", function(x, id, type) {
  unlist(.getQueryLen(x, getQueryID(x, id, type), 'query_id'))
})


.getHitNum <- .getterConstructor('hit_num', 'hit', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getHitNum", "taxonomyReportDB", function(x, id, type) {
  unlist(.getHitNum(x, getHitID(x, id, type), 'hit_id'))
})


.getDefinition <- .getterConstructor ('definition', 'hit', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getDefinition", "taxonomyReportDB", function(x, id, type) {
  unlist(.getDefinition(x, getHitID(x, id, type), 'hit_id'))
})


.getHitLen <- .getterConstructor ('length', 'hit', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getHitLen", "taxonomyReportDB", function(x, id, type) {
  unlist(.getHitLen(x, getHitID(x, id, type), 'hit_id'))
})

.getHspID <- .getterConstructor('hsp_id', 'hsp', WHERE = 'hit_id',
                                VAL = 'hit_id', TABLE = 'taxonomy', 
                                as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getHspID", "taxonomyReportDB", function(x, id, type) {
  unlist(.getHspID(x, getHitID(x, id, type), 'hit_id'))
})


.getHspNum <- .getterConstructor('hsp_num', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getHspNum", "taxonomyReportDB", function(x, id, type) {
  unlist(.getHspNum(x, getHitID(x, id, type), 'hit_id'))
})


.getBitscore <- .getterConstructor('bit_score', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'numeric')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getBitscore", "taxonomyReportDB", function(x, id, type) {
  unlist(.getBitscore(x, getHitID(x, id, type), 'hit_id'))
})


.getScore <- .getterConstructor('score', 'hsp', WHERE = 'hit_id',
                                VAL = 'hit_id', TABLE = 'taxonomy', 
                                as = 'numeric')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getScore", "taxonomyReportDB", function(x, id, type) {
  unlist(.getScore(x, getHitID(x, id, type), 'hit_id'))
})


.getEvalue <- .getterConstructor('evalue', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'numeric')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getEvalue", "taxonomyReportDB", function(x, id, type) {
  unlist(.getEvalue(x, getHitID(x, id, type), 'hit_id'))
})

.getQueryFrom <- .getterConstructor('query_from', 'hsp', WHERE = 'hit_id',
                                    VAL = 'hit_id', TABLE = 'taxonomy', 
                                    as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getQueryFrom", "taxonomyReportDB", function(x, id, type) {
  unlist(.getQueryFrom(x, getHitID(x, id, type), 'hit_id'))
})


.getQueryTo <- .getterConstructor('query_to', 'hsp', WHERE = 'hit_id',
                                  VAL = 'hit_id', TABLE = 'taxonomy', 
                                  as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getQueryTo", "taxonomyReportDB", function(x, id, type) {
  unlist(.getQueryTo(x, getHitID(x, id, type), 'hit_id'))
})


.getHitFrom <- .getterConstructor('hit_from', 'hsp', WHERE = 'hit_id',
                                  VAL = 'hit_id', TABLE = 'taxonomy', 
                                  as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getHitFrom", "taxonomyReportDB", function(x, id, type) {
  unlist(.getHitFrom(x, getHitID(x, id, type), 'hit_id'))
})


.getHitTo <- .getterConstructor('hit_to', 'hsp', WHERE = 'hit_id',
                                VAL = 'hit_id', TABLE = 'taxonomy', 
                                as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getHitTo", "taxonomyReportDB", function(x, id, type) {
  unlist(.getHitTo(x, getHitID(x, id, type), 'hit_id'))
})


.getQueryFrame <- .getterConstructor('query_frame', 'hsp', WHERE = 'hit_id',
                                     VAL = 'hit_id', TABLE = 'taxonomy', 
                                     as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getQueryFrame", "taxonomyReportDB", function(x, id, type) {
  unlist(.getQueryFrame(x, getHitID(x, id, type), 'hit_id'))
})


.getHitFrame <- .getterConstructor('hit_frame', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getHitFrame", "taxonomyReportDB", function(x, id, type) {
  unlist(.getHitFrame(x, getHitID(x, id, type), 'hit_id'))
})


.getIdentity <- .getterConstructor('identity', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getIdentity", "taxonomyReportDB", function(x, id, type) {
  unlist(.getIdentity(x, getHitID(x, id, type), 'hit_id'))
})


.getPositive <- .getterConstructor('positive', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getPositive", "taxonomyReportDB", function(x, id, type) {
  unlist(.getPositive(x, getHitID(x, id, type), 'hit_id'))
})


.getGaps <- .getterConstructor('gaps', 'hsp', WHERE = 'hit_id',
                               VAL = 'hit_id', TABLE = 'taxonomy', 
                               as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getGaps", "taxonomyReportDB", function(x, id, type) {
  unlist(.getGaps(x, getHitID(x, id, type), 'hit_id'))
})


.getAlignLen <- .getterConstructor('align_len', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'integer')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getAlignLen", "taxonomyReportDB", function(x, id, type) {
  unlist(.getAlignLen(x, getHitID(x, id, type), 'hit_id'))
})


.getQuerySeq <- .getterConstructor('qseq', 'hsp', WHERE = 'hit_id',
                                   VAL = 'hit_id', TABLE = 'taxonomy', 
                                   as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getQuerySeq", "taxonomyReportDB", function(x, id, type) {
  unlist(.getQuerySeq(x, getHitID(x, id, type), 'hit_id'))
})


.getHitSeq <- .getterConstructor('hseq', 'hsp', WHERE = 'hit_id',
                                 VAL = 'hit_id', TABLE = 'taxonomy', 
                                 as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getHitSeq", "taxonomyReportDB", function(x, id, type) {
  unlist(.getHitSeq(x, getHitID(x, id, type), 'hit_id'))
})


.getMatch <- .getterConstructor('midline', 'hsp', WHERE = 'hit_id',
                                VAL = 'hit_id', TABLE = 'taxonomy', 
                                as = 'character')
#' @export
#' @rdname taxonomyReportDB-methods
setMethod("getMatch", "taxonomyReportDB", function(x, id, type) {
  unlist(.getMatch(x, getHitID(x, id, type), 'hit_id'))
})
