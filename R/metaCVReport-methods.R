#' find similarities between Blast and MetaCV results
#' 
#' @description The results of the two programms will be analyised by \code{tax_id}, 
#' \code{rank} and \code{scientific_name}, basing on the \code{query_def} field 
#' 
#' @param taxonomyReportDB
#' @param metaCVReport
#' @param taxonDB  connection to taxonomy DB
#' 
#' @return data.frame
#' 
#' @rdname compare MetaCVwithBlast
#' @export
compareMetaCVwithBlast <- function(taxonomyReportDB, metaCVReport, taxonDB) {
  # extract query_id(s) for lapply
  query_id <- db_query(taxonomyReportDB, "SELECT query_id from query", 1L)
  cmp <- as.data.frame(do.call(rbind, lapply(query_id, function(x) {
    # get the actual query_def
    taxonomyReport_def <-  gsub(" .*$", "", getQueryDef(taxonomyReportDB, x, 'query_id'))
    # find occurance index in metaCVReport
    idx <- which(metaCVReport[['query_def']] %in% taxonomyReport_def)
    if (length(idx) > 0) {
      # adjust the metaCVReport taxon to the used parameters and create data.frame 
      metaTaxon <- .resolveNoRank(taxonDB(metaCVReport[idx, 'tax_id'], taxDB$taxon_db), taxonDB)
      cbind(query_dev = metaCVReport[idx, 'query_def'],
            metaCV_tax_id = getTaxID(metaTaxon),
            metaCV_scientific_name = getScientificName(metaTaxon),
            metaCV_rank = getRank(metaTaxon),
            blast_tax_id = getTaxID(taxonomyReportDB, x, 'query_id'),
            blast_scientific_name = getScientificName(taxonomyReportDB, x, 'query_id'),
            blast_rank = getRank(taxonomyReportDB, x, 'query_id'))
    }
  })))
}

setMethod('countTaxa', 'metaCVReport', function(x, id = NULL) {
  if (is.null(id)) {
    df <- ddply(x, .(tax_id), summarise, count = length(tax_id))
  } else {
    df <- ddply(x[which(x['tax_id'] == id),], .(tax_id), summarise, count = length(tax_id))
  }
  df
})

setMethod('countTaxa', 'taxonomyReportDB', function(x, id = NULL){
  if (is.null(id)) {
    df <- as.data.frame(db_query(x,
                                 "SELECT tax_id, COUNT(tax_id) FROM taxonomy GROUP by tax_id"))
    colnames(df) <- c('tax_id', 'count')
  } else {
    df <- as.data.frame(db_query(x, 
                                 paste("SELECT tax_id, COUNT(tax_id) 
                                       FROM taxonomy WHERE tax_id =", id,
                                       "GROUP BY tax_id")))
    colnames(df)<- c('tax_id', 'count')                    
  }
  df
})
