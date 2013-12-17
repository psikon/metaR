#' Generate a taxon assigner
#'
#' @usage taxonomyReportDB.generator(blast_db_path, taxonomy_db_path = NULL, coverage_threshold = 0.5,
#' bitscore_tolerance = 0.98, ranks = c("species", "genus", "family", "order",
#' "class", "phylum", "kingdom", "superkingdom"))
#' @param taxon_db_path
#' @param coverage_threshold
#' @param bitscore_tolerance
#' @param ranks
#' @param log
#' @return A function that can be passed to \code{processChunks}.
#' @keywords internal
taxonomyReportDB.generator <- function(
  metadata,
  taxon_db_path = tempfile(pattern="taxondb", fileext=".db"),
  coverage_threshold = 0.5,
  bitscore_tolerance = 0.98,
  ranks = c("species", "genus", "family", "order", "class", "phylum", "kingdom", "superkingdom"),
  log = NULL
) {
  assert_that(coverage_threshold > 0, coverage_threshold <= 1)
  assert_that(bitscore_tolerance > 0, bitscore_tolerance <= 1)
  txndb <- .taxonomyReportDB(conn(blastReportDB(db_path=taxon_db_path)), metadata = metadata)
  
  chunkify <- function(log, chunkid) {
    if (!is.null(log)) {
      path <- paste0(strsplit(log, "\\.")[[1]], collapse=paste0(chunkid, "."))
      if (!file.exists(path))
        file.create(path)
      path
    } else {
      NULL
    }
  }
  
  function(..., chunkid) {
    blstdb <- list(...)[[1]]
    assert_that(is(blstdb, "blastReportDB"))
    log <- chunkify(log, chunkid)
    
    blastr:::do_log(log, "Assigning taxa\n\n")
    query_id <- getQueryID(blstdb, log=log)
    txndf <- .assignTaxa(blast_db=blstdb, query_id=query_id, coverage_threshold=coverage_threshold,
                         bitscore_tolerance=bitscore_tolerance, ranks=ranks, .unique=TRUE, log=log)
    hit_range <- paste0(attr(txndf, "hits"), collapse=",")
    
    message(" -- Updating taxonomy report database\n")
    blastr:::do_log(log, "Updating hit table\n\n")
    hitdf <- db_query(blstdb, paste0('select * from hit where hit_id in (', hit_range, ')'),  log=log)
    db_bulk_insert(txndb, 'hit', hitdf, log=log)
    
    blastr:::do_log(log, "Updating query table\n\n")
    querydf <- db_query(blstdb, paste0('select distinct query_id, query_def, query_len from query ',
                                       'where query_id in (select query_id from hit where hit_id in (',
                                       hit_range, '))'), log=log)
    db_bulk_insert(txndb, 'query', querydf, log=log)
    
    blastr:::do_log(log, "Updating hsp table\n\n")
    hspdf <- db_query(blstdb, paste0('select * from hsp where hit_id in (', hit_range, ')'), log=log)
    hspdf <- do.call('rbind', lapply(split.data.frame(hspdf, as.factor(hspdf$query_id)), function(x) {
      x[which(x$bit_score >= max(x$bit_score)*bitscore_tolerance), ]
    }))
    db_bulk_insert(txndb, 'hsp', hspdf, log=log)
    
    blastr:::do_log(log, "Updating taxonomy table\n\n")
    db_bulk_insert(txndb, 'taxonomy', txndf, log=log)
    validObject(txndb)
    invisible(NULL)
  }
}


#' @rdname taxonomyReportDB-class
#' @export
generate.TaxonomyReport <- function(blast_db_path,
                                    metadata,
                                    taxon_db_path,
                                    chunksize = 1000,
                                    coverage_threshold = 0.5,
                                    bitscore_tolerance = 0.98,
                                    ranks = c("species", "genus", "family", "order", "class", "phylum", "kingdom", "superkingdom"),
                                    ...) {
  log <- list(...)$log
  verbose <- list(...)$verbose %||% TRUE
  blstdb <- blastReportDBConnect(db_path=blast_db_path)
  streamer <- blastReportStream.generator(blstdb, chunksize, log=log)
  assigner <- taxonomyReportDB.generator(metadata,
                                         taxon_db_path, 
                                         coverage_threshold, 
                                         bitscore_tolerance, 
                                         ranks, log = log)
  processChunks(streamer, assigner, nb.parallel.jobs=1)
  txndb <- taxonomyReportDBConnect(taxon_db_path, metadata)
  txndb
}

