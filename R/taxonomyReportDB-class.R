#' @include utils.R
#' @include db-functions.R
#' @import methods
#' @import blastr
#' @importFrom RSQLite dbListTables dbListFields dbGetQuery
#' @importFrom assertthat assert_that is.readable
NULL


#' taxonomyReportDB-class
#' 
#' \sQuote{\code{taxonomyReportDB}} is an S4 class that represents a connection
#' to an SQLite database holding blast records and taxonomic classifications
#' organised in four tables:
#' 
#' \bold{query} with fields:
#' 
#' \itemize{
#'    \item query_id     INTEGER    Primary key
#'    \item query_def    TEXT
#'    \item query_len    INTEGER
#' }
#' 
#' \bold{hit} with fields:
#' 
#' \itemize{
#'    \item query_id     INTEGER
#'    \item hit_id       INTEGER    Primary key
#'    \item hit_num      INTEGER
#'    \item gene_id      TEXT
#'    \item accession    TEXT
#'    \item definition   TEXT
#'    \item length       INTEGER
#' }
#' 
#' \bold{hsp} with fields:
#'
#' \itemize{
#'    \item query_id     INTEGER
#'    \item hit_id       INTEGER    
#'    \item hsp_id       INTEGER    Primary key
#'    \item hsp_num      INTEGER
#'    \item bit_score    FLOAT
#'    \item score        INTEGER
#'    \item evalue       FLOAT
#'    \item query_from   INTEGER
#'    \item query_to     INTEGER
#'    \item hit_from     INTEGER
#'    \item hit_to       INTEGER
#'    \item query_frame  INTEGER
#'    \item query_frame  INTEGER
#'    \item identity     INTEGER
#'    \item positive     INTEGER
#'    \item gaps         INTEGER
#'    \item align_len    INTEGER
#'    \item qseq         TEXT
#'    \item hseq         TEXT
#'    \item midline      TEXT
#' }
#' 
#' \bold{taxonomy} with fields:
#'
#' \itemize{
#'    \item query_id         INTEGE^R    Primary key
#'    \item tax_id           TEXT
#'    \item scientific_name  TEXT
#'    \item rank             TEXT
#' }
#'      
#' @name taxonomyReportDB-class
#' @rdname taxonomyReportDB-class
#' @exportClass taxonomyReportDB
NULL
.taxonomyReportDB <- setRefClass(
  Class='taxonomyReportDB',
  contains='blastReportDB',
  methods=list(
    initialize=function(...) {
      callSuper(...)
      if (!.con %has_tables% "taxonomy") {
        createTable(.con, 'taxonomy', taxonomy_db.sql())
      }
    })
)

setValidity('taxonomyReportDB', function(object) {
  errors <- character()
  if (length(dbListTables(conn(object))) == 0L) {
    return("No tables in 'taxonomyReportDB'")
  }
  if (!all(c("hit", "hsp", "query", "taxonomy") %in% dbListTables(conn(object)))) {
    errors <- c(errors, "Table missing from 'taxonomyReportDB'\n")
  }
  if (!all(c("query_id", "tax_id", "scientific_name", "rank") %in% dbListFields(conn(object), "taxonomy"))) {
    errors <- c(errors, "Fields missing from table 'taxonomy'")
  }
  if (length(errors) == 0L) TRUE else errors
}) 


#' @aliases show,taxonomyReportDB-method
#' @rdname taxonomyReportDB-class
setMethod('show', 'taxonomyReportDB',
          function(object) {
            showme <- sprintf('%s connection object with:\n| %s queries | %s hits | %s hsps | %s taxa |',
                              sQuote(class(object)), 
                              db_count(object, "query"),
                              db_count(object, "hit"),
                              db_count(object, "hsp"),
                              db_count(object, "taxonomy"))
            cat(showme, sep="\n")
          })


#' @usage taxonomyReportDB(blast_db, taxon_db_path = "", coverage_threshold = 0.5,
#' bitscore_tolerance = 0.98, ranks = c("species", "genus", "tribe", "family", "order",
#' "class", "phylum", "kingdom", "superkingdom"), .progress = "text")
#' @param blst_db A \code{\link[blastr]{blastReportDB}} object.
#' @param taxon_db_path Path to a \code{taxonomyReportDB}. This can be an existing database file
#' where new rows will be appended, or a database is created from scratch.
#' @param coverage_threshold threshold for the selection of hits
#' @param bitscore_tolerance tolerance value for the selection of hsps.
#' @param ranks vector of taxonomic ranks along which the least common ancestor.
#' is determined.
#' @param .progress
#' @rdname taxonomyReportDB-class
#' @export
taxonomyReportDB <- function(
  blast_db_path,
  taxon_db_path = "",
  coverage_threshold = 0.5,
  bitscore_tolerance = 0.98,
  ranks = c("species", "genus", "tribe", "family", "order", "class", "phylum", "kingdom", "superkingdom")
) {
  # try to connect to an existing taxonomy database or, if this fails,
  # create a blastReportDB and attach the 'taxonomy' table to it  
  if (is.null(tryCatch(txndb <- taxonomyReportDBConnect(taxon_db_path), error=function(e) NULL))) {
    txndb <- .taxonomyReportDB(conn(blastReportDB(db_path = taxon_db_path)))
  }
  if (missing(blast_db_path)) {
    return(txndb)
  }
  blstdb <- blastReportDBConnect(blast_db_path)
  ## Assign Taxa to a Blast queries
  message(' -- Assigning Taxa')
  query_id <- getQueryID(blstdb)
  txndf <- .assignTaxa(blast_db=blstdb, query_id=query_id, coverage_threshold=coverage_threshold,
                       bitscore_tolerance=bitscore_tolerance, ranks=ranks, .unique=TRUE)
  hit_range <- paste0(attr(txndf, "hits"), collapse=",")
  
  message(" -- Updating hit table")
  hitdf <- db_query(blstdb, paste0('select * from hit where hit_id in (', hit_range, ')'))
  db_bulk_insert(txndb, 'hit', hitdf)
  
  message(' -- Updating query table')
  querydf <- db_query(blstdb, paste0('select distinct query_id, query_def, query_len from query ',
                                     'where query_id in (select query_id from hit where hit_id in (',
                                     hit_range, '))'))
  db_bulk_insert(txndb, 'query', querydf)

  message(' -- Updating hsp table')
  hspdf <- db_query(blstdb, paste0('select * from hsp where hit_id in (', hit_range, ')'))
  hspdf <- do.call('rbind', lapply(split.data.frame(hspdf, as.factor(hspdf$query_id)), function(x) {
    x[which(x$bit_score >= max(x$bit_score)*bitscore_tolerance), ]
  }))
  db_bulk_insert(txndb, 'hsp', hspdf)

  message(' -- Updating taxonomy table')
  db_bulk_insert(txndb, 'taxonomy', txndf)
  validObject(txndb)
  txndb
}


#' @usage taxonomyReportDBConnect(db_path)
#' @return A \code{\linkS4class{taxonomyReportDB}} object.
#' @rdname taxonomyReportDB-class
#' @export
taxonomyReportDBConnect <- function(db_path) {
  if (db_path == ":memory:") {
    stop("Cannot connect to an in-memory database", call.=FALSE)
  }
  if (db_path == "") {
    stop("Cannot connect to a temporary database", call.=FALSE)
  }
  db <- .taxonomyReportDB(db_connect(db_path))
  validObject(db)
  db
}


#insertNewDataSet <- function(taxonomyReportDB)
