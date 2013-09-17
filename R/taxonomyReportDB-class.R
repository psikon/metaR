#'@import methods
#'@importFrom RSQLite dbListTables
#'@importFrom RSQLite dbListFields
#'@importClassesFrom RSQLite SQLiteConnection
#'@importClassesFrom RSQLite SQLiteObject
#'@importClassesFrom RSQLite dbObjectId
#'@importClassesFrom DBI DBIConnection
#'@importClassesFrom DBI DBIObject
#'@importClassesFrom blastr blastReportDB
#'@importFrom blastr blastReportDBConnect
#'@importFrom rmisc db_count
#'@importFrom rmisc db_connect
#'@importFrom rmisc db_create
#'@importFrom assertthat assert_that
#'@importFrom assertthat is.readable
NULL

.valid.taxonomyReportDB <- function (object) {
  errors <- character()
  if (!all(c("hit", "hsp", "query","taxonomy") %in% dbListTables(object))) {
    errors <- c(errors, "Table missing from taxonomyReportDB'\n")
  }
  if (!all(c("query_id", "hit_id", "gene_id", "accession", "tax_id",
             "scientific_name", "rank") 
           %in% dbListFields(object, "taxonomy"))) {
    errors <- c(errors, "Field missing from table 'taxonomy'")
  }
  if (length(errors) == 0L) TRUE else errors
}

#' taxonomyReportDB-class
#' 
#' \sQuote{\code{taxonomyReportDB}} is an S4 class that represents a connection
#' to an SQLite database holding blast records and taxonomical classifications
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
#'    \item query_id         INTEGER
#'    \item hit_id           INTEGER     Primary key
#'    \item gene_id          TEXT   
#'    \item accession        TEXT
#'    \item tax_id           TEXT
#'    \item scientific_name  TEXT
#'    \item rank             TEXT
#' }
#'      
#' @name taxonomyReportDB-class
#' @rdname taxonomyReportDB-class
#' @exportClass taxonomyReportDB
setClass('taxonomyReportDB', contains='blastReportDB',validity=.valid.taxonomyReportDB)

#' @aliases show,taxonomyReportDB-method
#' @rdname taxonomyReportDB-class
setMethod('show', 'taxonomyReportDB',
          function (object) {
            showme <- sprintf('%s object with:\n| %s querys | %s hits | %s hsps | %s taxonomies |',
                              sQuote(class(object)), 
                              db_count(object, "query"),
                              db_count(object, "hit"),
                              db_count(object, "hsp"),
                              db_count(object, "taxonomy"))
            cat(showme, sep="\n")
          })


#' @usage taxonomyReportDBConnect(db_path)
#' @return A \code{\linkS4class{taxonomyReportDB}} object.
#' @rdname taxonomyReportDB-class
#' @export
taxonomyReportDBConnect <- function (db_path) {
  assert_that(is.readable(db_path))
  con <- db_connect(db_path)
  new("taxonomyReportDB", con)
}

#' @usage createTaxonomyReportDB(blast_db,taxonomy_table)
#' @rdname taxonomyReportDB-class
#' @export
createTaxonomyReportDB <- function (db_name,blastReportDB,taxonomy_table, bitscore_tolerance) {
  # create a new database with blastReportDB Schema
  con <- db_create(db_name, dbSchema = blastr:::blast_db.sql)
  # reduce and insert the hit table 
  message('Creating hit table ')
  updateTable(con,'hit', do.call(rbind, llply(taxonomy_table$hit_id, 
                .fun = function(x) {
                  db_query(blastReportDB, paste("SELECT * FROM hit WHERE hit_id =", x))
                }, .progress = "text")))
  # reduce and insert the query table
  message('Creating query table ')
  updateTable(con, 'query', unique(do.call(rbind, llply(taxonomy_table$hit_id,
                .fun = function(x) {
                  db_query(blastReportDB,paste("SELECT * FROM query 
                          WHERE query_id = (SELECT query_id FROM hit 
                                            WHERE hit_id =", x, ")"))
                  }, .progress = "text"))))
  # and finally reduce and insert the hsp table
  message('Creating hsp table ')
  updateTable(con, 'hsp', do.call(rbind, llply(taxonomy_table$hit_id,
                .fun = function(x) {
                  # need to filter again towards bitscore_tolerance
                  .filterHsp(db_query(blastReportDB, 
                                      paste("SELECT * FROM hsp 
                                             WHERE hit_id = (SELECT hit_id FROM hit 
                                                             WHERE hit_id =", x, ")")),
                             perc = bitscore_tolerance)
  }, .progress = "text")))
  message('Creating taxonomy table ')
  createTable(db = con, tbl = 'taxonomy',tbl_scheme = taxonomy_create.sql)
  updateTable(db = con, tbl = 'taxonomy', df = taxonomy_table)
  new('taxonomyReportDB',con)
}

#insertNewDataSet <- function(taxonomyReportDB)
