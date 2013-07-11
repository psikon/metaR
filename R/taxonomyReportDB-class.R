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
#'    \item query_id        INTEGER
#'    \item hit_id          INTEGER     Primary key
#'    \item gene_id         TEXT   
#'    \item accession       TEXT
#'    \item tax_id          TEXT
#'    \item scintific_name  TEXT
#'    \item rank            TEXT
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
            n <- db_count(object, "taxonomy")
            showme <- sprintf('%s object with %s query rows',
                              sQuote(class(object)), n)
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

#' @usage taxonomyReportDBConnect(blast_db,taxonomy_table)
#' @return A \code{\linkS4class{taxonomyReportDB}} object.
#' @rdname taxonomyReportDB-class
#' @export
createTaxonomyReportDB <- function (blast_db,taxonomy_table) {
  createTaxonomyTable(blast_db)
  updateTaxonomyTable(blast_db,taxonomy_table)
  new("taxonomyReportDB",blast_db)
}


setGeneric("getQueryId", function(x, ...) standardGeneric("getQueryId"))

.getQueryId <- getterConstructor ('query_id', 'taxonomy', WHERE='tax_id',as='integer')
setMethod("getQueryId", "taxonomyReportDB", function (x,id) {
  unlist(.getQueryId(x,id))
})

setGeneric("getHitId", function(x, ...) standardGeneric("getHitId"))

.getHitId <- getterConstructor ('hit_id', 'taxonomy', WHERE='tax_id',as='integer')
setMethod("getHitId", signature='taxonomyReportDB', function (x,id) {
  unlist(.getHitId(x,id))
})

setGeneric("getGeneId", function(x, ...) standardGeneric("getGeneId"))

.getGeneId <- getterConstructor ('gene_id', 'taxonomy', WHERE='tax_id')
setMethod("getGeneId", "taxonomyReportDB", function (x,id) {
  unlist(.getGeneId(x,id))
})
setGeneric("getAccession", function(x, ...) standardGeneric("getAccession"))

.getAccession <- getterConstructor ('accession', 'taxonomy', WHERE='tax_id')
setMethod("getAccession", "taxonomyReportDB", function (x,id) {
  unlist(.getAccession(x,id))
})
setGeneric("getScientificName", function(x, ...) standardGeneric("getScientificName"))

.getScientificName <- getterConstructor ('scientific_name', 'taxonomy', WHERE='tax_id')
setMethod("getScientificName", "taxonomyReportDB", function (x,id) {
  unlist(.getScientificName(x,id))
})

setGeneric("getRank", function(x, ...) standardGeneric("getRank"))

.getRank <- getterConstructor ('rank', 'taxonomy', WHERE='tax_id')
setMethod("getRank", "taxonomyReportDB", function (x,id) {
  unlist(.getRank(x,id))
})

setGeneric("getQueryDef", function(x, ...) standardGeneric("getQueryDef"))

.getQueryDef <- getterConstructor ('query_def', 'query', WHERE='query_id',
                                    FROM2='taxonomy',VAL='tax_id',as='character')
setMethod("getQueryDef", "taxonomyReportDB", function (x,id) {
  unlist(.getQueryDef(x,id))
})