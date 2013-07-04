#'@importFrom ncbi taxonDBConnect
#'@importClassesFrom blastr blastReportDB
#'@importFrom rmisc db_query
#'@importFrom rmisc db_bulk_insert
#'@importFrom rmisc db_query
NULL

#'
#'@export
connectTaxonDB <- function(path_to_db) {
  list(taxon_db=taxonDBConnect(path_to_db),geneid_db=geneidDBConnect(path_to_db))
}
