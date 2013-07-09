#'@importFrom ncbi taxonDBConnect
#'@importClassesFrom blastr blastReportDB
#'@importFrom rmisc db_query
#'@importFrom rmisc db_bulk_insert
#'@importFrom rmisc has_tables
NULL

taxonomy_create.sql <- 'CREATE TABLE taxonomy(
                        query_id        INTEGER,
                        hit_id          INTEGER,
                        gene_id         TEXT,
                        accession       TEXT,
                        tax_id          INTEGER,
                        scientific_name	TEXT,
                        rank            TEXT,
                        superkingdom    TEXT,
                        PRIMARY KEY (hit_id),
                        FOREIGN KEY (query_id) REFERENCES query (query_id),
                        FOREIGN KEY (hit_id) REFERENCES query (hit_id)
                );
                CREATE INDEX Ftaxonomy_query ON taxonomy (query_id);
                CREATE INDEX Ftaxonomy_hit ON taxonomy (hit_id);
                CREATE INDEX Ftaxonomy_tax ON taxonomy (tax_id);
                CREATE INDEX Ftaxaonomy_tax_hit_query ON hsp (query_id, hit_id, tax_id);
                '

taxonomy_drop.sql <- 'DROP TABLE taxonomy;'

#'establish a connecton to taxonomy DB
#'
#'@description establish a list of connection objects to the local installations of the
#'taxon.db and geneid.db files
#'
#'@param path_to_db location of the taxonomy db
#'
#'@rdname db-functions
#'@export
connectTaxonDB <- function(path_to_db) {
  list(taxon_db=taxonDBConnect(path_to_db),geneid_db=geneidDBConnect(path_to_db))
}

#'extend the blast database with a taxonomy table
#'
#'@description the orignial blast database generated from a XML file will be extended by
#'a taxonomy table containing informations about the taxonomical classification for 
#'a specific query. The table contains the slots:
#'\itemize{
#'  \item{query_id}{identifier for the query}
#'  \item{hit_id}{identifier for the best hit used for classification}
#'  \item{gene_id}{blast identifier for a sequence}
#'  \item{accession}{blast identifier for a sequence}
#'  \item{tax_id}{identifier in the ncbi taxonomy}
#'  \item{scientific_name}{name in the ncbi taxonomy}
#'  \item{rank}{rank in the ncbi taxonomy}
#'  \item{superkingdom}{Bacteria, Eukaryota or unclassified}
#'  }
#'  
#'@param blastDB blastReportDB object
#'
#'@rdname db-functions
#'@export
createTaxonomyTable <- function(blastDB) {
  if (blastDB %has_tables% 'taxonomy') {
    db_query(blastDB,taxonomy_drop.sql)
  }
  db_query(blastDB,taxonomy_create.sql)
}

#'update taxonomy table of blastReportDB
#'
#'@param blastDB
#'@param df 
#'
#'@rdname db-functions
#'@export
updateTaxonomyTable <- function(blastDB,df) {
  db_bulk_insert(blastDB,"taxonomy",df)
}

dropDatabaseBySuperKingdom <- function(blastDB,classifier){
  if (!tolower(classifier) %in% c('bacteria','eukaryota','unclassified')) {
    stop(paste(classifier," must be in 'bacteria','eukaryota','unclassified'"))
  }
  
  
}

