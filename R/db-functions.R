#'@importFrom ncbi taxonDBConnect
#'@importClassesFrom blastr blastReportDB
#'@importFrom rmisc db_query
#'@importFrom rmisc db_bulk_insert
#'@importFrom rmisc db_query
NULL

taxonomy.sql <- 'CREATE TABLE taxonomy(
                        query_id        INTEGER,
                        hit_id          INTEGER,
                        gene_id         TEXT,
                        accession       TEXT,
                        tax_id          INTEGER,
                        scientific_name	TEXT,
                        rank            TEXT,
                        PRIMARY KEY (query_id),
                        FOREIGN KEY (query_id) REFERENCES query (query_id),
                        FOREIGN KEY (hit_id) REFERENCES query (hit_id)
                );
                CREATE INDEX Ftaxonomy_query ON taxonomy (query_id);
                CREATE INDEX Ftaxonomy_hit ON taxonomy (hit_id);
                CREATE INDEX Ftaxonomy_tax ON taxonomy (tax_id);
                CREATE INDEX Ftaxaonomy_tax_hit_query ON hsp (query_id, hit_id, tax_id);
                '

#'
#'@export
connectTaxonDB <- function(path_to_db) {
  list(taxon_db=taxonDBConnect(path_to_db),geneid_db=geneidDBConnect(path_to_db))
}

db_query(blastCon,taxonomy.sql)
