
# connection herstellen
blastCon <- blastReportDBConnect("/home/psehnert/daten/SPICEIII/miseq/sample64/blast.test.db")
taxCon <- connectTaxonDB("/home/psehnert/daten/SPICEIII/miseq/scripts/metpipe/program/db/")
# ort der taxon db


# extract tables for overview
hit <- db_query(blastCon, "SELECT * from hit")
hsp <- db_query(blastCon, "SELECT * from hsp")
taxonomy <- db_query(blastCon, "SELECT * from taxonomy")

db_df <- assignTaxon(1:1000, 
                     taxRanks = c("species", "genus", "tribe", "family", "order",
                                  "class", "phylum", "kingdom", "superkingdom"),
                     blast_db = blastCon, 
                     taxon_db = taxCon)

## Funktionen schreiben 
# db_bulk_insert mit reset_at
# doku
# 
createTaxonomyTable(blastDB=blastCon)
updateTaxonomyTable(blastCon,db_df)
