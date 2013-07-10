
# connection herstellen
blastCon <- blastReportDBConnect("blast.test.db")
taxCon <- connectTaxonDB("../")
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
# doku
# 
createTaxonomyTable(blastDB=blastCon)
updateTaxonomyTable(blastCon,db_df)
