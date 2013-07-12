
# connection herstellen
blastReport <- blastReportDBConnect("../blast.test.db")
taxDB <- connectTaxonDB("/home/psehnert/daten/SPICEIII/miseq/scripts/metpipe/program/db/")

# taxonomy data.frame erstellen
db_df <- assignTaxon(1:1000, 
                     taxRanks = c("species", "genus", "tribe", "family", "order",
                                  "class", "phylum", "kingdom", "superkingdom"),
                     blast_db = blastReport, 
                     taxon_db = taxDB)




# Klassifizierer für datenbank split             
classify <- classify(db_df,'superkingdom',taxDB)

# alles in ein neues Objekt umschichten
taxReport <- createTaxonomyReportDB('taxonomy', blastReport, db_df, 0.98)
taxReport


blast64 <- blastReportDBConnect("/home/psehnert/daten/SPICEIII/miseq/sample64/blastn/sample64.db")
blast64
db_count(blast64,'query')
db_df <- assignTaxon(1:)