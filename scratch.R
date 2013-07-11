
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
taxReport <- createTaxonomyReportDB(blastReport,db_df)
taxReport
