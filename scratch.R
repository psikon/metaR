require(metaR)
require(rmisc)

## doku compelieren
library(devtools)
document(pkg = ".",clean = T)
Sys.setenv("PKG_CXXFLAGS" = "-std=c++11")

# connection herstellen
blastReport <- blastReportDBConnect("../blast.test.db")
#taxDB <- connectTaxonDB("/home/psehnert/daten/metagenomics/scripts/metpipe/program/db")
taxDB <- connectTaxonDB("../")

taxonomy <- taxonomyReportDBConnect(db_path="taxonomy.db")



ncbi::.ncbi_taxon_ranks()
