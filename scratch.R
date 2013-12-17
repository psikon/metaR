require(metaR)
require(rmisc)

## doku compelieren
library(devtools)
document(pkg = ".",clean = T)
Sys.setenv("PKG_CXXFLAGS" = "-std=c++11")

# connection herstellen
metadata64 <- list(
  SampleId = 64, SampleName = "Sample64",
  Location = "Jakarta Bay, Indonesia", Host = "Epinephelus fuscoguttatus",
  Environment = "free living", Description = "fecal sample"
)
blastReport <- blastReportDBConnect("../blast.test.db")
generate.TaxonomyReport(blast_db_path = "../blast.test.db",
                        metadata = metadata64,
                        taxon_db_path = "../test.db",
                        bitscore_tolerance = 0.90)
test <- taxonomyReportDBConnect("../taxonomy.db",metadata64)
biom_test <- to_biom(db_list=list(test))



