require(devtools)
require(assertthat)
require(blastr)
require(rmisc)
require(RSQLite)
require(ncbi)
require(IRanges)
require(plyr)
require(roxygen2)

# connection herstellen
con <- blastReportDBConnect("/home/psehnert/daten/SPICEIII/miseq/sample64/blast.test.db")
# ort der taxon db
taxon_db <- "/home/psehnert/daten/SPICEIII/miseq/scripts/metpipe/program/db/"

# extract tables for overview
hit <- db_query(con, "SELECT * from hit")
hsp <- db_query(con, "SELECT * from hsp")

# testvar
id <- 289
x <- con
# spaeterer Funktionskoerper


assignTaxon(query_id=200:500, taxRanks = c("species", "genus", "tribe", "family", "order",
                                           "class", "phylum", "kingdom", "superkingdom"),
            blast_db = con, taxon_db = taxon_db)




assignTaxon <- function (query_id,
                         bitscore_tolerance = 0.98,
                         coverage_threshold = 0.5,
                         taxRanks = c("species", "genus", "tribe", "family", "order",
                                      "class", "phylum", "kingdom", "superkingdom"),
                         blast_db,
                         taxon_db)
{
  ans <- lapply(query_id, .assignTaxon, bitscore_tolerance = bitscore_tolerance,
                coverage_threshold = coverage_threshold, taxRanks = taxRanks, 
                blast_db = blast_db, taxon_db = taxon_db)
  ans <- compact(ans)
  do.call('rbind', ans)
}

.assignTaxon <- function (query_id,
                          bitscore_tolerance = 0.98,
                          coverage_threshold = 0.5,
                          taxRanks = c("species", "genus", "tribe", "family", "order",
                                       "class", "phylum", "kingdom", "superkingdom"),
                          blast_db,
                          taxon_db)
{
  # filter hits for query coverage
  coverage_threshold_idx <- which(getQueryCoverage(blast_db, query_id) >= coverage_threshold)
  candidate_hits <- getHit(blast_db, query_id)[coverage_threshold_idx, ]
  cat(query_id, sep="\n")
  if (nrow(candidate_hits) >= 1) {
    # filter Hsp(s) for query Coverage
    candidate_hsps <- .getSelectedHits(blast_db, query_id, getHitID(blast_db, query_id)[coverage_threshold_idx])
    candidate_hsps <- .filterHsp(blast_db, candidate_hsps, perc = bitscore_tolerance)
    candidate_hits <- .reduceHitsFromHsps(candidate_hits, candidate_hsps)
    row.names(candidate_hits) <- NULL
    LCA(query_table=candidate_hits, taxon_db, taxRanks)
  } else {
    NULL
  }
}


