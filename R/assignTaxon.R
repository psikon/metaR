#' assign Taxon(s) to a blast query
#'
#'@description This algorithm filter the hits of a query under a number of conditions, to
#'remove false annotations from the query.
#'
#'First all hit(s) of a blast query with a specific query coverage (default: >=50%) will be selected.
#'The corresponding hsp(s) are determined and a tolerance, based on the bitscore will be calculated 
#'(default: best bitscore * 98%= tolerance). All hsp(s) within this tollerance will be selected 
#'for further processing and the rest discarded. Afterwards the number of hit(s) will be
#'adjusted. For these remaining hit(s) the lca (least common ancestor) will be seeked.
#'
#'@seealso \code{\link{LCA}}
#'
#'@param query_id 1:n query_id(s) from blastReportDB object 
#'@param bitscore_tolerance tolerance value for the selection of hsp(s)
#'@param coverage_threshold threshold for the selection of hit(s)
#'@param taxRanks vector of defined taxonomic ranks
#'@param blast_db blastReportDB object
#'@param taxon_db taxon_db object
#'
#'@return data.frame 
#'
#'@importFrom rmisc compact
#'@importFrom blastr getQueryCoverage
#'
#'@rdname taxonomy
#'@export

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