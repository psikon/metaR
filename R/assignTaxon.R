##
##' @keywords internal
.fetch_hits <- function(blast_db, id, idx = NULL, what = "*", ...) {
  range <- paste0(id, collapse = ",")
  res <- blastr::db_query(blast_db , paste("select", what, "from hit where query_id in (", range, ")"), ...)
  sres <- split.data.frame(res, as.factor(res$query_id))
  if (!is.null(idx)) {
    assert_that(length(sres) == length(idx))
    sres <- .mapply(function(x, i) x[i, ], list(x = sres, i = idx), NULL)
  }
  sres[vapply(sres, nrow, 1) != 0]
}


##' @keywords internal
## returns all hsps matching a specific query_id and hit_id or NA
.filter_hsps <- function(blast_db, hits, bitscore_tolerance, ...) {
  lapply(hits, function(h) {
    qid <- unique(h$query_id)
    hid <- paste0(h$hit_id, collapse = ",")
    stmt <- paste0("select query_id, hit_id, bit_score from hsp where query_id = ", qid, " AND hit_id in (", hid, ") ")
    hsps <- blastr::db_query(blast_db, stmt, ...)
    # get all hsps for which bit_score >= tolerance threshold
    hsps <- hsps[hsps$bit_score >= max(hsps$bit_score)*bitscore_tolerance, ]
    # make sure they are sorted by bit_score
    hsps <- arrange(hsps, desc(hsps$bit_score))
    # remove duplicates
    hsps[!duplicated(hsps$hit_id), ]
  })
}

.filter_best_hsps <- function(blast_db, hits) {
  lapply(hits, function(h) {
    qid <- unique(h$query_id)
    hid <- paste0(h$hit_id, collapse = ",")
    stmt <- paste0("SELECT query_id, hit_id, bit_score FROM hsp WHERE query_id = ", qid, 
                   " AND bit_score = (SELECT MAX(bit_score) FROM hsp WHERE query_id = ",qid,")")
    
    hsps <- blastr::db_query(blast_db, stmt)
    # make sure they are sorted by bit_score
    hsps <- arrange(hsps, desc(hsps$bit_score))
    # remove duplicates
    hsps[!duplicated(hsps$hit_id), ]
  })
}

##' @keywords internal
## after filtering of the hsp(s) the hit(s) have to be adjusted to prevent hit(s) without hsp(s)
.compact_hits <- function(hits, hsps) {
  hits <- do.call('rbind', hits)
  hsps <- do.call('rbind', hsps)
  hits <- hits[hits$hit_id %in% hsps$hit_id, ]
  row.names(hits) <- NULL
  unname(split.data.frame(hits, as.factor(hits$query_id)))
}

.assignBestTaxa <- function(blast_db, query_id,
                            ranks = c("species", "genus", "family", "order", "class", "phylum", "kingdom", "superkingdom")) {
  message(" -- Filtering by best hit")
  hits <- .fetch_hits(conn(blast_db), id = query_id)
  hsps <- .filter_best_hsps(conn(blast_db), hits) 
  hits <- .compact_hits(hits, hsps)
  message(" -- Searching for taxonomy of best hit")
  
}

##' @keywords internal
.assignTaxa <- function(
  blast_db,
  query_id,
  coverage_threshold = 0.5,
  bitscore_tolerance = 0.98,
  ranks = c("species", "genus", "family", "order", "class", "phylum", "kingdom", "superkingdom"),
  .unique = TRUE,
  log = NULL
) {
  # filter hits for query coverage
  cvg_idx <- NULL
  if (coverage_threshold > 0) {
    message(" -- Filtering by query coverage")
    cvg <- compactEmpty(getQueryCoverage(blast_db, id = query_id, log = log))
    cvg_idx <- lapply(cvg, function(cvg) which(cvg >= coverage_threshold))
  }
  hits <- .fetch_hits(blast_db, id = query_id, idx = cvg_idx, 
                      "query_id, hit_id, gene_id", log = log)
  # filter hsps basing on tolerance threshold
  message(" -- Filtering by bit score")
  hsps <- .filter_hsps(blast_db, hits, bitscore_tolerance, log = log)
  hits <- .compact_hits(hits, hsps)
  message(" -- Searching for least common ancestors")
  ans <- LCA.apply(hits, ranks, log = log)
  if (.unique) {
    hits <- ans$hit_id
    ans$hit_id <- NULL
    ans <- unique.data.frame(ans)
    row.names(ans) <- NULL
    attr(ans, "hits") <- hits
    ans
  } else {
    ans
  }
}

#' Assign Taxa to Blast queries.
#'
#' @description This algorithm selects Blast hits based on a series of conditions
#' to improve the taxonomic assignment to a query sequence.
#' 
#' First only hits for a query with a query coverage above \code{coverage_threshold} (default: >= 50\%)
#' will be retained. Next, for all remaining hits only hsps are retained with a bitscore within a
#' lower bound defined by the overall maximum bitscore * \code{bitscore_tolerance} (default: 0.98).
#' Hits with no remaining hsps are discarded, and for the remaining hits the least common ancestor
#' \code{\link{LCA}} is determined.
#'
#' @param blast_db A \code{\link[blastr]{blastReportDB}} object.
#' @param coverage_threshold threshold for the selection of hits
#' @param bitscore_tolerance tolerance value for the selection of hsps
#' @param query_id Indices of \emph{query_id}s to annotate (default: all query_ids
#' in \code{blast_db}). 
#' @param ranks vector of taxonomic ranks along which the least common ancestor
#' is determined.
#' @param ...
#' @return A \code{data.frame} 
#' @seealso \code{\link{LCA}}
#' @rdname assignTaxa
#' @name assignTaxa
#' @export
assignTaxa <- function(blast_db, coverage_threshold = 0.5, bitscore_tolerance = 0.98,
                        query_id = NULL, ranks = c("species", "genus", "family", "order", "class", "phylum", "kingdom", "superkingdom"),
                        ...) {
  assert_that(coverage_threshold >= 0, coverage_threshold <= 1)
  assert_that(bitscore_tolerance > 0, bitscore_tolerance <= 1)
  dots <- list(...)
  log <- dots$log
  .unique <- dots$.unique %||% TRUE
  if (is.null(query_id)) {
    query_id <- getQueryID(blast_db, log = log)
  }
  .assignTaxa(blast_db, query_id, coverage_threshold, bitscore_tolerance, ranks, .unique, log)
}


