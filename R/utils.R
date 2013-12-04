#' @include all-generics.R
#' @importFrom plyr arrange desc
#' @importFrom assertthat "on_failure<-"
NULL

is.empty <- function(x) {
  is.null(x) || length(x) == 0L || (length(x) == 1L && !nzchar(x))
}
on_failure(is.empty) <- function(call, env) {
  paste0(deparse(call$x), " is not empty.")
}

"%||%" <- function(a, b) {
  if (is.empty(a)) force(b) else a
}

"%|na|%" <- function(a, b) {
  if (is.null(a) || all(is.na(a))) force(b) else a
}

## Vectorized default operators
"%|%" <- function(a, b) ifelse(nzchar(a), a, b)

"%|NA|%" <- function(a, b) ifelse(is.na(a), b, a)

"%ni%" <- Negate(`%in%`)

compact <- function(x) {
  x[!vapply(x, is.null, FALSE, USE.NAMES=FALSE)]
}

compactChar <- function(x) {
  x[vapply(x, nzchar, FALSE, USE.NAMES=FALSE)]
}

compactEmpty <- function(x) {
  x[!vapply(x, function(x) length(x)==0, FALSE, USE.NAMES=FALSE)]
}

trim <- function(x, trim = '\\s+') {
  assert_that(is.vector(x))
  gsub(paste0("^", trim, "|", trim, "$"), '', x)
}

#' Number of unique elements in a vector.
#' 
#' A wrapper around \code{length(unique(x))}
#' 
#' @param x vector
#' @param ... passed to \code{\link{unique}}
#' @keywords internal
nunique <- function(x, ...) {
  if (is.factor(x)) {
    length(levels(x))
  } else {
    length(unique(x, ...))
  }
}

setMethod("has_ranks", "Taxon", function(x, ranks) {
  any(getRank(getLineage(x)) %in% ranks)
})
setMethod("has_ranks", "TaxonList", function(x, ranks) {
  vapply(x, has_ranks, ranks=ranks, FUN.VALUE=FALSE, USE.NAMES=FALSE)
})


# returns all hsp(s) matching a specific query_id and hit_id or NA
.getSelectedHsps <- function (blastReportDB, qid, hid) {
  stmts <- paste("SELECT * from hsp WHERE query_id=", qid, "AND hit_id=", hid)
  as.data.frame(do.call(rbind, lapply(stmts, FUN = function(stmt) {
    db_query(blastReportDB, stmt) %||% NA_character_ 
  })))
}

#'@keywords internal
.getHit <- function(blast_db, id, what = "*") {
  db_query(blast_db , paste("SELECT", what, "FROM hit WHERE query_id=", id))
}

#'@keywords internal
.getHsp <- function (blast_db, id, what = "*") {
  db_query(blast_db, paste("SELECT", what, "FROM hsp WHERE query_id=", id))
}

#'@keywords internal
.filterHsp <- function(hsps, tolerance) { 
  # get all hsp(s) bit_score >= tolerance threshold
  hsps <- hsps[which(hsps$bit_score >= max(hsps$bit_score)*tolerance), ]  
  # sort them descending by bit_score
  hsps <- arrange(hsps, desc(hsps$bit_score))
  # remove duplicates
  hsps <- hsps[!duplicated(hsps$hit_id), ]
  hsps
}

#'@keywords internal
# after filtering of the hsp(s) the hit(s) have to be adjusted to prevent hit(s) without hsp(s)
.discardEmptyHits <- function(hits, hsps) {
  hits[hits$hit_id %in% hsps$hit_id, ]
}

setMethod('.resolveNoRank', 'Taxon',
          function(taxon) {
            if (ncbi::getRank(taxon) != 'no rank') {
              return(taxon)
            } else {
              Recall(taxonDB(ncbi::getParentTaxID(taxon)))
            } 
          })
setMethod('.resolveNoRank', 'TaxonList',
          function (taxon, taxon_db) {
            ncbi:::TaxonList(lapply(taxon, .resolveNoRank))
          })

# extend the setAs frunction to convert taxon in data.frame
setAs("Taxon", "data.frame", function (from) {
  data.frame(tax_id = from@TaxId, scientific_name = from@ScientificName, rank = from@Rank,
             check.names = FALSE, stringsAsFactors = FALSE)
})

setAs("TaxonList", "data.frame", function (from) {
  do.call('rbind', lapply(from, as, Class = 'data.frame'))
})

#' @keywords internal
.getterConstructor <- function(SELECT, FROM, ..., as = 'character') {
  function (x, id, type) {
    args <- list(...)
    type <- match.arg(type, c("tax_id", "query_id", "hit_id"))
    stmts <- trim(paste("SELECT", SELECT, 'FROM', FROM,
                        if (is.null(args$WHERE)) {
                          paste('WHERE', type, '=', id)
                        } else {
                          paste('WHERE', args$WHERE, '=')
                        },
                        if (!is.null(args$VAL) && !is.null(args$TABLE)) {
                          paste('(SELECT', args$VAL, 'FROM', args$TABLE,
                                'WHERE', type, '=', id,')')
                        }))
    AS <- match.fun(paste0('as.', as))
    lapply(stmts, function(stmt) {
      AS(db_query(x, stmt, 1L) %||% NA_character_)
    })
  }
}



