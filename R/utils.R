#' @importFrom rmisc trim
#' @importFrom rmisc '%||%'
#' @importFrom plyr arrange
#' @importFrom plyr desc
NULL

#' check ranks of taxon(s) against reference
#' 
#' @param x taxon object
#' @param ranks character vector of taxonomical ranks
#' 
#' @return \code{TRUE/FALSE}
#' 
#' @rdname has_ranks
#' @export
setGeneric("has_ranks", function (x, ranks, ...) standardGeneric("has_ranks"))
setMethod("has_ranks", "Taxon", function (x, ranks) {
  any(getRank(getLineage(x)) %in% ranks)
})
setMethod("has_ranks", "TaxonList", function (x, ranks) {
  vapply(x, has_ranks, ranks=ranks, FUN.VALUE=logical(1), USE.NAMES=FALSE)
})

# returns all hsp(s) matching a specific query_id and hit_id or NA
.getSelectedHits <- function (x, qid, hid) {
  stmts <- paste("SELECT * from hsp WHERE query_id=",qid,"AND hit_id=",hid)
  as.data.frame(do.call(rbind,lapply(stmts, FUN=function(stmt) {
    db_query(x, stmt) %||% NA_character_ 
  })))
}
#'@keywords internal
.getHit <- function (x,id) {
  db_query(x,paste("SELECT * FROM hit WHERE query_id=",id))
}
#'@keywords internal
.getHsp <- function (x,id) {
  db_query(x,paste("SELECT * FROM hsp WHERE query_id=",id))
}
#'@keywords internal
.filterHsp <- function(df, perc) { 
  # get all hsp(s) bit_score >= tolerance threshold
  df <- df[which(df[ 'bit_score' ] >= max(df[ 'bit_score' ]) * perc),]  
  # sort them descending by bit_score
  df <- arrange(df, desc(df[ 'bit_score' ]))
  # remove duplicates
  df <- df[!duplicated(df[ 'hit_id' ]),]
  df
}

#'@keywords internal
# after filtering of the hsp(s) the hit(s) have to be adjusted to prevent hit(s) without hsp(s)
.reduceHitsFromHsps <- function(hits, hsps) {
  hits[apply(hits, 1, function(x) any(x %in% hsps$hit_id)), ]
}


#'@keywords internal
# recursive walk through the taxonomy tree until taxon has a valid rank
setGeneric('.resolveNoRank', function(taxon, taxonDB, ...) standardGeneric('.resolveNoRank'))
setMethod('.resolveNoRank', 'Taxon',
          function (taxon, taxonDB) {
            if (getRank(taxon) != 'no rank') {
              return (taxon)
            }
            else {
              Recall(taxonDB(getParentTaxId(taxon), taxonDB), taxonDB)
            } 
          })
setMethod('.resolveNoRank', 'TaxonList',
          function (taxon, taxonDB) {
            ncbi:::TaxonList(lapply(taxon, .resolveNoRank, taxonDB = taxonDB))
          })

# extend the setAs frunction to convert taxon in data.frame
setAs("Taxon", "data.frame", function (from) {
  data.frame(tax_id = from@TaxId, scientific_name = from@ScientificName, rank = from@Rank,
             check.names=FALSE, stringsAsFactors=FALSE)
})

setAs("TaxonList", "data.frame", function (from) {
  do.call('rbind', lapply(from, as, Class='data.frame'))
})

#' classify tax_id(s) by taxRanks
#'
#'@param df data.frame for classification
#'@param taxRank valid rank from ncbi taxonomy
#'@param taxon_db connection object to taxonomy db
#'
#'@return x
#'
#'@export


KronaTable <- function() {
  print("create input file for krona webtools")  
}

#'@keywords internal
.getterConstructor <- function(SELECT, FROM, ..., as = 'character') {
  function (x, id, typ) {
    args <- list(...)
    assert_that(typ %in% c('tax_id', 'query_id', 'hit_id'))
    stmts <- trim(paste("SELECT", SELECT, 'FROM', FROM,
                        if (is.null(args$WHERE)) {
                          paste('WHERE', typ, '=', id)
                        } else {
                          paste('WHERE', args$WHERE,'=')
                        },
                        if (!is.null(args$VAL) && !is.null(args$TABLE)) {
                          paste('(SELECT', args$VAL, 'FROM', args$TABLE,
                                'WHERE', typ, '=', id,')')
                        }))
    AS <- match.fun(paste0('as.', as))
    lapply(stmts, function(stmt) {
                AS( db_query(x, stmt,1L) %||% NA_character_ )
            })
  }
}



