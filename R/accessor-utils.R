#' @importFrom IRanges IRanges
#' @importFrom plyr arrange
#' @importFrom plyr desc
#' @importFrom ncbi getRank
NULL

setGeneric("has_ranks", function (x, ranks, ...) standardGeneric("has_ranks"))
setMethod("has_ranks", "Taxon", function (x, ranks) {
  any(getRank(getLineage(x)) %in% ranks)
})
setMethod("has_ranks", "TaxonList", function (x, ranks) {
  vapply(x, has_ranks, ranks=ranks, FUN.VALUE=logical(1), USE.NAMES=FALSE)
})

#.validRanks <- .targetRanks[-c(1,length(.targetRanks))] 

compactRanks <- function (x) {
  x[has_ranks(x, .validRanks)]
}


#' select all hit(s) specified by a query_id from blastReportDB
#'
getHit <- function(x,id) {
  db_query(x,paste("SELECT * FROM hit where query_id=",id))
}

#' select all hsp(s) of hit(s) specified by a query_id from blastReportDB
#' 
#' @param x an \code{\link{BlastReportDB}} connection object
getHsp <- function(x,id) {
  db_query(x,paste("SELECT * FROM hsp where query_id=",id))
}


.getSelectedHits <- function (x, qid, hid) {
  stmts <- paste("SELECT * from hsp WHERE query_id=",qid,"AND hit_id=",hid)
  as.data.frame(do.call(rbind,lapply(stmts, FUN=function(stmt) {
    db_query(x, stmt) %||% NA_character_ 
  })))
}

.getSelectedRange <- function(x,df) {
  stmts <- paste("SELECT hit_id, query_from, query_to, hit_from, hit_to FROM 
                 hsp WHERE query_id=",unique(df$query_id),"AND hit_id=",unique(df$hit_id))
  lapply(stmts, FUN=function(stmt) {
    pos <- db_query(x, stmt) %||% NA_character_
    IRanges(start=pos$query_from,end=pos$query_to,names=pos$hit_id)
  })
}

.filterHsp <- function(x,df,perc) { 
  # order the hsps for 
  df <- df[which(df['bit_score']>=max(df['bit_score'])*perc),]  
  df <- arrange(df,desc(x=df['bit_score']))
  df <- df[!duplicated(df['hit_id']),]
  df
}

.reduceHitsFromHsps <- function(hits, hsps) {
  hits[apply(hits, 1, function(x) any(x %in% hsps$hit_id)), ]
}

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


setAs("Taxon", "data.frame", function (from) {
  data.frame(tax_id = from@TaxId, scientific_name = from@ScientificName, rank = from@Rank,
             check.names=FALSE, stringsAsFactors=FALSE)
})

setAs("TaxonList", "data.frame", function (from) {
  do.call('rbind', lapply(from, as, Class='data.frame'))
})

