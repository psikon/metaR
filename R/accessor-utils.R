#' @importFrom IRanges IRanges
#' @importFrom plyr arrange
#' @importFrom plyr desc
#' @importFrom ncbi getRank
#' @importFrom ncbi getParentTaxId
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


#' select complete hit(s) 
#' 
#' @description wrapper function to get complete hit(s) from hit table in 
#' blast database
#' 
#'@param x \code{\link{blastReportDB}} object
#'@param id query_id
#'
#'@return data.frame
#'
#'@rdname accessor-utils
#'@export
getHit <- function(x,id) {
  db_query(x,paste("SELECT * FROM hit where query_id=",id))
}

#' select complete hsp(s) 
#' 
#' @description wrapper function to get complete hsp(s) from hsp table in 
#' blast database
#' 
#'@param x \code{\link{blastReportDB}} object
#'@param id query_id
#'
#'@return data.frame
#'
#'@rdname accessor-utils
#'@export
getHsp <- function(x,id) {
  db_query(x,paste("SELECT * FROM hsp where query_id=",id))
}

# returns all hsp(s) matching a specific query_id and hit_id or NA
.getSelectedHits <- function (x, qid, hid) {
  stmts <- paste("SELECT * from hsp WHERE query_id=",qid,"AND hit_id=",hid)
  as.data.frame(do.call(rbind,lapply(stmts, FUN=function(stmt) {
    db_query(x, stmt) %||% NA_character_ 
  })))
}
# returns all hsp(s) matching a specific query_id and hit_id or NA
.getSelectedRange <- function(x,df) {
  stmts <- paste("SELECT hit_id, query_from, query_to, hit_from, hit_to FROM 
                 hsp WHERE query_id=",unique(df$query_id),"AND hit_id=",unique(df$hit_id))
  lapply(stmts, FUN=function(stmt) {
    pos <- db_query(x, stmt) %||% NA_character_
    IRanges(start=pos$query_from,end=pos$query_to,names=pos$hit_id)
  })
}
#'@keywords internal
.filterHsp <- function(x,df,perc) { 
  # get all hsp(s) bit_score >= tolerance threshold
  df <- df[which(df['bit_score']>=max(df['bit_score'])*perc),]  
  # sort them descending by bit_score
  df <- arrange(df,desc(x=df['bit_score']))
  # remove duplicates
  df <- df[!duplicated(df['hit_id']),]
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
#'find term of super kingdom of a tax_id
#'
#'@param tax_id
#'
#'@export
assignSuperKingdom <- function (tax_id) {
  if (tax_id == 2759) {
    return("Eukaryota")
  } else if (tax_id == 2) {
    return("Bacteria")
  } else {
    return("unclassified")
  }
}