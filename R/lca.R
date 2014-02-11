#' @importFrom ncbi taxonByGeneID getTaxID getByRank .ncbi_taxon_ranks
#' @importFrom iterators iter nextElem
#' @importFrom assertthat '%has_name%'
NULL

#' Find the least common ancestor for a blast query with multiple hits
#'
#' @description if a \emph{query_id} has multiple hit(s) with multiple taxa, the least common 
#' ancestor (lca) of this hit(s) should be found. 
#' Firstly all hit(s) will be assigned a \emph{taxon} at a specified starting rank. If 
#' the taxa are different on this starting rank the taxa will be mapped to the next
#' higher rank of the taxonomy and also checked for uniqueness. This procedur will be 
#' repeated until the top rank is reached. 
#'
#' @note To improve the performance of this algorithm the hit(s) will be first checked
#' for differences in the top rank. If there exists differences the query will be discarded.
#'
#' @param hits dataframe containing hits from a blast search 
#' @param ranks vector of levels of the ncbi taxonomy 
#'
#' @return A \code{data.frame}  
#'
#' @seealso \code{\link{assignTaxon()}}
#'
#'@rdname lca
#'@export
LCA <- function(
  hits,
  ranks = c("species", "genus", "tribe", "family", "order", "class", "phylum", "kingdom", "superkingdom"),
  log = NULL
) {
  # check the data frame for required fields
  assert_that(
    hits %has_name% 'query_id',
    hits %has_name% 'hit_id',
    hits %has_name% 'gene_id'
  )
  # check the ranks for valid ncbi rank designations
  ncbi_ranks <- ncbi:::.ranks
  if (!all(ranks %in% ncbi_ranks)) {
    stop("'ranks' must be of ", sQuote(paste0(ncbi_ranks[-c(1, length(ncbi_ranks))], collapse=', ')))
  }
  #sort the ranks in ascending order,e.g.  from species to superkingdom
  ranks <- names(rev(sort(sapply(ranks, match, ncbi_ranks))))
  #determine the toprank
  topRank <- ranks[length(ranks)]
  print(hits$gene_id)
  # get the first taxon object
  taxa <- taxonByGeneID(hits$gene_id, log = log)
  
  # If all taxa have non-unique TaxIds at the superkingdom 
  # level discard this query number and move on
  if (nunique(getByRank(taxa, topRank, 'TaxId')) > 1 || 
        all(getByRank(taxa, topRank, 'TaxId') %in% NA_character_)) {
    return(NULL)
  }
  
  # create an iterator for the ranks
  rankIt <- iter(ranks)
  # init TaxId(s) at with starting rank
  taxids <- getByRank(taxa, nextElem(rankIt), 'TaxId')
  # if the starting rank is undefined move ahead until the rank is specified
  while (all(is.na(taxids)))
    taxids <- getByRank(taxa, nextElem(rankIt), 'TaxId')
  
  # get the taxon objects
  taxa <- taxonDB(taxids, log = log)
  if (length(taxa) == 1)
  {
    # if only one taxon find create the resulting data.frame
    return(cbind(hits[, c('query_id', 'hit_id')], as(taxa, 'data.frame')))
  }
  else
  {
    # check if the taxa have all valid ranks (defined ranks between starting rank
    # and toprank contained in the given rank object)
    valid <- which(has_ranks(taxa, ranks))
    # remove non valid hits from data.frame and taxon object
    hits <- hits[valid, ]
    taxa <- taxa[valid]
    # determine the linage and unique TaxId(s)
    lineage <- getLineage(taxa)
    taxids <- unique(getTaxID(taxa))
    # traverse through the ranks until all TaxId(s) are unique
    while (length(taxids) > 1 || all(is.na(taxids)))
      taxids <- unique(getByRank(lineage, nextElem(rankIt), 'TaxId'))
    
    # create the resulting data.frame
    txdf <- as(taxonDB(taxids, log = log), 'data.frame')
    return(cbind(hits[, c('query_id', 'hit_id')], txdf)) 
  }
}


LCA.apply <- function(hits, ranks, log=log) {
  # check that the ranks are valid ncbi rank designations
  ncbi_ranks <- ncbi::.ncbi_taxon_ranks()
  if (!all(ranks %in% ncbi_ranks)) {
    stop("'ranks' must be of ", sQuote(paste0(ncbi_ranks[-c(1, length(ncbi_ranks))], collapse=', ')))
  }
  # sort the ranks in ascending order, i.e. from species to superkingdom
  ranks <- names(rev(sort(sapply(ranks, match, ncbi_ranks))))
  #determine the top rank
  topRank <- ranks[length(ranks)]
  
  .lca <- function(hit) {
    # get the first taxon object
    txl <- taxonByGeneID(hit$gene_id, log=log)
    # If all taxa have non-unique TaxIds at the superkingdom 
    # level discard this query number and move on
    if (nunique(txid <- getByRank(txl, topRank, 'TaxId')) > 1 || all(is.na(txid))) {
      return(NULL)
    }
    # create an iterator for the ranks and retrieve initial taxIds for the first rank
    iranks <- iter(ranks)
    taxids <- getByRank(txl, nextElem(iranks), 'TaxId')
    # if the initial rank is undefined for all taxa move on until the rank is specified
    while (all(is.na(taxids))) {
      taxids <- getByRank(taxa, nextElem(iranks), 'TaxId')
    }
    # get new taxon objects if there are any differences to the initial list of taxa
    if (any(getTaxID(txl) %ni% taxids)) {
      txl <- taxonDB(taxids, log=log)
    }
    if (length(txl) == 1) {
      # if there is only one taxon, jump out
      return(cbind(hit[, c('query_id', 'hit_id')], as(txl, 'data.frame')))
    } else {
      # check if all taxa have valid ranks (defined as ranks between the starting rank
      # and toprank contained in the given rank object) and, if necessary, remove non-valid
      # hits from the data frame and the taxon object 
      if (sum(valid <- has_ranks(txl, ranks)) < nrow(hit)) {
        hit <- hit[valid, ]
        txl <- txl[valid]
      }
      # get the linage and unique TaxId(s)
      lineage <- getLineage(txl)
      taxids <- unique(getTaxID(txl))
      # traverse through the ranks until all TaxId(s) are unique
      while (length(taxids) > 1 || all(is.na(taxids))) {
        taxids <- unique(getByRank(lineage, nextElem(iranks), 'TaxId'))
      }
      # jump out
      txdf <- as(taxonDB(taxids, full=FALSE, log=log), 'data.frame')
      return(cbind(hit[, c('query_id', 'hit_id')], txdf)) 
    }
  }

  bindList(compact(lapply(hits, .lca)))

} 





