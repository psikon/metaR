#'@importClassesFrom ncbi Taxon
#'@importClassesFrom ncbi TaxonList
#'@importFrom ncbi taxonByGeneID
#'@importFrom ncbi getByRank
#'@importFrom ncbi taxonDB
#'@importFrom ncbi getLineage
#'@importFrom ncbi getTaxId
#'@importFrom iterators iter
#'@importFrom iterators nextElem
#'@importFrom assertthat assert_that
#'@importFrom assertthat '%has_name%'
#'@importFrom rmisc nunique
#'@importFrom rmisc is.empty
NULL

#'find the least common ancestor inside a blast query with multiple hits
#'
#'@description if a \emph{query_id} has multiple hit(s) with multiple taxa, the least common 
#'ancestor (lca) of this hit(s) must be find. 
#'Firstly all hit(s) will be assigned a \emph{taxon} at a specified starting rank. If 
#'the taxa are different on this starting rank the taxa will be mapped to the next
#'higher rank of the taxonomy and also checked for uniqueness. This procedur will be 
#'repeated until the top rank is reached. 
#'
#'@note To improve the performance of this algorithm the hit(s) will be first checked
#'for differences in the top rank. If there exists differences the query will be discarded.
#'
#'@param query_table  dataframe containing hits from a blast search 
#'@param taxon_db     list of connections to taxon_db and geneid_db
#'@param taxRanks     vector of levels of the ncbi taxonomy 
#'
#'@return data.frame  
#'
#'@seealso \code{\link{assignTaxon()}}
#'
#'@rdname lca
#'@export
LCA <- function(query_table,
                taxon_db,
                taxRanks = c("species", "genus", "tribe", "family", "order",
                             "class", "phylum", "kingdom", "superkingdom"))
{
  # check the data frame for required fields
  assert_that(query_table %has_name% 'query_id',
              query_table %has_name% 'hit_id',
              query_table %has_name% 'gene_id',
              query_table %has_name% 'accession')
  # check the ranks for valid ncbi rank designations
  if (!all(taxRanks %in% ncbi:::.ranks)) {
    stop("'taxRanks' must be of ", paste0(ncbi:::.ranks[-c(1, length(ncbi:::.ranks))], collapse=', '))
  }
  #sort the ranks in ascending order,e.g.  from species to superkingdom
  taxRanks <- names(rev(sort(sapply(taxRanks, match, ncbi:::.ranks))))
  #determine the toprank
  topRank <- taxRanks[length(taxRanks)]
  # get the first taxon object
  geneid_db <- taxon_db[['geneid_db']]
  taxon_db <- taxon_db[['taxon_db']]
  taxa <- taxonByGeneID(query_table[["gene_id"]], geneid_db, taxon_db)
  # Condition 1: when the taxa have multiple TaxId(s) at the superkingdom 
  # rank discard this query number and go to the next
  if (nunique(getByRank(taxa, topRank, 'TaxId')) > 1) {
    return(NULL)
  }
  # create an iterator for the ranks
  rankIt <- iter(taxRanks)
  # init TaxId(s) at with starting rank
  taxids <- getByRank(taxa, nextElem(rankIt), 'TaxId')
  # when starting rank is undefined go ahead until tank is specified
  while (all(is.na(taxids))) 
    taxids <- getByRank(taxa, nextElem(rankIt), 'TaxId')
  # again get the taxon object
  taxa <- taxonDB(taxids, taxon_db)
  if (length(taxa) == 1) {
    # if only one taxon find create the resulting data.frame
    cbind(query_table[, c('query_id', 'hit_id', 'gene_id', 'accession')],
          as(taxa, 'data.frame'))
  } else {
    # check if the taxa have all valid ranks (defined ranks between starting rank
    # and toprank contained in the given rank object)
    valid <- which(has_ranks(taxa, taxRanks))
    # remove non valid hits from data.frame and taxon object
    query_table <- query_table[valid, ]
    taxa <- taxa[valid]
    # determine the linage and unique TaxId(s)
    lineage <- ncbi::getLineage(taxa)
    taxids <- unique(ncbi::getTaxId(taxa))
    while (length(taxids) > 1 || all(is.na(taxids))) {
      # traverse through the ranks until all TaxId(s) are unique
      taxids <- unique(getByRank(lineage, nextElem(rankIt), 'TaxId'))
    }
    # create the resulting data.frame
    cbind(query_table[, c('query_id', 'hit_id', 'gene_id', 'accession')],
          as(taxonDB(unique(taxids), taxon_db), 'data.frame')) 
  }
}