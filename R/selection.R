#' select db entries by rank
#' 
#' @description This function search in the database for given specifications based 
#' on the taxonomical rank of the database entry.
#' 
#'@param taxonomyReportDB a \code{taxonomyReportDB} object
#'@param taxRank a valid NCBI taxonomy rank (e.g. \emph{'genus'})
#'@param classifier name of rank or species
#'@param taxon_db \code{taxon_db} object
#'
#'@return data.frame
#'
#'@rdname selectByRank
#'@export
selectByRank <- function(taxonomyReportDB, taxRank, classifier, taxon_db) {
  # check for valid taxRanks
  if (!taxRank %in% ncbi:::.ranks) {
    stop("'taxRank' must be one of ", paste0(ncbi:::.ranks[-c(1, length(ncbi:::.ranks))], collapse=', '))
  }
  # create a TaxonList with all hit(s) in hit table
  hit_id <- db_query(taxonomyReportDB, "SELECT hit_id FROM taxonomy", 1L)
  taxa <- getTaxon(taxonomyReportDB, id=hit_id, typ='hit_id', taxon_db)
  # search for hit_id(s) matching the classifier
  id <- hit_id[which(tolower(getByRank(taxa, taxRank, value='ScientificName')) == tolower(classifier))]
  # create a data.frame with them
  do.call('rbind', lapply(id, function(i) db_query(taxonomyReportDB, paste('SELECT * FROM taxonomy WHERE hit_id =', i))))
}


#' filter \code{metaCVReport} by score 
#' 
#'@description get only the entries with a score greater than x, to prevent 
#'wrongly classified entries
#'
#'@details The score refers to the composition identity between 
#' the query read and the best target gene, and ranges from 0 
#' to 100. In a analysis of the MetaCV developers, reads with very low scores are highly 
#' possible to be wrongly classified and should be filtered. 
#' Recommended is  a set of cutoff values 
#' (minimal scores to pass) according to different read lengths: 
#' \itemize{
#'   \item Read length  Minimal score
#'   \item      100 bp      20
#'   \item      200 bp      10
#'   \item      400 bp       5
#'   \item      600 bp       4
#'   \item      800 bp       3
#'   \item     1000 bp       3
#' } 
#'
#'@param x metaCVReport object
#'@param score 
#'
#'@return metaCVReport
# 
#'@rdname selectByScore
#'@export
selectByScore <- function(metaCVReport, score) {
  sel <-  metaCVReport[which(metaCVReport[, 'score'] >= score), ]
  new('metaCVReport',sel)
}
  
