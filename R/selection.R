#' select db entries by rank
#' 
#' @description This function search in the database for given specifications based 
#' on the taxonomical rank of the database entry.
#' 
#'@param x \code{taxonomyReportDB} object
#'@param taxRank a valid NCBI taxonomy rank (e.g. \emph{'genus'})
#'@param classifier name of rank or species
#'@param taxon_db \code{taxon_db} object
#'
#'@return data.frame
#'
#'@rdname selectByRank.Rd
#'@export
selectByRank <- function(x, taxRank, classifier, taxon_db) {
  if (!taxRank %in% ncbi:::.ranks) {
    stop("'taxRank' must be one of ", paste0(ncbi:::.ranks[-c(1, length(ncbi:::.ranks))], collapse=', '))
  }
  hit_id <- db_query(x, "SELECT hit_id FROM taxonomy", 1L)
  taxa <- getTaxon(x=taxReport, id=hit_id, typ='hit_id', taxon_db=taxDB)
  id <- hit_id[which(tolower(getByRank(taxa, taxRank, value='ScientificName')) == tolower(classifier))]
  do.call('rbind', lapply(id, function(i) db_query(x, paste('SELECT * FROM taxonomy WHERE hit_id =', i))))
}


#' filter MetaCVReport by score 
#' 
#'@param x metaCVReport object
#'@param score 
#'
#'@return metaCVReport
#'
#'@export
selectByScore <- function(x,score) {
  sel <-  x[which(x[,'score'] >= score),]
  class(x[,'score'])
}
  
