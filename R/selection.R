


classify <- function(df,taxRank,taxon_db) {
  if (!taxRank %in% ncbi:::.ranks) {
    stop("'taxRank' must be of ", paste0(ncbi:::.ranks[-c(1, length(ncbi:::.ranks))], collapse=', '))
  }
  x <- getByRank(taxonDB(df[['tax_id']],taxon_db[[1]]),
                 rank=taxRank,
                 value='TaxId')
  x[is.na(x)] <- "unclassified"
  x
}


selectByRank(x=taxReport, taxRank='genus', classifier='epinephelus', taxon_db=taxDB)


selectByRank <- function(x, taxRank, classifier, taxon_db) {
  if (!taxRank %in% ncbi:::.ranks) {
    stop("'taxRank' must be one of ", paste0(ncbi:::.ranks[-c(1, length(ncbi:::.ranks))], collapse=', '))
  }
  hit_id <- db_query(x, "SELECT hit_id FROM taxonomy", 1L)
  taxa <- getTaxon(x=taxReport, id=hit_id, typ='hit_id', taxon_db=taxDB)
  id <- hit_id[which(tolower(getByRank(taxa, taxRank, value='ScientificName')) == tolower(classifier))]
  do.call('rbind', lapply(id, function(i) db_query(x, paste('SELECT * FROM taxonomy WHERE hit_id =', i))))
}
  





  
  r <- db_df[grep(classifier, tolower(getByRank(getTaxon(taxReport,
                                                         db_df$hit_id,
                                                         'hit_id',
                                                         taxDB),
                                                taxRank,value='ScientificName'))),]