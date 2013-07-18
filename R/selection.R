


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

selectByRank <- function(x, taxRank, classifier, taxon_db) {
  if (!taxRank %in% ncbi:::.ranks) {
    stop("'taxRank' must be of ", paste0(ncbi:::.ranks[-c(1, length(ncbi:::.ranks))], collapse=', '))
  }
  test <- lapply(db_query(taxReport,"select hit_id from taxonomy",1L),
                FUN= function(x) {
                  if (tolower(classifier) %in% 
                      tolower(getByRank(getTaxon(taxReport,x,'hit_id',taxDB),
                                        taxRank,
                                        value='ScientificName'))) {
                      TRUE
                  } else {
                    FALSE
                  }
  })
  
  r <- db_df[grep(classifier, tolower(getByRank(getTaxon(taxReport,
                                                         db_df$hit_id,
                                                         'hit_id',
                                                         taxDB),
                                                taxRank,value='ScientificName'))),]