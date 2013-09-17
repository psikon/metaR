#' create an input file for Krona Webtools
#'
#'@description creates an tab-separated input file for the text import funtion of the 
#'Krona Webtools consisting of the the count of taxons and the corresponding linage
#'
#'@param taxonomyReportDB \code{taxonomyReportDB} object
#'@param output location and name of the output file
#'
#'@seealso runKronaWebtools
#'@export

createKronaFile <- function(taxonomyReportDB, output, taxon_db) {

  # count the occurences of the tax_ids
  df <- countTaxa(taxonomyReportDB)
  # get for every counted tax_id the linage 
  df <- cbind(df, linage = vapply(df$tax_id, 
                                function(x) {
                                  paste(unique(taxonDB(as.integer(x),
                                                       taxon_db[['taxon_db']])@Lineage@ScientificName),
                                        collapse = "\t")
                                }, 
                                "character"))  
  # convert and save the results in a tab separeted file
  write.table(df[, -1], output, quote = F, sep = "\t", row.names = F, col.names = F)
}
#' extract from the blast db the blast tabular output 
#'
#'@description extract 12 fields from the blast db:
#'
#'\item query_def
#'\item subject id (example: gi|350280512|gb|JF719726.1|)
#'\item perc_identity
#'\item align_len
#'\item mismatches
#'\item gap opens
#'\item query_from
#'\item query_to
#'\item hit_from
#'\item hit_to
#'\item evalue
#'\item bitscore
#'
#'
blastXML_to_tab <- function(taxonomyReportDB, output, taxon_db) {
  
  #hsps <- db_query()
  # suche hsps raus
  # ermittle die Hit ids
  # such die query Ids raus
  #df <- cbind(db_query(taxonomyReportDB,'SELECT query_def FROM query',1L),
  #            'Platzhalter',
  #            db_query(taxonomyReportDB,'SELECT align_len FROM hsp',1L))
}
#' run the Krona Webtools from R
#' 
#' @description Wrapper function for running the Krona Webtools Script \emph{ImportText.pl} from
#' the R-Environment. The needed Input file can be created with \link{createKronaFile()}-function.
#' 
#' @param input tab-separated input file (created by \link{createKronaFile()})
#' @param output location and name for the output
#' @param program_path location of the Krona Webtool binaries
#' 
#' @seealso createKronaFile
#'@export
runKronaWebtools <- function(input, output, program_path) {
  if (! strsplitN(program_path,"",nchar(program_path)) %in% "/") {
    paste0(program_path,"/")
  }
  system(paste0('perl ', program_path, 'scripts/ImportText.pl -o ', output, " ", input),
         ignore.stdout = T,
         ignore.stderr = T)
}