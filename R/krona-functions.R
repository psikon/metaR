#' create an input file for Krona Webtools
#'
#'@description creates an tab-separated input file for the text import funtion of the 
#'Krona Webtools consisting of the the count of taxons and the corresponding linage
#'
#'@param taxonomyReport \code{taxonomyReport} object
#'@param output location and name of the output file
#'
#'@seealso runKronaWebtools
#'@export
createKronaFile <- function(taxonomyReport, output) {
  # count the occurences of the tax_ids
  df <- countTaxa(taxonomyReport)
  # get for every counted tax_id the linage 
  df <- cbind(df, linage=vapply(df$tax_id, 
                                function(x) {
                                  paste(unique(taxonDB(as.integer(x),
                                                       taxDB[[1]])@Lineage@ScientificName),
                                        collapse="\t")
                                }, 
                                "character"))  
  # convert and save the results in a tab separeted file
  write.table(df[, -1], output, quote=F, sep="\t", row.names=F, col.names=F)
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