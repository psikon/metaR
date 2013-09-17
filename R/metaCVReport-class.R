#' metaCVReport class
#'
#' metaCVReport is an S4 class that provides a container for data retrieved
#' from the .res outputfile of metaCV
#' 
#' metaCVReport objects have 7 slots:
#' \itemize{
#'   \item  query_def       name of read in input file
#'   \item  score           score of the detection with MetaCV
#'   \item  kegg_id         identifier for KEGG Pathway
#'   \item  cog_id          identifier for ?
#'   \item  gene_id         identifier for NCBI sequence database
#'   \item  tax_id          identifier for NCBI taxonomy database
#'   \item  scientific_name name in NCBI taxonomy database
#'   
#' }
#' 
#' @param ... Slots for \sQuote{metaCV-Report} instances.
#' @name metaCVReport-class
#' @rdname metaCVReport-class
#' 
#' @exportClass metaCVReport
#' @aliases show,metaCVReport-method
setOldClass('data.frame')
setClass("metaCVReport", contains="data.frame")
#validität hinzufügen

# specialized show Method for metaCVReport object
setMethod("show","metaCVReport",
          function(object){
            n <- nrow(object)
            showme <- sprintf('%s object with %s query rows',
                              sQuote(class(object)), n)
            cat(showme, sep = "\n")
          })

#'@param path
#'
#'@rdname metaCVReport-class
#'@export
importMetaCV <- function(path) {
  assert_that(is.readable(path))
  tmp <- as.data.frame(do.call(cbind,
                               scan(path, quiet=TRUE,
                                    list(character(0), integer(0), integer(0), 
                                         character(0), character(0), integer(0), 
                                         character(0)),
                                    sep = "\t", na.strings = "_")), 
                       stringsAsFactors = F)
  colnames(tmp) <- c('query_def', 'score', 'gene_id', 'kegg_id', 
                     'cog_id', 'tax_id', 'scientific_name')
  new("metaCVReport", tmp)
}
