#' metaCVReport class
#'
#' metaCVReport is an S4 class that provides a container for data retrieved
#' from the .res outputfile of metaCV
#' 
#' metaCVReport objects have 5 slots:
#' \itemize{
#'   \item{read_name}{Name of the Read in Input File}
#'   \item{score}{Score of the detection with MetaCV}
#'   \item{geneID}{ID for NCBI database}
#'   \item{taxID}{ID for NCBI Taxonomy database}
#'   \item{taxname}{Name in NCBI Taxonomy database}
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

# specialized show Method for metaCVReport object
setMethod("show","metaCVReport",
          function(object){
            n <- nrow(object)
            showme <- sprintf('%s object with %s query rows',
                              sQuote(class(object)), n)
            cat(showme, sep="\n")
          })

#'@rdname metaCVReport-class
#'@export
importMetaCV <- function(path) {
  assert_that(is.readable(path))
  tmp <- as.data.frame(do.call(cbind,
                               scan(path, quiet=TRUE,
                                    list(character(0), integer(0), integer(0), 
                                         character(0), character(0), integer(0), 
                                         character(0)),
                                    sep="\t", na.strings="_")), 
                       stringsAsFactors=F)
  colnames(tmp) <- c('query_def', 'score', 'gene_id', 'kegg_id', 
                     'cog_id', 'tax_id', 'scientific_name')
  new("metaCVReport",tmp)
}
