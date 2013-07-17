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
#' @rdname MetaCVReport-class
#' 
#' @exportClass metaCVReport
#' @aliases show,metaCVReport-method
setOldClass('data.frame')
setClass("metaCVReport", contains="data.frame")

#specialized show Method for metaCVReport object
setMethod("show","metaCVReport",
          function(object){
            n <- nrow(object)
            showme <- sprintf('%s object with %s query rows',
                              sQuote(class(object)), n)
            cat(showme, sep="\n")
          })


  