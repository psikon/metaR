#' metaCVReport class
#'
#' metaCVReport is an S4 class that provides a container for data retrieved
#' from the .res outputfile of metaCV
#' 
#' metaCVReport objects have 5 slots:
#' \describe{
#'   \item{read_name}{Name of the Read in Input File}
#'   \item{score}{Score of the detection with MetaCV}
#'   \item{geneID}{ID for NCBI database}
#'   \item{taxID}{ID for NCBI Taxonomy database}
#'   \item{taxname}{Name in NCBI Taxonomy database}
#'   \item{count}{Number of reads mapped to this taxID}
#'   
#' }
#' 
#' @param ... Slots for \sQuote{metaCV-Report} instances.
#' @name metaCVReport-class
#' @rdname metaCVReport-class
#' @exportClass metaCVReport
#' @aliases show,metaCVReport-method
setClass("metaCVReport",
         representation(read_name="character",
                        score="integer",
                        geneID="integer",
                        taxID="integer",
                        taxname="character",
                        count="integer"),
         prototype(read_name=NA_character_,
                   score=NA_integer_,
                   geneID=NA_integer_,
                   taxID=NA_integer_,
                   taxname=NA_character_,
                   count=NA_integer_)
         )

#specialized show Method for metaCVReport object
setMethod("show","metaCVReport",
          function(object){
            n <- length(object@read_name)
            showme <- sprintf('%s object with %s query rows',
                              sQuote(class(object)), n)
            cat(showme, sep="\n")
          })


  