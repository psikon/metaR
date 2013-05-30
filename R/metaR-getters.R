# getReadName============================
#
#' Getter methods for MetaCVReport
#' 
#' @param object A \code{\linkS4class{metaCVReport}] object.
#' @param x index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getReadName", function(object,x) standardGeneric("getReadName"))

#' @export
setMethod("getReadName",signature='metaCVReport',function(object,x){
 object@read_name[x]
})

# getScore============================
#
#' Getter methods for MetaCVReport
#' 
#' @param object A \code{\linkS4class{metaCVReport}] object.
#' @param x index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getScore", function(object,x) standardGeneric("getScore"))

#' @export
setMethod("getScore",signature='metaCVReport',function(object,x){
  object@score[x]
})

# getGeneID============================
#
#' Getter methods for MetaCVReport
#' 
#' @param object A \code{\linkS4class{metaCVReport}] object.
#' @param x index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getGeneID", function(object,x) standardGeneric("getGeneID"))

#' @export
setMethod("getGeneID",signature='metaCVReport',function(object,x){
  object@geneID[x]
})

# getTaxID============================
#
#' Getter methods for MetaCVReport
#' 
#' @param object A \code{\linkS4class{metaCVReport}] object.
#' @param x index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getTaxID", function(object,x) standardGeneric("getTaxID"))

#' @export
setMethod("getTaxID",signature='metaCVReport',function(object,x){
  object@taxID[x]
})


# getTaxName============================
#
#' Getter methods for MetaCVReport
#' 
#' @param object A \code{\linkS4class{metaCVReport}] object.
#' @param x index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getTaxName", function(object,x) standardGeneric("getTaxName"))
#' @export
setMethod("getTaxName",signature='metaCVReport',function(object,x){
  object@taxname[x]
})


# getCount============================
#
#' Getter methods for MetaCVReport
#' 
#' @param object A \code{\linkS4class{metaCVReport}] object.
#' @param x index of the Object
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("getCount", function(object,x) standardGeneric("getCount"))

#' @export
setMethod("getCount",signature='metaCVReport',function(object,x){
  object@count[x]
})
