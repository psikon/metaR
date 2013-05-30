#setGeneric("createMetaCVReport", 
#           function (path) 
#             standardGeneric("createMetaCVReport"))
#
#setMethod("createMetaCVReport",'metaCVReport',
#          function (path) {
#              res <- scan(path,
#                          list(character(0), integer(0), integer(0), character(0), 
#                               character(0),integer(0),character(0)),
#                          sep="\t",na.strings="_")
#              object <- new("metaCVReport",read_name=res[[1]],
#                                           score=res[[2]],
#                                           geneID=res[[3]],
#                                           taxID=res[[6]],
#                                          taxname=res[[7]],
#                                           count=res[[6]])
#                                  
#              object }
#)


# createMetaCVReport============================
#
#' Import the Results of the .res File from MetaCV to a R and creates an 
#' \code{\linkS4class{metaCVReport}] object
#' 
#' @param path path to the resulting .res File from metaCV
#' 
#' @rdname MetaCVReport-methods
#' @export
createMetaCVReport <- function(path) {
            res <- scan(path,
                        list(character(0), integer(0), integer(0), character(0), 
                             character(0),integer(0),character(0)),
                        sep="\t",na.strings="_")
            object <- new("metaCVReport",read_name=res[[1]],
                          score=res[[2]],
                          geneID=res[[3]],
                          taxID=res[[6]],
                          taxname=res[[7]],
                          count=res[[6]])
            object
          }
# researchTaxIDs============================
#
#' find a better taxID for the GeneID of the object in the 
#' NCBI taxonomy database
#' 
#' @param metaCVReport A \code{\linkS4class{metaCVReport}] object.
#' @param path path to the taxonomy database
#' 
#' @rdname MetaCVReport-methods
#' @export
researchTaxIds <- function(metaCVReport,path_to_DB) {
  print("Hier werden geneIDs neue taxIDs zugeordnet")
}

# researchTaxIDs============================
#
#' find the linage in the NCBI taxonomy database basing on the 
#' geneID
#' 
#' @param geneID ID for NCBI database
#' 
#' @rdname MetaCVReport-methods
#' @export
assignLinage <- function(geneID) {
  print("ordnet der geneID die passende Linage zu")
}
# mergeMetaCVwithBlast============================
#
#' compare the results of a MetaCVReport object with the results of a 
#' BlastReportDB object to find hits contained in both of them
#' 
#' @param metaCVReport A \code{\linkS4class{metaCVReport}] object.
#' @param blastReportDB
#' 
#' @rdname MetaCVReport-methods
#' @export
mergeMetaCVwithBlast <- function(metaCVReport,blastReportDB) {
  print("vergleicht die GeneIDs von MetaCV mit den Ergebnissen von Blast")
}

# extractEukaryoticDB============================
#
#' find only the eukaryotic hits in the blastReportDB object, basing on the 
#' linage and seperate them in an own database
#' 
#' @param metaCVReport A \code{\linkS4class{metaCVReport}] object.
#' @param path path to the taxonomy database
#' 
#' @rdname MetaCVReport-methods
#' @export
extractEukaryoticDB <- function(path_to_DB) {
  print("suche nach eukaryotischen reads")
  print("extrahiere aus den einzelnen tables die betreffenden werte")
  print("erzeuge neue Datenbank aus den Werten")
}

# extractEukaryoticDB============================
#
#' find only prokaryotic hits in the blastReportDB object, basing on the
#' linage and separate them in an own database
#' 
#' @param metaCVReport A \code{\linkS4class{metaCVReport}] object.
#' @param path path to the taxonomy database
#' 
#' @rdname MetaCVReport-methods
#' @export
extractProkaryoticDB <- function(path_to_DB) {
  print("suche nach eukaryotischen reads")
  print("extrahiere aus den einzelnen tables die betreffenden werte")
  print("erzeuge neue Datenbank aus den Werten")
}