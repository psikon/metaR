# metaCVReport============================
#
#' Import the Results of the .res File from MetaCV to a R and creates an 
#' \code{\linkS4class{metaCVReport}] object
#' 
#' @param path path to the resulting .res File from metaCV
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("metaCVReport", 
           function (path) 
             standardGeneric("metaCVReport"))

setMethod("metaCVReport", signature='character',
          function (path) {
            res <- scan(path, quiet=TRUE,
                        list(character(0), integer(0), integer(0), character(0), 
                             character(0), integer(0), character(0)),
                        sep="\t", na.strings="_")
            new("metaCVReport",
                .Data = list(readName=res[[1]], score=res[[2]],
                             geneID=res[[3]], taxID=res[[6]],
                             taxName=res[[7]]),
                row.names = seq_along(res[[1]]))
          })


# taxonCount============================
#
#' creates a df, that contains the counts ordered by taxIds and the 
#' corresponding taxName
#' 
#' @param x A \code{\linkS4class{metaCVReport}] object.
#' 
#' @rdname MetaCVReport-methods
#' @export
setGeneric("taxonCount", function(x, ...) standardGeneric("taxonCount"))

setMethod("taxonCount", "metaCVReport",
          function (x) {
            # count the taxIds
            count <- tapply(x[,"taxID"], list(x[,"taxID"]), length)
            # get the names and merge them with the original object
            taxID <- names(count)
            tmp <- unique(merge(list(taxID = as.integer(taxID)), x[,c("taxID", "taxName")]))
            # fix rownames
            rownames(tmp) <- NULL
            cbind(tmp, count = unname(count))
          })

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
  
  
  # vergleiche die gene ids und die tax ids zwischen den beiden datensätzen 
  # füge eine extra spalte ein welches programm dieses fund gemacht hat
  # blastn - metacv - beide
  
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