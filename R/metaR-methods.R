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

# mergeMetaCVwithBlast============================
#
#' merge MetaCV with Blast results
#'
#' compare the results of a MetaCVReport object with the results of a 
#' BlastReportDB object to find hits contained in both of them
#' 
#' @param metaCVReport A metaCVReport object.
#' @param blastReportDB blastReportDB
#' 
#' @rdname MetaCVReport-methods
#' @export
mergeMetaCVwithBlast <- function(metaCVReport,blastReportDB) {
  print("vergleicht die GeneIDs von MetaCV mit den Ergebnissen von Blast")
  
  
  # vergleiche die gene ids und die tax ids zwischen den beiden datensätzen 
  # füge eine extra spalte ein welches programm dieses fund gemacht hat
  # blastn - metacv - beide
  
}
