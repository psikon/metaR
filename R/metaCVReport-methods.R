NULL
# compareMetaCVwithBlast <- function(taxonomyReportDB,metaCVReport,taxonDB) {
#   query_id <- db_query(blast,"SELECT query_id from query",1L)
#   metaCVReport_defs <- metaCVReport[['query_def']]
#   cmp <- as.data.frame(do.call(rbind,lapply(query_id,function(x){
#     taxonomyReport_def <-  getQueryDef(blast,x,'query_id')
#     meta <- metaCVReport[which(all(gsub(" .*$", "", taxonomyReport_def) %in% metaCVReport_defs)),]
#     if(nrow(meta) >= 1) {
#       cbind(taxonomyReport_def,meta$tax_id)
#     } else {
#       NULL
#     }
#   })))
# }
# 
# 
# 
# 
# test <- metacv[which(metacv$query_def %in% getQueryDef(blast,query_id,'query_id')),]
# meta_defs <- metacv$query_def
# blast_defs <- getQueryDef(blast,query_id,'query_id')
# blast_tax <- getTaxID(blast,query_id,'query_id')
# 
# t <- as.data.frame(do.call(rbind,lapply(query_id, function(x){
#   
#   def <- getQueryDef(blast,x,'query_id')
#   if(all(gsub(" .*$", "", def) %in% meta_defs)) {
#     meta <- metacv[which(metacv$query_def %in% gsub(" .*$", "", def)),]
#     meta <- meta[,-c(2,3,4,5)]
#     meta <- cbind(meta,meta_class=getByRank(taxonDB(meta$tax_id,taxDB[[1]]),
#                                             'class',
#                                             value='ScientificName'))
#     blast_entries <- cbind(blast_def=def,
#                            blast_query=x,
#                            blast_taxid=getTaxID(blast,x,'query_id'),
#                            blast_rank=getRank(blast,x,'query_id'),
#                            blast_class=getByRank(taxonDB(getTaxID(blast,x,'query_id'),
#                                                          taxDB[[1]]),
#                                                  'class',value='ScientificName'))
#     tmp <- cbind(meta,blast_entries)
#   } else {
#     NULL
#   }
# })))
# # setMethod("taxonCount", "metaCVReport",
# #           function (x) {
# #             # count the taxIds
# #             count <- tapply(x[,"taxID"], list(x[,"taxID"]), length)
# #             # get the names and merge them with the original object
# #             taxID <- names(count)
# #             tmp <- unique(merge(list(taxID = as.integer(taxID)), x[,c("taxID", "taxName")]))
# #             # fix rownames
# #             rownames(tmp) <- NULL
# #             cbind(tmp, count = unname(count))
# #           })
# # 
# # # mergeMetaCVwithBlast============================
# # #
# # #' merge MetaCV with Blast results
# # #'
# # #' compare the results of a MetaCVReport object with the results of a 
# # #' BlastReportDB object to find hits contained in both of them
# # #' 
# # #' @param metaCVReport A metaCVReport object.
# # #' @param blastReportDB blastReportDB
# # #' 
# # #' @rdname MetaCVReport-methods
# # #' @export
# # mergeMetaCVwithBlast <- function(metaCVReport,blastReportDB) {
# #   print("vergleicht die GeneIDs von MetaCV mit den Ergebnissen von Blast")
# #   
# #   
# #   # vergleiche die gene ids und die tax ids zwischen den beiden datensätzen 
# #   # füge eine extra spalte ein welches programm dieses fund gemacht hat
# #   # blastn - metacv - beide
# #   
# # }
