library(rmisc)
library(blastr)
library(devtools)
library(ncbi)
library(data.table)
library(XML)
library(plyr)


# important variables
db <- "/home/psehnert/daten/SPICEIII/miseq/scripts/metpipe/program/db/"
db.path <- "/home/psehnert/daten/SPICEIII/miseq/sample64/blastn/sample64.db"
res.path <- "/home/psehnert/daten/SPICEIII/miseq/scripts/metpipe/result/metacv/metpipe.res"

#### get MetaCV results ####

# create the object
x <- metaCVReport(res.path)
x
#create count df
count <- taxonCount(x)
getScore(x)

# get all with score >= 5
getGeneIDbyTaxID(x,641)
pos <- getByScore(x,8)

# taxids mappen
test <- pos[1:100,]
test2 <- 

taxonByGeneID(getGeneID(x,1),db)
getTaxID(x=x,5)
taxonDB(geneid=getGeneID(x,2),db)
taxonDB(taxid=getTaxID(x=x,3),dbPath=db)
taxonDB(2,db)

library(RSQLite)

con <- db_connect(db.path)
# generate test data
query <- db_query(con, "SELECT * FROM query LIMIT 1000")
# get hits according to the query_ids in query
hit <- db_query(con,paste0("SELECT * FROM hit WHERE query_id IN (",
                paste(query$query_id,collapse=","),")"))
# get hsps according tp the query_ids in query
hsp <- db_query(con, paste0("SELECT * FROM hsp WHERE query_id IN (",
                            paste(query$query_id,collapse=","),")"))


query9 <- hit[1:2,]
# testdatensatz
test <- db_query(con, paste0("SELECT * FROM hit WHERE query_id IN (9,12)"))
hsp_test <- hsp[1:31,]
hsp_test$score <- NULL
hsp_test$evalue <- NULL
hsp_test$qseq <- NULL
hsp_test$query_frame <- NULL
hsp_test$hit_frame <- NULL
hsp_test$midline <- NULL
hsp_test$hseq<- NULL
hsp_test$positive <- NULL
hsp_test$hsp_id <- NULL

selectHSP <- function(con,query_id) {
  library(IRanges)
  # create subset
  tmp <- db_query(con,paste0("select * 
                             from hsp 
                             WHERE query_id IN (",query_id,")"))
  hitIDs <- tmp$hit_id

  for(i in 1:length(hitIDs)) {
    #  case 1: only one hsp - keep it
    if(nrow(tmp[ tmp$hit_id == hitIDs[i],]) == 1 ) {
      print("only one hsp per hit --> keep it")
      tmp <- subset(tmp,tmp$hit_id != hitIDs[i])
      hitIDs <- tmp$hit_id
    } else {
      
      # case 2: non overlapping hsps
      if query start && query end +10% 
      
    }
  }
    
      
      else {
    
    
  }}
  
  
  # case 1: only one hsp - keep it
  if (nrow(subset(hsp_test,query_id==12))==1) {
    print("alone")
  }
  if ()
}

