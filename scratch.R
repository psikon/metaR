require(devtools)
require(assertthat)
require(blastr)
require(rmisc)
require(RSQLite)
require(ncbi)
require(IRanges)
require(plyr)

# connection herstellen
con <- blastReportDBConnect("/home/psehnert/daten/SPICEIII/miseq/sample64/blast.test.db")
taxon <- "/home/psehnert/daten/SPICEIII/miseq/scripts/metpipe/program/db/"

getHit <- function(x,id) {
  db_query(x,paste("SELECT * FROM hit where query_id=",id))
}

getHsp <- function(x,id) {
  db_query(x,paste("SELECT * FROM hsp where query_id=",id))
}

# extract tables for overview
hit <- db_query(con, "SELECT * from hit")
hsp <- db_query(con, "SELECT * from hsp")

# generate test data
hit_test <- getHit(con,74)
hsp_test <- getHsp(con,74)

# filter hits for query coverage
cHits <- getHit(x,id)[which(getQueryCoverage(x,id) >=0.50),]

# filter Hsp(s) for query Coverage
chsp <- getSelected(con, 74,getHitID(x,id)[which(getQueryCoverage(x,id) >=0.50)])

getSelected <- function (x, qid,hid) {
  stmts <- paste("SELECT * from hsp WHERE query_id=",qid,"AND hit_id=",hid)
  as.data.frame(do.call(rbind,lapply(stmts, FUN=function(stmt) {
    db_query(x, stmt) %||% NA_character_ 
  })))
}

getSelectedRange(con,chsp)
getSelectedRange <- function(x,df) {
  stmts <- paste("SELECT hit_id, query_from, query_to, hit_from, hit_to FROM 
                 hsp WHERE query_id=",df$query_id,"AND hit_id=",df$hit_id)
  lapply(stmts, FUN=function(stmt) {
    db_query(x, stmt) %||% NA_character_
  })
}
         
getQueryRange(x,id)
reduce(chsp)
# rank the hsps for bitscore
chsp <- arrange(chsp,desc()
)

voteTaxID <- function(x,id) {
  hits <- db_query(con,paste("SELECT * from hit WHERE query_id=",id))
  taxa <- taxonByGeneID(hits[,'gene_id'],taxon)
  taxa@.Data
  
  getTaxId(taxa)
  vapply(taxa,getParentTaxId,character(1))
  getParentTaxId(taxa)
}

hit_ids <- 

cHits <- 


                   

getAlignLen(x,id)
getHitID(x,id)[which(getQueryCoverage(x,id) >=0.50)]

vapply
cHsp <- apply(getHsp(x,id),FUN= subset()

selectHits <- function(x,id) {
 
 cHits <- getHit(x,id)
 cHsp <- getHsp(x,id)
 taxa <- taxonByGeneID(cHits$gene_id[1],taxon)
 taxa@ParentTaxId
 taxonDB(8139,taxon)
 select <- candidates[getHitCoverage(x,id),]
 candidates <- db_query(con,paste("select * from hit WHERE queryid=",id,AND hit_id ==))
}











