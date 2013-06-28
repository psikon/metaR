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
# ort der taxon db
taxon <- "/home/psehnert/daten/SPICEIII/miseq/scripts/metpipe/program/db/"

# extract tables for overview
hit <- db_query(con, "SELECT * from hit")
hsp <- db_query(con, "SELECT * from hsp")

# testvar
id <- 1233
x <- con
# spaeterer Funktionskoerper
#lapply(hit$query_id,FUN=function(id) {

# filter hits for query coverage
cHits <- getHit(x,id)[which(getQueryCoverage(x,id) >=0.50),]

#if(nrow(cHits)>=1){
# filter Hsp(s) for query Coverage
chsp <- getSelected(con, id,getHitID(x,id)[which(getQueryCoverage(x,id) >=0.50)])
chsp <- filterHsp(con,chsp,perc=0.98)
cHits <- reduceHitsFromHsps(cHits,chsp)

taxa <- taxonByGeneID(cHits$gene_id, taxon)
print(getRank(taxa))
getLineage(taxa)
getParentTaxId(taxa)
taxonDB(getParentTaxId(taxa), taxon)
#if (getRank(taxa)=='no rank') {
#  print(id)
#  break
# }}}
       )
getTaxId(test)
test
test[[6]]