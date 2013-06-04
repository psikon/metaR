library(rmisc)
library(blastr)
library(devtools)
library(ncbi)
library(data.table)
library(XML)
library(plyr)


# important variables
db <- "/home/psehnert/daten/SPICEIII/miseq/scripts/metpipe/program/db/"
xmlpath <- "/home/psehnert/daten/SPICEIII/miseq/sample64/blastn/blastn.xml"
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
pos <- getByScore(x,10)
x[which(x[,"taxID"] == i), "geneID"]









