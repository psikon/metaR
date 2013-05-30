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

metaCV <- createMetaCVReport(res.path)
metaCV

