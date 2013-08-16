require(metaR)


## doku compelieren
library(devtools)
document(pkg = ".",clean = T)
Sys.setenv("PKG_CXXFLAGS" = "-std=c++11")

# connection herstellen
blastReport <- blastReportDBConnect("../blast.test.db")
#taxDB <- connectTaxonDB("/home/psehnert/daten/metagenomics/scripts/metpipe/program/db")
taxDB <- connectTaxonDB("../")


# taxonomy data.frame erstellen
db_df <- assignTaxon(1:10000, 
                    taxRanks = c("species", "genus", "tribe", "family", "order",
                                  "class", "phylum", "kingdom", "superkingdom"),
                    blastReportDB = blastReport, 
                    taxon_db = taxDB)

# alles in ein neues Objekt umschichten
taxReport <- createTaxonomyReportDB('taxonomy.db', blastReport, db_df, 0.98)
# nur Prokaryoten raussuchen
eukaryota <- selectByRank(taxReport,taxRank="superkingdom",'eukaryota',taxDB)
eukaryota <- createTaxonomyReportDB('eukaryota.db',blastReport,eukaryota,0.98)
eukaryota

# metaCV importieren
metacv <- importMetaCV('/home/psehnert/daten/metagenomics/sample64/metacv/metpipe.res')
metacv <- selectByScore(metacv,8)
metacv
cmp <- compareMetaCVwithBlast(blast,metacv,taxDB)

# krona webtools
createKronaFile(eukaryota, "krona-test.txt", taxDB)
runKronaWebtools(input = "krona-test.txt", output = "krona-output.html",
                 program_path = "/home/psehnert/daten/metagenomics/scripts/metpipe/programs/krona/")

# getByRank wrapper integrieren

####################################
# taxonomyReportDB - getter Tester #
####################################

countTaxa(taxReport)
countTaxa(taxReport,2)
#taxonomy
getQueryID(taxReport,94232,'tax_id')
getQueryID(taxReport,5,'hit_id')
getQueryID(taxReport,18,'query_id')

getHitID(taxReport,94232,'tax_id')
getHitID(taxReport,5,'hit_id')
getHitID(taxReport,18,'query_id')

getGeneID(taxReport,94232,'tax_id')
getGeneID(taxReport,5,'hit_id')
getGeneID(taxReport,18,'query_id')

getAccession(taxReport,94232,'tax_id')
getAccession(taxReport,5,'hit_id')
getAccession(taxReport,18,'query_id')

getTaxID(taxReport,94232,'tax_id')
getTaxID(taxReport,5,'hit_id')
getTaxID(taxReport,18,'query_id')

getScientificName(taxReport,94232,'tax_id')
getScientificName(taxReport,5,'hit_id')
getScientificName(taxReport,18,'query_id')

getRank(taxReport,94232,'tax_id')
getRank(taxReport,5,'hit_id')
getRank(taxReport,18,'query_id')

# ncbi wrapper
getTaxon(taxReport,94232,'tax_id',taxDB)
getTaxon(taxReport,5,'hit_id',taxDB)
getTaxon(taxReport,18,'query_id',taxDB)

getLineage(taxReport,94232,'tax_id',taxDB)
getLineage(taxReport,5,'hit_id',taxDB)
getLineage(taxReport,18,'query_id',taxDB)

getOtherName(taxReport,293821,'tax_id',taxDB)
getOtherName(taxReport,16,'hit_id',taxDB)
getOtherName(taxReport,18,'query_id',taxDB)

getParentTaxID(taxReport,293821,'tax_id',taxDB)
getParentTaxID(taxReport,16,'hit_id',taxDB)
getParentTaxID(taxReport,18,'query_id',taxDB)

#getByRank(taxReport,293821,'tax_id','phylum',taxDB)
#getByRank(taxReport,16,'hit_id','phylum',taxDB)
#getByRank(taxReport,18,'query_id','phylum',taxDB)

# query table 
getQueryDef(taxReport,38293,'tax_id')
getQueryDef(taxReport,79,'hit_id')
getQueryDef(taxReport,153,'query_id')

getQueryLen(taxReport,94232,'tax_id')
getQueryLen(taxReport,5,'hit_id')
getQueryLen(taxReport,18,'query_id')

# hit table
getHitNum(taxReport,38293,'tax_id')
getHitNum(taxReport,5,'hit_id')
getHitNum(taxReport,18,'query_id')

getDefinition(taxReport,38293,'tax_id')
getDefinition(taxReport,5,'hit_id')
getDefinition(taxReport,18,'query_id')

getHitLen(taxReport,94232,'tax_id')
getHitLen(taxReport,5,'hit_id')
getHitLen(taxReport,18,'query_id')

# hsp table
getHspID(taxReport,94232,'tax_id')
getHspID(taxReport,5,'hit_id')
getHspID(taxReport,18,'query_id')

getHspNum(taxReport,94232,'tax_id')
getHspNum(taxReport,5,'hit_id')
getHspNum(taxReport,18,'query_id')

getBitscore(taxReport,94232,'tax_id')
getBitscore(taxReport,5,'hit_id')
getBitscore(taxReport,18,'query_id')

getScore(taxReport,94232,'tax_id')
getScore(taxReport,5,'hit_id')
getScore(taxReport,18,'query_id')

getEvalue(taxReport,94232,'tax_id')
getEvalue(taxReport,5,'hit_id')
getEvalue(taxReport,18,'query_id')

getQueryFrom(taxReport,94232,'tax_id')
getQueryFrom(taxReport,5,'hit_id')
getQueryFrom(taxReport,18,'query_id')

getQueryTo(taxReport,94232,'tax_id')
getQueryTo(taxReport,5,'hit_id')
getQueryTo(taxReport,18,'query_id')

getHitFrom(taxReport,94232,'tax_id')
getHitFrom(taxReport,5,'hit_id')
getHitFrom(taxReport,18,'query_id')

getHitTo(taxReport,94232,'tax_id')
getHitTo(taxReport,5,'hit_id')
getHitTo(taxReport,18,'query_id')

getQueryFrame(taxReport,94232,'tax_id')
getQueryFrame(taxReport,5,'hit_id')
getQueryFrame(taxReport,18,'query_id')

getHitFrame(taxReport,94232,'tax_id')
getHitFrame(taxReport,5,'hit_id')
getHitFrame(taxReport,18,'query_id')

getGaps(taxReport,94232,'tax_id')
getGaps(taxReport,5,'hit_id')
getGaps(taxReport,18,'query_id')

getPositive(taxReport,94232,'tax_id')
getPositive(taxReport,5,'hit_id')
getPositive(taxReport,18,'query_id')

getIdentity(taxReport,94232,'tax_id')
getIdentity(taxReport,5,'hit_id')
getIdentity(taxReport,18,'query_id')

getAlignLen(taxReport,94232,'tax_id')
getAlignLen(taxReport,5,'hit_id')
getAlignLen(taxReport,18,'query_id')

getQuerySeq(taxReport,94232,'tax_id')
getQuerySeq(taxReport,5,'hit_id')
getQuerySeq(taxReport,18,'query_id')

getHitSeq(taxReport,94232,'tax_id')
getHitSeq(taxReport,5,'hit_id')
getHitSeq(taxReport,18,'query_id')

getMatch(taxReport,94232,'tax_id')
getMatch(taxReport,5,'hit_id')
getMatch(taxReport,18,'query_id')

################################
# metaCVReport - getter Tester #
################################

metaCV <- importMetaCV('../metacv.test.res')

countTaxa(metaCV,2)
countTaxa(metaCV)

getQueryDef(metaCV,2)
getScore(metaCV,2)
getScientificName(metaCV,2)
getKeggID(metaCV,2)
getCogID(metaCV,2)
getTaxID(metaCV,2)
getGeneID(metaCV,2)
