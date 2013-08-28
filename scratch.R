require(metaR)
require(ncbi)
require.all('digest', 'roxygen2', 'R.utils')
document(pkg=".", clean=T)
my <- edit(roxygen2:::rd_arguments)
reassignInPackage("rd_arguments", pkgName="roxygen2", my)
document(".", clean=TRUE)
devtools::test()

# connection herstellen
blastReportDB <- blastReportDBConnect("~/local/workspace/bigBlastParser/test.db")
taxDB <- connectTaxonDB("~/local/workspace/taxonomy/")
taxReportDB <- taxonomyReportDBConnect('cache//taxonomy.db')


# taxonomy data.frame erstellen
db_df <- assignTaxon(1:1000, 
                     taxRanks = c("species", "genus", "tribe", "family", "order",
                                  "class", "phylum", "kingdom", "superkingdom"),
                     blast_db = blastReportDB, 
                     taxon_db = taxDB)

# alles in ein neues Objekt umschichten
taxReportDB <- createTaxonomyReportDB('cache/taxonomy.db', blastReportDB, db_df, 0.98)
taxReportDB <- taxonomyReportDBConnect('cache/taxonomy.db')

selectByRank(x=taxReport, taxRank='genus', classifier='epinephelus', taxon_db=taxDB)

hitid <- db_query(taxReportDB, "SELECT hit_id FROM taxonomy", 1L)
taxid <- getTaxID(x=taxReportDB, id=hitid, type='hit_id')
taxonDB(taxid, taxon_db=taxDB[['taxon_db']])

save(taxid, file="../ncbi/cache/taxid.rdata")


undebug(taxonDB)
undebug(ncbi:::new_taxon)
undebug(ncbi:::dbGetTaxon)
undebug(ncbi:::dbGetLineage)
debug(ncbi:::Lineage)
debug(ncbi:::new_Lineage)
undebug(new)
taxa <- getTaxon(x=taxReport, hit_id, type='hit_id', taxDB)





r <- db_df[grep(classifier, tolower(getByRank(getTaxon(taxReport,
                                                       db_df$hit_id,
                                                       'hit_id',
                                                       taxDB),
                                              taxRank,value='ScientificName'))),]


###TODO

# getByRank wrapper integrieren
# selection

# getter Tester
getQueryId(taxReport,94232,'tax_id')
getQueryId(taxReport,5,'hit_id')
getQueryId(taxReport,18,'query_id')

getHitId(taxReport,94232,'tax_id')
getHitId(taxReport,5,'hit_id')
getHitId(taxReport,18,'query_id')

getGeneId(taxReport,94232,'tax_id')
getGeneId(taxReport,5,'hit_id')
getGeneId(taxReport,18,'query_id')

getAccession(taxReport,94232,'tax_id')
getAccession(taxReport,5,'hit_id')
getAccession(taxReport,18,'query_id')

getTaxId(taxReport,94232,'tax_id')
getTaxId(taxReport,5,'hit_id')
getTaxId(taxReport,18,'query_id')

getScientificName(taxReport,94232,'tax_id')
getScientificName(taxReport,5,'hit_id')
getScientificName(taxReport,18,'query_id')

getRank(taxReport,94232,'tax_id')
getRank(taxReport,5,'hit_id')
getRank(taxReport,18,'query_id')

getTaxon(taxReport,94232,'tax_id',taxDB)
getTaxon(taxReport,5,'hit_id',taxDB)
getTaxon(taxReport,18,'query_id',taxDB)

getLineage(taxReport,94232,'tax_id',taxDB)
getLineage(taxReport,5,'hit_id',taxDB)
getLineage(taxReport,18,'query_id',taxDB)

getOtherName(taxReport,293821,'tax_id',taxDB)
getOtherName(taxReport,16,'hit_id',taxDB)
getOtherName(taxReport,18,'query_id',taxDB)

getParentTaxId(taxReport,293821,'tax_id',taxDB)
getParentTaxId(taxReport,16,'hit_id',taxDB)
getParentTaxId(taxReport,18,'query_id',taxDB)

getParentTaxId(taxReport,293821,'tax_id',taxDB)
getParentTaxId(taxReport,16,'hit_id',taxDB)
getParentTaxId(taxReport,18,'query_id',taxDB)

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
getHspId(taxReport,38293,'tax_id')
getHspId(taxReport,5,'hit_id')
getHspId(taxReport,18,'query_id')

getHspNum(taxReport,94232,'tax_id')
getHspNum(taxReport,5,'hit_id')
getHspNum(taxReport,18,'query_id')

getBitScore(taxReport,94232,'tax_id')
getBitScore(taxReport,5,'hit_id')
getBitScore(taxReport,18,'query_id')

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

getMidline(taxReport,94232,'tax_id')
getMidline(taxReport,5,'hit_id')
getMidline(taxReport,18,'query_id')


