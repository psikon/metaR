\name{getReadName}
\alias{assignLinage}
\alias{createMetaCVReport}
\alias{extractEukaryoticDB}
\alias{extractProkaryoticDB}
\alias{getCount}
\alias{getGeneID}
\alias{getReadName}
\alias{getScore}
\alias{getTaxID}
\alias{getTaxName}
\alias{mergeMetaCVwithBlast}
\alias{researchTaxIds}
\title{Getter methods for MetaCVReport}
\usage{
  createMetaCVReport(path)

  researchTaxIds(metaCVReport, path_to_DB)

  assignLinage(geneID)

  mergeMetaCVwithBlast(metaCVReport, blastReportDB)

  extractEukaryoticDB(path_to_DB)

  extractProkaryoticDB(path_to_DB)
}
\arguments{
  \item{object}{A \code{\linkS4class{metaCVReport}]
  object.}

  \item{x}{index of the Object}

  \item{object}{A \code{\linkS4class{metaCVReport}]
  object.}

  \item{x}{index of the Object}

  \item{object}{A \code{\linkS4class{metaCVReport}]
  object.}

  \item{x}{index of the Object}

  \item{object}{A \code{\linkS4class{metaCVReport}]
  object.}

  \item{x}{index of the Object}

  \item{object}{A \code{\linkS4class{metaCVReport}]
  object.}

  \item{x}{index of the Object}

  \item{object}{A \code{\linkS4class{metaCVReport}]
  object.}

  \item{x}{index of the Object}

  \item{path}{path to the resulting .res File from metaCV}

  \item{metaCVReport}{A \code{\linkS4class{metaCVReport}]
  object.}

  \item{path}{path to the taxonomy database}

  \item{geneID}{ID for NCBI database}

  \item{metaCVReport}{A \code{\linkS4class{metaCVReport}]
  object.}

  \item{blastReportDB}{}

  \item{metaCVReport}{A \code{\linkS4class{metaCVReport}]
  object.}

  \item{path}{path to the taxonomy database}

  \item{metaCVReport}{A \code{\linkS4class{metaCVReport}]
  object.}

  \item{path}{path to the taxonomy database}
}
\description{
  Getter methods for MetaCVReport

  Getter methods for MetaCVReport

  Getter methods for MetaCVReport

  Getter methods for MetaCVReport

  Getter methods for MetaCVReport

  Getter methods for MetaCVReport

  Import the Results of the .res File from MetaCV to a R
  and creates an \code{\linkS4class{metaCVReport}] object

  find a better taxID for the GeneID of the object in the
  NCBI taxonomy database

  find the linage in the NCBI taxonomy database basing on
  the geneID

  compare the results of a MetaCVReport object with the
  results of a BlastReportDB object to find hits contained
  in both of them

  find only the eukaryotic hits in the blastReportDB
  object, basing on the linage and seperate them in an own
  database

  find only prokaryotic hits in the blastReportDB object,
  basing on the linage and separate them in an own database
}

