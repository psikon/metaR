\name{getReadName}
\alias{assignLinage}
\alias{extractEukaryoticDB}
\alias{extractProkaryoticDB}
\alias{getByScore}
\alias{getGeneID}
\alias{getGeneIDbyTaxID}
\alias{getReadName}
\alias{getScore}
\alias{getTaxID}
\alias{getTaxName}
\alias{mergeMetaCVwithBlast}
\alias{metaCVReport}
\alias{researchTaxIds}
\alias{taxonCount}
\title{Getter methods for MetaCVReport}
\usage{
  researchTaxIds(metaCVReport, path_to_DB)

  assignLinage(geneID)

  mergeMetaCVwithBlast(metaCVReport, blastReportDB)

  extractEukaryoticDB(path_to_DB)

  extractProkaryoticDB(path_to_DB)
}
\arguments{
  \item{x}{A \code{\linkS4class{metaCVReport}] object.}

  \item{i}{index of the Object}

  \item{x}{A \code{\linkS4class{metaCVReport}] object.}

  \item{i}{index of the Object}

  \item{x}{A \code{\linkS4class{metaCVReport}] object.}

  \item{i}{index of the Object}

  \item{x}{A \code{\linkS4class{metaCVReport}] object.}

  \item{i}{index of the Object}

  \item{x}{A \code{\linkS4class{metaCVReport}] object.}

  \item{i}{index of the Object}

  \item{x}{A \code{\linkS4class{metaCVReport}] object.}

  \item{i}{index of the Object}

  \item{x}{A \code{\linkS4class{metaCVReport}] object.}

  \item{i}{score}

  \item{path}{path to the resulting .res File from metaCV}

  \item{x}{A \code{\linkS4class{metaCVReport}] object.}

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

  get only the entries with a score greater than x, to
  prevent wrongly classified entries

  Import the Results of the .res File from MetaCV to a R
  and creates an \code{\linkS4class{metaCVReport}] object

  creates a df, that contains the counts ordered by taxIds
  and the corresponding taxName

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
\details{
  The score refers to the composition identity between the
  query read and the best target gene, and ranges from 0 to
  100. In our analysis, reads with very low scores are
  highly possible to be wrongly classified and should be
  filtered. Recommended is a set of cutoff values (minimal
  scores to pass) according to different read lengths: Read
  length Minimal score 100 bp 20 200 bp 10 400 bp 5 600 bp
  4 800 bp 3 1000 bp 3
}

