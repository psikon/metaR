\name{metaCVReport}
\alias{mergeMetaCVwithBlast}
\alias{metaCVReport}
\alias{taxonCount}
\title{Import a metaCV .res file}
\usage{
  mergeMetaCVwithBlast(metaCVReport, blastReportDB)
}
\arguments{
  \item{path}{path to the resulting .res File from metaCV}

  \item{x}{A metaCVReport object.}

  \item{metaCVReport}{A metaCVReport object.}

  \item{blastReportDB}{blastReportDB}
}
\description{
  Import the Results of the .res File from MetaCV to a R
  and creates an metaCVReport object

  creates a code{data.frame}, that contains the counts
  ordered by taxIds and the corresponding taxName

  compare the results of a MetaCVReport object with the
  results of a BlastReportDB object to find hits contained
  in both of them
}

