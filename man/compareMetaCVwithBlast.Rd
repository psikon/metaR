\name{compareMetaCVwithBlast}
\alias{compareMetaCVwithBlast}
\title{find similarities between Blast and MetaCV results}
\usage{
compareMetaCVwithBlast(taxonomyReportDB, metaCVReport)
}
\arguments{
  \item{taxonomyReportDB}{}

  \item{metaCVReport}{}

  \item{taxonDB}{connection to taxonomy DB}
}
\value{
data.frame
}
\description{
The results of the two programms will be analyised by
\code{tax_id}, \code{rank} and \code{scientific_name},
basing on the \code{query_def} field
}

