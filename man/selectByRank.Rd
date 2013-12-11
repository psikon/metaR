\name{selectByRank}
\alias{selectByRank}
\title{select db entries by rank}
\usage{
selectByRank(txDB, taxRank, classifier)
}
\arguments{
  \item{txDB}{a \code{taxonomyReportDB} object}

  \item{taxRank}{a valid NCBI taxonomy rank (e.g.
  \emph{'genus'})}

  \item{classifier}{name of rank or species}

  \item{taxon_db}{\code{taxon_db} object}
}
\value{
data.frame
}
\description{
This function search in the database for given
specifications based on the taxonomical rank of the
database entry.
}

