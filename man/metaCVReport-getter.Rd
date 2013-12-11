\docType{methods}
\name{getQueryDef,metaCVReport-method}
\alias{getCogID,metaCVReport-method}
\alias{getGeneID,metaCVReport-method}
\alias{getKeggID,metaCVReport-method}
\alias{getQueryDef,metaCVReport-method}
\alias{getScientificName,metaCVReport-method}
\alias{getScore,metaCVReport-method}
\alias{getTaxID,metaCVReport-method}
\title{Getter for metaCVReport}
\usage{
\S4method{getQueryDef}{metaCVReport}(x, id)

\S4method{getScore}{metaCVReport}(x, id)

\S4method{getGeneID}{metaCVReport}(x, id)

\S4method{getKeggID}{metaCVReport}(x, id)

\S4method{getCogID}{metaCVReport}(x, id)

\S4method{getTaxID}{metaCVReport}(x, id)

\S4method{getScientificName}{metaCVReport}(x, id)
}
\arguments{
  \item{metaCVReport}{a \code{metaCVReport} object}

  \item{id}{tax_id}

  \item{x}{A \linkS4class{blastReport} or
  \linkS4class{IterationList} object.}
}
\description{
Getter for metaCVReport
}

