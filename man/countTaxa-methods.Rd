\docType{methods}
\name{countTaxa}
\alias{countTaxa}
\alias{countTaxa,metaCVReport-method}
\alias{countTaxa,taxonomyReportDB-method}
\title{Count the occurences of \code{tax_id}s}
\usage{
countTaxa(x, ...)

\S4method{countTaxa}{metaCVReport}(x, id = NULL)

\S4method{countTaxa}{taxonomyReportDB}(x, id = NULL)
}
\arguments{
  \item{metaCVReport}{a \code{metaCVReport} object}

  \item{taxonomyReportDB}{a \code{taxonomyReportDB} object}

  \item{id}{tax_id (if only one tax_id have to be count)}
}
\value{
data.frame
}
\description{
count the occurences of tax_id(s) in a given metaCVReport
or taxonomyReportDB object and return a dataframe
consisting of the tax_id(s) itself and their frequencies in
the object. it is also possible to only count one special
tax_id
}

