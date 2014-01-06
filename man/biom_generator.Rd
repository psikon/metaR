\name{biom_generator}
\alias{biom_generator}
\title{Generate a parser function for converting taxonomyReportDB objects to a biom object}
\usage{
biom_generator(id = NULL, format = "Biological Observation Matrix 1.0",
  format_url = "http://biom-format.org/documentation/format_versions/biom-1.0.html",
  type = "OTU table")
}
\arguments{
  \item{id}{a field that can be used to id a table}

  \item{format}{name and version of the current biom
  format}

  \item{static}{URL providing format details}

  \item{type}{Table type (a controlled vocabulary)}
}
\description{
Generate a parser function for converting taxonomyReportDB
objects to a biom object
}
\keyword{internal}

