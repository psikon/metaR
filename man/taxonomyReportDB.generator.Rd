\name{taxonomyReportDB.generator}
\alias{taxonomyReportDB.generator}
\title{Generate a taxon assigner}
\usage{
taxonomyReportDB.generator(blast_db_path, taxonomy_db_path = NULL, coverage_threshold = 0.5,
bitscore_tolerance = 0.98, ranks = c("species", "genus", "family", "order",
"class", "phylum", "kingdom", "superkingdom"))
}
\arguments{
  \item{taxon_db_path}{}

  \item{coverage_threshold}{}

  \item{bitscore_tolerance}{}

  \item{ranks}{}

  \item{compact_queries}{}

  \item{log}{}
}
\value{
A function that can be passed to \code{processChunks}.
}
\description{
Generate a taxon assigner
}
\keyword{internal}

