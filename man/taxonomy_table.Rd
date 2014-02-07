\name{taxonomy_table}
\alias{taxonomy_table}
\title{Generate an observation metadata table}
\usage{
taxonomy_table(txdb, otu_prefix = "OTU_", include_qseq = FALSE)
}
\arguments{
  \item{txdb}{A \code{\linkS4class{taxonomyReportDB}}
  object.}

  \item{otu_prefix}{String before numeric OTU indices.}

  \item{include_qseq}{Include the query sequence (if
  available) into the output.}
}
\value{
A \code{data.frame}.
}
\description{
Generate an observation metadata table
}
\keyword{internal}

