\name{metaCVReport-class}
\alias{metaCVReport-class}
\alias{show,metaCVReport-method}
\title{metaCVReport class}
\arguments{
  \item{...}{Slots for \sQuote{metaCV-Report} instances.}
}
\description{
  metaCVReport is an S4 class that provides a container for
  data retrieved from the .res outputfile of metaCV
}
\details{
  metaCVReport objects have 5 slots: \describe{
  \item{read_name}{Name of the Read in Input File}
  \item{score}{Score of the detection with MetaCV}
  \item{geneID}{ID for NCBI database} \item{taxID}{ID for
  NCBI Taxonomy database} \item{taxname}{Name in NCBI
  Taxonomy database}

  }
}

