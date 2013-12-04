\name{metaCVReport-class}
\alias{importMetaCV}
\alias{metaCVReport-class}
\alias{show,metaCVReport-method}
\title{metaCVReport class}
\usage{
  importMetaCV(path)
}
\arguments{
  \item{...}{Slots for \sQuote{metaCV-Report} instances.}

  \item{path}{}
}
\description{
  metaCVReport is an S4 class that provides a container for
  data retrieved from the .res outputfile of metaCV
}
\details{
  metaCVReport objects have 7 slots: \itemize{ \item
  query_def name of read in input file \item score score of
  the detection with MetaCV \item kegg_id identifier for
  KEGG Pathway \item cog_id identifier for ?  \item gene_id
  identifier for NCBI sequence database \item tax_id
  identifier for NCBI taxonomy database \item
  scientific_name name in NCBI taxonomy database

  }
}

