\name{getReadName}
\alias{getByScore}
\alias{getGeneID}
\alias{getGeneIDbyTaxID}
\alias{getReadName}
\alias{getScore}
\alias{getTaxID}
\alias{getTaxName}
\alias{metaCVReport-class}
\alias{show,metaCVReport-method}
\title{Getter methods for MetaCVReport}
\arguments{
  \item{x}{A metaCVReport object.}

  \item{i}{index of the Object}

  \item{x}{A metaCVReport object.}

  \item{i}{index of the Object}

  \item{x}{A metaCVReport object.}

  \item{i}{index of the Object}

  \item{x}{A metaCVReport object.}

  \item{i}{index of the Object}

  \item{x}{A metaCVReport object.}

  \item{i}{index of the Object}

  \item{x}{A metaCVReport object.}

  \item{i}{taxID(s) for searching}

  \item{x}{A metaCVReport object.}

  \item{i}{score}

  \item{...}{Slots for \sQuote{metaCV-Report} instances.}
}
\description{
  Getter methods for MetaCVReport

  Getter methods for MetaCVReport

  Getter methods for MetaCVReport

  Getter methods for MetaCVReport

  Getter methods for MetaCVReport

  get for a specifig taxID all geneID(s)

  get only the entries with a score greater than x, to
  prevent wrongly classified entries

  metaCVReport is an S4 class that provides a container for
  data retrieved from the .res outputfile of metaCV
}
\details{
  The score refers to the composition identity between the
  query read and the best target gene, and ranges from 0 to
  100. In our analysis, reads with very low scores are
  highly possible to be wrongly classified and should be
  filtered. Recommended is a set of cutoff values (minimal
  scores to pass) according to different read lengths:
  \itemize{ \item{Read length}{Minimal score} \item{100 bp
  }{20} \item{200 bp}{10} \item{400 bp}{5} \item{600 bp}{4}
  \item{800 bp}{3} \item{1000 bp}{3} }

  metaCVReport objects have 5 slots: \itemize{
  \item{read_name}{Name of the Read in Input File}
  \item{score}{Score of the detection with MetaCV}
  \item{geneID}{ID for NCBI database} \item{taxID}{ID for
  NCBI Taxonomy database} \item{taxname}{Name in NCBI
  Taxonomy database}

  }
}

