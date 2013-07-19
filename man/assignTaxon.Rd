\name{assignTaxon}
\alias{assignTaxon}
\title{assign Taxon(s) to a blast query}
\usage{
  assignTaxon(query_id, bitscore_tolerance = 0.98,
    coverage_threshold = 0.5,
    taxRanks = c("species", "genus", "tribe", "family", "order", "class", "phylum", "kingdom", "superkingdom"),
    blast_db, taxon_db)
}
\arguments{
  \item{query_id}{1:n \emph{query_id(s)} from
  \code{blastReportDB} object}

  \item{bitscore_tolerance}{tolerance value for the
  selection of hsp(s)}

  \item{coverage_threshold}{threshold for the selection of
  hit(s)}

  \item{taxRanks}{vector of defined taxonomic ranks}

  \item{blast_db}{\code{blastReportDB} object}

  \item{taxon_db}{\code{taxon_db} object}
}
\value{
  data.frame
}
\description{
  This algorithm filter the hits of a query under a number
  of conditions, to remove false annotations from the
  query.

  First all hit(s) of a blast query with a specific query
  coverage (default: >=50\%) will be selected. The
  corresponding hsp(s) are determined and a tolerance,
  based on the bitscore will be calculated (default: best
  bitscore * 98\%= tolerance). All hsp(s) within this
  tollerance will be selected for further processing and
  the rest discarded. Afterwards the number of hit(s) will
  be adjusted. For these remaining hit(s) the least common
  ancestor will be seeked.
}
\seealso{
  \code{\link{LCA}}
}

