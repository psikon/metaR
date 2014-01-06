\name{assignTaxa}
\alias{assignTaxa}
\title{Assign Taxa to Blast queries.}
\usage{
assignTaxa(blast_db, coverage_threshold = 0.5, bitscore_tolerance = 0.98,
  query_id = NULL, ranks = c("species", "genus", "family", "order", "class",
  "phylum", "kingdom", "superkingdom"), ...)
}
\arguments{
  \item{blast_db}{A \code{\link[blastr]{blastReportDB}}
  object.}

  \item{coverage_threshold}{threshold for the selection of
  hits}

  \item{bitscore_tolerance}{tolerance value for the
  selection of hsps}

  \item{query_id}{Indices of \emph{query_id}s to annotate
  (default: all query_ids in \code{blast_db}).}

  \item{ranks}{vector of taxonomic ranks along which the
  least common ancestor is determined.}

  \item{...}{}
}
\value{
A \code{data.frame}
}
\description{
This algorithm selects Blast hits based on a series of
conditions to improve the taxonomic assignment to a query
sequence.

First only hits for a query with a query coverage above
\code{coverage_threshold} (default: >= 50\%) will be
retained. Next, for all remaining hits only hsps are
retained with a bitscore within a lower bound defined by
the overall maximum bitscore * \code{bitscore_tolerance}
(default: 0.98). Hits with no remaining hsps are discarded,
and for the remaining hits the least common ancestor
\code{\link{LCA}} is determined.
}
\seealso{
\code{\link{LCA}}
}

