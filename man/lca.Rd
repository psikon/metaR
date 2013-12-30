\name{LCA}
\alias{LCA}
\title{Find the least common ancestor for a blast query with multiple hits}
\usage{
LCA(hits, ranks = c("species", "genus", "tribe", "family", "order", "class",
  "phylum", "kingdom", "superkingdom"), log = NULL)
}
\arguments{
  \item{hits}{dataframe containing hits from a blast
  search}

  \item{ranks}{vector of levels of the ncbi taxonomy}
}
\value{
A \code{data.frame}
}
\description{
if a \emph{query_id} has multiple hit(s) with multiple
taxa, the least common ancestor (lca) of this hit(s) should
be found. Firstly all hit(s) will be assigned a
\emph{taxon} at a specified starting rank. If the taxa are
different on this starting rank the taxa will be mapped to
the next higher rank of the taxonomy and also checked for
uniqueness. This procedur will be repeated until the top
rank is reached.
}
\note{
To improve the performance of this algorithm the hit(s)
will be first checked for differences in the top rank. If
there exists differences the query will be discarded.
}
\seealso{
\code{\link{assignTaxon()}}
}

