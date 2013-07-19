\name{LCA}
\alias{LCA}
\title{find the least common ancestor inside a blast query with multiple hits}
\usage{
  LCA(query_table, taxon_db,
    taxRanks = c("species", "genus", "tribe", "family", "order", "class", "phylum", "kingdom", "superkingdom"))
}
\arguments{
  \item{query_table}{dataframe containing hits from a blast
  search}

  \item{taxon_db}{list of connections to taxon_db and
  geneid_db}

  \item{taxRanks}{vector of levels of the ncbi taxonomy}
}
\value{
  data.frame
}
\description{
  if a \emph{query_id} has multiple hit(s) with multiple
  taxa, the least common ancestor (lca) of this hit(s) must
  be find. Firstly all hit(s) will be assigned a
  \emph{taxon} at a specified starting rank. If the taxa
  are different on this starting rank the taxa will be
  mapped to the next higher rank of the taxonomy and also
  checked for uniqueness. This procedur will be repeated
  until the top rank is reached.
}
\note{
  To improve the performance of this algorithm the hit(s)
  will be first checked for differences in the top rank. If
  there exists differences the query will be discarded.
}
\seealso{
  \code{\link{assignTaxon()}}
}

