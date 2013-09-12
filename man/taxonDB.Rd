\name{connectTaxonDB}
\alias{connectTaxonDB}
\alias{createTaxonDB}
\alias{updateTaxonDB}
\title{Create or connect a local install of NCBI Taxonomy database}
\usage{
  connectTaxonDB(path_to_db)

  createTaxonDB(path_to_db)

  updateTaxonDB(path_to_db)
}
\arguments{
  \item{path_to_db}{parent directory for the taxonomy db}
}
\description{
  Create, update or connect to a local installation of the
  NCBI Taxonomy Database. The Connection object is returned
  as a list of connections to \code{'taxon.db'} and
  \code{'geneid.db'}.
}

