\name{connectTaxonDB}
\alias{connectTaxonDB}
\alias{createTaxonDB}
\alias{updateTaxonDB}
\title{Create or connect to a local install of NCBI Taxonomy database}
\usage{
  connectTaxonDB(db_path = getOption("ncbi.taxonomy.path"))

  createTaxonDB(db_path = getOption("ncbi.taxonomy.path"))

  updateTaxonDB(db_path = getOption("ncbi.taxonomy.path"))
}
\arguments{
  \item{db_path}{Parent directory of the taxonomy databases
  \bold{taxon.db} and \bold{geneid.db}. This path should be
  permanently configured by setting the option
  \code{ncbi.taxonomy.path}.}
}
\description{
  Create, update or connect to a local installation of the
  NCBI Taxonomy Database. The Connection object is returned
  as a list of connections to \code{'taxon.db'} and
  \code{'geneid.db'}.
}

