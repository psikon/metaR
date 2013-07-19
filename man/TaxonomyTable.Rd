\name{createTaxonomyTable}
\alias{createTaxonomyTable}
\title{extend and update the blast database}
\usage{
  createTaxonomyTable()
}
\arguments{
  \item{blastDB}{\code{blastReportDB} object}
}
\description{
  the orignial blast database generated from a XML file
  will be extended and updated by a taxonomy table
  containing informations about the taxonomical
  classification for a specific query.

  \section{Slots:}{ \describe{
  \item{\code{query_id}:}{identifier for the query}
  \item{\code{hit_id}:}{identifier for the best hit used
  for classification} \item{\code{gene_id}:}{blast
  identifier for a sequence} \item{\code{accession}:}{blast
  identifier for a sequence}
  \item{\code{tax_id}:}{identifier in the ncbi taxonomy}
  \item{\code{scientific_name}:}{name in the ncbi taxonomy}
  \item{\code{rank}:}{rank in the ncbi taxonomy}
  \item{\code{superkingdom}:}{Bacteria, Eukaryota or
  unclassified} } }
}

