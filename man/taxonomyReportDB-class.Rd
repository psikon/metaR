\name{taxonomyReportDB-class}
\alias{createTaxonomyReportDB}
\alias{show}
\alias{show,taxonomyReportDB-method}
\alias{taxonomyReportDB-class}
\alias{taxonomyReportDBConnect}
\title{taxonomyReportDB-class}
\usage{
  taxonomyReportDBConnect(db_path)

  createTaxonomyReportDB(blast_db,taxonomy_table)
}
\value{
  A \code{\linkS4class{taxonomyReportDB}} object.
}
\description{
  \sQuote{\code{taxonomyReportDB}} is an S4 class that
  represents a connection to an SQLite database holding
  blast records and taxonomical classifications organised
  in four tables:
}
\details{
  \bold{query} with fields:

  \itemize{ \item query_id INTEGER Primary key \item
  query_def TEXT \item query_len INTEGER }

  \bold{hit} with fields:

  \itemize{ \item query_id INTEGER \item hit_id INTEGER
  Primary key \item hit_num INTEGER \item gene_id TEXT
  \item accession TEXT \item definition TEXT \item length
  INTEGER }

  \bold{hsp} with fields:

  \itemize{ \item query_id INTEGER \item hit_id INTEGER
  \item hsp_id INTEGER Primary key \item hsp_num INTEGER
  \item bit_score FLOAT \item score INTEGER \item evalue
  FLOAT \item query_from INTEGER \item query_to INTEGER
  \item hit_from INTEGER \item hit_to INTEGER \item
  query_frame INTEGER \item query_frame INTEGER \item
  identity INTEGER \item positive INTEGER \item gaps
  INTEGER \item align_len INTEGER \item qseq TEXT \item
  hseq TEXT \item midline TEXT }

  \bold{taxonomy} with fields:

  \itemize{ \item query_id INTEGER \item hit_id INTEGER
  Primary key \item gene_id TEXT \item accession TEXT \item
  tax_id TEXT \item scientific_name TEXT \item rank TEXT }
}

