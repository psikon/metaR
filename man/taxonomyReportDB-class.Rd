\docType{methods}
\name{generate.BlastReport}
\alias{generate.BlastReport}
\alias{generate.TaxonomyReport}
\alias{show,taxonomyReportDB-method}
\alias{taxonomyReportDB}
\alias{taxonomyReportDB-class}
\alias{taxonomyReportDBConnect}
\title{taxonomyReportDB-class}
\usage{
generate.BlastReport(fastq, chunksize = 1000, db = "16SMicrobial",
  max_hits = 25, evalue = 1e-06, perc_identity = 90, n.threads = NULL)

generate.TaxonomyReport(blast_db_path, taxon_db_path, chunksize = 1000,
  coverage_threshold = 0.5, bitscore_tolerance = 0.98,
  ranks = c("species", "genus", "tribe", "family", "order", "class", "phylum",
  "kingdom", "superkingdom"), ...)

\S4method{show}{taxonomyReportDB}(object)

taxonomyReportDB(blast_db, taxon_db_path = "", coverage_threshold = 0.5,
bitscore_tolerance = 0.98, ranks = c("species", "genus", "tribe", "family", "order",
"class", "phylum", "kingdom", "superkingdom"), .progress = "text")

taxonomyReportDBConnect(db_path)
}
\arguments{
  \item{blst_db}{A \code{\link[blastr]{blastReportDB}}
  object.}

  \item{taxon_db_path}{Path to a \code{taxonomyReportDB}.
  This can be an existing database file where new rows will
  be appended, or a database is created from scratch.}

  \item{coverage_threshold}{threshold for the selection of
  hits}

  \item{bitscore_tolerance}{tolerance value for the
  selection of hsps.}

  \item{ranks}{vector of taxonomic ranks along which the
  least common ancestor. is determined.}

  \item{.progress}{}

  \item{object}{Any R object}
}
\value{
A \code{\linkS4class{taxonomyReportDB}} object.
}
\description{
\sQuote{\code{taxonomyReportDB}} is an S4 class that
represents a connection to an SQLite database holding blast
records and taxonomic classifications organised in four
tables:
}
\details{
\bold{query} with fields:

\itemize{ \item query_id INTEGER Primary key \item
query_def TEXT \item query_len INTEGER }

\bold{hit} with fields:

\itemize{ \item query_id INTEGER \item hit_id INTEGER
Primary key \item hit_num INTEGER \item gene_id TEXT \item
accession TEXT \item definition TEXT \item length INTEGER }

\bold{hsp} with fields:

\itemize{ \item query_id INTEGER \item hit_id INTEGER \item
hsp_id INTEGER Primary key \item hsp_num INTEGER \item
bit_score FLOAT \item score INTEGER \item evalue FLOAT
\item query_from INTEGER \item query_to INTEGER \item
hit_from INTEGER \item hit_to INTEGER \item query_frame
INTEGER \item query_frame INTEGER \item identity INTEGER
\item positive INTEGER \item gaps INTEGER \item align_len
INTEGER \item qseq TEXT \item hseq TEXT \item midline TEXT
}

\bold{taxonomy} with fields:

\itemize{ \item query_id INTEGE^R Primary key \item tax_id
TEXT \item scientific_name TEXT \item rank TEXT }
}

