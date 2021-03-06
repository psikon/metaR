\name{createTable}
\alias{createTable}
\alias{dropTable}
\alias{updateTable}
\title{create, update or drop a SQL-Table}
\usage{
createTable(db, tbl, tbl_scheme)

updateTable(db, tbl, df, ...)

dropTable(db, tbl)
}
\arguments{
  \item{blast_db}{connection to a database}

  \item{tbl}{name of table}

  \item{sql}{string with table scheme}

  \item{df}{data.frame generated by assignTaxon()}
}
\description{
wrapper for database manipulation in SQL
}

