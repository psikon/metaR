\docType{methods}
\name{getTaxon}
\alias{getAccession,taxonomyReportDB-method}
\alias{getAlignLen,taxonomyReportDB-method}
\alias{getBitscore,taxonomyReportDB-method}
\alias{getDefinition}
\alias{getDefinition,taxonomyReportDB-method}
\alias{getEvalue,taxonomyReportDB-method}
\alias{getGaps,taxonomyReportDB-method}
\alias{getGeneID,taxonomyReportDB-method}
\alias{getHitFrame,taxonomyReportDB-method}
\alias{getHitFrom,taxonomyReportDB-method}
\alias{getHitID,taxonomyReportDB-method}
\alias{getHitLen,taxonomyReportDB-method}
\alias{getHitNum,taxonomyReportDB-method}
\alias{getHitSeq,taxonomyReportDB-method}
\alias{getHitTo,taxonomyReportDB-method}
\alias{getHspID,taxonomyReportDB-method}
\alias{getHspNum,taxonomyReportDB-method}
\alias{getIdentity,taxonomyReportDB-method}
\alias{getLineage}
\alias{getLineage,taxonomyReportDB-method}
\alias{getMatch,taxonomyReportDB-method}
\alias{getOtherName}
\alias{getOtherName,taxonomyReportDB-method}
\alias{getParentTaxID}
\alias{getParentTaxID,taxonomyReportDB-method}
\alias{getPositive,taxonomyReportDB-method}
\alias{getQueryDef,taxonomyReportDB-method}
\alias{getQueryFrame,taxonomyReportDB-method}
\alias{getQueryFrom,taxonomyReportDB-method}
\alias{getQueryID,taxonomyReportDB-method}
\alias{getQueryLen,taxonomyReportDB-method}
\alias{getQuerySeq,taxonomyReportDB-method}
\alias{getQueryTo,taxonomyReportDB-method}
\alias{getRank}
\alias{getRank,taxonomyReportDB-method}
\alias{getScientificName}
\alias{getScientificName,taxonomyReportDB-method}
\alias{getScore,taxonomyReportDB-method}
\alias{getTaxID}
\alias{getTaxID,taxonomyReportDB-method}
\alias{getTaxon}
\alias{getTaxon,taxonomyReportDB-method}
\title{Getter for taxonomyReportDB}
\usage{
getTaxon(x, ...)

getScientificName(x, ...)

getTaxID(x, ...)

getParentTaxID(x, ...)

getRank(x, ...)

getOtherName(x, ...)

getLineage(x, ...)

getDefinition(x, ...)

\S4method{getTaxID}{taxonomyReportDB}(x, id, type)

\S4method{getParentTaxID}{taxonomyReportDB}(x, id, type)

\S4method{getScientificName}{taxonomyReportDB}(x, id, type)

\S4method{getRank}{taxonomyReportDB}(x, id, type)

\S4method{getTaxon}{taxonomyReportDB}(x, id, type)

\S4method{getLineage}{taxonomyReportDB}(x, id, type)

\S4method{getOtherName}{taxonomyReportDB}(x, id, type)

\S4method{getQueryID}{taxonomyReportDB}(x, id, type)

\S4method{getHitID}{taxonomyReportDB}(x, id, type)

\S4method{getGeneID}{taxonomyReportDB}(x, id, type)

\S4method{getAccession}{taxonomyReportDB}(x, id, type)

\S4method{getQueryDef}{taxonomyReportDB}(x, id, type)

\S4method{getQueryLen}{taxonomyReportDB}(x, id, type)

\S4method{getHitNum}{taxonomyReportDB}(x, id, type)

\S4method{getDefinition}{taxonomyReportDB}(x, id, type)

\S4method{getHitLen}{taxonomyReportDB}(x, id, type)

\S4method{getHspID}{taxonomyReportDB}(x, id, type)

\S4method{getHspNum}{taxonomyReportDB}(x, id, type)

\S4method{getBitscore}{taxonomyReportDB}(x, id, type)

\S4method{getScore}{taxonomyReportDB}(x, id, type)

\S4method{getEvalue}{taxonomyReportDB}(x, id, type)

\S4method{getQueryFrom}{taxonomyReportDB}(x, id, type)

\S4method{getQueryTo}{taxonomyReportDB}(x, id, type)

\S4method{getHitFrom}{taxonomyReportDB}(x, id, type)

\S4method{getHitTo}{taxonomyReportDB}(x, id, type)

\S4method{getQueryFrame}{taxonomyReportDB}(x, id, type)

\S4method{getHitFrame}{taxonomyReportDB}(x, id, type)

\S4method{getIdentity}{taxonomyReportDB}(x, id, type)

\S4method{getPositive}{taxonomyReportDB}(x, id, type)

\S4method{getGaps}{taxonomyReportDB}(x, id, type)

\S4method{getAlignLen}{taxonomyReportDB}(x, id, type)

\S4method{getQuerySeq}{taxonomyReportDB}(x, id, type)

\S4method{getHitSeq}{taxonomyReportDB}(x, id, type)

\S4method{getMatch}{taxonomyReportDB}(x, id, type)
}
\arguments{
  \item{x}{a \code{taxonomyReportDB} object.}

  \item{id}{an identifier}

  \item{type}{type of identifier. One of \sQuote{tax_id},
  \sQuote{query_id}, or \sQuote{hit_id}.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
Getter for taxonomyReportDB
}

