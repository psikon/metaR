\name{selectByScore}
\alias{selectByScore}
\title{filter \code{metaCVReport} by score}
\usage{
  selectByScore(metaCVReport, score)
}
\arguments{
  \item{x}{metaCVReport object}

  \item{score}{}
}
\value{
  metaCVReport
}
\description{
  get only the entries with a score greater than x, to
  prevent wrongly classified entries
}
\details{
  The score refers to the composition identity between the
  query read and the best target gene, and ranges from 0 to
  100. In a analysis of the MetaCV developers, reads with
  very low scores are highly possible to be wrongly
  classified and should be filtered. Recommended is a set
  of cutoff values (minimal scores to pass) according to
  different read lengths: \itemize{ \item Read length
  Minimal score \item 100 bp 20 \item 200 bp 10 \item 400
  bp 5 \item 600 bp 4 \item 800 bp 3 \item 1000 bp 3 }
}

