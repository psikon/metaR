\name{blastReportDB.generator}
\alias{blastReportDB.generator}
\title{Generate a blast reporter}
\usage{
blastReportDB.generator(db = "16SMicrobial", max_hits = 25, evalue = 1e-6,
perc_identity = 90, n.threads = floor(detectCores()*0.25))
}
\arguments{
  \item{db}{}

  \item{max_hits}{}

  \item{evalue}{}

  \item{perc_identity}{}

  \item{n.threads}{}
}
\value{
A handler function that can be passed to
\code{processChunks}.
}
\description{
Generate a blast reporter
}
\keyword{internal}

