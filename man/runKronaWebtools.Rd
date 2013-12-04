\name{runKronaWebtools}
\alias{runKronaWebtools}
\title{run the Krona Webtools from R}
\usage{
  runKronaWebtools(input, output, program_path)
}
\arguments{
  \item{input}{tab-separated input file (created by
  \link{createKronaFile()})}

  \item{output}{location and name for the output}

  \item{program_path}{location of the Krona Webtool
  binaries}
}
\description{
  Wrapper function for running the Krona Webtools Script
  \emph{ImportText.pl} from the R-Environment. The needed
  Input file can be created with
  \link{createKronaFile()}-function.
}
\seealso{
  createKronaFile
}

