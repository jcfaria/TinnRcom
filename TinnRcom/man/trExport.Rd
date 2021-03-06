\name{trExport}
\alias{trExport}

\title{
  Export the content of an object in a foreign format
}

\description{
  \code{trExport} is a generic function (S3 based) to convert objects into
  foreign formats and write it to a file or a connection.
}

\usage{
  trExport(x,
           type="raw",
           file="clipboard")
}

\arguments{
  \item{x}{The object to trExport.}
  \item{type}{\code{type} gives the format in which the object should be
    converted. The list of recognized formats for this object is obtained with
    \code{type="typelist"}.}
  \item{file}{The path to the file where the converted object should be
    extracted.}
}

\value{
  Return the result from specif trExportation methods (invisibly if the file
  is written).
}

\author{
  Jos� Cl�udio Faria \email{joseclaudio.faria@gmail.com}
}

\keyword{IO}
\keyword{file}
