\name{trArgs}
\alias{trArgs}

\title{
  Show function arguments in a human-readable way
}

\description{
  \code{trArgs()} displays function arguments in a better way than \code{args()}
  does. It is primarily intended for code completion and call tip for Tinn-R project.
}

\usage{
trArgs(fname,
       txt='',
       pkg='',
       classfor,
       sep='|')
}

\arguments{
  \item{fname}{A string with the name of a function.}
  \item{txt}{A string with a \R valid object.}
  \item{pkg}{A string with a package name.}
  \item{classfor}{A string with a \R valid object.}
  \item{sep}{The field separator character.
    The function arguments are separated by this character.
    '|' is the default.}
}

\value{
  A string with the calling syntax of the function.
}

\author{
  José Cláudio Faria \email{joseclaudio.faria@gmail.com}\cr
}

\note{
  \code{Args()} is supposed to display S3 and S4 methods, and primitives
  adequately,... but this is not full implemented yet in the current version!
}

\keyword{utilities}
