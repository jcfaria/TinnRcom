\name{trArgs}
\alias{trArgs}

\title{
  Show function arguments in a human-readable way - get a call tip
}

\description{
  \code{trArgs()} displays function arguments in a better way than \code{args()}
  does. It is primarily intended for code tips in GUIs.
}

\usage{
trArgs(fname,
       txt='',
       pkg='',
       classfor)
}

\arguments{
  \item{fname}{A string with the name of a function.}
  \item{txt}{A string with a \R valid object.}
  \item{pkg}{A string with a package name.}
  \item{classfor}{A string with a \R valid object.}
}

\value{
  A string with the calling syntax of the function
}

\author{
  Original authors: Philippe Grosjean \email{phgrosjean@sciviews.org}\cr
                    Jakson A. de Aquino \email{jalvesaq@gmail.com}\cr
  Adaptations: José Cláudio Faria \email{joseclaudio.faria@gmail.com}\cr
}

\note{
  \code{Args()} is supposed to display S3 and S4 methods, and primitives
  adequately,... but this is not implemented yet in the current version!
}

\keyword{utilities}
