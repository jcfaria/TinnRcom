\name{trComplete}
\alias{trComplete}

\title{
  Get a completion list for a R code fragment
}

\description{
  A list with matching items is returned in a string.
}

\usage{
  trComplete(code,
             pattern='',
             sep='\t')
}

\arguments{
  \item{code}{A piece of \R code (in a character string) to analyze.}
  \item{pattern}{A string.}
  \item{sep}{Separator to use between items in the file (if file is not \code{NULL}).}
}

\value{
  A string with the completion list (items separated by \sQuote{sep}), or
  an unique string completing unambiguously the current code, starting with
  \sQuote{sep}.
}

\details{
  The completion list is context-dependent, and it is calculated as if the
  code was entered at the command line.

  If the code ends with \code{$}, then the function look for
  items in a \code{list} or \code{data.frame} whose name is the last identifier.

  If the code ends with \code{@}, then the function look for slots of the
  corresponding S4 object.

  If the code ends with \code{::}, then it looks for objects in a namespace.

  If the code ends with a partial identifier name, the function returns all
  visible matching keywords.

  There are other situations where the completion can be calculated, see
  the help of \code{rc.settings()}.
}

\author{
  José Cláudio Faria \email{joseclaudio.faria@gmail.com}\cr
}

\keyword{utilities}
