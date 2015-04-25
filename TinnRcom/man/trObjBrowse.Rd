\name{trObjBrowse}
\alias{trObjSearch}
\alias{trObjList}

\title{
  Functions to implement the an object browser (R explorer) in Tinn-R
}

\description{
  These functions provide features required to implement a complete object
  browser in Tinn-R as GUI client.
}

\usage{
  trObjSearch(sep="\t",
              path=NULL)

  trObjList(id="default",
            envir=.GlobalEnv,
            all.names=TRUE,
            pattern="",
            group="",
            all.info=FALSE,
            sep="\t",
            path=NULL)
}

\arguments{
  \item{sep}{Separator to use between items in the file (if path is not NULL).}
  \item{path}{The path where to write a temporary file with the requested
    information. Use \code{path=NULL} (default) if you don't want to pass this data by mean of a file.}
  \item{id}{The id of the object browser (you can run several ones concurrently, providing you give them different ids).}
  \item{envir}{An environment, or the name of the environment, or the position in the \code{search()} path.}
  \item{all.names}{Does it display all names (including hidden variables starting with '.')?}
  \item{pattern}{A pattern to match for selecting variables.}
  \item{group}{A group to filter.}
  \item{all.info}{Do we return all the information (envir as first column or not (by default).}
}

\details{
  \code{trObjSearch()} lists the search path.
  \code{trObjList()} lists objects in a given environment.
}

\value{
  Depending on the function, a list, a string, a reference to an external,
  temporary file or \code{TRUE} in case of success or \code{FALSE} otherwise
  is returned invisibly.
}

\author{
  José Cláudio Faria \email{joseclaudio.faria@gmail.com}\cr
}

\keyword{misc}
