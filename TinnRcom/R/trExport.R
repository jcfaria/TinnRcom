#=======================================================================
# Orinal author: Philippe Grosjean
# Original code: [svIO] export.default function
# Adapted by   : Jos� Cl�udio Faria
# Objective    : To supply the current necessity of the Tinn-R project
# Date         : 2015/03/11 - 14:04:15
#=======================================================================

trExport <- function(x,
                     type='raw',
                     file='clipboard')
{
  # ASCII sub-function
  exportASCII <- function(x,
                          file)
  {
    treated <- FALSE
    if(is.vector(x)) {
      txt <- paste(x,
                   collapse = '\t')
      treated <- TRUE
    }
    if(is.matrix(x)) {
      txt <- as.character(x)

      txt <- paste(apply(x,
                         1,
                         FUN=paste,
                         collapse='\t'),
                   collapse='\n')

      treated <- TRUE
    }
    if(!treated) {
      tmpfile <- tempfile()

      sink(tmpfile)

      print(x)

      sink()

      txt <- readLines(tmpfile)

      txt <- paste(txt,
                   collapse='\n')
    }

    ifelse(file != 'clipboard',
           tmpfile <- file(file,
                           open='w'),
           tmpfile <- file)

    cat(txt,
        file=tmpfile)

    if(file != 'clipboard')
      close(tmpfile)

    invisible(return(TRUE))
  }

  # RAW sub-function
  exportRaw <- function(x,
                        file)
  {
    ifelse(file != 'clipboard',
           tmpfile <- file(file,
                           open='w'),
           tmpfile <- file)

    dput(x,
         file=tmpfile)

    if(file != 'clipboard')
      close(tmpfile)

    invisible(return(TRUE))
  }

  # Compute the expression
  if(is.expression(x))
     xexp <- x
  else
    xexp <- NULL

  if(!is.expression(xexp) ||
     is.null(xexp)) {
    xexp <- substitute(x)

    # To make sure that non conventional names will be correctly evaluated, we use backticks!
    if(is.character(xexp))
      xexp <- parse(text=paste('`',
                    xexp,
                    '`',
                    sep=''))

    xexp <- as.expression(xexp)
  }

  # Process the command in the standard function 
  x <- eval(xexp,
            envir=.GlobalEnv)

  res <- switch(type,
    'typelist'= unique(c('raw',
                         'ascii')),
    exportRaw(x,
              file),

    'ascii' = exportASCII(x,
                          file))

  ifelse(type == 'typelist',
         return(res),
         invisible(res))
}
