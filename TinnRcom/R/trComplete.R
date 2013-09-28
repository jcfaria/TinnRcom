#=======================================================================
# Orinal author: Philippe Grosjean
# Original code: [svMisc] Complete and guiComplete function
# Adapted by   : José Cláudio Faria
# Objective    : To supply the current necessity of the Tinn-R project
# Date         : 2013/09/24 - 17:19:40
#=======================================================================

trComplete <- function (code,
                        pattern='',
                        sep = '\t')
{
  # Get package
  get.pkg <- function(fname)
  {
    res <- getAnywhere(fname)

    res <- grep('package:',
                res$where,
                value=TRUE)

    pkg <- gsub('package:',
                '',
                res)

    return(pkg)
  }

  # Get object
  get.object <- function(string)
  {
    pos <- regexpr('[a-zA-Z0-9_\\.]+$',
                   string)

    object <- substring(string,
                        pos)

    return(object)
  }

  object <- get.object(code)

  pkg <- get.pkg(object)

  res <- .DollarNames(eval(as.name(object)),
                      pattern)
  
  res <- paste(paste(paste(res, 
                           sep, 
                           sep=''), 
                     collapse=''),
               '[',
               pkg,
               ']',
               '<',
               object,
               '>',
               sep='')                                 
  
  return(res)
}
