#=======================================================================
# Orinal author: Jakson Alves de Aquino
# Original code: [vimcom] package
# Adapted by   : José Cláudio Faria
# Objective    : To supply the current necessity of the Tinn-R project
# Date         : 2013/10/30 - 12:20:30
#=======================================================================

trArgs <- function(fname,
                   txt='',
                   pkg='',
                   classfor,
                   sep='|')
{

  # Function to format output
  format.out <- function(arg,
                         pk,
                         fname)
  {
    return(paste(arg,
                 ' [',
                 pk,
                 ']',
                 '<',
                 fname,
                 '>',
                 sep=''))
  }

  # Args from primitives
  primitive.args <- function(x)
  {
    fun <- get(x)
    f <- capture.output(args(x))

    f <- sub(') $',
             '',
             sub('^function \\(',
                 '',
                 f[1]))

    f <- strsplit(f,
                  ',')[[1]]

    f <- sub('^ ',
             '',
             f)

#    f <- sub('=',
#             '\x07',
#             f)

    paste(f,
          collapse=sep)
  }
  
  # Get package for not exported function
  get.hidden <- function(fname)
  {
    res <- getAnywhere(fname)

    res <- grep('namespace:',
                res$where,
                value=TRUE)

    res <- gsub('namespace:',
                '',
                res)

    return(res)
  }

  # Main
  frm <- NA
  fmeth <- NA

  try(classfor <- eval(parse(text=classfor)),
      silent=TRUE)

  if(!missing(classfor)) {
    if(length(grep(fname,
                   names(.knownS3Generics))) > 0) {
      curwarn <- getOption('warn')

      options(warn=-1)

      #if(exists(as.character(substitute(classfor))))
      #  if(is.character(classfor))
      #    classfor <- eval(parse(text=classfor))

      try(classfor <- classfor,
          silent=TRUE)  # classfor may be a function

      try(.theclass <- class(classfor),
          silent=TRUE)

      options(warn=curwarn)

      if(exists('.theclass')) {
        for(i in 1:length(.theclass)) {
          fmeth <- paste(fname,
                         '.',
                         .theclass[i],
                         sep='')

          if (length(argsAnywhere(fmeth))) {
            fname <- fmeth

            frm <- formals(argsAnywhere(fmeth))

            break
          }
        }
      }
    }
  }

  if(is.na(frm[1])) {
    if(pkg == '') {
      deffun <- paste(fname,
                      '.default',
                      sep='')

      if (existsFunction(deffun)) {
        fname <- deffun

        fmeth <- deffun
      } else if(!existsFunction(fname)) {
        return(format.out('Object not found',
                          'Loaded packages',
                          fname))
      }

      if(is.primitive(get(fname))) {
        pkname <- find(fname,
                       mode='function')

        if(length(pkname) > 0)
          info <- gsub('package:',
                       '',
                       pkname[1])

        return(format.out(primitive.args(fname),
                          info,
                          fname))
      }
      else {
        fmeth <- fname

        frm <- formals(fmeth)
      }
    } else {
      idx <- grep(paste(':',
                        pkg,
                        '$',
                        sep=''),
                  search())

      ff <- 'NULL'

      tr <- try(ff <- get(paste(fname,
                                '.default',
                                sep=''),
                          pos=idx),
                silent=TRUE)

      if(class(tr)[1] == 'try-error')
        ff <- get(fname,
                  pos=idx)

      frm <- formals(ff)
    }
  }

  res <- NULL

  for (field in names(frm))
    if (field != '...')
      res <- append(res,
                    paste(field,
                          deparse(frm[[field]]),
                          sep='='))
    else
      res <- append(res,
                    field)

  res <- grep(paste('^',
                    txt,
                    sep=''),
              res,
              value=TRUE)

  res <- paste(res,
               collapse=sep)

  if(length(res) == 0 ||
       res == '') {
    res <- 'NO_ARGS'
  } else {
    if(pkg == '') {
      pkname <- find(fname,
                     mode='function')

      if(!length(pkname))
        pkname <- get.hidden(fname)
      
      info <- NA
      if(length(pkname) > 0)
        info <- gsub('package:',
                     '',
                     pkname[1])

      if(!is.na(info) &&
           !is.na(fmeth)) {
        return(format.out(res,
                          info,
                          fmeth))
      }
    }
  }

  return(format.out(res,
                    pkg,
                    fname))
}
