#=======================================================================
# Orinal author: Philippe Grosjean
# Original code: [svMisc] objList function
# Adapted by   : José Cláudio Faria
# Objective    : To supply the current necessity of the Tinn-R project
# Date         : 2013/09/24 - 17:20:57
#=======================================================================

trObjList <- function(id='default',
                      envir=.GlobalEnv,
                      all.names=TRUE,
                      pattern='',
                      group='',
                      all.info=FALSE,
                      sep='\t',
                      path=NULL)
{
  # Make sure that id is character
  id <- as.character(id)[1]
  if(id == '') id <- 'default'

  # Format envir as character (use only first item provided!)
  if(is.environment(envir))
    envir <- deparse(substitute(envir))

  if(is.numeric(envir))
    envir <- search()[envir[1]]

  envir <- as.character(envir)[1]

  # Get the current position in the search path for envir
  pos <- match(envir,
               search(),
               nomatch=-1)

  if(pos < 1)
    # NOT FOUND, return nothing
    pos <- 1
  # Environment found
  else {
    # Get the list of objects in this environment
    Items <- ls(pos=pos,
                all.names=all.names,
                pattern=pattern)

    if(length(Items) == 0)
      if(all.info)
        return(invisible(data.frame(Name=character(),
                                    Dims=character(),
                                    Group=character(),
                                    Class=character(),
                                    Recusive=logical(),
                                    stringsAsFactors=FALSE)))
      else
        return(data.frame(Envir=character(),
                          Name=character(),
                          Dims=character(),
                          Group=character(),
                          Class=character(),
                          Recusive=logical(),
                          stringsAsFactors=FALSE))

    # Get characteristics of all objects
    'describe' <- function(name,
                           pos='.GlobalEnv',
                           all.info=FALSE) {
      # get a vector with five items:
      # Name, Dims, Group, Class and Recursive
      obj <- get(name,
                 pos=pos)
      res <- c(Name=name,
               Dims=if(is.null(Dim <- dim(obj)))
                      length(obj)
                    else
                      paste(Dim, collapse='x'),
               Group=typeof(obj),
               Class=class(obj)[1],
               Recursive=!inherits(obj,
                                   'function') && is.recursive(obj))
      if(all.info)
        res <- c(Envir=pos,
                 res)
      return(res)
    }

    res <- data.frame(t(sapply(Items,
                               describe,
                               pos=envir,
                               all.info=all.info)),
                      stringsAsFactors=FALSE)

    # Recalculate groups into meaningful ones for the object explorer
    # 1) Correspondance of typeof() and group depicted in the browser
    GrpTable <- c(
      'NULL',      'language',  'list',        'function',  'language',
      'language',  'language',  'function',    'function',  'language',
      'logical',   'numeric',   'numeric',     'complex',   'character',
      'language',  'language',  'language',    'list',      'language',
      'S4',        'language',  'raw',         'language')

    names(GrpTable) <- c(
      'NULL',      'symbol',    'pairlist',    'closure',   'environment',
      'promise',   'language',  'special',     'builtin',   'char',
      'logical',   'integer',   'double',      'complex',   'character',
      '...',       'any',       'expression',  'list',      'bytecode',
      'S4',        'weakref',   'raw',         'externalptr')

    Groups <- GrpTable[res$Group]

    # 2) All Groups not being language, function or S4 whose class is
    #    different than typeof are flagged as S3 objects
    Filter <- !(Groups %in% c('language', 'function', 'S4'))
    Groups[Filter][res$Group[Filter] != res$Class[Filter]] <- 'S3'

    # 3) Special case for typeof=double and class=numeric
    Groups[res$Group == 'double'] <- 'numeric'

    # 4) integers of class factor become factor in group
    Groups[res$Class == 'factor'] <- 'factor'

    # 5) Objects of class 'data.frame' are also group 'data.frame'
    Groups[res$Class == 'data.frame'] <- 'data.frame'

    # 6) Objects of class 'Date' or 'POSIXt' are of group 'DateTime'
    Groups[res$Class == 'Date'] <- 'DateTime'
    Groups[res$Class == 'POSIXt'] <- 'DateTime'

    # Reaffect groups
    res$Group <- Groups

    # Transform into a character vector
    res <- apply(res,
                 1,
                 paste,
                 collapse=sep)
  }

  if(length(res) == 1 && res == '')
    res <- data.frame('',
                      '',
                      '',
                      '',
                      stringsAsFactors=FALSE)
  else
    res <- data.frame(t(data.frame(strsplit(res, '\t'))),
                      stringsAsFactors=FALSE)[ , -5]

  colnames(res) <- c('Name',
                     'Dim',
                     'Group',
                     'Class')
  rownames(res) <- NULL

  # Group conversion to current necessity of Tinn-R
  trTable <- c(
    'vector',   'vector',    'vector',     'vector',
    'vector',   'vector',    'vector',     'data.frame',
    'list',     'function',  'other',      'other',
    'other',    'other')

  names(trTable) <- c(
    'numeric',  'complex',   'character',  'logical',
    'factor',   'DateTime',  'raw',        'data.frame',
    'list',     'function',  'NULL',       'language',
    'S3',       'S4')

  trGroup <- trTable[res$Group]
  trGroup[res$Class == 'matrix'] <- 'matrix'
  trGroup[res$Class == 'array']  <- 'array'
  trGroup[res$Class == 'table']  <- 'table'
  trGroup[res$Class == 'mts']    <- 'other'
  res$Group <- trGroup

  # Filter
  if(group != '')
    if (group == 'data')
      res <- res[trGroup != 'function', ]
    else
      res <- res[trGroup == group, ]

  # Final result
  if(is.null(path))
    return(res)
  else
    write.table(res,
                file=path,
                row.names=FALSE,
                quote=FALSE,
                sep ='\t')
}
