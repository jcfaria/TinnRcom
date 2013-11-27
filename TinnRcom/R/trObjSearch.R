#=======================================================================
# Orinal author : Philippe Grosjean
# Original code : [svMisc] objSearch function
# Adapted by    : José Cláudio Faria
# Objective     : To supply the current necessity of the Tinn-R project
# Date          : 2013/10/23 - 13:10:09
#=======================================================================

trObjSearch <- function(sep='\t',
                        path=NULL)
{
  res <- data.frame(search())
  if(is.null(path))
    return(res)
  else
    write.table(res,
                file=path,
                row.names=FALSE,
                quote=FALSE,
                sep=sep)
}
