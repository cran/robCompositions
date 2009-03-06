.First.lib <- function(lib,pkg)
{
	version <- citation("robCompositions")$note
	version <- substr(version, (nchar(version)-5), nchar(version))
	cat("\n --------")	
	#cat("\n robCompositions has been loaded \n\n")
	cat("\n robCompositions version",version,"has been loaded \n\n")
	cat(" --------\n\n")
}

