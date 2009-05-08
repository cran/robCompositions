.onLoad <- function(lib, pkg) {
	library.dynam("robCompositions",pkg,lib)
	cat("\n ----------------------------------------------")	
	cat("\n --------")	
	cat("\n robCompositions, ", citation("robCompositions")$note, "loaded." )
	cat("\n --------") 
	cat("\n ----------------------------------------------\n\n") 
}