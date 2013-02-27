constSum <- function(x, const=1){
	return(x / rowSums(x) * const)
}