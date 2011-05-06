ternaryDiag <- function(x, name=colnames(x), grid=TRUE, gridCol=grey(0.6), mcex=1.2, ...)
{
# Ternary plot
#
# x ... matrix with 3 columns
# name ... names of the variables
# grid ... TRUE if grid should be plotted
# "..." ... further graphical parameters, see par
	
	
	if (is.null(name)) { name <- c("P1", "P2", "P3")}
	if (length(name) != 3){ 
		warning("incorrect length of name. Variable names P1, P2 and P3 are used instead.")
	}
	if (dim(x)[2] > 3){ 
		warning("only the first three parts are used for plotting")
		x <- x[,1:3]
	}
	if (dim(x)[2] < 3){ 
		stop("x must include 3 variables/parts")
	}
	s <- rowSums(x)
	if (any(s <= 0))
		stop("each row of the input `object' must have a positive sum")
	dat <- x/s 
#	dat <- constSum(x)
	
	xp <- dat[,2] + dat[,3]/2
	yp <- dat[,3] * sqrt(3)/2
	
	par(pty="s")
	plot(xp,yp,xlim=c(0,1),ylim=c(0,0.9), 
			frame.plot=FALSE, xaxt="n", yaxt="n", xlab="", ylab="", ...)
	
	segments(0,0,1,0)
	segments(0,0,1/2,sqrt(3)/2)
	segments(1/2,sqrt(3)/2,1,0)
	
	mtext(name[1],side=1, line=-1, at=-0.05,cex=mcex)
	mtext(name[2],side=1, line=-1, at=1.05,cex=mcex)
	text(0.5, 0.9, name[3],cex=mcex)
	
	if(grid)
	{    
		b <- sqrt(c(0.03,0.12,0.27,0.48))
#		segments(c(0.2,0.4,0.6,0.8,0.2,0.4,0.6,0.8,0.1,0.2,0.3,0.4), 
#				 c(rep(0,8), b),
#		         c(seq(0.1,0.9,0.1), c(0.9,0.8,0.7,0.6)),
#				 c(b, rev(b), b), col=gridCol, lty="dashed")
#				 
#		 )
		segments(0.2,0, 0.1,sqrt(0.03), col=gridCol, lty="dashed")
		segments(0.4,0, 0.2,sqrt(0.12), col=gridCol, lty="dashed")
		segments(0.6,0, 0.3,sqrt(0.27), col=gridCol, lty="dashed")
		segments(0.8,0, 0.4,sqrt(0.48), col=gridCol, lty="dashed")
		segments(0.2,0, 0.6,sqrt(0.48), col=gridCol, lty="dashed")
		segments(0.4,0, 0.7,sqrt(0.27), col=gridCol, lty="dashed")
		segments(0.6,0, 0.8,sqrt(0.12), col=gridCol, lty="dashed")
		segments(0.8,0, 0.9,sqrt(0.03), col=gridCol, lty="dashed")
		segments(0.1,sqrt(0.03), 0.9,sqrt(0.03), col=gridCol, lty="dashed")
		segments(0.2,sqrt(0.12), 0.8,sqrt(0.12), col=gridCol, lty="dashed")
		segments(0.3,sqrt(0.27), 0.7,sqrt(0.27), col=gridCol, lty="dashed")
		segments(0.4,sqrt(0.48), 0.6,sqrt(0.48), col=gridCol, lty="dashed")
		
		text(0.5,0.66,"0.8", col=gridCol, cex = 0.6)
		text(0.5,0.49,"0.6", col=gridCol, cex = 0.6)
		text(0.5,0.32,"0.4", col=gridCol, cex = 0.6)
		text(0.5,0.14,"0.2", col=gridCol, cex = 0.6)
		text(0.95,0.21,"0.8", col=gridCol, cex = 0.6, srt = 60)
		text(0.86,0.35,"0.6", col=gridCol, cex = 0.6, srt = 60)
		text(0.75,0.54,"0.4", col=gridCol, cex = 0.6, srt = 60)
		text(0.64,0.72,"0.2", col=gridCol, cex = 0.6, srt = 60)
		text(0.05,0.21,"0.8", col=gridCol, cex = 0.6,srt = 300)
		text(0.14,0.35,"0.6", col=gridCol, cex = 0.6,srt = 300)
		text(0.25,0.54,"0.4", col=gridCol, cex = 0.6,srt = 300)
		text(0.36,0.72,"0.2", col=gridCol, cex = 0.6,srt = 300)
	}
	
}

