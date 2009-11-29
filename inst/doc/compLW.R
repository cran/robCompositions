x <- read.table("lw09.csv", sep=";", dec=".", header=TRUE)

# nur Gemeinden:
gem <- x[substr(x[,"Nr."],5,5) != 0,]
head(gem)

# nur Anteile:
nchar(colnames(x))
w <- which(substr(colnames(gem),nchar(colnames(gem)), nchar(colnames(gem))) == "p")
anteile <- gem[, c(2,w)]
head(anteile)


# biplots:

library(compositions)
library(robCompositions)
library(robustbase)

# standard biplot for raw data
x <- anteile[,5:8]
rownames(x) <- anteile[,1]

## Problem mit rownames noch nicht geloest...:
rn <- ifelse(duplicated(substr(rownames(x),1,3)), paste(substr(rownames(x),1,3),1, sep=""), substr(rownames(x),1,3))
j=0
for(i in 1:nrow(x)){
	TR <- ifelse(substr(rn[i], 4,4) == "1", TRUE, FALSE)
	if(TR) j <- j + 1
	if(j == 9) j <- 3
	rn[i] <- ifelse(substr(rn[i], 4,4) == "1", paste(substr(rn[i],1,3), j, sep=""), rn[i])
	
}
rownames(x) <- rn 
##


pca <- princomp(x)
summary(pca)
biplot(pca,main="Election - standard biplot",cex=1) 

# robust biplot for raw data
x11()
pcarob <- princomp(x,covmat=covMcd(x))
summary(pcarob)
biplot(pcarob,main="Election - robust biplot",cex=1)

#Standard compositional biplot
x11()
x <- acomp(x)
y <- clr(x)
cpca <- princomp(x)
summary(cpca)
biplot(cpca,main="Election - standard CoDa biplot",cex=1) 

# robust compositional biplot
x11()
pdf("ooeWahl.pdf")
pca <- pcaCoDa(x)
summary(princomp(ilr(x),covmat <- covMcd(ilr(x))))
rownames(pca$scores) <- rownames(x)
biplot(-pca$scores[,1:2],-pca$loadings[,1:2],main="Election - robust CoDa biplot",cex=1)
dev.off()


