
library(FactoMineR)

data<- read.csv(file="Donnee.csv",header=TRUE,sep=";",dec=",")
data
#remplacer decathlon avec le nom de votre fichier  exel
summary(decathlon)
pairs(decathlon,main="Olympique")

res.pca <- PCA("Donnee", quanti.sup = 11:12, quali.sup=13,ind.sup=1:5)
summary(res.pca)
summary(res.pca,ncp=10)
summary(res.pca,nbelements=Inf)
summary(res.pca,nbelements=Inf,file="essais.txt")
res.pca$eig
cumsum(res.pca$eig)
res.pca$eig/sum(res.pca$eig)*100
cumsum(res.pca$eig/sum(res.pca$eig)*100)
inertie<-(res.pca$eig/sum(res.pca$eig))*100
barplot(res.pca$eig[,2],ylab="% d'inertie",names.arg=NULL)
title("Eboulis des valeurs propres en %")
res.pca$cr
x11()
plot(res.pca,cex=0.7,habillage=13,invisible=c("ind.sup","quali"))
plot(res.pca,cex=0.7,Shadow=c("ind.sup","quali"))
plot(res.pca,cex=0.7,habillage=13,select="cos2 0.7")
plot(res.pca,cex=0.7,habillage=13,select="cos2 0.7",unselect=0)

plot(res.pca,cex=0.7,habillage=13,select="cos2 0.7",unselect=1)
plot(res.pca,cex=0.7,select="cos2 5")
#plot(res.pca,cex=0.7,habillage=13,select="contrib 5")
plot(res.pca,axes=c(2,3))
plot(res.pca,choix="var",axes=c(1,3))
plot(res.pca,choix="var",select="contrib 5")
barplot(res.pca$eig[,2],ylab="% d'inertie",names.arg=NUres.hcpc<-HCPC(res)
res.hcpc<-HCPC(res.pca)