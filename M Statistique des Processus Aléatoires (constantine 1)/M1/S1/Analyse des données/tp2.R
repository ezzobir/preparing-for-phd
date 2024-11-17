library(FactoMineR)
data(children)

View(children)
summary(children)
res<-CA(children,row.sup=15:18,col.sup=6:8)
summary(res)
#summary.CA(res,ncp=2)
summary.CA(res,nbelements=Inf,file="essais2.txt")
plot(res)
plot(res,shadow=TRUE)

plot(res,invisible=c("row.sup","col.sup"),shadow=TRUE,cex=0.8,selectRow="cos2 0.7",selectCol="cos2 0.7",unselect=0)
plot(res,invisible=c("row.sup","col.sup"),shadow=TRUE,cex=0.8,selectRow="cos2 0.7",selectCol="cos2 0.7",unselect=1)
plot(res,invisible=c("row.sup","col.sup"),shadow=TRUE,cex=0.8,selectRow="cos2 0.7",selectCol="cos2 0.7",unselect="grey70")
plot(res,invisible=c("row.sup","col.sup"),shadow=TRUE,cex=0.8,selectRow="cos2 4",selectCol="cos2 3",unselect=1)


