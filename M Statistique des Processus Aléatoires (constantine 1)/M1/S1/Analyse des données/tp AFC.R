#	Chargement  les packages : 
	library("FactoMineR")
	library("factoextra")
	library("gplots")
	library("corrplot")

#	Importation des des données : 
#On charge le tableau de type "csv" en utilisant l’assertion suivante :
data<- read.csv(file="troubles psychologiques.csv",header=TRUE,sep=";",dec=",")
data
dt<- as.table(as.matrix(data),dim=dim(dt))
dt
#	nommer les lignes et les colonnes : (coder )
rownames(dt)<-c("Bégaiement","Troubles de l'articulation","Dyslexie","Encoprésie","Enurésie","Retardscolaire","Echecscolaire","Refusscolaire","Phobiescolaire","Angoisse","Anxiété","Phobie","Hystérie","trouble Névrotique obssessionnelle","Agressivité/Violences","Hyperactivité","Fugue","Asthme","Migraines","diabète","Maltraitance A l'ecole","viol/sexuel","Etatdepressif")
colnames(dt)<-c("Primaire(filles)","Primaire(garçons)","Moyen(filles)","Moyen(garçons)","Secondaire(filles)","Secondaire(garçons)")
dt


#Visualisation des données :
#Pour une premiere visualisation des données : en utilise la commande ballonplot comme suit 

balloonplot(t (dt),main="troubles psychologiques aux niveaux scolaires",xlab="niveaux",ylab="troubles psychologiques",label=FALSE,show.margins=FALSE)

#	Test de « chi deux » :
#Pour étudier l’indépendance entre les deux variables en utilise le test de chi deux avec la  commande « chisq.test»
chisq<-chisq.test(dt)
chisq
#	Application d’une A.F.C. :
#On utilise la fonction « CA » du package ("FactoMineR") pour appliquer l’A.F.C.
res.ca <- CA (dt)
res.ca
#	Pour avoir un résumé des résultats
summary(res.ca)
#	Pour le calcul des valeurs propres 
#On utilise la commande suivante :
eig.val<-get_eigenvalue(res.ca)
eig.val
#Pour ploter un graphique des valeurs propres ordonnées de la plus grande à la plus petite valeur
fviz_screeplot(res.ca, addlabels = TRUE, ylim=c(0,100))
#	La représentation graphiquesdes deux profiles
"Pour représenter graphiquement les deux profiles lignes et colonnes sur le même graphe on utilise la commande suivante
fviz_ca_biplot (res.ca, repel = TRUE)
#	La représentation des contributions et des cos2
#Pour représentation des contribution des lignes dans l’axe 1 et 2
fviz_contrib(res.ca, choice = "row", axes = 1)
fviz_contrib(res.ca, choice = "row", axes = 2)
#La représentation des contribution des colonnes dans l’axe 1 et 2
fviz_contrib(res.ca, choice = "col", axes = 1)
fviz_contrib(res.ca, choice = "col", axes = 2)
#Le barplot du cos2 des points-lignes dans les dim 1et2
fviz_cos2(res.ca, choice = "row", axes = 1:2)
#Le barplot du cos2 des colonnes dans les dim 1et2
fviz_cos2 (res.ca, choice = "col", axes = 1:2)
#Le cos 2 des points lignes et colonnes dans les 5 dimensions
corrplot(row$contrib, is.corr=FALSE) 
corrplot(row$cos2, is.corr = FALSE)
#	Plus de commandes pour plus d’informations :
fviz_ca_biplot (res.ca, repel = TRUE)
row<- get_ca_row(res.ca)
row
head(row$coord)
fviz_ca_row(res.ca, repel = TRUE)
head(row$cos2, 4)
fviz_ca_row (res.ca, col.row = "cos2",gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),   repel = TRUE)
head(row$contrib)           
fviz_ca_row (res.ca, col.row = "contrib",gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
col <- get_ca_col(res.ca)
col
head(col$coord)                
fviz_ca_col (res.ca)
head(col$cos2)      
fviz_ca_col (res.ca, col.col = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)          
head(col$contrib)                
fviz_ca_biplot (res.ca,map = "rowprincipal", arrow = c(TRUE, TRUE),repel = TRUE)
fviz_ca_biplot (res.ca, map = "colgreen", arrow = c (TRUE, FALSE),repel = TRUE)
res.desc<- dimdesc(res.ca, axes = c(1, 2))              
res.desc
head(res.desc[[1]]$row, )               
head(res.desc[[1]]$col, )
res.desc[[2]]$row
