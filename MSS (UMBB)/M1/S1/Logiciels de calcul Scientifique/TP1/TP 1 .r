#Correction du TP 01 du moule LCS 
print('_________________Q1_________________')
#La fonction "identical" permet de comparer deux valeurs. Elle retourne "TRUE" si elles sont égales
#La fonction "print" est utilisée dans le but d'afficher le résultat d'une instruction
print(identical(pi , log(640320^3+744)/sqrt(163)))
print('_________________Q2_________________')
#On crée une vecteur en mettant les éléments qui le constitue entre parenthèses précédées par la lettre "c"
#le symbole "<-" est utilisé pour affecter une donnée à une variable
print(notes <- c(8 , 11.5 , 19.5 , 5.5 , 5 , 5.5 , 10.5 , 4.5 , 0.5))
print('_________________Q3_________________')
print(notes <- c(notes , 14))
print('_________________Q4_________________')
#notes/2 nous permet de ramener les notes sur 10
#notes/2 >= 5 nous permet de créer un vecteur de Booléen : "TRUE" si la note correspondante est supérieure ou égale à 5 "FALSE" sinon
#La fonction "sum" permet de faire la somme des éléments du vecteur entre parenthèses. Les "TRUE" vont être considérés comme des 1 et les "FALSE" comme des 0
print(sum(notes/2 >= 5))
print('_________________Q5_________________')
#La fonction "length" donne le nombre d'éléments d'un vecteur et par la même occasion l'indice du dernier élément du vecteur
print((notes[1]+notes[5]+notes[length(notes)])/3)
print('_________________Q6_________________')
#L'instruction notes[notes >= 10] nous donne uniquement les éléments du vecteur notes qui sont supérieure ou égale à 10
print(notes >= 10)
print(notes[notes >= 10])
print(length(notes[notes >= 10]))
print('_________________Q7_________________')
#On cherche la plus petite note entière donc il faudra déjà commencer par extraire les notes entières du vecteur notes
#La fonction "floor" nous donne la partie entière d'un nombre
print(floor(notes))
print(notes[notes == floor(notes)])
print(min(notes[notes == floor(notes)]))
print('_________________Q8_________________')
#L'instruction suivante nous permet de réduire toutes les notes de 2 points
print(notes2 <- notes - 2)
#L'instruction nous donne la taille du vecteur composé par les éléments négatifs de notes2 
print(length(notes2[notes2 < 0]))
#L'instruction suivante nous remplace tous les éléments négatifs du vecteur notes2 par 0
notes2[notes2 < 0] <- 0
print(notes2)
print('_________________Q9_________________')
print(notes)
#La fonction "rev" inverse les éléments du vecteur notes
print(rev(notes))
#La fonction "sort" ordonne les éléments du vecteur notes
print(sort(notes))
#La fonction "prod" calcul le produit des éléments du vecteur notes
print(prod(notes))
#La fonction "mean" donne la moyenne des éléments du vecteur notes
print(mean(notes))
#La fonction "range" donne le min et le max des éléments du vecteur notes
print(range(notes))
#La fonction "median" donne la médiane des éléments du vecteur notes
print(median(notes))
#La fonction "str" donne des infos sur les éléments du vecteur notes 
print(str(notes))
print('_________________Q10_________________')
print(adn.seq <- factor(c("A","C","A","A","G","A","T","G","C","C","A","T","T","G","T","C")))
#La fonction "levels" nous donne les modalités de l'échantillon 
#La fonction "nlevels" nous donne le nombre de modalités de l'échantillon 
print(levels(adn.seq))
print(nlevels(adn.seq))
print('_________________Q11_________________')
print(length(adn.seq[adn.seq=="A"]))
print(length(adn.seq[adn.seq=="C"]))
print(length(adn.seq[adn.seq=="G"]))
print(length(adn.seq[adn.seq=="T"]))
print('_________________Q12_________________')
#Avant de charger le fichier LCS.data il faut l'extraire de fichier .rar ensuite il faut aller dans le menu Session ensuite set working directory et puis préciser le dossier dans lequel se trouve le fichier extrait
Y <- read.csv("LCS.data")
#Les fonctions "length" et "ncol" retournent la même chose le nombre de colonne
print(length(Y))
print(ncol(Y))
#La fonction "nrox" returne le nombre de ligne ou chaque ligne représente un individu de l'échantillon
print(nrow(Y))
#La fonction "names" retourne les noms des colonnes
print(names(Y))
#La fonction "head" donne un aperçu du data.frame Y
print(head(Y))
#La fonction "summary" donne les statistiques standards de chaque colonne du data.frame (min, max, quartiles, moyenne et la médiane)  
print(summary(Y))
print('_________________Q13_________________')
print(Y[53,])
#L'instruction Y[53,] nous retourne les données de l'individu 53
print(c(Y$correcteur.CC[53],Y$correcteur.examen[53]))
#Le signe $ permet de choisir une colonne par son nom ici par exemple l'instruction nous retourne le nom du correcteur du CC de l'étudiant 53
print(c(Y$CC[167],Y$examen[167],Y$mention[167]))
print('_________________Q14_________________')
print(length(Y$examen[Y$examen >= 10]))
print(mean(Y$CC[Y$correcteur.CC >= "EG"]))
#La proportion des étudiants qui ont progressé revient à diviser le nombre des étudiants qui ont une note d'examen plus grande que la note du CC par le nombre d'étudiant total
print(sum(Y$examen > Y$CC)/nrow(Y))
#Nous allons chercher le correcteur qui a donné en moyenne une mauvaise note par rapport aux autres
print(levels(factor(Y$correcteur.examen)))
#Avec cette instruction on a récupéré les noms des correcteurs  
print(c("ALC :",mean(Y$examen[Y$correcteur.examen == "ALC"]))) 
print(c("BR :",mean(Y$examen[Y$correcteur.examen == "BR"]))) 
print(c("CFG :",mean(Y$examen[Y$correcteur.examen == "CFG"]))) 
print(c("DH :",mean(Y$examen[Y$correcteur.examen == "DH"]))) 
print(c("EG :",mean(Y$examen[Y$correcteur.examen == "EG"]))) 
print(c("EN :",mean(Y$examen[Y$correcteur.examen == "EN"]))) 
print(c("HP :",mean(Y$examen[Y$correcteur.examen == "HP"])))
#C'est le correcteur HP
print('_________________Q15_________________')
print(c("Moyenne",mean(Y$examen)))
print(c("Ecart type",sd(Y$examen)))
print(c("Variance",var(Y$examen)))
print(c("Médiane",median(Y$examen)))
print(c("Minimum",min(Y$examen)))
print(c("Maximum",max(Y$examen)))
print(c("Quartiles",quantile(Y$examen)))
print(c("Interquartile (Q3-Q1)",IQR(Y$examen)))
summary(Y$examen)
print('_________________Q16_________________')
#La moyenne tronquée d'ordre 10 est la moyenne des notes sans compter les 10 notes les plus faibles et les 10 notes les plus forte
#Il y a une erreur dans la question : il n'est pas précisé de quelle moyenne il s'agit. En fait, on veut calculer la moyenne des moyennes tronquée d'ordre 10
print(mean(sort(Y$moyenne)[11:(nrow(Y)-10)]))
#Il est important d'ordonner le vecteur avant de tronquer
#Tronquer revient à ignorer les 10 premiers éléments du vecteur ordonner ainsi que les 10 derniers éléments
print('_________________Q17_________________')
par(mfrow = c(2,2))
barplot(table(Y$correcteur.examen),main = "correcteur d'examen")
#On remarque graphiquement que c'est le correcteur ALC qui a corrigé le plus de copie 
print('______________Q18 & Q19______________')
hist(Y$examen)
hist(Y$examen , breaks = c(0,15,20))
print('_________________Q20_________________')
boxplot(Y$examen)
print('_________________Q21_________________')
plot(Y$CC~Y$examen)
print('_________________Q22_________________')
boxplot(Y$examen~Y$correcteur.examen)
print('_________________Q23_________________')
par(mfrow = c(1,2))
stripchart(Y$examen~Y$correcteur.examen)
print('_________________Q24_________________')
stripchart(Y$examen~Y$correcteur.examen,method="jitter")
#La méthode jitter permet de mettre en lumière les répititions