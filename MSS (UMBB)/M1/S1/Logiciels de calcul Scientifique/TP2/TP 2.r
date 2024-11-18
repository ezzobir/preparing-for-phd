library(MASS)
print('_________________Q1_________________')
#L'instruction suivante nous permet de diviser la fenêtre d'affichage des graphes en quatre case
par(mfrow = c(2 , 2))
#Chacune des 4 instructions suivantes va nous permettre d'afficher l'histogramme des notes de chaque critère
hist(painters$Composition)
hist(painters$Drawing)
hist(painters$Colour)
hist(painters$Expression)
print('_________________Q2_________________')
(moyenne <- (painters$Composition + painters$Drawing + painters$Colour + painters$Expression) / 4)
print('_________________Q3_________________')
sum(moyenne) / length(moyenne)
(Var <- sum(moyenne^2) / length(moyenne) - Moy^2)
#ou Var <- sum((moyenne - Moy)^2) / length(moyenne)
(Ecart <- sqrt(Var))
mean(moyenne)
var(moyenne)
sd(moyenne)
# on voit ici que la varriance calculée par la fonction var est différente de la variance que nous avons calculé
# ceci est dû au fait que la fonction var donne la variance empirique corrigée et donc il suffit de divisier par n-1 au lieu de n
sum((moyenne - Moy)^2) / (length(moyenne) - 1)
print('_________________Q4_________________')
par(mfrow = c(1 , 1))
hist(moyenne)
print('_________________Q5_________________')
1 - pnorm(-2)
#pour la deuxième question on peut transformer notre variable en une variable centrée réduite.
pnorm(4/3)
#on peut aussi préciser la moyenne et l'écart type directement en argument 
pnorm(39 , mean = 35 , sd = 3)
pnorm(53 , mean = 35 , sd = 3) - pnorm(37 , mean = 35 , sd = 3)
# ici c'est la loi binomiale 15 étantle nombre de lancer total et 0.5 la probabilité d'avoir pile en un seul lancer
dbinom(9 , 15 , 0.5)
1 - pbinom(9 , 15 , 0.5)# ou sum(dbinom(10:15 , 15 , 0.5))
1 - pbinom(8 , 15 , 0.5)
1 - pbinom(9 , 15 , 0.5)
pbinom(13 , 15 , 0.8) - pbinom(8 , 15 , 0.8)
print('_________________Q6_________________')
alpha <- c(0.05 , 0.1 , 0.9)
qnorm(alpha)
qchisq(alpha , 10)
qt(alpha , 5)
qf(alpha , 2 , 5)
print('_________________Q7_________________')
dloi <- function(x , b){
  d <- 2*x/b^2
  d[x < 0] <- 0
  d[x > b] <- 0
  return(d)
}
print('_________________Q8_________________')
dloi(-1:5 , 3)
print('_________________Q9_________________')
ploi <- function(x , b){
  p <- x^2/b^2
  p[x < 0] <- 0
  p[x > b] <- 1
  return(p)
}
print('_________________Q10_________________')
par(mfrow=c(1,2))
curve( dloi(x, 3) , from = -5 , to = 5 )
curve( ploi(x, 3) , from = -5 , to = 5 )
print('_________________Q11_________________')
qloi <- function(alpha , b){
  q <- b * sqrt(alpha)
  q[alpha == 0] <- 0
  q[alpha == 1] <- b
  return(q)
}
print('_________________Q12_________________')
rloi <- function(n , b){
  r <- qloi(runif(n) , b)
  return(r)
}
print('_________________Q13_________________')
par(mfrow = c(2, 2))
for (n in c(10 , 100 , 1000 , 10000)){
  hist(rloi(n , 3) , freq = FALSE)
  curve(dloi(x, 3), add = TRUE , col = "red")
  title(n)
}
#freq = FALSE nous permet de faire un histogramme de densité et non pas de fréquence
#add = TRUE nous permet de mettre la densité de notre loi dans le même graphe 
