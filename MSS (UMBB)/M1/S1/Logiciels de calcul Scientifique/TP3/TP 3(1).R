#Correction du TP 03 du moule LCS 
print('_________________Q1_________________')
runifa <- function(n) {
  if(!exists("param")) 
    param <<- sample(10:20, 1)
  runif(n, min = 0, max = param)
}
estim <- function(X) {
  return(2*mean(X))
}
print('_________________Q2_________________')
a <- replicate(1000 , estim(runifa(100)))
mean(a)
print('_________________Q3_________________')
boxplot(a)
print('_________________Q4_________________')
estimk <- function(X , k) {
  return(((k+1) * mean(X^k))^(1/k))
}
a2 <- replicate(1000 , estimk(runifa(100) , 2))
a5 <- replicate(1000 , estimk(runifa(100) , 5))
boxplot(a,a2,a5 , names = c("k=1" , "k=2" , "k=5"))
print('_________________Q5_________________')
runknown <- function(n){
  bn <- rbinom(n , 1 , 0.5)
  bn * rnorm(n , -4 , 1) + (1 - bn) * rnorm(n , 10 , 1)
}
mean(runknown(100))
sd(runknown(100))
sqrt(50)
print('_________________Q6_________________')
hist(runknown(1000))
print('_________________Q7_________________')
plot(ecdf(runknown(1000)))
print('_________________Q8_________________')
(mean(runknown(1000))-3)/(sqrt(50)/sqrt(1000))
T <- replicate(1000 , (mean(runknown(1000))-3)/(sqrt(50)/sqrt(1000)))
print('_________________Q9_________________')
par(mfrow = c(1,1))
plot(ecdf(T))
curve(pnorm(x , 0 , 1) , add = TRUE , col = "red")
