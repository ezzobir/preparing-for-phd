print("--------------Fisher--------------")
Chisq1 <- function(mu, sig, n){
  (n - 1) * var(rnorm(n , mu , sig))/sig^2
}
mu = 40
sig = 3
n = 10
Chisq1(mu , sig , n)
Chisq1000 <- replicate(1000 , Chisq1(mu , sig , n))
hist(Chisq1000, freq = FALSE)
curve(dchisq(x, df = n - 1), add = TRUE)
print("--------------Student-------------")
Stu1 <- function(mu, sig, n){
  x <- rnorm(n , mu , sig)
  (mean(x) - mu)/(sd(x)/sqrt(n))
}
mu = 40
sig = 3
n = 10
Stu1(mu , sig , n)
Stu1000 <- replicate(1000 , Stu1(mu , sig , n))
hist(Stu1000, freq = FALSE)
curve(dt(x, df = n - 1), add = TRUE)
print("-----Intervalles de confiance-----")
print("----------------Q5----------------")
X <- rnorm(1000 , 15 , 3)
alpha <- 0.05
mean(X) + c(-1 , 1) * qnorm(1 - alpha/2)* 3/sqrt(1000)
print("----------------Q6----------------")
mean(X) + c(-1 , 1) * qt(1 - alpha/2 , 1000 - 1) * sd(X)/sqrt(1000)
print("----------------Q7----------------")
gen_IC <- function(X,alpha){
  n <- length(X)
  mean(X) + c(-1 , 1) * qt(1 - alpha/2 , n - 1) * sd(X)/sqrt(n)
}
print("----------------Q8----------------")
ICs <- replicate(100 , gen_IC(rnorm(100 , 15 , 3) , 0.05))
print("----------------Q9----------------")
plot_ICs(ICs , 15)
print("----------------Q10---------------")
(1000-1)/qchisq(1 - alpha , 1000 - 1) * var(X)
(1000-1)/qchisq(alpha , 1000 - 1) * var(X)
#intervalle bilatérale sqrt((1000-1)/c(qchisq(1 - alpha/2 , 1000 - 1) , qchisq(alpha/2 , 1000 - 1)) * var(X))
