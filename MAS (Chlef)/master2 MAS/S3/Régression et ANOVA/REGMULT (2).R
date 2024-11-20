
y <- c(5.7,5.8,6.1,6.5,6.8,6.8,7.1,21.3,18.7,14.5,7.4,9,
     11.7,9.5,9.5,8.8,9.3,8.6,7.7,10.8,6.6,11.7,11.9,
      10.8,7.6,11.3,10.8,9.2,11.6,12.8,12.7)
x1 <- c(11600,12490,10450,17140,14825,13730,19490,285000,
       183900,92500,25000,22350,36600,22500,31580,28750,
        22600,20300,19900,39800,19740,38990,50800,36200,
         31990,47700,36950,26950,36400,50900,49300)
x2 <- c(846,993,899,1390,1195,658,1331,5474,5987,2789,
        1597,1761,2165,1983,1984,1998,1580,1390,1396,
         2435,1242,2972,2958,2497,1998,2496,1998,1997,
        1984,2438,2473)
x3 <- c(32,39,29,44,33,32,55,325,300,209,74,74,101,85,
         85,89,65,54,66,106,55,107,150,122,66,125,89,92,
         85,97,125)
x4 <- c(650,790,730,955,895,740,1010,1690,2250,1485,1080,
         1100,1500,1075,1155,1140,1080,1110,1140,1370,940,
        1400,1550,1330,1300,1670,1560,1240,1635,1800,1570)
n <- length(y)
n
cste <- rep(1, n)
cste
X <- cbind(x1 = x1, x2 = x2, x3 = x3, x4 = x4, cste =cste)
X
y ; X
# L ja transposee de X
 t(X)
t(X) %*% X
 # Inversion de la matrice X^t X, soit (X^t X)^(-1)
solve(t(X) %*% X)
t(X) %*% y
(hatBeta <- (solve(t(X) %*% X)) %*% (t(X) %*% y))


yChap <- X %*% hatBeta
yChap
residus <- y - yChap
residus 



(SSR<- sum((yChap - mean(y))^2))
(SST<- sum((y - mean(y))^2))
(R2 <- SSR / SST)



m <- 4 # Nombre de variables explicatives
(R2a <- 1 - ((n-1) / (n-m-1)) * (1-R2))
(SSE <- SST-SSR)

(hatSigma2_u <- SSE / (n-m-1))

(varcov <- hatSigma2_u * solve(t(X) %*% X))
(hatSigmaBetas <- sqrt(diag(varcov)))



(tObs <- hatBeta / hatSigmaBetas)
> # On doit comparer les t_obs a la valeur tabulee d'une
> # Student a (n-m-1) d.d.l. pour un risque de premiere espece
> # alpha = 5%

(T_tab <- qt(p = 1-0.05/2, df = n-m-1))
ifelse(abs(tObs) > T_tab, "Rejet H_0", "Non rejet H_0")
 


2*pt(q = abs(tObs), df = n-m-1, lower.tail=FALSE)
 # La valeur calculee
(F_obs <- (R2 / m) / ((1-R2) / (n-m-1)))
# La valeur tabulee est :
 (F_tab <- qf(p = 1-0.05, df1 = m, df2 = n-m-1))
# p-value
 (1 - pf(q = F_obs, df1 = m, df2 = n-m-1))



df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
df
summary(reg <- lm(y~., data = df))



summary(reg <- lm(y ~ 1 + x1 + x2 + x3 + x4, data = df))
cor(df)



(T_tab <- qt(p = 1-0.05/2, df = 1427))


