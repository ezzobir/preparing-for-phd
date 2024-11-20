#---- Modèle de Gauss Markov estimé par OLS  DEBUT-------

n = 100 ; k = 3 ; 
Id=matrix(0,n,n) ; for(t in 1:n) Id[t,t]=1
X=matrix(0,n,k);      Xl=matrix(0,n,k);   XpXm1=matrix(0,k,k) ;
yl=NULL;y=NULL ;e2bar=0;e=NULL; Beta=NULL; u=NULL; Betachap=NULL; eps =NULL;yz=NULL;


for(t in 1:n)
 {
  X[t,1]=rnorm(1,5,1.5); X[t,2]=rnorm(1,15,3); X[t,3]=rnorm(1,4,20);  
 }
Beta[1]=2.5; Beta[2]=3; Beta[3]=-1; Sigma2=0.5; alpha=0.75;  eps = rnorm(n,sqrt(Sigma2))
y=X%*%Beta+eps; vary=var(y); Xpy=t(X)%*%y;
XpXm1=solve(t(X)%*%X); M=( Id-X%*%XpXm1%*%t(X))
BetaChap=XpXm1%*%Xpy; 
e=M%*%y; e2bar=t(e)%*%e; Sigma2Chap=e2bar/(n-k);
DW=0; for(t in 2:n) DW=DW+(e[t]-e[t-1])^2/e2bar;R2bar=1-Sigma2Chap/vary
cat("  BetaChap1\t"," BetaChap2\t"," BetaChap3\n",
" ",format(BetaChap[1], digits =6),"\t"," ",format(BetaChap[2], digits =6),"\t"," ",format(BetaChap[3], digits =6)," ","\n",
"(",format(sqrt(Sigma2Chap*XpXm1[1,1]), digits =4),")\t","(",format(sqrt(Sigma2Chap*XpXm1[2,2]), digits =4),")\t",
" (",format(sqrt(Sigma2Chap*XpXm1[3,3]), digits =4),")","\n\n",
"R2 = ",format(R2bar, digits =4),"\t","Sigma2Chap = ",format(Sigma2Chap, digits =4),"DW = ",format(DW, digits =4),"\n")

#---- Modèle de Gauss Markov estimé par OLS  FIN-------

#---- Modèle avec autocorrélation des résidus estimé par OLS  DEBUT------- 
u[1]=eps[1]; for(t in 2:n) {u[t]=alpha*u[t-1]+eps[t]}
y=X%*%Beta+u; vary=var(y); Xpy=t(X)%*%y;
XpXm1=solve(t(X)%*%X); 
BetaChapOLS=XpXm1%*%Xpy; 
e=M%*%y; e2bar=t(e)%*%e; Sigma2Chap=e2bar/(n-k);eOLS=e;

DW=0; for(t in 2:n) DW=DW+(e[t]-e[t-1])^2/e2bar; R2bar=1-Sigma2Chap/vary
cat("  BetaChapOLS1\t"," BetaChapOLS2\t"," BetaChapOLS3\n",
" ",format(BetaChapOLS[1], digits =6),"\t"," ",format(BetaChapOLS[2], digits =6),"\t"," ",format(BetaChapOLS[3], digits =6)," ","\n",
"(",format(sqrt(Sigma2Chap*XpXm1[1,1]), digits =4),")\t","(",format(sqrt(Sigma2Chap*XpXm1[2,2]), digits =4),")\t",
" (",format(sqrt(Sigma2Chap*XpXm1[3,3]), digits =4),")","\n\n",
"R2 = ",format(R2bar, digits =4),"\t","Sigma2 = ",format(Sigma2Chap, digits =4),"DW = ",format(DW, digits =4),"\n")
# -----Modèle avec autocorrélation des résidus estimé par OLS  FIN-------- 

# -------- Estimation par GLS faisable --------DEBUT-------

alphachap=0;uchap=NULL;BetaChapGLS=NULL;
for(t in 2:n) alphachap=alphachap+e[t]*e[t-1]/e2bar
for(t in 1:n) {e2bar=e2bar+e[t]*e[t]}
DW=0; for(t in 2:n) DW=DW+(e[t]-e[t-1])*(e[t]-e[t-1])/e2bar
Sigma2Chap=e2bar/(n-k+1); R2=1-Sigma2Chap/vary
 
for(t in 2:n)
 {yl[t]=y[t]-alphachap*y[t-1] 
  for(i in 1:k)   {Xl[t,i]=X[t,i]-alphachap*X[t-1,i]}
 }
yl[1]=sqrt(1-alphachap*alphachap)*y[1]; 
for(i in 1:k)  { Xl[1,i]=sqrt(1-alphachap*alphachap)*X[1,i]}

ybar=0 ;y2bar=0; e2bar=0;
for(t in 1:n)
 {
  ybar=t(yl)%*%Vec1/n;
  y2bar=t(yl)%*%yl/n;
 }
vary=n/(n-1)*(y2bar-ybar*ybar)
XpXm1=solve(t(Xl)%*%Xl); Xlpyl=t(Xl)%*%yl; M=( Id-Xl%*%XpXm1%*%t(Xl))
BetaChap=XpXm1%*%Xlpyl; 
e=M%*%yl;eGLS=e;

e2bar=t(e)%*%e;
DW=0; for(t in 2:n) DW=DW+(e[t]-e[t-1])*(e[t]-e[t-1])/e2bar
Sigma2Chap=e2bar/(n-k+1); R2=1-Sigma2Chap/vary
cat("  BetaChap1\t"," BetaChap2\t"," BetaChap3\n",
" ",format(BetaChap[1], digits =6),"\t"," ",format(BetaChap[2], digits =6),"\t"," ",format(BetaChap[3], digits =6)," ","\n",
"(",format(sqrt(Sigma2Chap*XpXm1[1,1]), digits =4),")\t","(",format(sqrt(Sigma2Chap*XpXm1[2,2]), digits =4),")\t",
" (",format(sqrt(Sigma2Chap*XpXm1[3,3]), digits =4),")","\n\n",
"R2 = ",format(R2, digits =4),"\t","Sigma2Chap = ",format(Sigma2Chap, digits =4),"DW = ",format(DW, digits =4),"\n")

# -------- Estimation par GLS faisable --------Fin-------



# -------- Exemple pour le test d'ajustement de Kolmogorov Smirnov --------DEBUT-------
n=50; m=3;sd=2;
y=rnorm(n,m,sd);
#y=runif(n,m-3*sd,m+3*sd);
Fn <- ecdf(y);
plot(Fn, verticals = TRUE, do.points = FALSE)
plot(function(y) pnorm(y,m,sd),m-3*sd,m+3*sd,col="red", add = TRUE)
Dn=max(abs(Fn(y)-pnorm(y,m,sd)))
sqrt(n)*Dn 

#talpha		1.2238 	1.358 	1.48 	1.627
#1-alpha 		0.9 		0.95 	0.975 	0.99


