

h <- 28
m <- 6
h %% m # 4


# M�todo de cuadrados de von Neumann
# con 4 d�gitos (no buena idea)
neumann <- function(seed){
  (floor( (seed^2)/10^2 )) %% 10^4 }

RCUAD <- function(n,seed){
  mu  <- rep(0,n)
  for(i in 1:n){
    seed  <- neumann(seed)
    mu[i] <- seed }
  return(mu/10^4)}
seed <- 9876  # cicla al 50-avo
n    <- 80
unif <- RCUAD(n,seed)


# con 10 d�gitos
neumann <- function(seed){
  (floor( (seed^2)/10^5 )) %% 10^10 }

RCUAD <- function(n,seed){
  mu  <- rep(0,n)
  for(i in 1:n){
    seed  <- neumann(seed)
    mu[i] <- seed }
  return(mu/10^10)}

n    <- 5000
seed <- 9182736450
seed <- 6582014372
seed <- 7676776767
seed <- 6462548788
unif <- RCUAD(n,seed)

# unif <- runif(n)
plot(unif,cex.axis=.7, cex.lab=.7,pch=".")
hist(unif,probability = TRUE,breaks=10)
abline(h=1,col="red",lwd=2)

56 %% 5
a

64646 %% 34

acf(unif, main="Generado uniformes con cuadrados medios", ylim=c(-.1,.2))











# Prueba de bondad de ajuste (ji-cuadrda)
n     <- 30
set.seed(76767)
datos <- rnorm(n,mean=20,sd=4)  # 
hist(datos)

range(datos)
int1 <- (-Inf,16)
int2 <- [16,19)
int3 <- [19,26)
int4 <- [26,Inf)

media <- mean(datos)  # estimadores de max. vero.
desv  <- sd(datos)

frecuencias observadas?
f1 <- sum(datos<16)                # 5
f2 <- sum((datos>=16)&(datos<19))  # 11
f3 <- sum((datos>=19)&(datos<26))  # 9
f4 <- sum(datos>=26)               # 5

#frecuencias esperadas?
p1 <- pnorm(16,mean=media,sd=desv)  
p2 <- pnorm(19,mean=media,sd=desv)-pnorm(16,mean=media,sd=desv)  
p3 <- pnorm(26,mean=media,sd=desv)-pnorm(19,mean=media,sd=desv)  
p4 <- 1-pnorm(26,mean=media,sd=desv)  
e1 <- n*p1
e2 <- n*p2
e3 <- n*p3
e4 <- n*p4

esperados <- c(e1,e2,e3,e4)
observados <- c(f1,f2,f3,f4)

cbind(esperados,observados)

Djicuad <- sum( (observados-esperados)^2 / esperados)

# D : 9.425  ..... es eso grande?
# eso es , Cual es el pvalor asociado aD?
# k-1-q grados de libertad = 4 - 1 - 2= 1

1-pchisq(Djicuad,1)  # 0.1126 .... decision?
# no rech normalidad




a





# usando runif

n    <- 5000
set.seed(54)
unif <- runif(n)
acf(unif, main="Generado uniformes con runif", ylim=c(-.1,.2))




randu <- function(seed) (a*seed) %% m

RANDU <- function(n,seed){
  a  <<- 2^16 + 3
  m  <<- 2^31         # solo para ilustrar variables locales -> globales
  mu  <- rep(0,n)
  for(i in 1:n){
    seed  <- randu(seed)
    mu[i] <- seed/m }
  return(list(mues=mu,lastseed=seed))}

# Uso de RANDU, generamos 5000 uniformes (0,1):
n    <- 5000
seed <- 45813
unif <- RANDU(n,seed)$mues
par2 <- matrix(unif,ncol=2,byrow=T)

par(mfrow=c(2,2), mar=c(4, 4, 3, 1) + 0.1)
plot(unif,pch=".",cex.axis=.7, cex.lab=.7)
hist(unif,cex.axis=.7, cex.lab=.7, cex.main=.7)
plot(par2[,1],par2[,2],pch=".",xlab="U(2n-1)", ylab="U(2n)",
     cex.axis=.7, cex.lab=.7)

seguir <- T
n      <- 60000
seed   <- 45813
sel    <- rep(0,3)
nr     <- 10000

while( seguir ){
  sal  <- RANDU(n,seed)
  rRAN <- matrix(sal$mues,ncol=3,byrow=T)
  sel  <- rbind(sel,rRAN[(rRAN[,2]>.5)&(rRAN[,2]<=.51),])
  if( dim(sel)[1] > nr ) seguir <- F
  seed <- sal$lastseed }

sel <- sel[-1,]

plot(sel[,1],sel[,3],xlab="U(3n-2)  (Generador RANDU)",
     ylab="U(3n)",pch=".", cex.axis=.7, cex.lab=.7)




RAND1 <- function(n,seed){
  a  <<- 16807
  m  <<- 2^31 - 1
  mu  <- rep(0,n)
  for(i in 1:n){
    seed  <- randu(seed)
    mu[i] <- seed/m }
  return(list(mues=mu,lastseed=seed))}

RAND1(100,767)





randu <- function(seed) (a*seed) %% m
RANWH <- function(n,seed){
  a  <<- c( 171, 172, 170)
  m  <<- c(30269, 30307, 30323)
  mu  <- rep(0,n)
  for(i in 1:n){
    seed  <- randu(seed)
    mu[i] <- (sum(seed/m)) %% 1 }
  return(list(mues=mu,lastseed=seed))}
n    <- 5000
seed <- c(67612,92318,652612)
unif <- RANWH(n,seed)$mues



3 %% 1
3.5 %% 1




# M�todo de cuadrados de von Neumann

# con 4 d�gitos (no buena idea)
neumann <- function(seed){
  (floor( (seed^2)/10^2 )) %% 10^4 }

RCUAD <- function(n,seed){
  mu  <- rep(0,n)
  for(i in 1:n){
    seed  <- neumann(seed)
    mu[i] <- seed }
  return(mu/10^4)}

seed <- 9876  # cicla al 50-avo
n    <- 80
unif <- RCUAD(n,seed)






# con 10 d�gitos
neumann <- function(seed){
  (floor( (seed^2)/10^5 )) %% 10^10 }

RCUAD <- function(n,seed){
  mu  <- rep(0,n)
  for(i in 1:n){
    seed  <- neumann(seed)
    mu[i] <- seed }
  return(mu/10^10)}

n    <- 5000
seed <- 9182736450
unif <- RCUAD(n,seed)

plot(unif,pch=".",cex.axis=.7, cex.lab=.7)













# Generar Exponenciales y Weibulls
set.seed(7479)
n     <- 2000
lam   <- 2
rexpo <- -(1/lam)*log(runif(n))
rexpr <- rexp(n,rate=lam)
teta  <- 2
beta  <- 2
rweio <- teta*(-log(runif(n)))^(1/beta)
rweir <- rweibull(n, shape=beta, scale=teta )
par(mfrow=c(2,2))
hist(rexpo, cex.axis=.7, cex.lab=.7, cex.main=.7, xlim=c(0,5.1),breaks=12)
hist(rexpr, cex.axis=.7, cex.lab=.7, cex.main=.7, xlim=c(0,5.1),breaks=12)
hist(rweio, cex.axis=.7, cex.lab=.7, cex.main=.7, xlim=c(0,6.8),breaks=12)
hist(rweir, cex.axis=.7, cex.lab=.7, cex.main=.7, xlim=c(0,6.8),breaks=12)






# Box Muller
n  <- 1000
u1 <- runif(n)
u2 <- runif(n)
x  <- sqrt(-2*log(u1))*cos(2*pi*u2)
y  <- sqrt(-2*log(u1))*sin(2*pi*u2)
r  <- c(-4,4)

par(mfrow=c(2,2), 
    mar=c(4, 4, 3, 1) + 0.1)
hist(x, cex.axis=.7, cex.lab=.7,col="cyan",
     cex.main=.7, xlim=r)
hist(y, cex.axis=.7, cex.lab=.7,col="cyan",
     cex.main=.7, xlim=r)
plot(x,y, pch=".", cex.axis=.7, 
     cex.lab=.7, xlim=r, ylim=r)
plot(rnorm(n),rnorm(n), pch=".", 
     cex.axis=.7, cex.lab=.7, xlim=r,
     ylim=r)




# Prueba de uniformidad Ji-Cuadrada
randu <- function(seed) (a*seed) %% m

RANDU <- function(n,seed){
  a  <<- 2^16 + 3
  m  <<- 2^31         # solo para ilustrar variables locales -> globales
  mu  <- rep(0,n)
  for(i in 1:n){
    seed  <- randu(seed)
    mu[i] <- seed/m }
  return(list(mues=mu,lastseed=seed))}

# Generamos 2000 uniformes (0,1):
n    <- 2000
seed <- 458
seed <- 45819
unif <- RANDU(n,seed)$mues

Oj <- hist(unif,breaks=seq(0,1,by=.1),plot=F)$counts
# Ji-cuadrada de Pearson
Ej  <- n/10
P   <- sum( ((Oj-Ej)^2)/Ej )          # [1] 5.23
pv  <- 1-pchisq(P,9)                  # [1] 0.8138153


hist(unif,breaks=seq(0,1,by=.1),col="cyan",freq=F)
abline(h=1,col="red",lwd=2)



# Kolmogorov-Smirnov
library(dgof)
ks.test(unif,"punif")


One-sample Kolmogorov-Smirnov test

data:  unif
D = 0.021372, p-value = 0.3205
alternative hypothesis: two-sided


# Funci�n de distribuci�n emp�rica
par(mfrow=c(2,1),mar=c(2,2,2,2))
n    <- 20
med  <- 200
des  <- 35
delt <- .10
M    <- 200
edf  <- ((1:n)-.5)/n
dat  <- qnorm(edf,mean=med,sd=des)
ran  <- max(dat) - min(dat)
lmin <- min(dat) - delt*ran
lmax <- max(dat) + delt*ran
xx   <- seq(lmin,lmax,length=M)
yy   <- pnorm(xx,mean=med,sd=des)
plot(dat,edf, xlim=c(lmin,lmax), ylim=c(0,1.05), mgp=c(1.5,.5,0),
     xlab="", ylab="", cex.axis=.8, cex.lab=.8,type="n",
     main="Funcion de Distribucion Empirica",cex.main=1)
rug(dat)
lines(xx,yy,col="red",lwd=2)
segments(lmin,0,dat[1],0,col="blue",lwd=2)
for(i in 1:(n-1)){ segments(dat[i],edf[i],dat[i+1],edf[i],col="blue",lwd=2) }
segments(dat[n],edf[n],lmax,edf[n],col="blue",lwd=2)
segments(dat[1],0,dat[1],edf[1],col="blue",lty=2)
for(i in 2:n){ segments(dat[i],edf[i-1],dat[i],edf[i],col="blue",lty=2) }
points(dat,edf,pch=20)
legend(lmin,1.05,legend=c("Fn(x): Empirica de datos perfectos","F0(x): Teorica"),
       col=c("blue","red"),lwd=2, cex=.8, bty="n")

edf  <- (1:n)/n
set.seed(5963471)
dat  <- sort(rnorm(n,mean=med,sd=des))
xx   <- seq(lmin,lmax,length=M)
yy   <- pnorm(xx,mean=med,sd=des)
plot(dat,edf, xlim=c(lmin,lmax), ylim=c(0,1.05), mgp=c(1.5,.5,0),
     xlab="", ylab="", cex.axis=.8, cex.lab=.8,type="n",
     main="Funcion de Distribucion Empirica",cex.main=1)
rug(dat)
lines(xx,yy,col="red",lwd=2)
segments(lmin,0,dat[1],0,col="blue",lwd=2)
for(i in 1:(n-1)){ segments(dat[i],edf[i],dat[i+1],edf[i],col="blue",lwd=2) }
segments(dat[n],1,lmax,1,col="blue",lwd=2)
segments(dat[1],0,dat[1],edf[1],col="blue",lty=2)
for(i in 2:n){ segments(dat[i],edf[i-1],dat[i],edf[i],col="blue",lty=2) }
points(dat,edf,pch=20)
legend(lmin,1.05,legend=c("Fn(x): Empirica","F0(x): Teorica"),
       col=c("blue","red"),lwd=2, cex=.8, bty="n")





# Prueba de normalidad. Pesos (en gramos) de 20 pollitos
vc05 <- 0.2940701         # valor obtenido del libro New Cambridge Statistical Tables
dat  <- sort(c(156,162,168,182,186,190,190,196,202,210,
               214,220,226,230,230,236,236,242,246,270))
datu <- unique(dat)
n    <- length(dat)
nu   <- length(datu)
aa   <- table(dat)
edf  <- cumsum(aa)/n
edf0 <- c(0,edf[-nu])
xx   <- seq(150,275,length=200)
yy   <- pnorm(xx,mean=200,sd=35)
ye   <- pnorm(datu,mean=200,sd=35)
yy   <- pnorm(xx,mean=mean(dat),sd=sd(dat))      # usando estimaciones los
ye   <- pnorm(datu,mean=mean(dat),sd=sd(dat))    # valores criticos no validos
Dn   <- max(max(abs(ye-edf)),max(abs(ye-edf0)))  #  0.1037224
ii   <- which(Dn == pmax(abs(ye-edf),abs(ye-edf0)))[1]
if( abs(ye[ii]-edf[ii])==Dn ){
  yD <- edf[ii] }else{
    yD <- edf0[ii] }

plot(datu,edf, xlim=c(150,275), ylim=c(0,1.05), mgp=c(1.5,.5,0),
     xlab="", ylab="", cex.axis=.8, cex.lab=.8,type="n",
     main="Estadistico de Kolmogorov-Smirnov")
rug(jitter(dat))
segments(150,0,datu[1],0,col="blue",lwd=2)
for(i in 1:(nu-1)){
  segments(datu[i],edf[i],datu[i+1],edf[i],col="blue",lwd=2)}
segments(datu[nu],1,275,1,col="blue",lwd=2)
lines(xx,yy,col="red",lwd=2)
points(datu,edf,pch=16)
points(c(datu[ii],datu[ii]),c(ye[ii],yD))
segments(datu[ii],ye[ii],datu[ii],yD,lwd=2,col="orange")

legend(155,1.05,legend=c("Fn(x)","F0(x)",paste("Dn = ",round(Dn,3)),
                         paste("ValCrit(.05) = ",round(vc05,3))),
       col=c("blue","red","orange","white"),lwd=2, cex=.8, bty="n")


# Kolmogorov-Smirnov
library(dgof)
ks.test(dat,"pnorm")


table(dat)


datj <- ifelse(dat==190,c(189,191),dat)
datj <- ifelse(datj==230,c(229,231),datj)
datj <- ifelse(datj==236,c(235,237),datj)

ks.test(datj,"pnorm",mean(datj),sd(datj))


table(datj)


# Simulaci�n de Dn para obtener p-valores
set.seed(656)
n  <- 20
d  <- 0.10369
M  <- 10000
j  <- 1:n
Dp <- rep(0,M)
for(i in 1:M){
  u <- sort(runif(n))
  Dp[i] <- ( max( cbind(j/n-u,u-(j-1)/n) ) >= d ) }
pvalor <- mean(Dp)
pvalor #  0.9668








# Rachas
M  <- 1000
r  <- rep(0,M)        # donde M es muy grande
n  <- 14
for( i in 1:M){
  orden <- sample( c(0,0,0,0,0,0,1,1,1,1,1,1,1,1) )
  r[i]  <- cuenta( orden ) }

cuenta <- function(a){
  run <- 1
  for(i in 2:n){
    if( a[i] == a[i-1] ){next}
    run <- run + 1}
  return(run)}

barplot( table(r) / M , main="Distribuci�n de rachas")

hist(table(r))

obs <- c(0,0,1,1,1,0,1,1,1,1,0,0,1,0)
cuenta(obs) # 7
# 7 rachas es un resultado probable => no rechazamos orden aleatorio
  
library(snpar)
runs.test(obs, exact = TRUE, alternative = c("two.sided", "less", "greater"))


a <- c(1,5,4,1,3,1,3,4,7)
plot(a,type="l")
points(a,pch=20)
abline(v=c(2,4,5,6,9),lty=2,col="red")



n <- 7
a <- function(x){
  n <<- 10
  return(x+n)
}
a(8)
n






h  <- 1;   r  <- 1
xx <- seq(0,2*h,length=200)
yu <- h + sqrt(r^2-(xx-h)^2)
yl <- h - sqrt(r^2-(xx-h)^2)
plot(h,h,xlab="",ylab="",type="n", mgp=c(1.5,.5,0), asp=1,
     xlim=c(0,2*h), ylim=c(0,2*h), cex.axis=.8, bty="n" )
lines(xx,yu,lwd=2,col="red")
lines(xx,yl,lwd=2,col="red")
segments(0,0,2*h,0,lwd=2,col="blue")
segments(2*h,0,2*h,2*h,lwd=2,col="blue")
segments(2*h,2*h,0,2*h,lwd=2,col="blue")
segments(0,2*h,0,0,lwd=2,col="blue")
abline(h=h,v=h,col=gray(.8))
text(1.4,.6,"C",cex=1.5,col="red")

