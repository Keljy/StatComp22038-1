## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(knitr)
knitr::kable(head(iris))
with(trees, symbols(Height, Volume, circles = Girth/16, inches = FALSE, bg = "deeppink", fg = "gray30"))
cat("Hello world.\n")

## -----------------------------------------------------------------------------
set.seed(22038)
a <- 2 #parameter a
b <- 2 #parameter b
n <- 1000 #sample size
U <- runif(n) #generate U
X <- b/((1-U)^(1/a)) #inverse trans
head(X)

## -----------------------------------------------------------------------------
f <- function(x){  #to draw the pdf of P(a,b)
  y <- a/b * (b/x)^(a+1)
  d <- data.frame(x=x,y=y)
  return(d)
}
x <- seq(b,50,0.5)
d <- f(x)

#library(ggplot2)
#data <- as.data.frame(X)
#p <- ggplot(data, aes(x = X))
#p + geom_density(color = "red") + geom_histogram(aes(x = X, y = ..density..),fill = "red", alpha = 0.2) + geom_line(data =d ,aes(x = x, y = y),col = "blue")

## -----------------------------------------------------------------------------
##f(x)
f <- function(x){
  y <- 1/beta(a,b) * x^(a-1) * (1-x)^(b-1)
  return(y)
}
##g(x)
g <- function(x) 1

beta.ar <- function(a,b,n){
  naccepts <- 0
  sample <- numeric(n)
  x.max <- (1-a)/(2-a-b)
  c <- f(x.max)
  while(naccepts < n){
    y <- runif(1)
    u <- runif(1)
    if(u <= f(y)/(c*g(y))){
      naccepts <- naccepts + 1
      sample[naccepts] = y
    }
  }
  return(sample)
}

## -----------------------------------------------------------------------------
set.seed(22038)
a <- 3
b <- 2
n <- 1000
X <- beta.ar(a,b,n)
head(X)

## -----------------------------------------------------------------------------
x <- seq(0,1,0.05)
d <- data.frame(x=x,y=f(x))

#library(ggplot2)
#data <- as.data.frame(X)
#p <- ggplot(data, aes(x = X))
#p + geom_density(color = "red") + geom_histogram(aes(x = X, y = ..density..),fill = "red", alpha = 0.2) + geom_line(data =d ,aes(x = x, y = y),col = "blue")

## -----------------------------------------------------------------------------
set.seed(22038)
r <- 4
beta <- 2
n <- 1000
Gamma <- rgamma(n,r,beta)
Y <- rexp(n,Gamma)
hist(Y)

## -----------------------------------------------------------------------------
r <- 4
beta <- 2

##to draw the pdf of Pareto distribution
f <- function(x){
  y <- r/(beta+x) * (beta/(beta+x))^r
  d <- data.frame(x=x,y=y)
  return(d)
}
x <- seq(0,15,0.5)
d <- f(x)

#library(ggplot2)
#data <- as.data.frame(Y) #Use the sample we generate in 3.12
#p <- ggplot(data, aes(x = Y))
#p + geom_density(color = "red") + geom_histogram(aes(x = Y, y = ..density..),fill = "red", alpha = 0.2) + geom_line(data =d ,aes(x = x, y = y),col = "blue")

## -----------------------------------------------------------------------------
quickSort <- function(x){
  if(length(x)<=1) return(x)
  point <- x[1]
  t <- x[-1]
  sv1 <- t[t<point]
  sv2 <- t[t>=point]
  sv1 <- quickSort(sv1)
  sv2 <- quickSort(sv2)
  return(c(sv1,point,sv2))
}

## -----------------------------------------------------------------------------
N <- c(10000,20000,40000,60000,80000)
times <- 100 #times for simulations
an <- numeric(5)
for (i in 1:5) {
  n <- N[i]
  time <- numeric(times)
  for (j in 1:times) {
    a <- sample(1:n,n,replace = FALSE)
    time[j] <- system.time({quickSort(a)})[1]
  }
  an[i] <- mean(time)
}
an

## -----------------------------------------------------------------------------
tn <- N*log(N)
myfit <- lm(tn ~ an)
myfit
plot(an,tn)
abline(myfit,col="red")

## -----------------------------------------------------------------------------
e = exp(1)
e-(e-1)^2

## -----------------------------------------------------------------------------
-5+10*e-3*e^2

## -----------------------------------------------------------------------------
p=(-1-2*e+e^2)/(-3+4*e-e^2) /2
p

## -----------------------------------------------------------------------------
n <- 100000
set.seed(114514)
U <- runif(n)
T1 <- exp(U)
T2 <- exp(1-U)
cov(T1,T2) #the theoretical value is -0.2342106
var(T1+T2) #the theoretical value is 0.01564999

## -----------------------------------------------------------------------------
sum(T1+T2)/(2*n)

## -----------------------------------------------------------------------------
sum(T1)/n

## -----------------------------------------------------------------------------
(var(T1)-var(T1+T2)/4)/var(T1)
#the theoretical value is 0.983835

## -----------------------------------------------------------------------------
set.seed(114)
n<-1000
g<-function(x){
  exp(-x^2/2)*x^2/sqrt(2*pi)*(x>1)
}
est<-numeric(2)
sd<-numeric(2)

#f1
x<-rweibull(n,2,sqrt(2))
gf<-g(x)/dweibull(x,2,sqrt(2))
est[1]<-mean(gf)
sd[1]<-sd(gf)

#f2
x<-rgamma(n,3,2)
gf<-g(x)/dgamma(x,3,2)
est[2]<-mean(gf)
sd[2] <- sd(gf)

est
sd

## -----------------------------------------------------------------------------
t<-seq(1,10,0.1)
g<-exp(-t^2/2)*t^2/sqrt(2*pi)
f1<-dweibull(t,2,sqrt(2))
f2<-dgamma(t,3,2)

plot(t,g,type="l",col="black",main="compare g(t), f1(t) and f2(t) ",ylim = c(0,0.5))   
lines(t,f1,col="red")  
lines(t,f2,col="blue")
legend("topright",legend =c('g(t)','f1(t)',"f2(t)") ,lty=1,col=c("black","red","blue")) 

## -----------------------------------------------------------------------------
t<-seq(1,10,0.1)
g<-exp(-t^2/2)*t^2/sqrt(2*pi)
f1<-dweibull(t,2,sqrt(2))
f2<-dgamma(t,3,2)
r1<-g/f1
r2<-g/f2
plot(t,r1,col="red", type = "l")
lines(t,r2,col="blue")
title(main="ratio function")
legend("topright",legend =c('g/f1(t)',"g/f2(t)") ,lty=1,col=c("red","blue")) 

## -----------------------------------------------------------------------------
set.seed(114)
M <- 10000
N <- 50 
k <- 5 
r <- M/k 
T5 <- numeric(k)
est <- matrix(0, N, 2)
#use reverse transform method
g<-function(x,a,b) exp(-x)/(1+x^2)*(x>a)*(x<b)
h<-function(u,a,b) -log(exp(-a)-u*(exp(-a)-exp(-b)))
fg<-function(x,a,b) g(x,a,b)/(exp(-x)/(exp(-a)-exp(-b)))
for (i in 1:N) {
  u<-runif(M)
  u.s<-runif(M/k)
  #importance sampling
  est[i, 1] <- mean(fg(h(u,0,1),0,1))
  #stratified importance sampling
  for(j in 1:k) T5[j]<-mean(fg(h(u.s,(j-1)/k,j/k),(j-1)/k,j/k))
  est[i, 2] <- sum(T5)
}
apply(est,2,mean)
apply(est,2,sd)

## -----------------------------------------------------------------------------
set.seed(114)
mu <- 2
sigma <- 1
n <- 1000
alpha <- 0.05
tests <- replicate(n,expr={
  x <- rlnorm(n,mu,sigma^2) #produce sample from lognormal distribution
  y <- log(x) #let Y=lnX,Y~normal distribution
  abs(sqrt(n/var(y))*(mu-mean(y)))<qt(1-alpha/2,n-1)
})
mean(tests)

## -----------------------------------------------------------------------------
count5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  #return 1 (reject) or 0 (do not reject H0)
  return(as.integer(max(c(outx, outy)) > 5))
}
Ftest <- function(x,y){
  Fp <- var.test(x, y)$p.value
  return(as.integer(Fp <= alpha.hat))
}
set.seed(114)
alpha.hat <- 0.055
n <- c(10, 20, 50, 100, 500, 1000)
mu1 <- mu2 <- 0
sigma1 <- 1
sigma2 <- 1.5
m <- 1e4
result <- matrix(0, length(n), 2)
for (i in 1:length(n)){
  tests <- replicate(m, expr={
    x <- rnorm(n[i], mu1, sigma1)
    y <- rnorm(n[i], mu2, sigma2)
    c(count5test(x, y), Ftest(x, y))
    })
  result[i, ] <- rowMeans(tests)
}
result <- as.data.frame(result,row.names = n)
colnames(result)<-c("Count Five test","F test")
result

## -----------------------------------------------------------------------------
x <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
n <- length(x)
lambda <- n/sum(x)
lambda

## -----------------------------------------------------------------------------
x <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
n <- length(x)
B <- 1e4
set.seed(12345)
lambdastar <- numeric(B)
lambda <- n/sum(x)
for(b in 1:B){
  xstar <- sample(x,replace=TRUE)
  lambdastar[b] <- n/sum(xstar) 
}
round(c(bias=mean(lambdastar)-lambda,se.boot=sd(lambdastar)),3)

## -----------------------------------------------------------------------------
library(boot)
x <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
n <- length(x)
meantime.boot <- function(x,i){
  mean(x[i])
}
boot.obj <- boot(x, statistic = meantime.boot, R = 2000)
print(boot.ci(boot.obj,type = c("basic", "norm", "perc","bca")))

## -----------------------------------------------------------------------------
mu<-0
b<-1
n<-10
m<-1000
library(boot)
set.seed(12345) 
boot.mean <- function(x,i) mean(x[i]) 
ci.norm<-ci.basic<-ci.perc<-matrix(NA,m,2) 
for(i in 1:m){
  R<-rnorm(n,mu) #sample from a normal population
  de <- boot(data=R,statistic=boot.mean, R = 999) 
  ci <- boot.ci(de,type=c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3] #the standard normal bootstrap CI
  ci.basic[i,]<-ci$basic[4:5] #the basic bootstrap CI
  ci.perc[i,]<-ci$percent[4:5] #the percentile CI
}

## -----------------------------------------------------------------------------
cat(' norm :','left=',mean(ci.norm[,1]>mu),'right=',mean(ci.norm[,2]<mu),'\n',
    'basic :','left=',mean(ci.basic[,1]>mu),'right=',mean(ci.basic[,2]<mu),'\n',
    'perc :','left=',mean(ci.perc[,1]>mu),'right=',mean(ci.perc[,2]<mu))

## -----------------------------------------------------------------------------
library(bootstrap)
data <- bootstrap::scor
sigma <- cov(data)
lambda <- eigen(sigma)$values
theta.hat <- lambda[1]/sum(lambda)
n <- nrow(data)
theta.jack <- numeric(n)
for (i in 1:n) {
  sigma <- cov(data[-i,])
  lambda <- eigen(sigma)$values
  theta.jack[i] <- lambda[1]/sum(lambda)
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))
round(c(original=theta.hat,bias.jack=bias.jack,se.jack=se.jack),3)

## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n <- length(magnetic) #in DAAG ironslag 
e1 <- e2 <- e3 <- e4 <- matrix(data = NA,nrow = n*(n-1)/2,ncol = 2)
k <- 0
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    y <- magnetic[-c(i,j)] 
    x <- chemical[-c(i,j)]
    k <- k + 1
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[i]
    e1[k,1] <- magnetic[i] - yhat1
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[j] 
    e1[k,2] <- magnetic[j] - yhat1
    
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[i] + J2$coef[3] * chemical[i]^2
    e2[k,1] <- magnetic[i] - yhat2
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[j] + J2$coef[3] * chemical[j]^2
    e2[k,2] <- magnetic[j] - yhat2
      
    J3 <- lm(log(y) ~ x) 
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[i]
    yhat3 <- exp(logyhat3) 
    e3[k,1] <- magnetic[i] - yhat3
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[j]
    yhat3 <- exp(logyhat3) 
    e3[k,2] <- magnetic[j] - yhat3
    
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[i]) 
    yhat4 <- exp(logyhat4)
    e4[k,1] <- magnetic[i] - yhat4
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[j]) 
    yhat4 <- exp(logyhat4)
    e4[k,2] <- magnetic[j] - yhat4
  }
}

## -----------------------------------------------------------------------------
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## -----------------------------------------------------------------------------
lm(formula = magnetic ~ chemical + I(chemical^2))

## -----------------------------------------------------------------------------
set.seed(0)
x <- rnorm(15,0,10)
y <- rnorm(15,5,10)
R <- 999
z <- c(x, y)
K <- 1:30
reps <- numeric(R)
t0 <- cor(x,y,method = "spearman")
for (i in 1:R) {
  x1 <- sample(x)
  y1 <- sample(y)
  reps[i] <- cor(x1,y1,method = "spearman")
  }
p <- mean(c(t0, reps) >= t0)
p

## -----------------------------------------------------------------------------
set.seed(0)
reps <- numeric(R)
t0 <- cor.test(x,y)$statistic
for (i in 1:R) {
  k <- sample(K, size = 15, replace = FALSE) 
  x1 <- z[k]
  y1 <- z[-k] #complement of x1
  reps[i] <- cor.test(x1,y1)$statistic
  }
p <- mean(c(t0, reps) >= t0)
p

## -----------------------------------------------------------------------------
set.seed(38)

lap_f = function(x) exp(-abs(x))/2

rw.Metropolis = function(sigma, x0, N){
  x = numeric(N)
  x[1] = x0
  u = runif(N)
  k = 0
  for (i in 2:N) {
    y = rnorm(1, x[i-1], sigma)
    if (u[i] <= (lap_f(y) / lap_f(x[i-1]))) x[i] = y 
    else {
      x[i] = x[i-1]
      k = k+1
    }
  }
  return(list(x = x, k = k))
}

N = 2000
sigma = c(0.05, 0.5, 2, 16)
x0 = 25
rw1 = rw.Metropolis(sigma[1],x0,N)
rw2 = rw.Metropolis(sigma[2],x0,N)
rw3 = rw.Metropolis(sigma[3],x0,N)
rw4 = rw.Metropolis(sigma[4],x0,N)

#compute the acceptance rates
Rej = matrix(c(rw1$k, rw2$k, rw3$k, rw4$k),1,4)
Acc = round((N-Rej)/N,4)
rownames(Acc) = c("acceptance rate")
colnames(Acc) = paste("sigma=",sigma)
print(Acc)

#plot
#par(mfrow=c(2,2))  #display 4 graphs together
rw = cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
for (j in 1:4) {
  plot(rw[,j], type="l",xlab=bquote(sigma == .(round(sigma[j],3))),
  ylab="X", ylim=range(rw[,j]))
}

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j]) 
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) #row means
  B <- n * var(psi.means) #between variance est.
  psi.w <- apply(psi, 1, "var")  #within variances
  W <- mean(psi.w) #within est.
  v.hat <- W*(n-1)/n + (B/n) #upper variance est.
  r.hat <- v.hat / W  #G-R statistic
  return(r.hat)
}
set.seed(38)
sigma = 0.05 #parameter of proposal distribution
k <- 4 #number of chains to generate 
n <- 15000 #length of chains
b <- 500 #burn-in length
#choose overdispersed initial values 
x0 <- c(-10, -5, 5, 10)
#generate the chains
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- rw.Metropolis(sigma,x0[i],n)$x
#compute diagnostic statistics 
psi <- t(apply(X, 1, cumsum)) 
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi)) 
#plot the sequence of R-hat statistics 
rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j]) 
plot(rhat[(b+1):n], type="l", xlab="", ylab="R") 
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
set.seed(38)
sigma = 0.5 #parameter of proposal distribution
k <- 4 #number of chains to generate 
n <- 10000 #length of chains
b <- 500 #burn-in length
#choose overdispersed initial values 
x0 <- c(-10, -5, 5, 10)
#generate the chains
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- rw.Metropolis(sigma,x0[i],n)$x
#compute diagnostic statistics 
psi <- t(apply(X, 1, cumsum)) 
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi)) 

#plot the sequence of R-hat statistics 
rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j]) 
plot(rhat[(b+1):n], type="l", xlab="", ylab="R") 
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
set.seed(38)
sigma = 2 #parameter of proposal distribution
k <- 4 #number of chains to generate 
n <- 5000 #length of chains
b <- 500 #burn-in length
#choose overdispersed initial values 
x0 <- c(-10, -5, 5, 10)
#generate the chains
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- rw.Metropolis(sigma,x0[i],n)$x
#compute diagnostic statistics 
psi <- t(apply(X, 1, cumsum)) 
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi)) 

#plot the sequence of R-hat statistics 
rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j]) 
plot(rhat[(b+1):n], type="l", xlab="", ylab="R") 
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
set.seed(38)
sigma = 16 #parameter of proposal distribution
k <- 4 #number of chains to generate 
n <- 2000 #length of chains
b <- 200 #burn-in length
#choose overdispersed initial values 
x0 <- c(-10, -5, 5, 10)
#generate the chains
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- rw.Metropolis(sigma,x0[i],n)$x
#compute diagnostic statistics 
psi <- t(apply(X, 1, cumsum)) 
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi)) 

#plot the sequence of R-hat statistics 
rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j]) 
plot(rhat[(b+1):n], type="l", xlab="", ylab="R") 
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
set.seed(38)
#initialize constants and parameters
N <- 5000 #length of chain
burn <- 1000 #burn-in length
X <- matrix(0, N, 2) #the chain, a bivariate sample
rho <- 0.9
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
s1 <- sqrt(1-rho^2)*sigma1 
s2 <- sqrt(1-rho^2)*sigma2
###### generate the chain #####
X[1, ] <- c(mu1, mu2) #initialize
for (i in 2:N) {
  x2 <- X[i-1, 2]
  m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2 
  X[i, 1] <- rnorm(1, m1, s1)
  x1 <- X[i, 1]
  m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1 
  X[i, 2] <- rnorm(1, m2, s2)
}
b <- burn + 1 
x <- X[b:N, ]

## -----------------------------------------------------------------------------
colMeans(x)
cor(x)
plot(x, main="", cex=.5, xlab=bquote(X[1]), ylab=bquote(X[2]), ylim=range(x[,2]))

## -----------------------------------------------------------------------------
x <- as.data.frame(x)
colnames(x) <- c("x","y")
lmfit <- lm(y~x,data = x)
plot(lmfit)

## -----------------------------------------------------------------------------
set.seed(38)
k <- 4 #number of chains to generate 
n <- 5000 #length of chains
b <- 200 #burn-in length
X <- matrix(0, n, 2) #the chain, a bivariate sample
#choose overdispersed initial values 
x0 <- c(-10, -5, 5, 10)
#generate the chains
X.GR <- matrix(0, nrow=k, ncol=n)
Y.GR <- matrix(0, nrow=k, ncol=n)
for (j in 1:k){
  X[1, ] <- c(x0[j], x0[j]) #initialize
  for (i in 2:n) {
    x2 <- X[i-1, 2]
    m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2 
    X[i, 1] <- rnorm(1, m1, s1)
    x1 <- X[i, 1]
    m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1 
    X[i, 2] <- rnorm(1, m2, s2)
  }
  X.GR[j, ] <- X[ ,1]
  Y.GR[j, ] <- X[ ,2]
}
#compute diagnostic statistics 
psi <- t(apply(X.GR, 1, cumsum)) 
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi)) 
#plot the sequence of R-hat statistics 
rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j]) 
plot(rhat[(b+1):n], type="l", xlab="", ylab="R") 
abline(h=1.2, lty=2)

psi <- t(apply(Y.GR, 1, cumsum)) 
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi)) 
#plot the sequence of R-hat statistics 
rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j]) 
plot(rhat[(b+1):n], type="l", xlab="", ylab="R") 
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
set.seed(1)
n <- 1000
am <- 2
ay <- 3
X <- rnorm(n,10,5)
alpha <- 0
beta <- 0
gamma <- 1
em <- rnorm(n,0,1)
ey <- rnorm(n,0,1)
M <- am + alpha*X +em
Y <- ay + beta*M +gamma*X +ey

## -----------------------------------------------------------------------------
library(lmPerm)
set.seed(1)
fit <- lmp(M ~ X,perm='Prob')
summary(fit)
fit2 <- lmp(Y ~ M + X,perm='Prob')
summary(fit2)

## -----------------------------------------------------------------------------
set.seed(1)
#library(mediation)
#a <- lm(M~X)
#b <- glm(Y~X+M)
#result <- mediate(a,b,treat='X',mediator='M',boot=T)
#summary(result)

## -----------------------------------------------------------------------------
set.seed(1)
n <- 1000
am <- 2
ay <- 3
X <- rnorm(n,10,5)
alpha <- 0
beta <- 1
gamma <- 1
em <- rnorm(n,0,1)
ey <- rnorm(n,0,1)
M <- am + alpha*X +em
Y <- ay + beta*M +gamma*X +ey

## -----------------------------------------------------------------------------
set.seed(1)
fit <- lmp(M ~ X,perm='Prob')
summary(fit)
fit2 <- lmp(Y ~ M + X,perm='Prob')
summary(fit2)

## -----------------------------------------------------------------------------
a <- lm(M~X)
b <- glm(Y~X+M)
#result <- mediate(a,b,treat='X',mediator='M',boot=T)
#summary(result)

## -----------------------------------------------------------------------------
set.seed(1)
n <- 1000
am <- 2
ay <- 3
X <- rnorm(n,10,5)
alpha <- 1
beta <- 0
gamma <- 1
em <- rnorm(n,0,1)
ey <- rnorm(n,0,1)
M <- am + alpha*X +em
Y <- ay + beta*M +gamma*X +ey

## -----------------------------------------------------------------------------
set.seed(1)
fit <- lmp(M ~ X,perm='Prob')
summary(fit)
fit2 <- lmp(Y ~ M + X,perm='Prob')
summary(fit2)

## -----------------------------------------------------------------------------
a <- lm(M~X)
b <- glm(Y~X+M)
#result <- mediate(a,b,treat='X',mediator='M',boot=T)
#summary(result)

## -----------------------------------------------------------------------------
set.seed(1)
n <- 1000
am <- 2
ay <- 3
X <- rnorm(n,10,5)
alpha <- 1
beta <- 1
gamma <- 1
em <- rnorm(n,0,1)
ey <- rnorm(n,0,1)
M <- am + alpha*X +em
Y <- ay + beta*M +gamma*X +ey

## -----------------------------------------------------------------------------
a <- lm(M~X)
b <- glm(Y~X+M)
#result <- mediate(a,b,treat='X',mediator='M',boot=T)
#summary(result)

## -----------------------------------------------------------------------------
findalpha <- function(N,b1,b2,b3,f0){
  x1 <- rpois(N,1)
  x2 <- rexp(N,1)
  x3 <- sample(0:1,N,replace=TRUE)
  g <- function(alpha){
    tmp <- exp(-alpha-b1*x1-b2*x2-b3*x3)
    p <- 1/(1+tmp)
    mean(p) - f0
  }
  solution <- uniroot(g,c(-20,0))
  alpha <- solution$root
  return(alpha)
}

## -----------------------------------------------------------------------------
set.seed(1)
findalpha(N=10^6,b1=0,b2=1,b3=-1,f0=0.1)
findalpha(N=10^6,b1=0,b2=1,b3=-1,f0=0.01)
findalpha(N=10^6,b1=0,b2=1,b3=-1,f0=0.001)
findalpha(N=10^6,b1=0,b2=1,b3=-1,f0=0.0001)

## -----------------------------------------------------------------------------
f0 <- c(0.1,0.01,0.001,0.0001)
alpha <- c(-3.169898,-6.073926,-8.715275,-11.2702)
plot(f0,alpha)

## -----------------------------------------------------------------------------
u <- c(11,8,27,13,16,0,23,10,24,2)
v <- c(12,9,28,14,17,1,24,11,25,3)
f <- function(lambda){
  sum((-u*exp(-lambda*u)+v*exp(-lambda*v))/(exp(-lambda*u)-exp(-lambda*v)))
}
root <- uniroot(f,c(0,10))
root$root

## -----------------------------------------------------------------------------
n <- 10
l0 <- 1 #let lambda0=1
for (i in 1:20) {
  l1 <- n/sum(1/l0+(u*exp(-l0*u)-v*exp(-l0*v))/(exp(-l0*u)-exp(-l0*v)))
  l0 <- l1
}
l1

## -----------------------------------------------------------------------------
a <- c(1,2)
dim(a)

## -----------------------------------------------------------------------------
a <- matrix(c(1,1,1,1),2,2)
is.matrix(a)
is.array(a)

## -----------------------------------------------------------------------------
df_coltypes <- data.frame(
  a = c("a", "b"),
  b = c(TRUE, FALSE),
  c = c(1L, 0L),
  d = c(1.5, 2),
  e = factor(c("f1", "f2"))
)
as.matrix(df_coltypes)

## -----------------------------------------------------------------------------
#0x2
data.frame(a = integer(), b = logical()) 
#2x0
data.frame(row.names = 1:2)
#0x0
data.frame()

## -----------------------------------------------------------------------------
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE) 
  (x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
library(purrr)
data1 <- data.frame(a=c(1,2,3),b=c(2,3,1))
data1
modify(data1, scale01)

## -----------------------------------------------------------------------------
data2 <- data.frame(a=c(1,2,3),b=c(2,3,1),c=c("a1","a2","a3"))
data2
modify_if(data2, is.numeric, scale01)

## -----------------------------------------------------------------------------
data1
vapply(data1,sd,numeric(1))

## -----------------------------------------------------------------------------
data2
vapply(data2[vapply(data2, is.numeric, logical(1))],sd, numeric(1))

## -----------------------------------------------------------------------------
set.seed(1)
#initialize constants and parameters
N <- 5000               #length of chain
X <- matrix(0, N, 2)    #the chain, a bivariate sample
rho <- 0.9             #correlation
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1

rbnR <- function(N){
  X <- matrix(0, N, 2)    #the chain, a bivariate sample
  s1 <- sqrt(1-rho^2)*sigma1
  s2 <- sqrt(1-rho^2)*sigma2
  ###### generate the chain #####
  X[1, ] <- c(mu1, mu2)            #initialize
  for (i in 2:N) {
      x2 <- X[i-1, 2]
      m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
      X[i, 1] <- rnorm(1, m1, s1)
      x1 <- X[i, 1]
      m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
      X[i, 2] <- rnorm(1, m2, s2)
  }
  return(X)
}
mat1 <- rbnR(N)

## ----eval=FALSE, include=FALSE------------------------------------------------
#  #include <Rcpp.h>
#  using namespace Rcpp;
#  // [[Rcpp::export]]
#  NumericMatrix rbnC(int N) {
#    NumericMatrix mat(N, 2);
#    double mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 1, rho = 0.9;
#    double x1 = 0, x2 = 0;
#    double m1 = 0, m2 = 0;
#    double s1 = sqrt(1-rho*rho)*sigma1;
#    double s2 = sqrt(1-rho*rho)*sigma2;
#    mat(0, 0) = mu1;
#    mat(0, 1) = mu2;
#    for(int i = 1; i < N; i++) {
#      x2 = mat(i-1, 1);
#      m1 = mu1 + rho*(x2-mu2)*sigma1/sigma2;
#      mat(i,0) = rnorm(1,m1,s1)[0];
#      x1 = mat(i-1,0);
#      m2 = mu2 + rho*(x1-mu1)*sigma2/sigma1;
#      mat(i,1) = rnorm(1,m2,s2)[0];
#    }
#    return(mat);
#  }

## ----eval=FALSE, include=FALSE------------------------------------------------
#  library(Rcpp)
#  dir_cpp <- '../Desktop/Rcpp/'
#  # Can create source file in Rstudio
#  sourceCpp(paste0(dir_cpp,"rbnC.cpp"))
#  mat2 <- rbnC(N)

## ----eval=FALSE, include=FALSE------------------------------------------------
#  qqplot(mat1[,1],mat2[,1])
#  abline(0,1,col="red")
#  qqplot(mat1[,2],mat2[,2])
#  abline(0,1,col="red")

## ----eval=FALSE, include=FALSE------------------------------------------------
#  library(microbenchmark)
#  ts <- microbenchmark(rbnR=rbnR(N),rbnC=rbnC(N))
#  summary(ts)[,c(1,3,5,6)]

