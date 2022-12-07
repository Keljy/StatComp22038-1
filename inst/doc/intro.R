## ----eval=FALSE---------------------------------------------------------------
#  my.jointCI <- function(data,type=c("T2","bonferroni"),alpha=0.05){
#    n <- nrow(data)
#    p <- ncol(data)
#    x.mean <- colMeans(data)
#    S <- cov(data)
#    s <- diag(S)
#    f <- qf(1-alpha,p,n-p)
#  
#    if(type == "T2"){
#      l <- x.mean - sqrt(p*(n-1)*f/(n-p))*sqrt(s/n)
#      r <- x.mean + sqrt(p*(n-1)*f/(n-p))*sqrt(s/n)
#      for (i in 1:p) {
#        cat(l[i],"< x",i,"<",r[i],"\n")
#      }
#    }else if(type == "bonferroni"){
#      t <- qt(1-alpha/(2*p),n-1)
#      l <- x.mean - t*sqrt(s/n)
#      r <- x.mean + t*sqrt(s/n)
#      for (i in 1:p) {
#        cat(l[i],"< x",i,"<",r[i],"\n")
#      }
#    }else{
#      print("type error!Please choose T2 or bonferroni!")
#      return(0)
#    }
#    result <- list(l,r)
#    return(result)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  my.ppca <- function(q,S){
#    Uq <- eigen(S)$vectors[,1:q]
#    eigenval <- eigen(S)$values
#    Dq <- diag(eigenval[1:q])
#    d <- ncol(S)
#    sigma2_ML <- sum(eigenval[-(1:q)])/(d-q)
#    I <- diag(rep(1,q))
#    matrix <- Dq-sigma2_ML*I
#    lamda <- solve(eigen(matrix)$vectors)%*%matrix%*%(eigen(matrix)$vectors)
#    lamda_sqrt <- diag(sqrt(diag(lamda)))
#    Sigma_sqrt <- (eigen(matrix)$vectors)%*%lamda_sqrt%*%solve(eigen(matrix)$vectors)
#    W_ML <- Uq%*%Sigma_sqrt
#    for (i in 1:q) {
#      W_ML[,i] <- W_ML[,i]/sqrt(sum(W_ML[,i]*W_ML[,i]))
#    }
#    result <- list(sigma2_ML=sigma2_ML,W_ML=W_ML)
#    return(result)
#  }

