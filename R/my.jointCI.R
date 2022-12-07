#' @title  my.jointCI
#' @description give the joint confidence interval of multivariate data
#' @param data multivariate data
#' @param type If type="T2",calculate the T2 joint CI.If type="bonferroni",calculate bonferroni joint CI.
#' @param alpha level
#' @return the joint confidence interval of multivariate data
#' @examples
#' \dontrun{
#' x <- rnorm(100,10,4)
#' y <- rnorm(100,20,8)
#' data <- matrix(data = c(x,y),100,2)
#' my.jointCI(data,type = "T2",alpha=0.05)
#' }
#' @export
my.jointCI <- function(data,type=c("T2","bonferroni"),alpha){
  n <- nrow(data)
  p <- ncol(data)
  x.mean <- colMeans(data)
  S <- cov(data)
  s <- diag(S)
  f <- qf(1-alpha,p,n-p)

  if(type == "T2"){
    l <- x.mean - sqrt(p*(n-1)*f/(n-p))*sqrt(s/n)
    r <- x.mean + sqrt(p*(n-1)*f/(n-p))*sqrt(s/n)
    for (i in 1:p) {
      cat(l[i],"< x",i,"<",r[i],"\n")
    }
  }else if(type == "bonferroni"){
    t <- qt(1-alpha/(2*p),n-1)
    l <- x.mean - t*sqrt(s/n)
    r <- x.mean + t*sqrt(s/n)
    for (i in 1:p) {
      cat(l[i],"< x",i,"<",r[i],"\n")
    }
  }else{
    print("type error!Please choose T2 or bonferroni!")
    return(0)
  }
  result <- list(l,r)
  return(result)
}

