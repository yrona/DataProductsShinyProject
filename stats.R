confidence.interval <- function(mu0,sd0,nsample,alpha, two.tailed = TRUE) {
  
  if (two.tailed) {
    alpha.effective = alpha/2
  } else {
    alpha.effective = alpha
  }
  epsilon <- qnorm(1-alpha.effective)*sd0/sqrt(nsample)
  
  my.result <- mu0 + c(-1,1) * epsilon
  
  my.result
}
  