#################################################################################
#################################################################################
#################################################################################
#################################################################################
#confidence interval

##proportion
proportion_ci <- function(y,n,confidence){
  
  wilson_ci <- function(y, n, confidence){
    #calcs
    phat <- y/n
    alpha <- 1-confidence
    z <- abs(qnorm(alpha/2))
    ntilde <- n + z**2
    ytilde <- y + 0.5 * z**2
    ptilde <- ytilde / ntilde
    
    #ci
    ci_l <- ptilde - (sqrt(n) * z * sqrt((phat*(1-phat)) + ((1/(4*n)) * z**2))) / ntilde
    ci_u <- ptilde + (sqrt(n) * z * sqrt((phat*(1-phat)) + ((1/(4*n)) * z**2))) / ntilde
    
    #final
    z <- round(z,2)
    ci_l <- round(ci_l,2)
    ci_u <- round(ci_u,2)
    phat <- round(phat,3)
    ptilde <- round(ptilde,3)
    ci <- data.frame(ci_l, phat, ptilde, ci_u, z, confidence, alpha)
    names(ci) <- c("Lower Bound", "P hat","P tilde", "Upper Bound", "Z score", "Confidence", "Alpha")
    row.names(ci) <- "Wilson"
    return(ci)
  }
  agresti_coull_ci <- function(y, n, confidence){
    #calcs
    phat <- y/n
    alpha <- 1-confidence
    z <- abs(qnorm(alpha/2))
    ntilde <- n + z**2
    if(confidence == 0.95){
      ptilde <- (y+2)/(n+4)
    }else{
      ptilde <- (y + 0.5 * z**2)/(ntilde)
    }
    
    #ci
    ci_l <- ptilde - (z * sqrt((ptilde*(1-ptilde))/ntilde))
    ci_u <- ptilde + (z * sqrt((ptilde*(1-ptilde))/ntilde))
    
    #final
    z <- round(z,2)
    ci_l <- round(ci_l,2)
    ci_u <- round(ci_u,2)
    phat <- round(phat,3)
    ptilde <- round(ptilde,3)
    ntilde <- round(ntilde,2)
    ci <- data.frame(ci_l, phat, ptilde, ci_u, z, confidence, alpha, ntilde)
    names(ci) <- c("Lower Bound", "P hat","P tilde", "Upper Bound", "Z score", "Confidence", "Alpha", "N tilde")
    row.names(ci) <- "Agresti-Coull"
    return(ci)
  }
  
  if(n <= 40){
    wilson_ci(y,n,confidence)
  }else{
    agresti_coull_ci(y,n,confidence)
  }
}


##confidence interval: exponential
exp_ci <- function(tbar, n, confidence){
  
  alpha <- 1-confidence
  
  numerator <- 2 * n * tbar
  bound_l <- round(numerator / qchisq(1-alpha/2,2*n),2)
  bound_c <- round(numerator / qchisq(alpha/2,2*n),2)
  
  ci <- data.frame(bound_l, bound_c)
  names(ci) <- c("Lower", "Upper")
  row.names(ci) <- "Exponential CI"
  return(ci)
}

#################################################################################
exp_ci(40, 15, .9)
proportion_ci(y=sum(data<55), n=length(data), confidence=0.95)
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#prediction interval
##normal pi
###page 37 HO11
normal_pi <- function(ybar, sigma, n, confidence){
  alpha <- 1-confidence
  bound_l <- round(ybar - abs(qt(alpha/2, n-1)) * sigma * sqrt(1+1/n),2)
  bound_u <- round(ybar + abs(qt(alpha/2, n-1)) * sigma * sqrt(1+1/n),2)
  ci <- data.frame(bound_l, bound_u)
  names(ci) <- c("Lower", "Upper")
  return(ci)
}

##exponential pi
###page 38 HO11
###W_n, W_(n+1) are both ~ chi squared with df = 2n
###pivot = W_(n+1)/W_n ~ F with df = 2, 2n
exp_pi <- function(wbar, n, confidence){
  alpha <- 1-confidence
  bound_l <- round(wbar * qf(alpha/2, 2, 2*n),2)
  bound_u <- round(wbar * qf(1-alpha/2, 2, 2*n),2)
  ci <- data.frame(bound_l, bound_u)
  names(ci) <- c("Lower", "Upper")
  return(ci)
}

#################################################################################
normal_pi(ybar=mean(data), sigma=sd(data), n=length(data), confidence=0.9)
exp_pi(40, 15, 0.95)
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#tolerance interval
##using data
tolerance_interval <- function(data, confidence = 0.95, coverage = 0.99){
  alpha = 1-confidence
  ti <- tolerance::nptol.int(data, alpha = alpha, P=coverage)
  ti.df <- data.frame(ti[[3]], ti[[4]])
  names(ti.df) <- c("Lower Bound", "Upper Bound")
  return(ti.df)
}
#################################################################################
tolerance_interval(data, confidence = 0.9, coverage = 0.95)
qnorm(.05/2)
#################################################################################
#################################################################################
#one-sided ti
ybar<-mean(y)
s<-sd(y)
n<-
gamma<-
prob<-
za<-qnorm(gamma)
zb<-qnorm(prob)
a<-1-(za^2)/(2*(n-1))
b<-(zb^2)-(za^2/n)
k<-(zb+sqrt(zb^2-a*b))/a

LB<-ybar-k*s
UB<-ybar+k*s

#################################################################################
#################################################################################
#mean, standard error based on a criteria
standard_error <- function(data, criteria){
  
  n <- length(data)
  
  p <- sum(data < criteria)/n
  se1 <- round(sqrt((p*(1-p))/n),3)
  
  boots <- matrix(0,n,9999)
  boots <- apply(boots, 2, function(x) sample(data, n, replace = TRUE))
  boots <- apply(boots, 2, function(x) sum(x < criteria)/n)
  
  boot_mean <- round(mean(boots),3)
  se2 <- round(sd(boots),3)
  
  se <- data.frame(p, se1, boot_mean, se2)
  names(se) <- c("P data", "SE data", "P Boostrap","SE Bootstrap")
  return(se)
}

#################################################################################
standard_error(data, criteria = 0)
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#sample size

##normal population mu sigma known
samplesize_sd_known <- function(sigma, confidence=0.95, delta){
  alpha <- 1-confidence
  z <- abs(qnorm(alpha/2))
  n <- (z**2 * sigma**2)/ delta**2  
  n <- round(n,0)
  foo <- paste("The sample size is", n, sep=" ")
  return(foo)
}
#################################################################################
##proportion
samplesize_p <- function(p, confidence = 0.95, delta){
  
  if(p >= 0 & p <= 0.1){
    px <- 0.1
  }else{
    if(p >= 0.8 & p <= 1){
      px <- 0.8
    }else{
      px <- p
    }
  }
  alpha <- 1-confidence
  z <- abs(qnorm(alpha/2))
  samplesize <- round((z**2 * px * (1-px))/delta**2,0)
  z <- round(z,2)
  results <- data.frame(samplesize, z, confidence, delta)
  names(results) <- c("Sample Size","Z value", "Confidence", "Delta")
  
  return(results)
}
#################################################################################
#normal-based sample size based on hypothesis test: given alpha, beta, sigma, delta, power
#based on population parameters
sample_size_mu_sigma_known <- function(power, confidence=0.95, sigma, delta, side){
  alpha <- 1-confidence
  beta <- 1-power
  
  if(side=="one-sided"){
    za <- qnorm(alpha)
    zb <- qnorm(beta)
    
    n <- ((sigma*(za+zb))/delta)**2
    n <- ceiling(n)
  }else{
    if(side=="two-sided"){
      za <- qnorm(alpha/2)
      zb <- qnorm(beta/2)
      
      n <- ((sigma*(za+zb))/delta)**2
      n <- ceiling(n)
    }
  }
  return(n)
}
#################################################################################
#n for based on power function, given p or can calculate it
sample_size_hypothesis_test_p <- function(y=NA,p0,p1=NA,delta=NA,power,alpha,type){
  
  if(is.na(p1)){
    p1 <- y/n
  }else{
    p1 <- p1
  }
  
  beta <- 1-power
  zb <- abs(qnorm(beta))
  delta <- ifelse(is.na(delta),abs(p1-p0),delta)
  
  if(!type %in% c("greater","less")){
    #if two sided
    za <- abs(qnorm(alpha/2))
  }else{
    #if greater or less than
    za <- abs(qnorm(alpha))
  }
  
  n <- (
    ((za * sqrt(p0*(1-p0)))+(zb*sqrt(p1*(1-p1))))
    /delta
  )**2
  
  n <- ceiling(n)
  return(n)
}
#################################################################################
#mu sigma  
samplesize(sigma=1.453, confidence = 0.95, delta=0.2934)
#proportion
samplesize_p(p=.5,confidence=.95, delta=.1)
#handout12 based on power function
samplesize_power(power=0.9, confidence=0.95, sigma=40, delta=15,side="one-sided")
#handout12 based on population mu, sigma
sample_size_mu_sigma_known(power, confidence=0.95, sigma, delta, side)
#for n based on unknown population but normal sample mu, sigma
power.t.test()
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#hypothesis tests
#normal w/ mean, sigma known
hypothesis_testing_mu_sd_known <- function(alpha,n,mu0,alt_mu,sigma,type){
  
  TS <- (alt_mu-mu0)/(sigma/sqrt(n))
  
  result1 <- "Reject null hypothesis"
  result2 <- "Fail to reject null hypothesis"
  
  if(type=="greater"){
    za <- qnorm(1-alpha)
    CV <- mu0 + za*(sigma/sqrt(n))
    power <- 1 - pnorm(za + (mu0-alt_mu)/(sigma/sqrt(n)))
    beta <- 1-power
    pvalue <- 1-pnorm(TS)
    result <- ifelse(alt_mu > CV,result1,result2)
    rowname <- "H1: mu > mu0"
  }else{
    if(type=="less"){
      za <- qnorm(alpha)
      CV <- mu0 + za*(sigma/sqrt(n))
      power <- pnorm(za + (mu0-alt_mu)/(sigma/sqrt(n)))
      beta <- 1-power
      pvalue <- pnorm(TS)
      result <- ifelse(alt_mu < CV,result1,result2)
      rowname <- "H1: mu < mu0"
    }else{
      za_lower <- qnorm(alpha/2)
      za_upper <- qnorm(1-alpha/2)
      CV <- mu0 + za_upper * (sigma/sqrt(n))
      result <- ifelse(abs(TS) > za_upper,result1,result2)
      power <- pnorm(za_lower + (mu0-alt_mu)/(sigma/sqrt(n)))+
        (1-pnorm(za_upper + (mu0-alt_mu)/(sigma/sqrt(n))))
      beta <- 1-power
      pvalue <- 2 * (1-pnorm(abs(TS)))
      rowname <- "H1: mu != mu0"
    }
  }
  
  #same for all
  ptest <- ifelse(pvalue < alpha,result1,result2)
  
  #make pretty
  TS <- round(TS,2)
  CV <- round(CV,2)
  power <- round(power,2)
  beta <- round(beta,2)
  pvalue <- round(pvalue,3)
  za <- round(za,2)
  
  #df
  out <- data.frame(n, mu0, alt_mu, TS, CV, result,power,beta, alpha, pvalue, ptest, za)
  names(out) <- c("n", "Mu Null", "Alt Mu", "Test Stat", "Critical Val", "Result"
                  ,"Power","Beta","Alpha","P value",  "Result: P value", "Z")
  row.names(out) <- rowname
  
  return(out)
}

#################################################################################
#hypothesis test for sd of normal population
power_sd <- function(alpha,null_sd,alt_sd,n, type){
  df <- n-1
  TS <- ((n-1)*alt_sd**2)/null_sd**2
  
  result1 <- "Reject null hypothesis"
  result2 <- "Fail to reject null hypothesis"
  
  if(type=="greater"){
    rowname <- "One-sided, H1: sd > sd0"
    confidence <- 1-alpha
    CV <- qchisq(confidence, df)
    TS_CV_result <- ifelse(TS >= CV,result1, result2)
    power <- 1 - pchisq((null_sd/alt_sd)**2 * qchisq(confidence, df),df)
    beta <- 1-power
    p_value <- 1-pchisq(TS,n-1)
  }else{
    if(type=="less"){
      rowname <- "One-sided, H1: sd < sd0"
      confidence <- 1-alpha
      CV <- qchisq(alpha, df)
      TS_CV_result <- ifelse(TS <= CV,result1, result2)
      power <- pchisq((null_sd/alt_sd)**2 * qchisq(alpha, df),df)
      beta <- 1-power
      p_value <- pchisq(TS,n-1)
      
    }else{
      rowname <- "Two-sided, H1: sd != sd0"
      confidence <- 1-alpha/2
      CV_l <- qchisq(confidence, df)
      CV_u <- qchisq(alpha/2, df)
      TS_CV_result <- ifelse(TS <= CV_l | TS >= CV_u, result1, result2 )
      power <- pchisq((null_sd/alt_sd)**2 * qchisq(alpha/2, df),df) +
        (1 - pchisq((null_sd/alt_sd)**2 * qchisq(confidence, df),df))
      beta <- 1-power
      p_value <- 2 * min(pchisq(TS,n-1), 1-pchisq(TS,n-1))
    }
  }
  
  #pvalue test same for all types
  pvalue_result <- ifelse(p_value<=alpha, result1, result2)
  
  #make pretty
  TS <- round(TS,2)
  CV <- round(CV,2)
  power <- round(power,2)
  beta <- round(beta,2)
  p_value <- round(p_value,3)
  
  #final
  if(type %in% c("greater","less")){
    out <- data.frame(TS,CV,TS_CV_result,power,beta,alpha,p_value,pvalue_result,df)
    names(out) <- c("Test Stat","Critical Val","Result 1", "Power", "Beta",
                    "Alpha","P value", "Result 2", "df")
    row.names(out) <- rowname
    return(out)    
  }else{
    CV_l <- round(CV_l,2)
    CV_u <- round(CV_u,2)
    out <- data.frame(TS, CV_l,CV_u,TS_CV_result,power,beta,alpha,p_value,pvalue_result,df)
    names(out) <- c("Test Stat","Critical Val Lower", "Critical Val Upper","Result 1",
                    "Power","Beta","Alpha","P value", "Result 2", "df")
    row.names(out) <- rowname
    return(out)    
  }
}
#################################################################################
#proportion hypothesis test
power_p <- function(y=NA, n, p0, p1=NA, alpha, type){
  sample_size_test <- min(n*p0, n*(1-p0))
  phat <- ifelse(is.na(p1),y/n,p1)
  confidence = 1-alpha
  result1 <- "Reject null hypothesis"
  result2 <- "Fail to reject null hypothesis"
  
  #small sample sizes
  if(sample_size_test < 5){
    method <- "Bin(n,p), n small"
    TS <- phat
    
    if(type=="greater"){
      CV <- qbinom(confidence,n,p0)
      TS_CV_results <- ifelse(TS>=CV,result1,result2)
      pvalue <- 1-pbinom(y-1,n,p0)
      power <- 1-pbinom(qbinom(confidence,n,po)-1,n,p1)
    }else{
      if(type=="less"){
        CV <- qbinom(alpha,n,p0)
        TS_CV_results <- ifelse(TS<=CV,result1,result2)
        pvalue <- pbinom(y,n,p0)
        power <- pbinom(qbinom(alpha,n,po),n,p1)
      }else{
        CV1 <- qbinom(alpha/2, n, p0)
        CV2 <- qbinom(1-alpha/2, n, p0)
        TS_CV_results(y <= CV1 | y > CV2, result1, result2)
        pvalue <- 2 * min(pbinom(y,n,po), 1-pbinom(y-1,n,p0))
        power <- pbinom(qbinom(alpha/2,n,p0),n,p1) + (1-pbinom(qbinom(1-alpha/2,n,p0)-1,n,p1))
      }
    }
    
  }else{
    #if n large
    method <- "Asymptotic, n large"
    TS <- (phat-p0)/sqrt((p0*(1-p0))/n)
    
    if(type=="greater"){
      CV <- qnorm(1-alpha)
      TS_CV_result <- ifelse(TS >= CV,result1,result2)
      pvalue <- 1-pnorm(TS)
      power <- 1-pnorm(CV * sqrt((p0*(1-p0))/(phat*(1-phat))) +
                         sqrt(n)*((p0-phat)/sqrt((phat*(1-phat)))))
    }else{if(type=="less"){
      CV <- qnorm(alpha)
      TS_CV_result <- ifelse(TS <= CV,result1,result2)
      pvalue <- pnorm(TS)
      power <- pnorm(CV * sqrt((p0*(1-p0))/(phat*(1-phat))) +
                       sqrt(n)*((p0-phat)/sqrt((phat*(1-phat)))))
    }else{
      #not equal to
      CV <- qnorm(1-alpha/2)
      TS_CV_result <- ifelse(abs(TS) >= CV,result1,result2)
      pvalue <- 2*(1-pnorm(abs(TS)))
      power <- pnorm(-CV * sqrt((p0*(1-po))/(phat*(1-phat))) +
                       sqrt(n)*(p0-p1)/sqrt(phat(1-phat))
      )
      + 1-pnorm(CV * sqrt((p0*(1-po))/(phat*(1-phat)))+
                  sqrt(n)*(p0-p1)/sqrt(phat(1-phat))
      )
    }
    }
  }
  
  #pvalue test same for all tests
  pvalue_results <- ifelse(pvalue <= alpha, result1,result2)
  
  #round
  power <- round(power,2)
  beta <- 1-power
  TS <- round(TS,2)
  CV <- round(CV,2)
  pvalue <- round(pvalue,3)
  
  #combind
  out <- data.frame(n,phat,p0,TS,CV,TS_CV_result,alpha,pvalue,pvalue_results,power,beta)
  names(out) <- c("n","P1","P0","Test Stat","Critical Val","Result 1"
                  ,"Alpha","P value","Result 2", "Power", "Beta")
  row.names(out) <- method
  return(out)
}
#################################################################################
hypothesis_testing_mu_sd_known(alpha=.05,n=10,mu0=10000,alt_mu=10500,sigma=1000,type="greater")
power_sd(alpha=0.05, null_sd=23.8, alt_sd=47.9, n=20, type="greater")
power_p(y=NA, n=50, p0=0.8, p1=0.9, alpha=0.05, type="greater")
#for sample mu, sigma (pop mu, sigma unknown) use power.t.test
power.t.test()
#################################################################################
#################################################################################
#################################################################################
#################################################################################
