#TESTS FOR 2 WAY CONTINGENCY TABLES
library(DescTools)
library(lawstat)

#Breslow-Day & CMH Tests for common odds ratios (must be 2x2xz , z>1)
    #Breslow Day: checks that there is not a significant difference between OR's (H0)
      y<-c()  #add all data values of matrix
      dim(y)<-c(2,2) #add dimensions of matrix
      
      BreslowDayTest(y, OR=NA, correct=FALSE)

      #CMH test: checks to see of the common odds ratio = 1, only use if you do NOT reject B-D test
      cmh.test(y)
  
#linear trend test for 2x2 tables (would use chi sq for larger contingency table)
linear.trend <- function(freq,NI,NJ,x,y) {
  #  Katari, Contingency Table Analysis
  # PARAMETERS:
  # freq: vector of the frequencies, given by rows
  # NI: number of rows
  # NJ: number of columns
  # x: vector of row scores
  # y: vector of column scores
  # RETURNS:
  # r: Pearsonís sample correlation
  # M2: test statistic
  # p.value: two-sided p-value of the asymptotic M2-test
  table <-matrix(freq, nrow = NI, ncol = NJ, byrow = TRUE);
  rowmarg<-addmargins(table)[,NJ+1][1:NI];
  colmarg<-addmargins(table)[NI+1,][1:NJ];
  n<-addmargins(table)[NI+1,NJ+1];
  xmean<-sum(rowmarg*x)/n;
  ymean<-sum(colmarg*y)/n;
  xsq<-sqrt(sum(rowmarg*(x-xmean)^2));
  ysq<-sqrt(sum(colmarg*(y-ymean)^2));
  r<-sum((x-xmean)%*%table%*%(y-ymean))/(xsq*ysq);
  M2=(n-1)*r^2;
  p.value <- 1-pchisq(M2,1);
  return(list(r=r,M2=M2, p.value=p.value)) 
  }
  
freq<-c()
  x<-c()
  y<-c()
  linear.trend(freq=freq,NI=3,NJ=3,x,y)
  #H0: x & y independent
  
#Finding odds ratios (2x2 table)
    #create your 2x2 table
    mat<-matrix(c(), nrow=, ncol=, byrow= )
    rownames(mat)<-c() #add row names
    colnames(mat)<-c() #add column names
    
    #calculate OR with 95% CI
    OddsRatio(mat, conf.level=0.95)
  
    
#McNemars matched pairs test: test for symmetry in 2x2 table/matrix, aka if the probability of cell [i,j]= probability of cell [j,i] for matched pairs
    #NOTE: want cells to sum to 25 ideally
    #H0: no difference in proportions between paired data
    sum(mat)
    mcnemar.test(mat, correct=TRUE) #use correct=TRUE if any cell has a count less than 5
    
    #if we do not have the correct sum, we need an exact test
    library(rcompanion)
    nominalSymmetryTest(mat)
  
      
#sensitivity & specificity
    library(caret)
    y<-c() #response values
    model<-glm(y ~ x, family='binomial')   #let x be the predictors
    actual_values<-y
    pred_values<-predict(model, type='response')
    
    #confusion matrix
    conf_mat<-confusionMatrix(actual_values, pred_values, cutoff=0.5) #generally use .5 cutoff
    
    #sensitivity: proportion of people with disease who test positive
   sensitivity(conf_mat)
    #specificity: proportion of people without disease who test negative
    specificity(conf_mat)
    
    
#hypothesis testing for a population proportion
    #sample size calculation for proportion inference
    
    #test that proportions (probs of success) is the same in multiple groups (H0) versus different between groups (Ha)
    probs<-c() #must have a probability for every group in your matrix, so a 2x2 matrix would have 2 values in this vector
    prop.test(mat, p=probs, alternative='two.sided', conf.level=0.95)
    
