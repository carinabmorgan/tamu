#Independence testing
  #H0: variables are independent, Ha: variables dependent
  # used for categorical count data
data<-read.csv()

  #create contingency table
    rowvar<- #row variable
    colvar<- #column variable
    table<-table(rowvar, colvar)
    
    #marginal distribution
      rowSums(table)
      colSums(table)
    
    #frequency distribution
      prop.table(table)
      
#Chi Square Test of Independence
  #use for large samples
  
  #check that Chi square test is appropriate
      chisq.test(table)$expected
    
#Fishers Exact Test for Independence
  #use for small samples, when n*p < 5 for any cell
      fisher.test(table)
      
#CMH test - use when you have multiple contingency tables due to a 3rd categorical variable
    #H0: relative proportions of 1 variable are independent of the otehr within the repeats
      
    #create multilevel table
      table2<-xtabs(count ~ predictors, data=data)
    
    #first check that the CMH test is appropriate by testing homogeneity of odds ratios using Breslow Day
      #H0: odds ratios are all equivalent, Ha: not all odds ratios are equal
      #if we reject H0, then we may use the CMH test 
    
      library(DescTools)
      BreslowDayTest(table2, correct=FALSE)
      
    #run CMH test
      #H0: common odds ratio = 1, Ha: common odds ratio is not 1
       mantelhaen.test(table2)
      
      