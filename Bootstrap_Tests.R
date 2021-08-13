#BOOTSTRAP TESTS

#median
library(boot)
y<-c()
B<-10000
boot_med<-function(y,indices) {
  y<-y[indices]
  median(y)
}
boot_med_out<-boot(y, boot_med, B)
boot.ci(boot_med_out, type=c('norm','perc','bca'))

plot(boot_med_out)

#mean
library(boot)
y<-c()
B<-10000
boot_mean<-function(y,indices) {
  y<-y[indices]
  mean(y)
}
boot_mean_out<-boot(y, boot_mean, B)
boot.ci(boot_mean_out, type=c('norm','perc','bca'))

plot(boot_mean_out)

