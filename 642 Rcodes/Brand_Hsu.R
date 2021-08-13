install.packages("lsmeans")
install.packages("ggplot2")
install.packages("gmodels")

library(lsmeans)
library(ggplot2)
library("gmodels")

### Author: Jeffrey R. Fetzer
### Purpose: Conducts Hsu's procedure for a specified model

mcb <- function(model, best = "smallest", alpha = .05)
{
    library(mvtnorm)

    if ("aov" %in% class(model) | "lm" %in% class(model)) {
        y <- model$model[,1]
        trt <- model$model[,2]
        dfMSE <- df.residual(model)
        MSE <- deviance(model)/dfMSE
    }

    data <- subset(data.frame(y, trt), is.na(y) == FALSE)
    means <- tapply(data[, 1], data[, 2], mean)
    ni <- tapply(data[, 1], data[, 2], length)
    N <- sum(ni)

    ntr <- length(ni)
    dcv <- qmvt(p=1-alpha, tail = "lower.tail", df = N-ntr, corr =
                matrix(rep(.5,(ntr-1)^2),ntr-1) + diag(ntr-1) * .5)$quantile

    if (best == "smallest") {
        min <- min(means)
        nmin <- min(means[means!=min(means)])
        m <- replace(rep(min, ntr), which(means==min, arr.ind=TRUE), nmin)
        k <- m + dcv*sqrt(MSE)*sqrt(2/ni[1])
        b <- (means < k)
        output <- data.frame(round(means, 3), round(m, 3), round(k, 3), b)
        names(output) <- c("means", "m", "k", "Best")
    }

    if (best == "largest") {
        max <- max(means)
        nmax <- max(means[means!=max(means)])
        m <- replace(rep(max, ntr), which(means==max, arr.ind=TRUE), nmax)
        k <- m - dcv*sqrt(MSE)*sqrt(2/ni[1])
        b <- (means > k)
        output <- data.frame(round(means, 3), round(m, 3), round(k, 3), b)
        names(output) <- c("means", "M", "k", "Best")
    }

    cat("Results for Hsu's procedure when 'best' is", best, "\n\n")

    print(output)
}

y = c(22, 20, 25, 17,26, 22, 27, 21,16, 20, 14, 18,
      20, 25, 26, 21,28, 29, 23, 24,22, 15, 19, 16 )

S1 = rep("S1",4) 
S2 = rep("S2",4) 
B1 = rep("B1",4) 
B2 = rep("B2",4)  
R1 = rep("R1",4)
R2 = rep("R2",4)
TYPE = c(S1,S2,B1,B2,R1,R2)
BRAND = as.factor(TYPE)

Brandmodel = lm(y ~ BRAND)

mcb(Brandmodel)





