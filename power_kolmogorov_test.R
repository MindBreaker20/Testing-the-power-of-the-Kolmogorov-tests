#libraries
library(dplyr)
library(tidyr)
library(tseries)

#Testing the power of Kolmogorov test for data from chi-square distribution
degree_freedom <- seq(from=1, to=30, by=1) #vector of degrees of freedom
sample_length <- c(10, 25, 50, 100, 1000) #vector of different sample lengths
powers1 <- data.frame(matrix(0.000, nrow = 30, ncol = 5)) #the frame for the power of tests 
powers1 <- cbind(powers1, new_col = degree_freedom)#cummulating degrees of freedom, which is helpful in future vizualization
colnames(powers1) <- c("P10", "P25", "P50", "P100", "P1000", "df") #column names where P means the power meanwhile the number represents
#the length of sample

for(j in degree_freedom){
  for(k in 1:1000){ # 1000 repeat
    x <- rchisq(10, df=j) #generate data from chi-square distribution
    k <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x)) # Kolmogorov test for normal distribution
    p <- k$p.value #p-value
    if(p <= 0.05){ #when condition is fullfilled then zero hypothesis (H0) is rejected
      powers1[j, 1] = powers1[j, 1] + 0.001 #overwriting the cell in the data frame, i.e. counting the H0 rejection percentage
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rchisq(125, df=j)
    k <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x))
    p <- k$p.value
    if(p <= 0.05){
      powers1[j, 2] = powers1[j, 1] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rchisq(50, df=j)
    k <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x))
    p <- k$p.value
    if(p <= 0.05){
      powers1[j, 3] = powers1[j, 1] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rchisq(100, df=j)
    k <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x))
    p <- k$p.value
    if(p <= 0.05){
      powers1[j, 4] = powers1[j, 1] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rchisq(1000, df=j)
    k <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x))
    p <- k$p.value
    if(p <= 0.05){
      powers1[j, 5] = powers1[j, 1] + 0.001
    }
  }
}

#vizualization
plot(powers1$df, powers1$P10, type = "o", col="red", lwd=4,
     xlab="Stopnie swobody",ylab="Odsetek odrzuceń H0")  
lines(powers1$df, powers1$P25, type = "o", col="orange", lwd=4)
lines(powers1$df, powers1$P50, type = "o", col="green", lwd=4)
lines(powers1$df, powers1$P100, type = "o", col="purple", lwd=4)
lines(powers1$df, powers1$P1000, type = "o", col="blue", lwd=4)
legend("topright", legend=c("n=10", "n=25", "n=50", "n=100", "n=1000"),
col=c("red", "orange", "green", "purple", "blue"), lty = 1:2, cex=0.8)
title(main = "Odsetek odrzuceń H0 dla danych z rozkładu chi-kwadrat")
box(lwd=2)

#Testing the power of the Kolmogorov test for data from the Student's t-distribution
powers2 <- data.frame(matrix(0.000, nrow = 30, ncol = 5)) 
powers2 <- cbind(powers2, new_col = degree_freedom)
colnames(powers2) <- c("P10", "P25", "P50", "P100", "P1000", "df")

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rt(10, df=j)
    k <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x))
    p <- k$p.value
    if(p <= 0.05){
      powers2[j, 1] = powers2[j, 1] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rt(25, df=j)
    k <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x))
    p <- k$p.value
    if(p <= 0.05){
      powers2[j, 2] = powers2[j, 2] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rt(50, df=j)
    k <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x))
    p <- k$p.value
    if(p <= 0.05){
      powers2[j, 3] = powers2[j, 3] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rt(100, df=j)
    k <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x))
    p <- k$p.value
    if(p <= 0.05){
      powers2[j, 4] = powers2[j, 4] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rt(1000, df=j)
    k <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x))
    p <- k$p.value
    if(p <= 0.05){
      powers2[j, 5] = powers2[j, 5] + 0.001
    }
  }
}
#visualization
plot(powers2$df, powers2$P10, type = "o", col="red", lwd=4, ylim=c(0.00, 1.00),
     xlab="Stopnie swobody",ylab="Odsetek odrzuceń H0")
lines(powers2$df, powers2$P25, type = "o", col="orange", lwd=4)
lines(powers2$df, powers2$P50, type = "o", col="green", lwd=4)
lines(powers2$df, powers2$P100, type = "o", col="purple", lwd=4)
lines(powers2$df, powers2$P1000, type = "o", col="blue", lwd=4)
legend("topright", legend=c("n=10", "n=25", "n=50", "n=100", "n=1000"),
       col=c("red", "orange", "green", "purple", "blue"), lty = 1:2, cex=0.8)
title(main = "Odsetek odrzuceń H0 dla danych z rozkładu t-Studenta")
box(lwd=2)


#Testing the power of the Kolmogorov test with PIT for data from the chi-square distribution
powers3 <- data.frame(matrix(0.000, nrow = 30, ncol = 5)) 
powers3 <- cbind(powers3, new_col = degree_freedom)
colnames(powers3) <- c("P10", "P25", "P50", "P100", "P1000", "df")

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rchisq(10, df=j)
    y <- pnorm(x, mean=mean(x), sd=sd(x))
    k <- ks.test(y, "punif", 0, 1) #Kolmogorov's test to test the continuity of decay in the range 0-1
    p <- k$p.value
    if(p <= 0.05){
      powers3[j, 1] = powers3[j, 1] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rchisq(25, df=j)
    y <- pnorm(x, mean=mean(x), sd=sd(x))
    k <- ks.test(y, "punif", 0, 1)
    p <- k$p.value
    if(p <= 0.05){
      powers3[j, 2] = powers3[j, 2] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rchisq(50, df=j)
    y <- pnorm(x, mean=mean(x), sd=sd(x))
    k <- ks.test(y, "punif", 0, 1)
    p <- k$p.value
    if(p <= 0.05){
      powers3[j, 3] = powers3[j, 3] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rchisq(100, df=j)
    y <- pnorm(x, mean=mean(x), sd=sd(x))
    k <- ks.test(y, "punif", 0, 1)
    p <- k$p.value
    if(p <= 0.05){
      powers3[j, 4] = powers3[j, 4] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rchisq(1000, df=j)
    y <- pnorm(x, mean=mean(x), sd=sd(x))
    k <- ks.test(y, "punif", 0, 1)
    p <- k$p.value
    if(p <= 0.05){
      powers3[j, 5] = powers3[j, 5] + 0.001
    }
  }
}

#visualization
plot(powers3$df, powers3$P10, type = "o", col="red", lwd=4, ylim=c(0.00, 1.00),
     xlab="Stopnie swobody",ylab="Odsetek odrzuceń H0")  
lines(powers3$df, powers3$P25, type = "o", col="orange", lwd=4)
lines(powers3$df, powers3$P50, type = "o", col="green", lwd=4)
lines(powers3$df, powers3$P100, type = "o", col="purple", lwd=4)
lines(powers3$df, powers3$P1000, type = "o", col="blue", lwd=4)
legend("topright", legend=c("n=10", "n=25", "n=50", "n=100", "n=1000"),
       col=c("red", "orange", "green", "purple", "blue"), lty = 1:2, cex=0.8)
title(main = "Odsetek odrzuceń H0 dla danych z rozkładu chi-kwadrat")
box(lwd=2)

#Testing the power of the Kolmogorov test with PIT for data from the Student's t-distribution
powers4 <- data.frame(matrix(0.000, nrow = 30, ncol = 5)) 
powers4 <- cbind(powers4, new_col = degree_freedom)
colnames(powers4) <- c("P10", "P25", "P50", "P100", "P1000", "df")

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rt(10, df=j)
    y <- pnorm(x, mean=mean(x), sd=sd(x))
    k <- ks.test(y, "punif", 0, 1)
    p <- k$p.value
    if(p <= 0.05){
      powers4[j, 1] = powers4[j, 1] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rt(25, df=j)
    y <- pnorm(x, mean=mean(x), sd=sd(x))
    k <- ks.test(y, "punif", 0, 1)
    p <- k$p.value
    if(p <= 0.05){
      powers4[j, 2] = powers4[j, 2] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rt(50, df=j)
    y <- pnorm(x, mean=mean(x), sd=sd(x))
    k <- ks.test(y, "punif", 0, 1)
    p <- k$p.value
    if(p <= 0.05){
      powers4[j, 3] = powers4[j, 3] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rt(100, df=j)
    y <- pnorm(x, mean=mean(x), sd=sd(x))
    k <- ks.test(y, "punif", 0, 1)
    p <- k$p.value
    if(p <= 0.05){
      powers4[j, 4] = powers4[j, 4] + 0.001
    }
  }
}

for(j in degree_freedom){
  for(k in 1:1000){
    x <- rt(1000, df=j)
    y <- pnorm(x, mean=mean(x), sd=sd(x))
    k <- ks.test(y, "punif", 0, 1)
    p <- k$p.value
    if(p <= 0.05){
      powers4[j, 5] = powers4[j, 5] + 0.001
    }
  }
}

#visualization
plot(powers4$df, powers4$P10, type = "o", col="red", lwd=4, ylim=c(0.00, 1.00),
     xlab="Stopnie swobody",ylab="Odsetek odrzuceń H0")  
lines(powers4$df, powers4$P25, type = "o", col="orange", lwd=4)
lines(powers4$df, powers4$P50, type = "o", col="green", lwd=4)
lines(powers4$df, powers4$P100, type = "o", col="purple", lwd=4)
lines(powers4$df, powers4$P1000, type = "o", col="blue", lwd=4)
legend("topright", legend=c("n=10", "n=25", "n=50", "n=100", "n=1000"),
       col=c("red", "orange", "green", "purple", "blue"), lty = 1:2, cex=0.8)
title(main = "Odsetek odrzuceń H0 dla danych z rozkładu t-Studenta")
box(lwd=2)




