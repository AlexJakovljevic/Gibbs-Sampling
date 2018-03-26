#Introduction to Mathematical Statistics sixth edition

alpha <- 10

x <- y <- rep(NA, 6000)

T <- 3000

x[1] <- 0
y[1] <- 0.01

for(i in 2:6000) {
  y[i] <- rgamma(n = 1, shape=alpha + x[i-1], rate=0.5)
  #x[i] <- rpois(n = 1, lambda=y[i])
  lambda=y[i]
  #print(lambda)
  x[i] <- round(rnorm(1,mean=lambda,sd=sqrt(lambda)))
}

#x <- x[-(1:T)]   # odbacivanje pocetka
#y <- y[-(1:T)] # odbacivanje pocetka

mean(x)
mean(y)

hist(x)
hist(y)