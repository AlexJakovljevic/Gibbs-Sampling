ro <- 3/5
teta <- rnorm(10000, 0, ro)
T <- 3000
teta1 <- teta2 <- rep(NA, 11000)
teta1[1] <- 1
teta2[1] <- 1 # incijalizacija
for(i in 2:11000){
  teta1[i] <- rnorm(1, ro*teta2[i-1], 1-ro*ro)
  teta2[i] <- rnorm(1, ro*teta1[i], 1-ro*ro)
}
teta1 <- teta1[-(1:T)]   # odbacivanje pocetka
teta2 <- teta2[-(1:T)] # odbacivanje pocetka

mean(teta1)
mean(teta2)

hist(teta1)
hist(teta2)

plot(teta1, teta2)