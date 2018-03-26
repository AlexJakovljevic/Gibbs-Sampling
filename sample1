# sustinske statistike uzorka
n <- 30
ybar <- 15
s2 <- 3

# uzorak iz zajednicke aposteriorne (mu, tau)
mu <- tau <- rep(NA, 11000)
T <- 1000    # pocetni deo koji odbacujemo
tau[1] <- 1  # incijalizacija
for(i in 2:11000) {
  mu[i] <- rnorm(n = 1, mean = ybar, sd = sqrt(1 / (n * tau[i - 1])))
  tau[i] <- rgamma(n = 1, shape = n / 2, scale = 2 / ((n - 1) * s2 + n * (mu[i] - ybar)^2))
}
mu <- mu[-(1:T)]   # odbacivanje pocetka
tau <- tau[-(1:T)] # odbacivanje pocetka

hist(mu)
hist(tau)
