#Problem: pretpostavimo da je Y iz normalne raspodele sa parametrima mu i 1/tau

# statistike uzorka
# velicina uzorka
n <- 30
# srednja vrednost uzorka
ybar <- 15
# varijansa uzorka
s2 <- 3

#inicijalizacija nizova
mu <- tau <- rep(NA, 11000)
# pocetni deo koji odbacujemo
T <- 1000
# incijalizacija
tau[1] <- 1
for(i in 2:11000) {
  #racunanje uslovne raspodele f(mu|tau)
  mu[i] <- rnorm(n = 1, mean = ybar, sd = sqrt(1 / (n * tau[i - 1])))
  #f(tau|mu)
  tau[i] <- rgamma(n = 1, shape = n / 2, scale = 2 / ((n - 1) * s2 + n * (mu[i] - ybar)^2))
  if(i%%500==0){
    plot(mu, tau)
    readline()
  }
}
# odbacivanje pocetaka
mu <- mu[-(1:T)]
tau <- tau[-(1:T)]

mean(mu)
mean(tau)

hist(mu)
hist(tau)

plot(mu, tau)