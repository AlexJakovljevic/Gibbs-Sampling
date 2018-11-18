#inicijalizacija parametara uslovnih raspodela
ro <- 0.6
#S - koliko prvih elemenata odbacujemo
S <- 3000
#inic niza
teta1 <- teta2 <- rep(NA, 10000)
# incijalizacija pocetka (ne preterano bitna zbog prirode algoritma)
teta2[1] <- teta1[1] <- 1;
for(i in 2:10000){
  #uslovna f(teta1|teta2)
  teta1[i] <- rnorm(n = 1, mean = ro * teta2[i - 1], sd = 1 - ro * ro)
  #uslovna f(teta2|teta1)
  teta2[i] <- rnorm(n = 1, mean = ro * teta1[i], sd = 1 - ro * ro)
  #stampanje napretka
}
# odbacivanje pocetka
teta1 <- teta1[-(1:S)]
teta2 <- teta2[-(1:S)]
mean(teta1)
mean(teta2)
hist(teta1)
hist(teta2)
plot(teta1, teta2)



# statistike uzorka
# velicina uzorka
n <- 30
# uzoracka sredina
xs <- 15
# uzoracka disperzija
s2 <- 3
#inicijalizacija nizova
mu <- tau <- rep(NA, 10000)
# pocetni deo koji odbacujemo
S <- 3000
# incijalizacija
tau[1] <- 1
for(i in 2:10000) {
  #racunanje uslovne raspodele f(mu|tau)
  mu[i] <- rnorm(n = 1, mean = xs, sd = sqrt(1 / (n * tau[i - 1])))
  #f(tau|mu)
  tau[i] <- rgamma(n = 1, shape = n / 2, scale = 2 / ((n - 1) * s2 + n * (mu[i] - xs)^2))
}
# odbacivanje pocetaka
mu <- mu[-(1:S)]
tau <- tau[-(1:S)]
mean(mu)
mean(tau)
hist(mu)
hist(tau)
plot(mu, tau)
