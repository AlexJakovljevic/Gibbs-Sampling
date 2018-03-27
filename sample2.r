#inicijalizacija parametara uslovnih raspodela
ro <- 3/5
teta <- rnorm(10000, 0, ro)
#T - koliko prvih elemenata odbacujemo
T <- 3000
#inic niza
teta1 <- teta2 <- rep(NA, 11000)
# incijalizacija pocetka (ne preterano bitna zbog prirode algoritma)
teta1[1] <- 1
teta2[1] <- 1
for(i in 2:11000){
  #uslovna f(teta1|teta2)
  teta1[i] <- rnorm(1, ro*teta2[i-1], 1-ro*ro)
  #uslovna f(teta2|teta1)
  teta2[i] <- rnorm(1, ro*teta1[i], 1-ro*ro)
  #stampanje napretka
  if(i%%1000==0 || i<25){
    plot(teta1, teta2)
    readline()
  }
}
# odbacivanje pocetka
teta1 <- teta1[-(1:T)]
teta2 <- teta2[-(1:T)]

mean(teta1)
mean(teta2)

hist(teta1)
hist(teta2)

plot(teta1, teta2)