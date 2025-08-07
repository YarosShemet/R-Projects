### ZALICZENIE - ekonometria praktyczna ###

### Zast¹p liczbê w nawiasie poni¿ej w³asnym numerem indeksu
set.seed(108887)

### Wygeneruj zbiór danych
N <- 200
sigma <- 1
epsilon <- rnorm(n = N, mean = 0, sd = sigma)
stopa <- rnorm(n = N, mean = 3, sd = 0.5)
beta <- matrix(c(10, 
                 0.8 + runif(1, min = 0, max = 0.1), 
                 -0.2 + runif(1, min = -0.15, max = 0.15)))
kredyt_0 <- rnorm(n = 1, mean = 50, sd = 3)

generate_y_kredyt <- function(kredyt_0, beta, stopa, epsilon){
  N <- length(epsilon)
  kredyt <- c(kredyt_0, rep(NA, N))
  for (ii in 1:N) {
    kredyt[ii+1] = beta[1] + beta[2]*kredyt[ii] + beta[3]*stopa[ii] + epsilon[ii]
  }
  return(kredyt)
}
kredyt <- generate_y_kredyt(kredyt_0, beta, stopa, epsilon)

makro <- data.frame(kredyt = kredyt[2:(N+1)], L_kredyt = kredyt[1:N], stopa)
rm(sigma, epsilon, beta, kredyt)

#Kredyt w zale¿noœci od stopy - model ADL
ADL_kredyt <- lm(kredyt ~ L_kredyt + stopa, data = makro)
summary(ADL_kredyt)
ADL_kredyt_tabela <- summary(ADL_kredyt)
beta_hat <- ADL_kredyt$coefficients
sigma_hat <- ADL_kredyt_tabela$sigma

# rozwi¹zanie -------------------------------------------------------------
#funkcja reakcji na impuls dla i-tego okresu to (b1)^i * b2, bo mamy model ADL
#dla pierwszego okresu reakcja na impuls wyniesie b2
#sk³adnik losowy wynosi 0
efekt_krancowy_0 <- beta_hat[3]
kredyt_stopa_0 <- as.matrix(generate_y_kredyt(kredyt_0, beta_hat, rep(0, 10), rep(0,10))[2:11])
kredyt_stopa_1 <- as.matrix(generate_y_kredyt(kredyt_0, beta_hat, rep(1, 10), rep(0,10))[2:11])
kredyt_roznica <- kredyt_stopa_1-kredyt_stopa_0 #skumulowana funkcja reakcji na impuls dla i-tego okresu
#parametryczny bootstrap - zak³adamy, ¿e rozk³ad sk³adnika losowego - normalny
R <- 1000
kredyt_roznica_R <- matrix(NA, ncol = 10, nrow = R)
for (rr in 1:R) {
  epsilon_rr <- rnorm(n = N+10, mean = 0, sd = sigma_hat)
  kredyt_rr <- generate_y_kredyt(kredyt_0, beta_hat, makro$stopa, epsilon_rr)
  y_rr <- kredyt_rr[2:(N+1)]
  L_y_rr <- kredyt_rr[1:N]
  beta_rr <- lm(y_rr ~ L_y_rr + makro$stopa)$coefficients
  y_rr <- generate_y_kredyt(kredyt_rr[N], beta_rr, rep(1,10), epsilon_rr[(N+1):(N+10)])
  y2_rr <- generate_y_kredyt(kredyt_rr[N], beta_rr, rep(0,10), epsilon_rr[(N+1):(N+10)])
  kredyt_roznica_R[rr,] <- (y_rr - y2_rr)[2:11]
}
confint <- apply(kredyt_roznica_R, 2, FUN=quantile, probs=c(0.05,0.95))
confint <- t(confint)
confint <- as.data.frame(confint)
colnames(confint) <- c("dolna_granica_90", "gorna_granica_90")
confint$prognoza_punktowa <- kredyt_roznica
confint$szerokosc <- confint$gorna_granica_90 - confint$dolna_granica_90
(confint)
sgh_zielony <- rgb(13, 85, 72, 160, maxColorValue = 255)
dziedzina <- 1:10
plot(dziedzina,
     kredyt_roznica, 
     type = "l", lty = "dashed",
     xlab = "kwarta³", ylab = "skumulowana reakcja na impuls kredytu", ylim = c(-3, 0))
points(kredyt_roznica, pch=16, col="red")
lines(1:10, confint$dolna_granica_90, col = sgh_zielony, lty = "dashed")
lines(1:10, confint$gorna_granica_90, col = sgh_zielony, lty = "dashed")


# dygresje-------------------------------------------------------------------------
#dowód, ¿e IRF poprawna
kredyt_krancowe <- matrix(NA, nrow=10, ncol=1)
kredyt_krancowe[1] <- beta_hat[3]
for (ii in 2:10) {
  kredyt_krancowe[ii] <- kredyt_roznica[ii]-kredyt_roznica[ii-1]
}
#kredyt_krancowe obliczony jest ze skumulowanej IRF, a jeœli ze wzoru (b1)^i * b2
kredyt_krancowe_def <- matrix(NA, nrow=10, ncol=1)
for (ii in 1:10) {
  kredyt_krancowe_def[ii] <- beta_hat[2]^(ii-1)*beta_hat[3]
}
cbind(kredyt_krancowe, kredyt_krancowe_def) #funkcja IRF zosta³a obliczona poprawnie


#czy oszacowania punktowe d¹¿¹ do mno¿nika d³ugookresowego?
mnoznik_dlugookresowy <- beta_hat[3]/(1-beta_hat[2])
#prognoza na 100 kwarta³ów do przodu
efekt_krancowy_0 <- beta_hat[3]
kredyt_stopa_0 <- as.matrix(generate_y_kredyt(kredyt_0, beta_hat, rep(0, 100), rep(0,100))[2:101])
kredyt_stopa_1 <- as.matrix(generate_y_kredyt(kredyt_0, beta_hat, rep(1, 100), rep(0,100))[2:101])
kredyt_roznica <- kredyt_stopa_1-kredyt_stopa_0 #skumulowana funkcja reakcji na impuls dla i-tego okresu
#parametryczny bootstrap
R <- 1000
kredyt_roznica_R <- matrix(NA, ncol = 100, nrow = R)
for (rr in 1:R) {
  epsilon_rr <- rnorm(n = N+100, mean = 0, sd = sigma_hat)
  kredyt_rr <- generate_y_kredyt(kredyt_0, beta_hat, makro$stopa, epsilon_rr)
  y_rr <- kredyt_rr[2:(N+1)]
  L_y_rr <- kredyt_rr[1:N]
  beta_rr <- lm(y_rr ~ L_y_rr + makro$stopa)$coefficients
  y_rr <- generate_y_kredyt(kredyt_rr[N], beta_rr, rep(1,100), epsilon_rr[(N+1):(N+100)])
  y2_rr <- generate_y_kredyt(kredyt_rr[N], beta_rr, rep(0,100), epsilon_rr[(N+1):(N+100)])
  kredyt_roznica_R[rr,] <- (y_rr - y2_rr)[2:101]
}
confint <- apply(kredyt_roznica_R, 2, FUN=quantile, probs=c(0.05,0.95))
confint <- t(confint)
confint <- as.data.frame(confint)
colnames(confint) <- c("dolna_granica_90", "gorna_granica_90")
confint$prognoza_punktowa <- kredyt_roznica
confint$szerokosc <- confint$gorna_granica_90 - confint$dolna_granica_90
(confint)
sgh_zielony <- rgb(13, 85, 72, 160, maxColorValue = 255)
dziedzina <- 1:100
plot(dziedzina,
     kredyt_roznica, 
     type = "l", lty = "dashed",
     xlab = "kwarta³", ylab = "skumulowana funkcja reakcji na impuls", ylim = c(-5, 1))
lines(1:100, confint$dolna_granica_90, col = sgh_zielony, lty = "dashed")
lines(1:100, confint$gorna_granica_90, col = sgh_zielony, lty = "dashed")
cbind(mnoznik_dlugookresowy, kredyt_roznica[100])
