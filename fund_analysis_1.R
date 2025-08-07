require(zoo)
require(ggplot2)
require(rugarch)
require(MASS)
require(moments)
require(forecast)

source("LoadFundData.R")
p  <- 0.05 


# a) ----------------------------------------------------------------------
T  <- length(r)                        # liczba obserwacji dla pelnej proby
N  <- 1250  # ustalamy probe na podstawie ktorej liczymy statystyki
P <- tail(P, N)
r  <- tail(r,N) # stopy zwrotu
R  <- coredata(r)

par(mfrow=c(2,1), cex = 0.7, bty="l")
plot(P, main="poziom", xlab="lata", ylab="stopy zwrotu")
plot(r, main="stopy zwrotu", xlab="lata", ylab="log. stopy zwrotu")

T  <- length(r)                        # liczba obserwacji dla pelnej proby
N  <- 1250                             # ustalamy probe na podstawie ktorej liczymy statystyki
r  <- tail(r,N)                        # stopy zwrotu
R  <- coredata(r)

mu = sum(R)/N

R0    <- R - mu
M2    <- sum(R0^2)/N
M3    <- sum(R0^3)/N
M4    <- sum(R0^4)/N

sig <- sqrt(M2)       ## zmiennosc
S   <- M3/(sig^3)     ## skosnosc
K   <- M4/(sig^4)     ## kurtoza

# Annualizacja
Nyear <- 250
muA   <- mu*Nyear #roczna stopa zwrotu
sigA  <- sig*sqrt(Nyear) #w skali roku 2,39%

#Standaryzacja
m  <- mean(R)
s  <- sd(R)
R0 <- (R-m)/s

# estymacja liczby stopni swobody
K     <- kurtosis(R0)
v0    <- 4+6/(K-3)         # metoda momentow

dt    <- fitdistr(R0, "t", m = 0, start = list(s=sqrt((v0-2)/v0), df=v0), lower=c(0.001,3))
v1    <- dt$estimate[[2]]


# b) ----------------------------------------------------------------------


#Empiryczna funkcja gestosci (kernel density plot) 
R0        <- (R-mu)/sig
bwdth     <- 0.1

ggplot(data.frame(R0), aes(x = R0)) +
  theme_bw() +
  geom_histogram(binwidth = bwdth, colour = "white", fill = "yellow4", size = 0.1) +
  stat_function(fun = function(x) dnorm(x)*N*bwdth, color = "red", size = 1) +                       # rozklad normalny
  stat_function(fun = function(x) ddist("std",x,shape=5)*N*bwdth, color = "black", size = 1)

# Tabela kwantyli
q              <- seq(0.01, 0.20, 0.01)
QQtable        <- data.frame(quantile(R0,q),qnorm(q))   # rozklad normalny
names(QQtable) <- c("kwantyl empiryczny", "kwantyl teoretyczny")

# QQ plot
q            <- seq(0.001, 0.999, 0.001)
Qemp         <- qnorm(q)    
#Qemp         <- quantile(rnorm(N),q)
Qemp         <- quantile(R0,q)                    # kwantyl empiryczny
#Qteo         <- qnorm(q)                          # kwantyl teoretyczny (rozklad normalny)
v=5
Qteo  <- qt(q,v)*sqrt((v-2)/v)             # rozklad t-Studenta (wariancja to v/v-2)
lim0    <- c(-5,5)                           # zakres na wykresie
par(mfrow=c(1,1), cex = 0.7, bty="l")
plot(Qemp,Qteo, main="QQplot", col="red", xlim = lim0, ylim = lim0,
     xlab="kwantyl empiryczny", ylab="kwantyl teoretyczny") # plots the results
abline(a=0,b=1, lwd=2)

require(MASS)

d0 <- fitdistr(R0, "normal")
d0$loglik #im wiêksza, tym lepsze dopasowanie

d1 <- fitdistr(R0, "t", m = 0, start = list(s=sqrt((v0-2)/v0), df=v0), lower=c(0.001,3))
d1$loglik  
v=d1$estimate[[2]]

# Comparison
(d1$loglik-d0$loglik)/N #na ile t-student lepszy od normalnego



# c) ----------------------------------------------------------------------

GARCHspec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=TRUE),        #specyfikacja modelu, #armaOrder = ¿adnych opóŸnieñ, #include.mean = œrednie stopy, sGARCH = standardowy
                        variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                        fixed.pars=list(shape=5), distribution.model = "std")   #zak³adany rozk³ad
#                        distribution.model = "norm")

GARCHfit <- ugarchfit(data = r, spec = GARCHspec, solver="solnp")   #estymacja modelu (dopasowanie), problem zbie¿noœci --> zmieniamy solver
round(coef(GARCHfit),5)

omega<-coef(GARCHfit)[2]
alpha<-coef(GARCHfit)[3]
beta<-coef(GARCHfit)[4]
eq_sig_2 <- omega/(1-alpha-beta)
eq_sig<-eq_sig_2^(1/2)
sd(r)

# VaR i ES (rozklad t-Studenta o 5 stopniach swobody)
q <- qdist("std", p=p, shape=5)
qf   <- function(x) qdist("std", p=x, shape=5)
GARCHvar   <- fitted(GARCHfit) + sigma(GARCHfit)*q 
GARCHes    <- fitted(GARCHfit) + sigma(GARCHfit)*(1/p * integrate(qf, 0, p)$value)

# wykresy
par(mfrow=c(2,1), cex = 0.7, bty="l")
plot(GARCHfit, which=1) 
plot(merge( r, GARCHvar, GARCHes), plot.type="single", col=c(1,2,3), main=paste(100*p,"% VaR i ES z modelu GARCH(1,1))",  sep=""), ylab="" )
legend("bottomright", c("VaR", "ES"), lty=1, col=2:3)


# d) ----------------------------------------------------------------------
source("MRFzR_FunkcjeBlok1.R")
p<-0.01 #lub 0.05
c(VaRhist(R,p)$VaR, VaRnorm(R,p)$VaR, VaRt(R,p,v1)$VaR, VaRCF(R,p)$VaR, VaREWMA(R,p)$VaR, VaRGARCH(R,p)$VaR)*100
c(VaRhist(R,p)$ES, VaRnorm(R,p)$ES, VaRt(R,p,v1)$ES, VaRCF(R,p)$ES, VaREWMA(R,p)$ES, VaRGARCH(R,p)$ES)*100
#sprawdziæ VaR_EWMA dla 0,05
#sprawdziæ ES_GARCH dla 0,05
# e) ----------------------------------------------------------------------
# Funkcje i wykres VaR dla ró¿nych p  #
#######################################
TabelaP = matrix(NA,100,7)
for(p in seq(0.001,0.1,0.001)){
  TabelaP[p*1000,] <- c(p,VaRhist(R,p)$VaR, VaRnorm(R,p)$VaR, VaRt(R,p,v1)$VaR, VaRCF(R,p)$VaR, VaREWMA(R,p)$VaR, VaRGARCH(R,p)$VaR)}

plot(TabelaP[,1],TabelaP[,2], main="Poziom tolerancji a VaR", 
     xlab="poziom tolerancji (p)", ylab="VaR", 
     ylim=c(min(TabelaP[,2:5]),max(TabelaP[,2:5])), xlim=c(0,0.1),col="black", lwd=2, type="l")
lines(TabelaP[,1],TabelaP[,3],col="red", lwd=2, type="l")
lines(TabelaP[,1],TabelaP[,4],col="green", lwd=2, type="l")
lines(TabelaP[,1],TabelaP[,5],col="orange", lwd=2, type="l")
lines(TabelaP[,1],TabelaP[,6],col="blue", lwd=2, type="l")
lines(TabelaP[,1],TabelaP[,7],col="violet", lwd=2, type="l")
legend("right", c("sym. historyczna","rozklad normalny","rozklad t","Cornish-Fischer", "EWMA", "GARCH"), lty=c(1,1,1,1,1,1), col=c("black", "red","green","orange", "blue", "violet"), bty="n")
abline(v=c(0.05) ,lwd=2, lty=2)