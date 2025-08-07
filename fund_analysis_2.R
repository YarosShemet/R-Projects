# Wartoœæ VAR dla 140-dniowego okresu -------------------------------------
source("MRFzR_FunkcjeBlok2.R")
source("LoadFundData.R")
require(zoo)
require(rugarch)
require(car)
require(moments)
require(knitr)
require(ggplot2)


T  <- length(r)                          # liczba obserwacji
p  <- 0.05                               # poziom tolerancji VaR
H  <- 140                                # horyzont

N     <- 1250                            # ustalamy probe na podstawie ktorej liczymy VaR
r     <- tail(r,N)                       # dane
R     <- coredata(r)

m   <- mean(R)
s   <- sd(R)
S   = skewness(R)
K   = kurtosis(R)-3

w_length  <- 21
q         <- 0.1

############################################################################################
# podpunkt 1
# Metoda square root of time dla rozk³adu normalnego     #
############################################################################################
mH  <- m*H
sH  <- s*sqrt(H)
VaRH_N    <- mH + sH*qnorm(p)            # VaR dla stopy zwrotu dla horyontu H

# wykres na density plot
x = seq(-4*sH,4*sH,0.01*sH)
df1 <- as.data.frame(cbind(x,dnorm(x,m,s))); names(df1) <- c("r","y"); df1$horyzont = "h=1" 
dfH <- as.data.frame(cbind(x,dnorm(x,mH,sH))); names(dfH) <- c("r","y"); dfH$horyzont = "h=H" 
df  = rbind(df1,dfH)
############################################################################################
# podpunkt 2
# Rozszerzenie Cornisha-Fischera    #
############################################################################################
PsiP    <- qnorm(p)
VaR1_CF  <- m + s*(PsiP + (PsiP^2-1)/6*S + (PsiP^3-3*PsiP)/24*(K) - (2*PsiP^3-5*PsiP)/36*(S^2) ) 

mH  <- m*H
sH  <- s*sqrt(H)
SH  <- S/sqrt(H)
KH  <- K/H

VaRH_CF        <- mH + sH*(PsiP + (PsiP^2-1)/6*SH + (PsiP^3-3*PsiP)/24*(KH) - (2*PsiP^3-5*PsiP)/36*(SH^2) ) 
############################################################################################
# podpunkt 3
# Symulacje Monte Carlo dla modelu GARCH                 #
############################################################################################
# liczba losowan w symulacjach Monte Carlo
M      <- 10000      
draws  <- matrix(NA,H,M)
# uzupelniamy kolejne kolumny macierzy
for(i in 1:M){
  draws[,i] <- rnorm(H, mean=m, sd=s)
}
draws0      <- colSums(draws)               # skumulowane zwroty
draws0      <- sort(draws0)                 # uporzadkowane obserwacje
# Specyfikacja modelu: GARCH(1,1) z r. normalnym
garch.spec <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)),
                         mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model = "norm")
#estymacja modelu
garch.fit  <- ugarchfit(data=R, spec=garch.spec)

# symulacje MC z modelu GARCH
garch.sim   <- ugarchsim(garch.fit, n.sim = H, n.start = 0, m.sim = M, startMethod = "sample")
draws       <- garch.sim@simulation$seriesSim

# Obliczenia VaR
draws2      <- colSums(draws) 
draws2      <- sort(draws2)                 # uporzadkowane obserwacje
M0          <- floor(M*p)                   # obserwacja dla p-tego kwintyla                   
VaRH_GARCH  <- draws2[M0]                   # porownaj z: quantile(draws0,p)

# wykres
df0 = as.data.frame(draws0); names(df0) = "draws"; df0$rozklad = "Normalny"
df2 = as.data.frame(draws2); names(df2) = "draws"; df2$rozklad = "GARCH"
df  = rbind(df0,df2)

ggplot(df, aes(x=draws, color=rozklad)) +
  geom_density(size=1.2)+
  scale_color_grey() + 
  theme_classic() +
  labs(title="Rozklad stop zwrotu dla dalszych horyzontow", y="", x="st. zwrotu", caption=" ") +
  geom_vline(xintercept=c(VaRH_N,VaRH_GARCH), linetype="dashed", size=1.2, color=c("gray","black"))  
############################################################################################
# podpunkt 4
# Wartosc zagrozona w sytuacji skrajnej: stressed VaR, S-VaR   #
############################################################################################
# 3.0. standardowy VaR/ES  
N    <- 1250               # liczba wykorzystanych obserwacji
R    <- tail(R,N)

# por. Temat 5
m0   <- mean(R)*H
s0   <- sd(R)*sqrt(H)

# 3.1. zaburzamy wartosc sredniej stopy zwrotu
MAmean    <- rollapply(r, width=w_length, mean, by=1, align="right")
m1        <- quantile(MAmean,q, names=FALSE)*H
s1        <- s0

# 3.2. zaburzamy wartosc dla odchylenia standardowego
MAsd      <- rollapply(r, width=w_length, sd, by=1, align="right")
m2        <- m0
s2        <- quantile(MAsd,1-q, names=FALSE)*sqrt(H)

# 3.3. polaczenie
m3        <- m1
s3        <- s2
S_VaR3    <- m3 + qnorm(p)*s3            # stressed VaR 

kable(100*c(VaRH_N,VaRH_CF,VaRH_GARCH,S_VaR3), col.names = "porownanie SVaR")
kable(1000*exp(c(VaRH_N,VaRH_CF,VaRH_GARCH,S_VaR3)), col.names = "Wartosc 1000PLN")

############################################################################################
# podpunkt -
# Dane do h=1:H   #
############################################################################################
# square root of time
TaKonc = matrix(NA,140,5)
Ta = matrix(NA,140,2)
for(i in seq(1,140,1)){
  mH  <- m*i
  sH  <- s*sqrt(i)
  VaRH_N    <- mH + sH*qnorm(p)
  wart = 1000*exp(VaRH_N)
  Ta[i,] <- c(i, wart)
}
TaKonc[,1] = Ta[,1]
TaKonc[,2] = Ta[,2]

# Rozszerzenie CF
Ta = matrix(NA,140,2)
for(i in seq(1,140,1)){
  mH  <- m*i
  sH  <- s*sqrt(i)
  SH  <- S/sqrt(i)
  KH  <- K/i
  VaRH_CF        <- mH + sH*(PsiP + (PsiP^2-1)/6*SH + (PsiP^3-3*PsiP)/24*(KH) - (2*PsiP^3-5*PsiP)/36*(SH^2) )
  wart = 1000*exp(VaRH_CF)
  Ta[i,] <- c(i, wart)
}
TaKonc[,3] = Ta[,2]

# MC dla GARCH
Ta = matrix(NA,140,2)
for(i in seq(1,140,1)){
  M      <- 10000     
  draws  <- matrix(NA,i,M)
  for(ii in 1:M){
    draws[,ii] <- rnorm(i, mean=m, sd=s)
  }
  draws0      <- colSums(draws)               # skumulowane zwroty
  draws0      <- sort(draws0)  
  garch.spec <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)),
                           mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
                           distribution.model = "norm")
  #estymacja modelu
  garch.fit  <- ugarchfit(data=R, spec=garch.spec)
  
  # symulacje MC z modelu GARCH
  garch.sim   <- ugarchsim(garch.fit, n.sim = i, n.start = 0, m.sim = M, startMethod = "sample")
  draws       <- garch.sim@simulation$seriesSim
  
  # Obliczenia VaR
  draws2      <- colSums(draws) 
  draws2      <- sort(draws2)                 # uporzadkowane obserwacje
  M0          <- floor(M*p)                   # obserwacja dla p-tego kwintyla                   
  VaRH_GARCH  <- draws2[M0]                   # 
  
  wart = 1000*exp(VaRH_GARCH)
  Ta[i,] <- c(i, wart)
}
TaKonc[,4] = Ta[,2]

# S-VaR
Ta = matrix(NA,140,2)
for(i in seq(1,140,1)){
  m0   <- mean(R)*i
  s0   <- sd(R)*sqrt(i)
  
  MAmean    <- rollapply(r, width=w_length, mean, by=1, align="right")
  m1        <- quantile(MAmean,q, names=FALSE)*i
  s1        <- s0
  
  MAsd      <- rollapply(r, width=w_length, sd, by=1, align="right")
  m2        <- m0
  s2        <- quantile(MAsd,1-q, names=FALSE)*sqrt(i)
  
  m3        <- m1
  s3        <- s2
  S_VaR3    <- m3 + qnorm(p)*s3            
  
  wart = 1000*exp(S_VaR3)
  Ta[i,] <- c(i, wart)
}
TaKonc[,5] = Ta[,2]
############################################################################################
# podpunkt -
# Wykres   #
############################################################################################
plot(TaKonc[,1], TaKonc[,2], main="Zmiana wartosci inwestycji 1000 PLN", 
     xlab="Horyzont", ylab="Wartosc w PLN", 
     ylim=c(min(TaKonc[,2:5]),max(1000)), xlim=c(1, 140), col="black", lwd=2, type="l")
lines(TaKonc[,1],TaKonc[,3],col="red", lwd=2, type="l")
lines(TaKonc[,1],TaKonc[,4],col="green", lwd=2, type="l")
lines(TaKonc[,1],TaKonc[,5],col="orange", lwd=2, type="l")
legend("bottomleft", c("Square root of time","Rozszerzenie CF","MC z GARCH","S-VaR"), lty=c(1,1,1,1), col=c("black", "red","green","orange"), bty="n")

# Analiza zmian ceny portfelu ---------------------------------------------

##############################################################
#                Investor Obligacji                          #
##############################################################

##############################################################
#           Standardowy scenariusz hipotetyczny              #
##############################################################

# Sklad portfela (Investor Obligacji - min 70% portfelu jest inwestowany w d³u¿ne papiery wartoœciowe, emitowane przez Skarb Pañstwa)
A1 = 0           # akcje krajowe
A2 = 0           # akcje zagraniczne
A3 = 0.7         # obligacje skarbowe, 5-letnie; Duracja portfelu - 5,04 - przyjmujemy jako 5 letnie. 
A4 = 0.3         # obligacje korporacyjne, roczne 
A5 = 0           # surowce
A = c(A1,A2,A3,A4,A5)

# Etap 1: analiza wrazliwosci wzgledem czynnikow ryzyka
# Super important! To s¹ reakcje portfelu na 1-procentow¹ zmianê czynnika

RF1 = sum(c(0, 0, 0, 0, 1)*A)    # procentowa reakcja na wzrost cen surowcow o 1%
RF2 = sum(c(1, 1, 0, 0, 0)*A)    # procentowa reackja na 1% wzrost indeksow gieldowych (w kraju i zagranica), czyli wzrost cen akcji 
RF3 = sum(c(0, 0,-5,-1, 0)*A)    # procentowa reakcja na wzrost krzywej dochodowosci na calej dlugosci o 100 pb.
#mozliwoœæ zmiany z -1 na -3 ze wzglêdu na okres obligacji korporacyjnych 
#mo¿liwoœæ zmiany z -5 do -3 ze wzglêdu na okres obligacje skarbowych 

# Etap 2: analizy scenariuszowe 
S1 = abs(RF1*50); #wzrost cen surowców o 50%, ale brak zmian cen metali szlachetnych
S2 = -abs(RF2*20); #spadek cen akcji o 20%
S3 = -abs(RF3*2); #przesuniêciu krzywej dochodowoœci na ca³ej d³ugoœci o 2 pkt. proc. do góry 

ScHipA <- S1+S2+S3;
ScHipA                       # hipotetyczny scenariusz skrajny  

##############################################################
# 2B. Unikatowy scenariusz hipotetyczny                      #
##############################################################

# Przyjmijmy, ze w scenariuszu wybuchnie wojna w Ukrainie:
# 1. wzrost cen surowców o 50%
# 2. ucieczka kapitalu z rynkow wschodzacych, w tym PL: deprecjacja PLN o 20%
# 3. odp³yw kaptalu z rynku obligacji: spadek krzywej doch. o 200 pb na ca³ej d³ugoœci 

# Wplyw na portfel
ScHipB <- RF1*(50) + RF2*(-20) + RF3*(2)

ScHipB
# Backtesting -------------------------------------------------------------
par(mfrow=c(2,1), cex = 0.7, bty="l")
plot(P, main="Poziom", xlab="", ylab="")
plot(r, main="stopy zwrotu", xlab="", ylab="")

T   <- length(r)                  # liczba obserwacji
p   <- 0.05                # poziom tolerancji VaR
N   <- 1250                       # ustalamy dlugosc proby (5lat)
r   <- tail(r,N)                  # dane
backN    <- 250                   # dlugosc danych do backtestu
w_length <- 1000                  # szerokosc okna estymacji

r_backN <- tail(r, backN)
plot(r_backN, main="stopy zwrotu", xlab="", ylab="") 
#ostatnie 250 obserwacji wykorzystywane dla backtestungu
### ostatnie obserwacje naszego szeregu cechuje bardzo wysoka, niezwyczajna dla obligacji zmiennoœæ, która wynika z inflacji

#Symulacja historyczna
varHS <- rollapply(r, width=w_length,
                   function(w){quantile(w,p)},
                   by=1, align="right") #daty na koñcu okresu
esHS  <- rollapply(r, width=w_length,
                   function(w){mean(w[w < quantile(w,p)])},
                   by=1, align="right")


varHS <- lag(varHS, -1)           # przesuwamy indeksy aby uwzglednic, ze VaR(t+1) = quantile(t)
rrHS    <- r[index(varHS)]          # zrealizowane zwroty, rr - realized returns
etaHS  <- tail(rrHS<=varHS, backN)  # szereg przekroczen (VaR violations)

# empiryczny udzial przekroczen (powinien byc rowny poziomowi tolerancji p)
nHS  <- sum(etaHS); nHS      # l. przekroczen
piHS <- mean(etaHS); piHS    # udzial przekroczen


### bardzo du¿a liczba przekroczeñ i wysoki udzia³ przekroczeñ, wiêkszy od 0.05 --> metoda jest nieistotna (zob. metodê œwiatel, tzn. Jakie liczby przekroczeñ daj¹ podstawy do odrzucenia modelu dla N = 250 i ro = 5%)
#metoda œwiate³ 
#dla n=250, p=1% zielona od 0 do 4, zolta od 5 do 9, czerwona 10+
#dla n=250, p=5% zielona od 0-16, zolta 17-24, czerwona 25+
#dla n=100, p=5% zielona od 0-7, zolta 8-12, czerwona 13+
#dla n=100, p=10% zielona od 0 do 13, zolta 14-20, czerwona 21+
var = varHS
eta = etaHS
pi  = piHS
n1  = nHS
#empEx <- rollapply(eta, width=250, mean, align="right")
# Test Kupca - pokrycie - zgodnoœæ udzia³u przekroczeñ z poziomem tolerancji
n  <- length(eta); n
n1 <- sum(eta);    n1
n0 <- n - n1;      n0
pi <- n1/n;        pi
kupiec_temp_HS <- VaRTest(alpha=p, actual=rrHS, VaR=var)
paste0("Statystyka testu Kupca: ", round(kupiec_temp_HS$uc.LRstat,3),"; p-value:", round(kupiec_temp_HS$uc.LRp,3))
### p-value<0.05 --> odrzucamy hipotezê zerow¹ o zgodnoœci udzia³u przekroczeñ
# Test Ch1
eta1 <- coredata(eta[-length(eta)]) #1/0 jest przekroczenie/nie ma
eta0 <- coredata(eta[-1])
n00 = sum(!eta1 & !eta0) # no exceedance after no exceedance
n01 = sum(!eta1 &  eta0)  # exceedance after no exceedance
n10 = sum( eta1 & !eta0)  # no exceedance after exceedance
n11 = sum( eta1 &  eta0)   # exceedance after exceedance

#n0  = n00 + n10
#n1  = n01 + n11

pi0HS = n01 / (n00+n01) # prawdopodobienstwo przekroczenia po braku przekroczenia
pi1HS = n11 / (n10+n11) # prawdopodobienstwo przekroczenia po przekroczeniu
pihs  = (n01+n11) / (n00+n01+n10+n11)
c(pi0HS, pi1HS, pihs)
christ1_temp_HS <- VaRTest(alpha=p, actual=coredata(rrHS), VaR=coredata(var))
paste0("Statystyka testu Ch1: ", round(christ1_temp_HS$cc.LRstat,3),"; p-value:", round(christ1_temp_HS$cc.LRp,3))
# Test Christoffersena 2 (niezale¿nego pokrycia) -  niezale¿noœæ + pokrycie 
christ2_temp_HS <- VaRTest(alpha=p, actual=coredata(rrHS), VaR=coredata(var))
paste0("Statystyka testu Ch2: ", round(christ2_temp_HS$cc.LRstat,3),"; p-value:", round(christ2_temp_HS$cc.LRp,3))
#p-value<0.05 -->
#odrzucamy hipotezê zerow¹ o tym, ¿e przekroczenie VaR w okresie t nie zale¿y od przekroczenia VaR w okresie t-1 i jest równe poziomowi tolerancji
#Test McNeila-Freya - czy odleg³oœci przekroczeñ od ES istotnie ró¿ni siê od 0
Neil_Frey_temp_HS <- ESTest(alpha=p, actual=coredata(rrHS), ES=coredata(esHS), VaR=coredata(varHS))
paste0("p-value:", round(Neil_Frey_temp_HS$p.value,3))

#Rozszerzenie Cornischa-Fischera
library(moments)
# rolowana srednia, odchylenie standardowe i momenty
MA_mean  <- rollapply(r, width=w_length, mean, by=1, align="right")
MA_std   <- rollapply(r, width=w_length, sd,   by=1, align="right")
MA_S   <- rollapply(r, width=w_length, skewness, by=1, align="right")
MA_K   <- rollapply(r, width=w_length, kurtosis, by=1, align="right")
MA_PsiP <- rollapply(r, width=w_length, function(w){qnorm(p)}, by=1, align = "right")
varCF  <- MA_mean + MA_std*(MA_PsiP + (MA_PsiP^2-1)/6*MA_S + (MA_PsiP^3-3*MA_PsiP)/24*(MA_K-3) - (2*MA_PsiP^3-5*MA_PsiP)/36*(MA_S^2)) 
varCF <- lag(varCF, -1)
rrCF    <- r[index(varCF)]          # zrealizowane zwroty, rr - realized returns
etaCF  <- tail(rrCF<=varCF, backN)  # szereg przekroczen (VaR violations)

# empiryczny udzial przekroczen (powinien byc rowny poziomowi tolerancji p)
nCF  <- sum(etaCF); nCF    # l. przekroczen
piCF <- mean(etaCF); piCF 

var = varCF
eta = etaCF
pi  = piCF
n1  = nCF

n  <- length(eta); n
n1 <- sum(eta);    n1
n0 <- n - n1;      n0
pi <- n1/n;        pi
kupiec_temp_CF <- VaRTest(alpha=p, actual=rrCF, VaR=var)
paste0("Statystyka testu Kupca: ", round(kupiec_temp_CF$uc.LRstat,3),"; p-value:", round(kupiec_temp_CF$uc.LRp,3))
eta1 <- coredata(eta[-length(eta)]) #1/0 jest przekroczenie/nie ma
eta0 <- coredata(eta[-1])
n00 = sum(!eta1 & !eta0) # no exceedance after no exceedance
n01 = sum(!eta1 &  eta0)  # exceedance after no exceedance
n10 = sum( eta1 & !eta0)  # no exceedance after exceedance
n11 = sum( eta1 &  eta0)   # exceedance after exceedance

#n0  = n00 + n10
#n1  = n01 + n11

pi0CF = n01 / (n00+n01) # prawdopodobienstwo przekroczenia po braku przekroczenia
pi1CF = n11 / (n10+n11) # prawdopodobienstwo przekroczenia po przekroczeniu
picf  = (n01+n11) / (n00+n01+n10+n11)
c(pi0CF, pi1CF, picf)

christ1_temp_HS <- VaRTest(alpha=p, actual=coredata(rrHS), VaR=coredata(var))
paste0("Statystyka testu Ch1: ", round(christ1_temp_HS$cc.LRstat,3),"; p-value:", round(christ1_temp_HS$cc.LRp,3))

christ2_temp_CF <- VaRTest(alpha=p, actual=coredata(rrCF), VaR=coredata(var))
paste0("Statystyka testu Ch2: ", round(christ2_temp_CF$cc.LRstat,3),"; p-value:", round(christ2_temp_CF$cc.LRp,3))
#brak testu Neila-Freya dla CF, poniewa¿ nie da siê obliczyæ ES 

#EWMA
### Zdecydowaliœmy na wybór EWMA, bo poprzednim razem (dla koñcowego okresu w lutym) wypad³a ta metoda lepiej
lambda = 0.94
EWMAspec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                       fixed.pars=list(alpha1=1-lambda, omega=0), distribution.model = "norm")
#fixed.pars=list(alpha1=1-lambda, omega=0,shape=5), distribution.model = "std")

## funkcja kwantylowa
qf     <- function(x) qdist("norm", p=x)
#qf     <- function(x) qdist("std", p=x, shape=5)
eqt <-  integrate(qf, 0, p)$value

varesEWMA <- rollapply(r, width=w_length,
                       function(w) {
                         frc <- ugarchforecast(EWMAspec,data=w, n.ahead=1)
                         var <- quantile(frc, p)
                         sigma <- sigma(frc)
                         es <- sigma*eqt/p
                         return(c(var, es))
                       },
                       by=1, align="right")
varesEWMA <- lag(varesEWMA, -1)
varEWMA <- varesEWMA[,1]
esEWMA  <- varesEWMA[,2]
rrEWMA    <- r[index(varEWMA)] 
etaEWMA <- (rrEWMA<varEWMA)

# empiryczny udzial przekroczen (powinien byc rowny poziomowi tolerancji p)
nEWMA  <- sum(etaEWMA); nEWMA    # l. przekroczen
piEWMA <- mean(etaEWMA); piEWMA


var = varEWMA
eta = etaEWMA
pi  = piEWMA
n1  = nEWMA

n  <- length(eta); n
n1 <- sum(eta);    n1
n0 <- n - n1;      n0
pi <- n1/n;        pi
kupiec_temp_EWMA <- VaRTest(alpha=p, actual=rrEWMA, VaR=var)
paste0("Statystyka testu Kupca: ", round(kupiec_temp_EWMA$uc.LRstat,3),"; p-value:", round(kupiec_temp_EWMA$uc.LRp,3))
eta1 <- coredata(eta[-length(eta)]) #1/0 jest przekroczenie/nie ma
eta0 <- coredata(eta[-1])
n00 = sum(!eta1 & !eta0) # no exceedance after no exceedance
n01 = sum(!eta1 &  eta0)  # exceedance after no exceedance
n10 = sum( eta1 & !eta0)  # no exceedance after exceedance
n11 = sum( eta1 &  eta0)   # exceedance after exceedance

#n0  = n00 + n10
#n1  = n01 + n11

pi0EWMA = n01 / (n00+n01) # prawdopodobienstwo przekroczenia po braku przekroczenia
pi1EWMA = n11 / (n10+n11) # prawdopodobienstwo przekroczenia po przekroczeniu
piewma  = (n01+n11) / (n00+n01+n10+n11)
c(pi0EWMA, pi1EWMA, piewma)

christ1_temp_EWMA <- VaRTest(alpha=p, actual=coredata(rrHS), VaR=coredata(var))
paste0("Statystyka testu Ch1: ", round(christ1_temp_EWMA$cc.LRstat,3),"; p-value:", round(christ1_temp_EWMA$cc.LRp,3))

christ2_temp_EWMA <- VaRTest(alpha=p, actual=coredata(rrEWMA), VaR=coredata(var))
paste0("Statystyka testu Ch2: ", round(christ2_temp_EWMA$cc.LRstat,3),"; p-value:", round(christ2_temp_EWMA$cc.LRp,3))
Neil_Frey_temp_EWMA <- ESTest(alpha=p, actual=coredata(rrEWMA), ES=coredata(esEWMA), VaR=coredata(varEWMA))
paste0("p-value:", round(Neil_Frey_temp_EWMA$p.value,3))


summary_HS <- c(piHS, round(kupiec_temp_HS$uc.LRp,3), round(christ2_temp_HS$cc.LRp, 3), round(Neil_Frey_temp_HS$p.value,3))
summary_CF <- c(piCF, round(kupiec_temp_CF$uc.LRp,3), round(christ2_temp_CF$cc.LRp, 3), NA)
summary_EWMA <- c(piEWMA, round(kupiec_temp_EWMA$uc.LRp,3), round(christ2_temp_EWMA$cc.LRp, 3), round(Neil_Frey_temp_EWMA$p.value,3))

summary <- cbind(summary_HS, summary_CF, summary_EWMA)
colnames(summary) <- c("HS", "CF", "EWMA")
rownames(summary) <- c("p0", "pi1", "pi", "test Kupca", "test Christoffersena 2", "test McNeila i Freya dla ES")
(summary)
#patrz¹c na tabelê, wybieramy najlepsz¹ metodê
ESVaRplot(alpha=p, actual=rrEWMA, VaR=varEWMA, ES=esEWMA); title(main="Model EWMA")

VaRplot(alpha=p, actual=tail(rrCF,backN), VaR=tail(varCF,backN))
title(main="Rozszerzenie Cornischa-Fischera: przekroczenia VaR")

VaRplot(alpha=p, actual=tail(rrHS,backN), VaR=tail(varHS,backN))
title(main="Symulacja historyczna: przekroczenia VaR")

### zale¿y od okresu - dla wyników koniec-luty, HS i CF radz¹ sobie lepiej (przyczyna - wojna i jeszcze bardziej podwy¿szona zmiennoœæ)







