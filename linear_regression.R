library(readxl)
dane_P <- read_excel("Dane_ceny_surowcow.xlsx")

dane_P$proportion_Colombo_Mombasa <- dane_P$KTEA_COLOMBO/dane_P$KTEA_MOMBASA
reg1 <- lm(KCOFFEE_ARABIC ~ proportion_Colombo_Mombasa+KSUGAR_EU+KCOCOA, dane_P)
summary(reg1)


library(lmtest)
library(stats)
bg8 <- bgtest(reg1, order=8)
acf(reg1$residuals, lag.max = 8)
coeftest(bg8)

library(gap)
?chow.test
plot(dane_P[[1]], dane_P$KCOFFEE_ARABIC)
y1 <- dane_P$KCOFFEE_ARABIC[1:29] #wektor zmiennej niezaleznej do zmiany 
y2 <- dane_P$KCOFFEE_ARABIC[30:60] #wektor zmiennej niezaleznej po zmianie
x1 <- cbind(dane_P$proportion_Colombo_Mombasa[1:29], dane_P$KSUGAR_EU[1:29], dane_P$KCOCOA[1:29]) #macierz zmiennych zaleznych do zmiany
x2 <- cbind(dane_P$proportion_Colombo_Mombasa[30:60], dane_P$KSUGAR_EU[30:60], dane_P$KCOCOA[30:60]) #macierz zmiennych zaleznych po zmianie
chow.test(y1, x1, y2, x2)



