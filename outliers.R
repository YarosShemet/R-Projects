rm(list=ls())
set.seed(108887)

n<-300
X <-rnorm(n, mean = 15, sd = 3) 
Y <- 12 + 1.7*X + rnorm(n, mean = 0, sd = 3)
Y[25] <- -30
Y[50] <- 10
Y[75] <- 60
fit <- lm(Y~X)

plot(Y~X, col="grey", pch=16) 
abline(fit, col="blue")
points(Y[25]~X[25], col="red", pch=16)
points(Y[50]~X[50], col="red", pch=16)
points(Y[75]~X[75], col="red", pch=16)
text(X[25], Y[25], '25', pos=3)
text(X[50], Y[50], '50', pos=3)
text(X[75], Y[75], '75', pos=1)
#analizuj¹c powy¿szy wykres mo¿na postawiæ tezê,
#¿e obserwacje 25 i 75 s¹ odstaj¹ce (obserwujemy du¿e reszty),
#a obserwacja 50 - wp³ywowa (nie ma du¿ych reszt, mo¿e ona wnosiæ istotne informacje i prawdopodobnie nie powsta³a w wyniku b³êdu)

library(olsrr)
library(car)
qqplot(fit$residuals,fit$fitted.values)
qqPlot(fit, main="QQ Plot")
#pierwszy etap diagnostyki - wykres kwantylowy - pokazuje istotne odchylenia na krañcach wykresu dla obserwacji 25 i 75,
#co wskazuje na to, ¿e to s¹ odstaj¹ce obserwacje

leveragePlots(fit) 
#wykres dŸwigni pokazuje, które obserwacje maj¹ najwiêkszy wp³yw na oszacowanie parametrów
#widzimy, ¿e 50-ta obserwacja znajduje siê na krañcu wykresu, a wiêc ma wiêksze znaczenie ni¿ pozosta³e 

outlierTest(fit)
#test statystyczny Bonferonni pozwala odrzuciæ H0 o niewystêpowaniu wartoœci odstaj¹cej dla 25 i 75 obserwacji

ols_plot_resid_stud(fit)
#odnotujemy, ¿e studentyzowane reszty dla obserwacji 25 nie mieszcz¹ siê w przedziale [-2;2]
#ponadto 25-ta zosta³a okreœlona jako wartoœæ odstaj¹ca

ols_plot_resid_lev(fit) 
#studentyzowane reszty na tle dŸwigni wskazuj¹, ¿e 25 i 75 obserwacje s¹ odstaj¹cymi
#obserwacja 50 jest i odstaj¹c¹, i wp³ywow¹

ols_plot_cooksd_bar(fit)
ols_plot_cooksd_chart(fit)
#metoda odleg³oœci Cooka jednoznacznie potwierdza, ¿e wszystkie badane obserwacje s¹ odstaj¹cymi

ols_plot_dffits(fit)
#metoda DFFITS pokazuje jak zmieni¹ siê wartoœci teoretyczne, jesli usuniemy jedn¹ obserwacj¹
#wszyskie badane obserwacje cechuje ponadprogowy wp³yw na wartoœæ teoretyczn¹ przy wy³¹czaniu z modelu

ols_plot_dfbetas(fit)
#metoda DFBETA pokazuje jak zmieni¹ siê oszacowania parametrów, jeœli usuniemy jedn¹ obserwacj¹
#wed³ug tej metody obserwacje 25 i 50 s¹ wp³ywowe, przy tym obserwacja 75 ledwie przekracza próg, co pozwala w¹tpiæ o jej istotnym wp³ywie

fit_drop_outliers <- lm(Y[-c(25, 75)]~X[-c(25, 75)])
fit_drop_all <- lm(Y[-c(25, 50, 75)]~X[-c(25, 50, 75)])
summary_coef <- cbind(coef(fit), coef(fit_drop_outliers), coef(fit_drop_all))
colnames(summary_coef) <- c("pe³ny zbiór", "bez odstaj¹cych niewp³ywowych", "bez wszystkich odstaj¹cych")
(summary_coef)
#wykorzysta³em metodê usuniêcia podejrzanych obserwacji, jednak nie jestem pewien czy te obserwacje s¹ rzeczywiœcie b³êdne
#w taki sposób mogê utraciæ cennê informacje, zw³aszcza w przypadku 50-tej obserwacji, która jest i wp³ywowa, i odstaj¹ca

library(robustbase)
library(L1pack)
fit_LTS <- ltsReg(Y~X, alpha=0.95)
#Metoda LTS usuwa obserwacje z najwy¿szymi resztami, zostawiaj¹c przy tym 95% pozostaje w próbie

fit_LAD <- lad(Y~X)
#Metoda LAD dopasowuje parametry do mediany, która jest bardziej odporna na wystêpowanie wartoœci odstaj¹cych

reg_coef<-cbind(coef(fit),coef(fit_LTS),coef(fit_LAD))
colnames(reg_coef)<-c("OLS","LTS","LAD")
(reg_coef)
#w przypadku metody LTS widzimy, ¿e oszacowanie parametrów s¹ wy¿sze i zbli¿one do tych, co otrzymaliœmy po usuniêciu wszystkich podejrzanych
#dla metody LAD oszacowania parametrów znacz¹co siê ró¿ni¹ od KMNK, zawy¿aj¹c przy tym oszacowanie przy X i zani¿aj¹c sta³¹


# Zadanie 2 ---------------------------------------------------------------
library(dplyr)
dane_all <- read.csv("TaylorRule.csv", sep =";", dec = ".")
dane <- dane_all[, 3:ncol(dane_all)]
dane <- dane %>% relocate(USA_IR, .after = GBR_IR)
attach(dane)
#mój indeks to 108887, wiêc Norwegia - mój kraj do analizy
fit <- lm(NOR_INF~NOR_IR+NOR_Y)
summary(fit)
plot(resid(fit), type="l")
avPlots(fit)
library(strucchange)
Chow_test_half <- sctest(NOR_INF~NOR_IR+NOR_Y, point = round(nrow(dane)/2), type = "Chow")$p.value #w po³owie próby
Chow_test_crisis <- sctest(NOR_INF~NOR_IR+NOR_Y, point = 106, type = "Chow")$p.value #jako miejsce dla porównania wybra³em 10/2008 - czyli wy¿ kryzysu ekonomicznego
#w obu przypadkach muszê odrzuciæ H0 na rzecz H1, która mówi ¿e wystêpuje zmiana strukturalna
#tabelka z p-value
sctest(fit) #ogólny test do ca³ego modelu
breakpoints(NOR_INF~NOR_IR+NOR_Y) 
#12/2003, 10/2007, 03/2012, 12/2016 - punkty w czasie, gdzie wyst¹pi³a zmiana strukturalna
#czyli mniej wiêcej ka¿de 4-5 lat odbywa³a siê zmiana strukturalna
#wi¹¿ê to z wprowadzeniem nowej polityki monetarnej ka¿de 5 lat (typowy okres dla analiz makroekonomicznych), choæ nie znam siê na gospodarce norweskiej

#dla dodania zmiennej binarnej wybieram najbli¿szy do zgadniêtego wy¿ej punkt - 94 - 10/2007
S<-seq(1, nrow(dane), by=1)
for (i in 1:nrow(dane)){
  if (S[i]>94){S[i]=1}
  else
  {S[i]=0}
}
fit_bin <- lm(NOR_INF~NOR_IR+NOR_Y+S)
summary(fit_bin)
plot(resid(fit_bin), type="l")
#skorygowany R-kwadrat istotnie siê zwiêkszy³ i dodanie zmiennej binarnej polepszy³o dopasowanie modelu


#dla ogólnych przypadków
index <- '108887'
#index <- 'XXXXXX'
my_sum <- as.integer(substring(index, 5, 5)) + as.integer(substring(index, 6, 6))
if (my_sum < 10){
  fit_general <- lm(dane[[my_sum]]~dane[[my_sum+11]]+dane[[my_sum+22]])
} else {
  my_sum <- my_sum - 9
  fit_general <- lm(dane[[my_sum]]~dane[[my_sum+11]]+dane[[my_sum+22]])
}
summary(fit_general)

breakpoints_general <- breakpoints(dane[[my_sum]]~dane[[my_sum+11]]+dane[[my_sum+22]])$breakpoints
selected_breakpoint <- breakpoints_general[round(length(breakpoints_general)/2)] #najbli¿szy do po³owy próby punkt zmiany
S<-seq(1, nrow(dane), by=1)
for (i in 1:nrow(dane)){
  if (S[i]>selected_breakpoint){S[i]=1} 
  else
  {S[i]=0}
}
fit_bin_general <- lm(dane[[my_sum]]~dane[[my_sum+11]]+dane[[my_sum+22]]+S)
summary(fit_bin_general)