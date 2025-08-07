rm(list=ls())

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
#analizując powyższy wykres można postawić tezę,
#że obserwacje 25 i 75 są odstające (obserwujemy duże reszty),
#a obserwacja 50 - wpływowa (nie ma dużych reszt, może ona wnosić istotne informacje i prawdopodobnie nie powstała w wyniku błędu)

library(olsrr)
library(car)
qqplot(fit$residuals,fit$fitted.values)
qqPlot(fit, main="QQ Plot")
#pierwszy etap diagnostyki - wykres kwantylowy - pokazuje istotne odchylenia na krańcach wykresu dla obserwacji 25 i 75,
#co wskazuje na to, że to są odstające obserwacje

leveragePlots(fit) 
#wykres dźwigni pokazuje, które obserwacje mają największy wpływ na oszacowanie parametrów
#widzimy, że 50-ta obserwacja znajduje się na krańcu wykresu, a więc ma większe znaczenie niż pozostałe 

outlierTest(fit)
#test statystyczny Bonferonni pozwala odrzucić H0 o niewystępowaniu wartości odstającej dla 25 i 75 obserwacji

ols_plot_resid_stud(fit)
#odnotujemy, że studentyzowane reszty dla obserwacji 25 nie mieszczą się w przedziale [-2;2]
#ponadto 25-ta została określona jako wartość odstająca

ols_plot_resid_lev(fit) 
#studentyzowane reszty na tle dźwigni wskazują, że 25 i 75 obserwacje są odstającymi
#obserwacja 50 jest i odstającą, i wpływową

ols_plot_cooksd_bar(fit)
ols_plot_cooksd_chart(fit)
#metoda odległości Cooka jednoznacznie potwierdza, że wszystkie badane obserwacje są odstającymi

ols_plot_dffits(fit)
#metoda DFFITS pokazuje jak zmienią się wartości teoretyczne, jesli usuniemy jedną obserwacją
#wszyskie badane obserwacje cechuje ponadprogowy wpływ na wartość teoretyczną przy wyłączaniu z modelu

ols_plot_dfbetas(fit)
#metoda DFBETA pokazuje jak zmienią się oszacowania parametrów, jeśli usuniemy jedną obserwacją
#według tej metody obserwacje 25 i 50 są wpływowe, przy tym obserwacja 75 ledwie przekracza próg, co pozwala wątpić o jej istotnym wpływie

fit_drop_outliers <- lm(Y[-c(25, 75)]~X[-c(25, 75)])
fit_drop_all <- lm(Y[-c(25, 50, 75)]~X[-c(25, 50, 75)])
summary_coef <- cbind(coef(fit), coef(fit_drop_outliers), coef(fit_drop_all))
colnames(summary_coef) <- c("pełny zbiór", "bez odstających niewpływowych", "bez wszystkich odstających")
(summary_coef)
#wykorzystałem metodę usunięcia podejrzanych obserwacji, jednak nie jestem pewien czy te obserwacje są rzeczywiście błędne
#w taki sposób mogę utracić cennę informacje, zwłaszcza w przypadku 50-tej obserwacji, która jest i wpływowa, i odstająca

library(robustbase)
library(L1pack)
fit_LTS <- ltsReg(Y~X, alpha=0.95)
#Metoda LTS usuwa obserwacje z najwyższymi resztami, zostawiając przy tym 95% pozostaje w próbie

fit_LAD <- lad(Y~X)
#Metoda LAD dopasowuje parametry do mediany, która jest bardziej odporna na występowanie wartości odstających

reg_coef<-cbind(coef(fit),coef(fit_LTS),coef(fit_LAD))
colnames(reg_coef)<-c("OLS","LTS","LAD")
(reg_coef)
#w przypadku metody LTS widzimy, że oszacowanie parametrów są wyższe i zbliżone do tych, co otrzymaliśmy po usunięciu wszystkich podejrzanych
#dla metody LAD oszacowania parametrów znacząco się różnią od KMNK, zawyżając przy tym oszacowanie przy X i zaniżając stałą


# Zadanie 2 ---------------------------------------------------------------
library(dplyr)
dane_all <- read.csv("TaylorRule.csv", sep =";", dec = ".")
dane <- dane_all[, 3:ncol(dane_all)]
dane <- dane %>% relocate(USA_IR, .after = GBR_IR)
attach(dane)
fit <- lm(NOR_INF~NOR_IR+NOR_Y)
summary(fit)
plot(resid(fit), type="l")
avPlots(fit)
library(strucchange)
Chow_test_half <- sctest(NOR_INF~NOR_IR+NOR_Y, point = round(nrow(dane)/2), type = "Chow")$p.value #w połowie próby
Chow_test_crisis <- sctest(NOR_INF~NOR_IR+NOR_Y, point = 106, type = "Chow")$p.value #jako miejsce dla porównania wybrałem 10/2008 - czyli wyż kryzysu ekonomicznego
#w obu przypadkach muszę odrzucić H0 na rzecz H1, która mówi że występuje zmiana strukturalna
#tabelka z p-value
sctest(fit) #ogólny test do całego modelu
breakpoints(NOR_INF~NOR_IR+NOR_Y) 
#12/2003, 10/2007, 03/2012, 12/2016 - punkty w czasie, gdzie wystąpiła zmiana strukturalna
#czyli mniej więcej każde 4-5 lat odbywała się zmiana strukturalna
#wiążę to z wprowadzeniem nowej polityki monetarnej każde 5 lat (typowy okres dla analiz makroekonomicznych), choć nie znam się na gospodarce norweskiej

#dla dodania zmiennej binarnej wybieram najbliższy do zgadniętego wyżej punkt - 94 - 10/2007
S<-seq(1, nrow(dane), by=1)
for (i in 1:nrow(dane)){
  if (S[i]>94){S[i]=1}
  else
  {S[i]=0}
}
fit_bin <- lm(NOR_INF~NOR_IR+NOR_Y+S)
summary(fit_bin)
plot(resid(fit_bin), type="l")
#skorygowany R-kwadrat istotnie się zwiększył i dodanie zmiennej binarnej polepszyło dopasowanie modelu


#dla ogólnych przypadków
index <- 'XXXXXX'
my_sum <- as.integer(substring(index, 5, 5)) + as.integer(substring(index, 6, 6))
if (my_sum < 10){
  fit_general <- lm(dane[[my_sum]]~dane[[my_sum+11]]+dane[[my_sum+22]])
} else {
  my_sum <- my_sum - 9
  fit_general <- lm(dane[[my_sum]]~dane[[my_sum+11]]+dane[[my_sum+22]])
}
summary(fit_general)

breakpoints_general <- breakpoints(dane[[my_sum]]~dane[[my_sum+11]]+dane[[my_sum+22]])$breakpoints
selected_breakpoint <- breakpoints_general[round(length(breakpoints_general)/2)] #najbliższy do połowy próby punkt zmiany
S<-seq(1, nrow(dane), by=1)
for (i in 1:nrow(dane)){
  if (S[i]>selected_breakpoint){S[i]=1} 
  else
  {S[i]=0}
}
fit_bin_general <- lm(dane[[my_sum]]~dane[[my_sum+11]]+dane[[my_sum+22]]+S)

summary(fit_bin_general)

