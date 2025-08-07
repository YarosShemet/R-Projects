################################################################################
################## House Price Index Poland Q1 2010 - Q3 2022 ##################
############################ Yaraslau Shemet ###################################
################################################################################
rm(list=ls())
library(readxl)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(lmtest)
library(tseries)
library(car)
library(sandwich)
library(prais)
library(stargazer)
library(ivreg)
DATA_full <- read_excel('D:/STUDIA/SGH/V semester/Metody Ekonometryczne/Projekt/HPI_data.xlsx', sheet=1)
DATA_full <- DATA_full %>% column_to_rownames(., var = 't')
#DATA_full <- read.csv2('D:/STUDIA/SGH/V semester/Metody Ekonometryczne/Projekt/hpi_pol.csv', row.names = 1) #sep=';' indeks=pierwsza kolumna
sum(is.na(DATA_full)) #brakuje dwóch obserwacji dla zmiennej House_disp_inc
#DATAnoNA <- na.omit(DATA_full) 
DATA <- subset(DATA_full, select = c(3,4,8,9,10,11,12,13)) #ostateczne zmienne dla oszacowania parametrów modeli
attach(DATA)

# Analiza danych ----------------------------------------------------------

summary(DATA)

#https://www.geeksforgeeks.org/how-to-create-correlation-heatmap-in-r/
cormat_full <- cor(na.omit(DATA_full)) #macierz korelacji dla całego zbioru
melted_cormat_full <- melt(round(cor(na.omit(DATA_full)), 2))
ggplot(data = melted_cormat_full, aes(x=Var1, y=Var2, fill=value)) + #wizualizacja macierzy korelacji
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 90))

cormat <- cor(DATA) #macierz korelacji dla ograniczonego zbioru
melted_cormat <- melt(round(cor(DATA), 2))
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)

hist(HPI_real) #prawoskośny rozkład

par(mfrow=c(3, 2))
plot(HPI_real, xlab='kwartał', type='l')
plot(LT_interest_rates, xlab='kwartał', type='l')
plot(Unemp_rate, xlab='kwartał', type='l')
plot(Prod_construction, xlab='kwartał', type='l')
plot(Prod_construction_costs, xlab='kwartał', type='l')
hist(log(HPI_real))

par(mfrow=c(2, 2))
plot(Unemp_rate, HPI_real)
plot(LT_interest_rates, HPI_real)
plot(Prod_construction, HPI_real)
plot(Prod_construction_costs, HPI_real)

adf.test(log(HPI_real))
adf.test(HPI_real_change)
adf.test(Unemp_rate)
adf.test(LT_interest_rates)
adf.test(Prod_construction)
adf.test(Prod_construction_costs)

adf.test(diff(log(HPI_real))) #różnice też są niestacjonarne
adf.test(diff(Unemp_rate)) 
adf.test(diff(LT_interest_rates))
adf.test(diff(Prod_construction))
adf.test(diff(Prod_construction_costs))

# Model adaptowany z literatury  ------------------------------------------------
#nie mogę logarytmować zmiany procentowe dla House_disp_inc
drake_model <- lm(log(HPI_real) ~ log(GDP_per_capita)+LT_interest_rates+Prod_construction, data=DATA_full) 
summary(drake_model)
resettest(drake_model) #odrzucam H0


# Konstrukcja modeli ------------------------------------------------------

#1 podstawowy
model_HPI_real <- lm(log(HPI_real)~LT_interest_rates+Unemp_rate+Prod_construction, data=DATA_full)
summary(model_HPI_real)
vif(model_HPI_real) # <10
par(mfrow=c(2, 1))
plot(model_HPI_real$residuals) #widoczny wzorzec kształtowania się reszt
hist(model_HPI_real$residuals) #brak normalności
jarque.bera.test(model_HPI_real$residuals) #odrzucam H0 o normalności rozkładu składnika losowego 
bptest(model_HPI_real) #brak podstaw (homoskedastyczność)
bgtest(model_HPI_real, order=4) #odrzucam H0 o braku autokorelacji rozkładu składnika losowego
Box.test(model_HPI_real$residuals,lag=4,type="Ljung-Box") #jw.
resettest(model_HPI_real) #brak podstaw (poprawna)
adf.test(model_HPI_real$residuals) #ryzyko regresji pozornej 

#2 eliminacja autokorelacji
par(mfrow = c(2,1))
acf(model_HPI_real$residuals) 
pacf(model_HPI_real$residuals)

HAC_NW=NeweyWest(model_HPI_real, lag=4, prewhite = TRUE) #odporny estymator
coeftest(model_HPI_real)
coeftest(model_HPI_real, vocv.=HAC_NW)

#DATA_full <- read.csv2('D:/STUDIA/SGH/V semester/Metody Ekonometryczne/Projekt/House_prices/hpi_pol.csv') #z kolumna time
PW_model <- prais_winsten(log(HPI_real)~LT_interest_rates+Unemp_rate+Prod_construction, data=DATA_full, index=DATA_full[,1])
summary(PW_model) #estymator iterowany Praisa-Winstena
bgtest(PW_model, order=4) #nadal występuje autokorelacja

DATA$HPI_real_lag <- c(NA, head(DATA$HPI_real, -1)) #dodanie opóźnienia
model_HPI_real_with_lag <- lm(log(HPI_real)~HPI_real_lag+LT_interest_rates+Unemp_rate+Prod_construction, data=DATA)
summary(model_HPI_real_with_lag)
bgtest(model_HPI_real_with_lag, order=4) #nie udało się wyeliminować autokorelację


#3 zmiana modelu 
model_HPI_real_change <- lm(HPI_real_change~LT_interest_rates+Unemp_rate+Prod_construction_costs, data=DATA_full)
summary(model_HPI_real_change)
vif(model_HPI_real_change) # <10
par(mfrow=c(2, 1))
plot(model_HPI_real_change$residuals) #brak wzorcu kształtowania się reszt
hist(model_HPI_real_change$residuals) #rozkład normalny
jarque.bera.test(model_HPI_real_change$residuals) #brak podstaw (rozkład normalny)
resettest(model_HPI_real_change) #brak podstaw (poprawna)
bptest(model_HPI_real_change) #brak podstaw (homoskedastyczność)
bptest(model_HPI_real_change, ~LT_interest_rates*Unemp_rate*Prod_construction_costs+I(LT_interest_rates^2)+I(Unemp_rate^2)+I(Prod_construction_costs^2), data=DATA_full)
bgtest(model_HPI_real_change, order=4) #brak podstaw (brak autokorelacji)
Box.test(model_HPI_real_change$residuals,lag=4,type="Ljung-Box") #jw.
adf.test(model_HPI_real_change$residuals) #stacjonarny
par(mfrow = c(1,2))
acf(model_HPI_real_change$residuals)
pacf(model_HPI_real_change$residuals)
durbinWatsonTest(model_HPI_real_change)
plot(model_HPI_real_change$fitted.values, type='l', col='green') 
lines(DATA[['HPI_real_change']], col='red')

stargazer(drake_model, model_HPI_real, model_HPI_real_with_lag, model_HPI_real_change, type='text')

# Pierwsze przyrosty ------------------------------------------------------
DATA_diff <- DATA[2:51, c(2, 3, 4, 7)]
DATA_diff$HPI_real <- diff(HPI_real)
DATA_diff$LT_interest_rates <- diff(LT_interest_rates)
DATA_diff$Unemp_rate <- diff(Unemp_rate)
DATA_diff$Prod_construction_costs <- diff(Prod_construction_costs)

model_HPI_real_diff <- lm(HPI_real~LT_interest_rates+Unemp_rate+Prod_construction_costs, data=DATA_diff)
summary(model_HPI_real_diff)
adf.test(model_HPI_real_diff$residuals)
bptest(model_HPI_real_diff)
bgtest(model_HPI_real_diff, order=4)

# MZI ---------------------------------------------------------------------
MZI_model = ivreg(HPI_real_change ~ LT_interest_rates+Unemp_rate+Prod_construction_costs|LT_interest_rates+CPI+Build_permits,data=DATA_full)
summary(MZI_model, diagnostics=TRUE)

stargazer(model_HPI_real_change, MZI_model, type='text')


# Dane przekrojowe  -------------------------------------------------------
OECD_DATA_full <- read_excel('D:/STUDIA/SGH/V semester/Metody Ekonometryczne/Projekt/HPI_data.xlsx', sheet=2)
#OECD_DATA_full <- read.csv2('D:/STUDIA/SGH/V semester/Metody Ekonometryczne/Projekt/hpi_oecd_2021.csv', row.names = 1)
summary(lm(log(Real_house_price_ind)~Interest_rate+Unemp_rate+Prod_constr, data=OECD_DATA_full)) #36 obserwacji
