
# Biblioteki --------------------------------------------------------------
list_of_packages <- c("readr" # wczytywanie danych
                      ,"dplyr" # manipulacja danymi
                      ,"rpart" # drzewa decyzyjne
                      ,"rpart.plot" # Ĺ‚adne wykresy dla drzew
                      ,"randomForest" # lasy losowe
                      ,"ROCR" # ocena jakoĹ›ci modelu - krzywa ROC, AUC, itd.
                      ,"MASS" # dobĂłr zmiennych do modelu
                      , "mlbench"
                      , "reshape2"
)                      
not_installed <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)
lapply(list_of_packages, library, character = TRUE)
library(mlbench)
library(readr)
library(rpart)
library(ggplot2)
#install.packages("Information")
library(Information)
library(reshape2)
#install.packages('ROSE')
library('ROSE')

# Dane i wykresy zmiennych --------------------------------------------------------
#https://www.kaggle.com/datasets/gauravtopre/bank-customer-churn-dataset
#https://rpubs.com/Rvge_mvsrter/939193 !!!do usuniecia!!!

raw_churn_data <- read_csv("D:/STUDIA/SGH/V semester/Indukowane reguły decyzyjne/Projekt/Bank Customer Churn Prediction.csv")
#raw_churn_data <- read_csv("C:/Folder/uni/archive/Bank Customer Churn Prediction.csv")
nrow(raw_churn_data) #ile obserwacji
ncol(raw_churn_data) #ile zmiennych
str(raw_churn_data) #opis danych
head(raw_churn_data)
sum(is.na(raw_churn_data)) #no missing values / brak brakow danych, wiec mozna skorzystac z regresji logistyzcnej

#od razu mozemy usunac zmienna customer_id --> jest zmienna unikalna i nie ma zadnego efektu
churn_data <- raw_churn_data[, 2:12]
summary(churn_data) #min, max, mean dla zmiennych liczbowych typu credit_score
attach(churn_data)
table(churn_data$churn) #zmienna objasniana - niezbilansowany zbiór danych
#trzeba skorzystać z undersamplingu - zbilansować klasy
barplot(table(churn_data$churn)/10000*100, ylab='%', main='Udzial klasy negatywnej i pozytywnej dla zmiennej objaśnianej', ylim=c(0,100))
#20 procentów klientów odeszło z banku

#credit score ----------------------------------------------------------------
#zmienna dyskretna - im wiecej, tym klient bardziej wiarygodny
#nominalna po przekształceniach
hist(credit_score, breaks=30) 
credit_score_churn <- as.data.frame(matrix(0, 5, 5))
credit_score_churn[,1] <- c('[350, 450]', '(450, 550]', '(550, 650]', '(650, 750]', '(750, 850]') 
colnames(credit_score_churn) <- c('score_group', 'count', 'group_share', 'churn', 'churn_rate')
for (i in seq(350, 750, by=100)) { #przedzialy wielkoscia 100
  if (i==350){
    count <- nrow(filter(churn_data, credit_score>=i & credit_score<=i+100))
    group_share <- count/10000*100
    churn <- nrow(filter(churn_data, credit_score>=i & credit_score<=i+100 & churn==1))
    churn_rate <- churn/count*100
    credit_score_churn[(i%/%100-2), 2:5] <- c(count, group_share, churn, churn_rate)  #[350, 450) [450, 550) ... [750, 850]
  } else {
    count <- nrow(filter(churn_data, credit_score>i & credit_score<=i+100)) #nie wlaczajac prawy kraniec
    group_share <- count/10000*100
    churn <- nrow(filter(churn_data, credit_score>i & credit_score<=i+100 & churn==1))
    churn_rate <- churn/count*100
    credit_score_churn[(i%/%100-2), 2:5] <- c(count, group_share, churn, churn_rate) 
  }
}
ggplot(credit_score_churn, aes(score_group, group_share, fill='group_share'))+ 
  geom_col(fill='blue')+
  geom_line(aes(score_group, churn_rate, group=1),
            colour = 'red', size = 1.2)+
  scale_y_continuous(
    limits = c(0, 100),
    name = "Udzial w zbiorze (%)",
    sec.axis = sec_axis(~.*1, name="Udział klasy pozytywnej (%)"))+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  ggtitle('Udzial w zbiorze vs udzial klasy pozytywnej w procentach wg scoringu kredytowego')


#country -----------------------------------------------------------------------
#zmienna nominalna - z ktorych krajow pochodzi klient
unique(country) #tylko 3 - Francja, Hiszpania, Niemcy
nrow(filter(churn_data, country == 'France'))/nrow(churn_data)*100 #ile klientow z kraju w procentach
nrow(filter(churn_data, country == 'Spain'))/nrow(churn_data)*100 
nrow(filter(churn_data, country == 'Germany'))/nrow(churn_data)*100
countries_churn <- churn_data %>% #dane dla roznych krajow
  group_by(., country) %>% 
  summarise(.,count=n(),group_share=n()/10000*100,churn=sum(churn), churn_rate=sum(churn)/n()*100)
#histogramy 
ggplot(countries_churn, aes(country, count, fill='count')) + 
  geom_col()+
  geom_col(aes(country, churn, fill='churn'))
#udzial w zbiorze vs udzial klasy pozytywnej w procentach
ggplot(countries_churn, aes(country, group_share, fill='group_share'))+ 
  geom_col(fill='blue')+
  geom_line(aes(country, churn_rate, group=1),
             colour = 'red', size = 1.2)+
  scale_y_continuous(
     limits = c(0, 100),
     name = "Udzial w zbiorze (%)",
     sec.axis = sec_axis(~.*1, name="Udział klasy pozytywnej (%)"))+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  ggtitle('Udzial w zbiorze vs udzial klasy pozytywnej w procentach wg państwa')

#gender -----------------------------------------------------------------------
unique(gender) #Female, Male zmienna binarna
gender <- as.factor(gender) #czy trzeba faktoryzowac
gender_churn <- churn_data %>% #dane dla roznych krajow
  group_by(., gender) %>% 
  summarise(.,count=n(),group_share=n()/10000*100,churn=sum(churn), churn_rate=sum(churn)/n()*100)
#udzial w zbiorze vs udzial klasy pozytywnej w procentach
ggplot(gender_churn, aes(gender, group_share, fill='group_share'))+ 
  geom_col(fill='blue')+
  geom_line(aes(gender, churn_rate, group=1),
            colour = 'red', size = 1.2)+
  scale_y_continuous(
    limits = c(0, 100),
    name = "Udział w zbiorze (%)",
    sec.axis = sec_axis(~.*1, name="Udział klasy pozytywnej (%)"))+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  ggtitle('Udział w zbiorze vs udział klasy pozytywnej w procentach wg płci')

#age ---------------------------------------------------------
hist(age, breaks=16) #95 - max, 15 - min; zmienna dyskretna
#nominalna po przekszałceniach
churn_data$age_group <- cut(churn_data$age,
                       breaks=c(15, 25, 30, 35, 40, 45, 50, 95),
                       labels=c('[15, 25]', '(25, 30]', '(30, 35]', '(35, 40]', '(40, 45]', '(45, 50]', '50<'))
count <- summary(churn_data$age_group)
group_share <- count/10000*100
labels <- c('([15, 25]', '(25, 30]', '(30, 35]', '(35, 40]', '(40, 45]', '(45, 50]', '50<')
churn <- c()
churn_rate <- c()
for (j in seq(1, 7)) {
  churn[j] <- nrow(filter(churn_data, age_group==labels[j] & churn==1))
  churn_rate[j] <- churn[j]/count[j]*100
}

age_group <- data.frame(labels, count, group_share, churn, churn_rate)
ggplot(age_group, aes(labels, group_share, fill='group_share'))+ 
  geom_col(fill='blue')+
  geom_line(aes(labels, churn_rate, group=1),
            colour = 'red', size = 1.2)+
  scale_y_continuous(
    limits = c(0, 100),
    name = "Udział w zbiorze (%)",
    sec.axis = sec_axis(~.*1, name="Udział klasy pozytywnej (%)"))+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  ggtitle('Udział w zbiorze vs udział klasy pozytywnej w procentach wg grupy wiekowej')

# tenure-------------------------------------------
#tenure - zmienna dyskretna - jak dlugo w latach klient ma konto w tym banku
unique(tenure) #length=11
tenure_churn <- churn_data %>% #dane dla roznych krajow
  group_by(., tenure) %>% 
  summarise(.,count=n(),group_share=n()/10000*100,churn=sum(churn), churn_rate=sum(churn)/n()*100)
#udzial w zbiorze vs udzial klasy pozytywnej w procentach
ggplot(tenure_churn, aes(tenure, group_share, fill='group_share'))+ 
  geom_col(fill='blue')+
  geom_line(aes(tenure, churn_rate, group=1),
            colour = 'red', size = 1.2)+
  scale_y_continuous(
    limits = c(0, 100),
    name = "Udzial w zbiorze (%)",
    sec.axis = sec_axis(~.*1, name="Udział klasy pozytywnej (%)"))+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  ggtitle('Udzial w zbiorze vs udzial klasy pozytywnej w procentach wg długości bycia klientem')+
  theme(plot.title = element_text(size = 12))  


# balance -----------------------------------------------------------------
#zmienna ciagla
hist(balance) #min - 0, max - 250898
#nominalna po przekszałceniach
balance_churn <- as.data.frame(matrix(0, 4, 5))
balance_churn[,1] <- c('(0)', '(0, 100 000]', '(100 000, 150 000]', '>150 000') 
colnames(balance_churn) <- c('balance_group', 'count', 'group_share', 'churn', 'churn_rate')
j=1
balance_groups = c(0, 100000, 150000)
for (i in 1:4) {
  if (i==1){
    count <- nrow(filter(churn_data, balance==balance_groups[i]))
    group_share <- count/10000*100
    churn <- nrow(filter(churn_data, balance==balance_groups[i] & churn==1))
    churn_rate <- churn/count*100
    balance_churn[j, 2:5] <- c(count, group_share, churn, churn_rate)
    j=j+1
  } else if (i==4 & j==4){
    count <- nrow(filter(churn_data, balance>balance_groups[i-1]))
    group_share <- count/10000*100
    churn <- nrow(filter(churn_data, balance>balance_groups[i-1] & churn==1))
    churn_rate <- churn/count*100
    balance_churn[j, 2:5] <- c(count, group_share, churn, churn_rate) 
    j=j+1
  } else {
    count <- nrow(filter(churn_data, balance>balance_groups[i-1] & balance<=balance_groups[i]))
    group_share <- count/10000*100
    churn <- nrow(filter(churn_data, balance>balance_groups[i-1] & balance<=balance_groups[i] & churn==1))
    churn_rate <- churn/count*100
    balance_churn[j, 2:5] <- c(count, group_share, churn, churn_rate) 
    j=j+1
    }
  }
ggplot(balance_churn, aes(balance_group, group_share, fill='group_share'))+ 
  geom_col(fill='blue')+
  geom_line(aes(balance_group, churn_rate, group=1),
            colour = 'red', size = 1.2)+
  scale_y_continuous(
    limits = c(0, 100),
    name = "Udział w zbiorze (%)",
    sec.axis = sec_axis(~.*1, name="Udział klasy pozytywnej (%)"))+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  ggtitle('Udział w zbiorze vs udział klasy pozytywnej w procentach wg stanu konta')

# products_number  --------------------------------------------------------
#zmienna dyskretna - ile produktów posiada klient
products_number = churn_data$products_number
unique(products_number) #length=4

dataframemy = data.frame(churn_data)
dataframemy$ID <- 1:nrow(dataframemy)
cond = filter(dataframemy, products_number==4 | products_number==3)
vectorToDel = cond$ID
churn_data = churn_data[-vectorToDel,]

products_number <- as.factor(products_number)
products_churn <- churn_data %>% #dane dla roznych krajow
  group_by(., products_number) %>% 
  summarise(.,count=n(),group_share=n()/10000*100,churn=sum(churn), churn_rate=sum(churn)/n()*100)
#udzial w zbiorze vs udzial klasy pozytywnej w procentach
ggplot(products_churn, aes(products_number, group_share, fill='group_share'))+ 
  geom_col(fill='blue')+
  geom_line(aes(products_number, churn_rate, group=1),
            colour = 'red', size = 1.2)+
  scale_y_continuous(
    limits = c(0, 100),
    name = "Udzial w zbiorze (%)",
    sec.axis = sec_axis(~.*1, name="Udział klasy pozytywnej (%)"))+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  ggtitle('Udzial w zbiorze vs udzial klasy pozytywnej w procentach wg liczby posiadanych produktów')+
  theme(plot.title = element_text(size = 12)) 
#nrow(filter(churn_data, products_number==4))
nrow(filter(churn_data, products_number==4 & churn==1)) #wszyscy co mieli 4 produkty, odeszli z banku
#mozliwa przyczyna - posiadanie wszystkich atrakcyjnych ofert od banku(nowe konta i produkty w innym banku staja sie atrakcyjniejsze)


# credit_card -------------------------------------------------------------
#zmienna binarna - czy posiada karte kredytowa
credit_card <- as.factor(credit_card) #czy trzeba faktoryzowac
credit_card_churn <- churn_data %>% #dane dla roznych krajow
  group_by(., credit_card) %>% 
  summarise(.,count=n(),group_share=n()/10000*100,churn=sum(churn), churn_rate=sum(churn)/n()*100)
#udzial w zbiorze vs udzial klasy pozytywnej w procentach
ggplot(credit_card_churn, aes(credit_card, group_share, fill='group_share'))+ 
  geom_col(fill='blue')+
  geom_line(aes(credit_card, churn_rate, group=1),
            colour = 'red', size = 1.2)+
  scale_y_continuous(
    limits = c(0, 100),
    name = "Udział w zbiorze (%)",
    sec.axis = sec_axis(~.*1, name="Udział klasy pozytywnej (%)"))+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  ggtitle('Udział w zbiorze vs udział klasy pozytywnej w procentach wg posiadania karty kredytowej')+
  theme(plot.title = element_text(size = 12)) 


# active_member -----------------------------------------------------------
#zmienna binarna - czy klient jest aktywnym? 
#co to znaczy aktywnym
active_member <- as.factor(active_member) #czy trzeba faktoryzowac
active_member_churn <- churn_data %>% #dane dla roznych krajow
  group_by(., active_member) %>% 
  summarise(.,count=n(),group_share=n()/10000*100,churn=sum(churn), churn_rate=sum(churn)/n()*100)
#udzial w zbiorze vs udzial klasy pozytywnej w procentach
ggplot(active_member_churn, aes(active_member, group_share, fill='group_share'))+ 
  geom_col(fill='blue')+
  geom_line(aes(active_member, churn_rate, group=1),
            colour = 'red', size = 1.2)+
  scale_y_continuous(
    limits = c(0, 100),
    name = "Udział w zbiorze (%)",
    sec.axis = sec_axis(~.*1, name="Udział klasy pozytywnej (%)"))+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  ggtitle('Udział w zbiorze vs udział klasy pozytywnej w procentach wg bycia aktywnym klientem')+
  theme(plot.title = element_text(size = 12)) 


# estimated_salary --------------------------------------------------------
#zmienna ciagla 
hist(estimated_salary) #min 11.58 max 199992.48
salary_churn <- as.data.frame(matrix(0, 4, 5))
salary_churn[,1] <- c('(0, 50 000]', '(50 000, 100 000]', '(100 000, 150 000]', '(150 000, 200 000]') 
colnames(salary_churn) <- c('salary_group', 'count', 'group_share', 'churn', 'churn_rate')
j=1
for (i in seq(0, 150000, by=50000)) { #przedzialy wielkoscia 50000$
    count <- nrow(filter(churn_data, estimated_salary>i & estimated_salary<=i+50000))
    group_share <- count/10000*100
    churn <- nrow(filter(churn_data, estimated_salary>i & estimated_salary<=i+50000 & churn==1))
    churn_rate <- churn/count*100
    salary_churn[j, 2:5] <- c(count, group_share, churn, churn_rate)
    j=j+1
}
ggplot(salary_churn, aes(salary_group, group_share, fill='group_share'))+ 
  geom_col(fill='blue')+
  geom_line(aes(salary_group, churn_rate, group=1),
            colour = 'red', size = 1.2)+
  scale_y_continuous(
    limits = c(0, 100),
    name = "Udział w zbiorze (%)",
    sec.axis = sec_axis(~.*1, name="Udział klasy pozytywnej (%)"))+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  ggtitle('Udział w zbiorze vs udział klasy pozytywnej w procentach wg szacowanego wynagrodzenia')


# Wartości odstające ------------------------------------------------------
#moga sie pojawic tylko dla zmiennych ciaglych
boxplot(churn_data[,c('balance', 'estimated_salary')])
#nie ma kolek dlatego nie ma outlierow


# Korelacja zmiennych objaśniających -------------------------------------------
#wybieramy tylko zmienne mierzalne
churn_data_for_cor <- churn_data %>% 
  dplyr::select(-c('churn')) %>% 
  dplyr::select_if(is.numeric)
melted_cormat <- melt(round(cor(churn_data_for_cor), 2))
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
#tylko slaba korelacja pomiedzy liczba produktow a stanem konta

# Information Value------------------------------
IV_before <- create_infotables(data = churn_data, y='churn')
IV_before$Summary

#mniej wiecej wszystko sie zgadza 
#na wykresach tenure, estimated_salary, credit_card - czerwone linie sa proste
#tzn. nie mozemy sprognozowac odejscie klienta ze wzgledu na dana ceche, bo IV jest mala 


# Modele do przeksztalcen zmiennych -----------------------------
#I. Log reg, tree (ROC, lift), forest bez zadnych zmian
#II. Log reg, tree (ROC, lift), forest po zbilansowaniu zbioru (50 na 50 churn)
#trzeba zbilansowac zbior 50% churn, 50% nie churn - inaczej bedzie problem z sensitivity (wychwycenie klasy pozytywnej)
#III. Log reg, tree (ROC, lift), forest z tymi przeksztalceniami, co polepsza model (trzeba sprobowac rozne kombinacje)
#IV. Wybor najlepszego modelu i reguly decyzyjne=liscie


# ЧАСТЬ МАЗУРА 
# Podział na zbiór treningowy i testowy 
# Podział dla CHURN_DATA
set.seed(36)
test_prop <- 0.25 #'[RED pytanie czy zostawiamy podział zbioru na 0.25?]
test.set.index <- (runif(nrow(churn_data)) < test_prop)
churn.test <- churn_data[test.set.index, ]
churn.train <- churn_data[!test.set.index, ]

# I. Model po przekształceniach (bazowy)
# REGRESJA LOGISTYCZNA 
base_reg_log <- glm(churn ~ ., 
                    data = churn.train, 
                    family = binomial
)

summary(base_reg_log)

# DRZEWO KLASYFIKACYJNE  
base_tree <- rpart(churn ~ .,
              data = churn.train,
              method = "class", minbucket = 100)
base_tree_D <- rpart(churn ~ .,
                   data = churn.train,
                   method = "class")
BAZA <- list()
### Regresje
BAZA[["Do wprowadzenia"]] <- table(predict(base_tree_D, new = churn.test, type = "class"), churn.test$churn)
### Drzewa
BAZA[["Po wprowadzeniu"]] <- table(predict(base_tree, new = churn.test, type = "class"), churn.test$churn)
lapply(BAZA, base_model_churn)
sapply(BAZA, base_model_churn)

base_tree

plot(base_tree)
text(base_tree, pretty = TRUE)

rpart.plot(base_tree, under = FALSE, tweak = 1.2, fallen.leaves = TRUE)

base_tree$variable.importance

churn.train$churn <- as.character(churn.train$churn)
churn.train$churn <- as.factor(churn.train$churn)

# LASY LOSOWE
base_rf <- randomForest(churn ~., 
                   data = churn.train)

# Użyteczne parametry:
# ntree - liczba drzew
# mtry - liczba zmiennych do losowego próbkowania jako kandydaci przy każdym podziale

## Wpływ zmiennych
varImpPlot(base_rf)
base_rf$importance

# Weryfikacja jakości klasyfikacji -  MACIERZ POMYŁEK  
base_confmat <- table(predict(base_tree, new = churn_data, type = "class"), churn_data$churn)
base_confmat
sum(diag(base_confmat)/sum(base_confmat))
base_error <- 1 - sum(diag(base_confmat)/sum(base_confmat))
base_error

# Wizualizacja macierzy pomyłek
ActualOutcomeDT <- factor(c(0, 0, 1, 1))
PredictedOutcomeDT <- factor(c(0, 1, 0, 1))
Y      <- c(7681, 1170, 282, 867)
df <- data.frame(ActualOutcomeDT, PredictedOutcomeDT, Y)

ggplot(data =  df, mapping = aes(x = ActualOutcomeDT, y = PredictedOutcomeDT)) +
  geom_tile(aes(fill = Y), colour = "black") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1, fontface = 'bold', color = 'white', size=10) +
  scale_fill_gradient(low = "#6D9EC1", high = "#ff8080") +
  theme_bw() + theme(legend.position = "none") + theme(axis.text = element_text(size=20)) + theme(axis.title = element_text(size=20,face="bold"))

# ROC 
CM <- list()
### Regresje
CM[["base_reg_log"]] <- table(ifelse(predict(base_reg_log, new = churn.test, type = "response") > 0.5, 1, 0), churn.test$churn)
### Drzewa
CM[["base_tree"]] <- table(predict(base_tree, new = churn.test, type = "class"), churn.test$churn)
### Las
CM[["base_rf"]] <- table(predict(base_rf, new = churn.test, type = "class"), churn.test$churn)

base_model_churn <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy # Misclassification Error Rate
  # inaczej: MER < - (false_positive + false_positive) / sum(classif_mx)
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # inaczej - Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}

lapply(CM, base_model_churn)
sapply(CM, base_model_churn)

# II. Model po przekształceniach (50/50)
set.seed(36)
test_prop <- 0.25
test.set.index <- (runif(nrow(churn_data)) < test_prop)
churn_data.test <- churn_data[test.set.index, ]
churn_data.train <- churn_data[!test.set.index, ]
table(churn_data.train$churn)

# UNDERsampling, dlatego N = 1539+1539
# zamiast churn_data_train - zbior treningowy
churn_balanced_under <- ovun.sample(churn ~ ., data = (churn_data.train), method = "under", N = 3078, seed=36)$data
table(churn_balanced_under$churn)

# REGRESJA LOGISTYCZNA 
under_reg_log <- glm(churn ~ ., 
                    data = churn_balanced_under, 
                    family = binomial
)

summary(base_reg_log)

# DRZEWO KLASYFIKACYJNE  
under_tree <- rpart(churn ~ .,
                   data = churn_balanced_under,
                   method = "class", minbucket = 100)
under_tree_D <- rpart(churn ~ .,
                    data = churn_balanced_under,
                    method = "class")
BAZA1 <- list()
### Regresje
BAZA1[["Do wprowadzenia"]] <- table(predict(under_tree_D, new = churn_balanced_under, type = "class"), churn_balanced_under$churn)
### Drzewa
BAZA1[["Po wprowadzeniu"]] <- table(predict(under_tree, new = churn_balanced_under, type = "class"), churn_balanced_under$churn)
lapply(BAZA1, under_model_churn)
sapply(BAZA1, under_model_churn)
under_tree

plot(under_tree)
text(under_tree, pretty = TRUE)

rpart.plot(under_tree, under = FALSE, tweak = 1.2, fallen.leaves = TRUE)

under_tree$variable.importance

churn_balanced_under$churn <- as.character(churn_balanced_under$churn)
churn_balanced_under$churn <- as.factor(churn_balanced_under$churn)

# LASY LOSOWE
under_rf <- randomForest(churn ~., 
                        data = churn_balanced_under)

# Użyteczne parametry:
# ntree - liczba drzew
# mtry - liczba zmiennych do losowego próbkowania jako kandydaci przy każdym podziale

## Wpływ zmiennych
varImpPlot(under_rf)
under_rf$importance


# Weryfikacja jakości klasyfikacji -  MACIERZ POMYŁEK  
under_confmat <- table(predict(under_tree, new = churn_balanced_under, type = "class"), churn_balanced_under$churn)
under_confmat
sum(diag(under_confmat)/sum(under_confmat))
under_error <- 1 - sum(diag(under_confmat)/sum(under_confmat))
under_error

# Wizualizacja macierzy pomyłek
ActualOutcomeDT_under <- factor(c(0, 0, 1, 1))
PredictedOutcomeDT_under <- factor(c(0, 1, 0, 1))
Y_under      <- c(1144, 336, 395, 1203)
df_under <- data.frame(ActualOutcomeDT_under, PredictedOutcomeDT_under, Y)

ggplot(data =  df_under, mapping = aes(x = ActualOutcomeDT_under, y = PredictedOutcomeDT_under)) +
  geom_tile(aes(fill = Y_under), colour = "black") +
  geom_text(aes(label = sprintf("%1.0f", Y_under)), vjust = 1, fontface = 'bold', color = 'white', size=10) +
  scale_fill_gradient(low = "#6D9EC1", high = "#ff8080") +
  theme_bw() + theme(legend.position = "none") + theme(axis.text = element_text(size=20)) + theme(axis.title = element_text(size=20,face="bold"))

# ROC 
BM <- list()
### Regresje
BM[["under_reg_log"]] <- table(ifelse(predict(under_reg_log, new = churn_balanced_under, type = "response") > 0.5, 1, 0), churn_balanced_under$churn)
### Drzewa
BM[["under_tree"]] <- table(predict(under_tree, new = churn_balanced_under, type = "class"), churn_balanced_under$churn)
### Las
BM[["under_rf"]] <- table(predict(under_rf, new = churn_balanced_under, type = "class"), churn_balanced_under$churn)

under_model_churn <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy # Misclassification Error Rate
  # inaczej: MER < - (false_positive + false_positive) / sum(classif_mx)
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # inaczej - Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}

lapply(BM, under_model_churn)
sapply(BM, under_model_churn)

# Przeksztalcenia dla modelu III
detach(churn_data)
transformed_churn_data <- churn_data
transformed_churn_data$credit_score <- cut(churn_data$credit_score, #ilorazowa --> porzadkowa
                           breaks=c(350, 450, 550, 650, 750, 850),
                           labels=c('Bad', 'OK', 'Not bad', 'Good', 'Great'))
#jesli same przedzialy
#transformed_churn_data$cat_credit_score <- cut(churn_data$credit_score,
#                                   breaks=c(350, 450, 550, 650, 750, 850))
#unique(transformed_churn_data$credit_score)

transformed_churn_data$country <- as.factor(transformed_churn_data$country) 

transformed_churn_data$gender <- as.factor(transformed_churn_data$gender)

transformed_churn_data$age <- churn_data$age_group
                              

#unique(transformed_churn_data$age)

transformed_churn_data$tenure <- cut(churn_data$tenure, #ilorazowa --> nominalna
                          breaks=c(-1, 1, 5, 10), #-1 zeby wlaczyc tez 0
                          labels=c('Nowy', 'Stały', 'Lojalny'))

transformed_churn_data$balance <- cut(churn_data$balance,
                                      breaks=c(-1, 1, 100000, 150000, 251000),
                                      labels=c('0', '(0, 100000]', '(100000, 150000]', '(150000, 251000]'))
#products_number, credit_card, active_number taki sam
transformed_churn_data$estimated_salary <- cut(churn_data$estimated_salary,
                                      breaks=c(0, 50000, 100000, 150000, 200000),
                                      labels=c('(0, 50000]', '(50000, 100000]', '(100000, 150000]', '(150000, 200000]'))
transformed_churn_data <- transformed_churn_data[, -6] #bez products_number
IV_after <- create_infotables(data = transformed_churn_data, y='churn')
IV_after$Summary

churn_data <- churn_data[, -12]
IV_before <- create_infotables(data = churn_data, y='churn')
IV_before$Summary
#credit_score - gorzej, age - lepiej, tenure - gorzej, balance - lepiej, salary - gorzej
#trzeba porownac macierz pomylek, SE, SP, ACC i wybrac najlepsze grupowania

# Model po przeksztalceniach 
# Podział dla transformed_churn_data
set.seed(36)
test_prop <- 0.25 #'[RED pytanie czy zostawiamy podział zbioru na 0.25?]
transformed.test.set.index <- (runif(nrow(transformed_churn_data)) < test_prop)
transformed.churn.test <- transformed_churn_data[transformed.test.set.index, ]
transformed.churn.train <- transformed_churn_data[!transformed.test.set.index, ]

# I. Model po przekształceniach (bazowy)
# REGRESJA LOGISTYCZNA 
transformed_reg_log <- glm(churn ~ ., 
                    data = transformed.churn.train, 
                    family = binomial
)

summary(transformed_reg_log)

# DRZEWO KLASYFIKACYJNE  
transformed_tree <- rpart(churn ~ .,
                   data = transformed.churn.train,
                   method = "class", minbucket = 100)
transformed_tree_D <- rpart(churn ~ .,
                          data = transformed.churn.train,
                          method = "class")
BAZA2 <- list()
### Regresje
BAZA2[["Do wprowadzenia"]] <- table(predict(transformed_tree_D, new = transformed.churn.test, type = "class"), transformed.churn.test$churn)
### Drzewa
BAZA2[["Po wprowadzeniu"]] <- table(predict(transformed_tree, new = transformed.churn.test, type = "class"), transformed.churn.test$churn)
lapply(BAZA2, base_model_churn)
sapply(BAZA2, base_model_churn)

transformed_tree

plot(transformed_tree)
text(transformed_tree, pretty = TRUE)

rpart.plot(transformed_tree, under = FALSE, tweak = 1.1, fallen.leaves = TRUE)

transformed_tree$variable.importance

transformed.churn.train$churn <- as.character(transformed.churn.train$churn)
transformed.churn.train$churn <- as.factor(transformed.churn.train$churn)

# LASY LOSOWE
transformed_rf <- randomForest(churn ~., 
                        data = transformed.churn.train,
                        na.action = na.roughfix)

# Użyteczne parametry:
# ntree - liczba drzew
# mtry - liczba zmiennych do losowego próbkowania jako kandydaci przy każdym podziale

## Wpływ zmiennych
varImpPlot(transformed_rf)
transformed_rf$importance

# Weryfikacja jakości klasyfikacji -  MACIERZ POMYŁEK  
transformed_confmat <- table(predict(transformed_tree, new = transformed_churn_data, type = "class"), transformed_churn_data$churn)
transformed_confmat
sum(diag(transformed_confmat)/sum(transformed_confmat))
transformed_error <- 1 - sum(diag(transformed_confmat)/sum(transformed_confmat))
transformed_error

# Wizualizacja macierzy pomyłek
ActualOutcomeDT_new <- factor(c(0, 0, 1, 1))
PredictedOutcomeDT_new <- factor(c(0, 1, 0, 1))
Y_new      <- c(7773, 1264, 190, 773)
df_new <- data.frame(ActualOutcomeDT_new, PredictedOutcomeDT_new, Y_new)

ggplot(data =  df_new, mapping = aes(x = ActualOutcomeDT_new, y = PredictedOutcomeDT_new)) +
  geom_tile(aes(fill = Y_new), colour = "black") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1, fontface = 'bold', color = 'white', size=10) +
  scale_fill_gradient(low = "#6D9EC1", high = "#ff8080") +
  theme_bw() + theme(legend.position = "none") + theme(axis.text = element_text(size=20)) + theme(axis.title = element_text(size=20,face="bold"))

# ROC 
RM <- list()
### Regresje
RM[["transformed_reg_log"]] <- table(ifelse(predict(transformed_reg_log, new = transformed.churn.test, type = "response") > 0.5, 1, 0), transformed.churn.test$churn)
### Drzewa
RM[["transformed_tree"]] <- table(predict(transformed_tree, new = transformed.churn.test, type = "class"), transformed.churn.test$churn)
### Las
RM[["transformed_rf"]] <- table(predict(transformed_rf, new = transformed.churn.test, type = "class"), transformed.churn.test$churn)

transformed_model_churn <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy # Misclassification Error Rate
  # inaczej: MER < - (false_positive + false_positive) / sum(classif_mx)
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # inaczej - Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}

lapply(RM, transformed_model_churn)
sapply(RM, transformed_model_churn)

# Najlepszy model #1
preds <- list()
### Regresje
preds[["regresja logistyczna"]] <- as.vector(predict(base_reg_log, newdata = churn.test, type = "response"))
### Drzewa
preds[["drzewo klasyfikacyjne"]] <- as.vector(predict(base_tree, newdata = churn.test)[, 2])
### Las
preds[["las losowy"]] <- as.vector(predict(base_rf, newdata = churn.test, type = "prob")[, 2])

## krzywa ROC (Receiver Operating Characteristic) - potrzebuje "ciaglej" prognozy
plot(performance(prediction(preds[["regresja logistyczna"]], churn.test$churn), "tpr", "fpr"), lwd = 2, colorize = T) 

for (i in 1:length(preds)){
  plot(performance(prediction(preds[[i]], churn.test$churn), "tpr", "fpr"), lwd = 2, colorize = F, col = i, add = ifelse(i == 1, FALSE, TRUE)) 
}

abline(coef = c(0, 1), lty = 2, lwd = 0.5)

legend(0.6, 0.4, 
       legend = names(preds),
       col = 1:length(preds), 
       lty = rep(1, length(preds))
)
title(main = "Krzywe ROC", cex.main = 2)

# AUC (Area Under Curve) - pole pod krzywa ROC
(performance(prediction(preds[["regresja logistyczna"]], churn.test$churn), "auc")@y.values[[1]])

for (i in 1:length(preds)){
  cat(names(preds)[i], ": ", performance(prediction(preds[[i]], churn.test$churn), "auc")@y.values[[1]], "\n")
}

# Lift chart
plot(performance(prediction(preds[["regresja logistyczna"]], churn.test$churn), "lift", "rpp"), lwd = 2, col = "darkblue") 

for (i in 1:length(preds)){
  plot(performance(prediction(preds[[i]], churn.test$churn), "lift", "rpp"), lwd = 2, colorize = F, col = i, lty = i, add = ifelse(i == 1, FALSE, TRUE)) 
}

legend(0.6, 2.4, 
       legend = names(preds),
       col = 1:length(preds), 
       lty = 1:length(preds)
)
title(main = "Lift chart", cex.main = 2)

churn.1 <- rpart(churn~., data = churn.train, method = "class")

prognoza_ciagla <- predict(churn.1, newdata = churn.test)
prognoza_ciagla <- as.vector(prognoza_ciagla[,2])

# krzywa ROC - potrzebuje "ciaglej" prognozy
plot(performance(prediction(prognoza_ciagla,churn.test$churn),"tpr","fpr"),lwd=2, col = "darkblue") 
legend("topleft", inset = .02, legend = "drzewo klasyfikacyjne", col = "darkblue", lty = 1:2, cex=1) 
title(main = "Krzywa ROC", cex.main = 2)
abline(coef = c(0, 1), lty = 2, lwd = 0.5)

# AUC (Area Under Curve) - pole pod krzywa ROC
perf_auc <- performance(prediction(prognoza_ciagla, churn.test$churn),"auc")
perf_auc@y.values[[1]]

# Lift chart
plot(performance(prediction(prognoza_ciagla,churn.test$churn),"lift","rpp"),lwd=3, col = "darkblue") 
legend("topright", inset = .02, legend = "drzewo klasyfikacyjne", col = "darkblue", lty = 1:2, cex=1) 
title(main = "Lift chart", cex.main = 2)
