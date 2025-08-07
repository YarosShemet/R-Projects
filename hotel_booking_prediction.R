### Yaraslau Shemet ###

#Polskie znaki sa omijane zeby uniknac problemow z kodowaniem

# Ladowanie paczek --------------------------------------------------------

list_of_packages <- c("dplyr", 
                      "ggplot2",
                      "ggpubr", 
                      "stargazer",
                      "reshape2", 
                      "MASS",
                      "rpart", 
                      "rpart.plot",
                      "ROCR", 
                      "randomForest"
                      )

not_installed <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)

lapply(list_of_packages, library, character = TRUE)

# Zrodlo danych -----------------------------------------------------------

### https://www.sciencedirect.com/science/article/pii/S2352340918315191 ###

### Antonio N., de Almeida A., Nunes L. "Hotel booking demand datasets" ###

raw_data <- read.csv("D:/STUDIA/SGH_mag/SEMESTR_1/Prezentacja_wizualizacja/Projekt/hotel_bookings.csv")


# Opis i transformacja danych ----------------------------------------------------

data <- raw_data

#hotel
data$hotel[data$hotel=='Resort Hotel']<-'Resort'
data$hotel[data$hotel=='City Hotel']<-'City'
unique(data$hotel)
data$hotel <- as.factor(data$hotel)

data$is_canceled <- as.factor(data$is_canceled)

#arrival_date
unique(data$arrival_date_year)

length(unique(data$arrival_date_month))==12 #wszystkie 12 miesiecy
data$arrival_date_month <- match(data$arrival_date_month, month.name) #transformacja z wbudowana zmienna 

max(data$arrival_date_week_number) #standardowo rok ma 52 tygodnie, ale jest to mozliwe - ISO 8601
data <- data %>% dplyr::select(-c(arrival_date_week_number))

#stays_nights
sum(data$stays_in_weekend_nights==0 & data$stays_in_week_nights==0 & data$is_canceled==0) #680 obserwacji
#View(data[data$stays_in_weekend_nights==0 & data$stays_in_week_nights==0 & data$is_canceled==0, ])

#adults, children, babies
sum(data$adults==0 & data$children==0 & data$babies==0) #180 obserwacji
#View(data[data$adults==0 & data$children==0 & data$babies==0, ])
data <- data %>% filter(!(data$adults==0 & data$children==0 & data$babies==0))

#meal
unique(data$meal) #UNDEFINED
data$meal <- as.factor(data$meal)

#country
length(unique(data$country)) #178 krajow, NULL
data$country <- as.factor(data$country)

#market segment and distribution channel
unique(data$market_segment) #bardziej wezsze pojecie odpowiadajace charakterystykom goscia, UNDEFINED
data$market_segment <- as.factor(data$market_segment)
unique(data$distribution_channel) 
data <- data %>% dplyr::select(-c(distribution_channel))

#is_repeated
sum(data$is_repeated_guest==0 & (data$previous_cancellations>0 | data$previous_bookings_not_canceled>0)) #6235 obserwacji
data$is_repeated_guest <- as.factor(data$is_repeated_guest)
#View(data[data$is_repeated_guest==0 & (data$previous_cancellations>0 | data$previous_bookings_not_canceled>0), ])
data <- data %>% filter(!(data$is_repeated_guest==0 & (data$previous_cancellations>0 | data$previous_bookings_not_canceled>0)))
data <- data %>% dplyr::select(-c(previous_bookings_not_canceled))

#room_type
sort(unique(data$reserved_room_type))
data$reserved_room_type <- as.factor(data$reserved_room_type)
sum(data$reserved_room_type != data$assigned_room_type)
sum(data$is_canceled==1 & data$reservation_status=='Check-Out') #anulowanie tylko przed zameldowaniem sie, nie ma mozliwosci przerwac pobyt
data <- data %>% dplyr::select(-c(assigned_room_type))

#deposit_type
unique(data$deposit_type)
data$deposit_type <- as.factor(data$deposit_type)

#agent, company
data$agent <- as.numeric(data$agent) #NULL zamienione na NA
length(unique(data$agent)) #334 agentow

data$company <- as.numeric(data$company) #NULL zamienione na NA
length(unique(data$company)) #346 spolek

#customer_type
unique(data$customer_type)
data$customer_type <- as.factor(data$customer_type)

#reservation_status
unique(data$reservation_status)
sum((data$is_canceled==1 & !(data$reservation_status %in% c('No-Show', 'Canceled'))) | (data$is_canceled==0 & data$reservation_status!='Check-Out'))
data <- data %>% dplyr::select(-c(reservation_status, reservation_status_date))

# Braki danych --------------------------------------------------
#ilosciowo
apply(data, 2, anyNA)
sum(is.na(data$children))
cat("% brakow danych - dzieci: ", sum(is.na(data$children))/nrow(data)*100)
data %>% count(children)
data$children[is.na(data$children)] <- 0 #zamiana NA na 0 
sum(is.na(data$agent))
cat("% brakow danych - agenty: ", sum(is.na(data$agent))/nrow(data)*100) #powyzej 5% 
data <- data %>% dplyr::select(-c(agent))
sum(is.na(data$company))
cat("% brakow danych - firmy: ", sum(is.na(data$company))/nrow(data)*100) #powyzej 5%
data <- data %>% dplyr::select(-c(company))
apply(data, 2, anyNA)

#jakosciowo
data %>% count(meal)
1108/nrow(data)*100
data$meal[data$meal=='Undefined'] <- 'SC'
data %>% count(market_segment)
2/nrow(data)*100
data$market_segment[data$market_segment=='Undefined'] <- 'Complementary'
data %>% count(country)
238/nrow(data)*100

data <- na.omit(data)

#duplikaty
duplicates <- data %>%
  group_by_all() %>%
  filter(n()>1) %>%
  ungroup()
#grupy, kazdy gosc traktowany osobnie


# Analiza jednoczynnikowa -----------------------------------------------------------------
options(scipen=999)

count_perc <- function(data, var){ #funkcja tworzy datasety z liczba i procentem kategorii
  data <- data %>% 
    group_by({{var}}) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(perc = n / sum(n))
  return(data)
}
#zmienna objasniana
df_cancel <- count_perc(data, is_canceled)
plot_1 <- ggplot(df_cancel, aes(x=is_canceled, y=n, fill=is_canceled))+
                geom_bar(stat='identity')+
                labs(x="Odwolanie rezerwacji", y='Liczba obserwacji', title = "Rozklad gosci wedlug odwolania rezerwacji")+
                scale_x_discrete(labels=c("Nie", "Tak"))+
                geom_text(aes(label=paste0(round(perc*100, 1), '%'), vjust=-0.5))+
                theme(plot.title = element_text(hjust = 0.5))+
                theme(legend.position="none")
plot_1
#niezbilansowany

#dwie kategorie
df_hotels <- count_perc(data, hotel)
plot_hotels <- ggplot(df_hotels, aes(x=hotel, y=n, fill=hotel))+
  geom_bar(stat='identity')+
  labs(x="Rodzaj hotelu", y='Liczba obserwacji', title = "Rozklad gosci wedlug rodzaju hotelu")+
  scale_x_discrete(labels=c("Miejski", "Wypoczynkowy"))+
  geom_text(aes(label=paste0(round(perc*100, 1), '%'), vjust=-0.5))+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(legend.position="none")

df_repeated <- count_perc(data, is_repeated_guest)
plot_repeated <- ggplot(df_repeated, aes(x=is_repeated_guest, y=n, fill=is_repeated_guest))+
  geom_bar(stat='identity')+
  labs(x="Nowy gosc", y='Liczba obserwacji', title = "Rozklad gosci wedlug powtorzenia sie")+
  scale_x_discrete(labels=c("Tak", "Nie"))+
  geom_text(aes(label=paste0(round(perc*100, 1), '%'), vjust=-0.5))+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(legend.position="none")

plot_2 <- ggarrange(plot_hotels, plot_repeated, ncol=2, nrow=1)
plot_2

#wiecej niz dwie
df_meal <- count_perc(data, meal)
plot_meal <- ggplot(df_meal, aes(x=meal, y=n, fill=meal))+
  geom_bar(stat='identity')+
  labs(x="Wyzywienie", y='Liczba obserwacji', title = "Rozklad posilkow")+
  scale_x_discrete(labels=c("Bed & Breakfast", "Pelne", "Niepelne", "Brak"))+
  geom_text(aes(label=paste0(round(perc*100, 1), '%'), vjust=-0.5))+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(legend.position="none")

df_segment <- count_perc(data, market_segment)
plot_segment <- ggplot(df_segment, aes(x=market_segment, y=n, fill=market_segment))+
  geom_bar(stat='identity')+
  labs(x="Segment", y='Liczba obserwacji', title = "Rozklad gosci wedlug segmentu")+
  scale_x_discrete(labels=c("Lotnicy", "Uzupelniajace", "Korpo", "Bezposrednie", "Grupy", "Biura offline", "Biura online"))+
  geom_text(aes(label=paste0(round(perc*100, 1), '%'), vjust=-0.5))+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  #theme(axis.text.x = element_text(size=9, angle = 15))+
  theme(legend.position="none")

df_room_type <- count_perc(data, reserved_room_type) 
plot_room <- ggplot(df_room_type, aes(x=reserved_room_type, y=n, fill=reserved_room_type))+
  geom_bar(stat='identity')+
  labs(x="Typ pokoja", y='Liczba obserwacji', title = "Rozklad gosci wedlug pokoja")+
  scale_x_discrete(labels=c("A", "B", "C", "D", "E", "F", "G", "H", "L"))+
  geom_text(aes(label=paste0(round(perc*100, 1), '%'), vjust=-0.5), size=3)+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(legend.position="none")

df_deposit <- count_perc(data, deposit_type)
plot_deposit <- ggplot(df_deposit, aes(x=deposit_type, y=n, fill=deposit_type))+
  geom_bar(stat='identity')+
  labs(x="Kaucja", y='Liczba obserwacji', title = "Rozklad gosci wedlug wniesienie kaucji")+
  scale_x_discrete(labels=c("Brak kaucji", 'Nierefundowana', "Refundowana"))+
  geom_text(aes(label=paste0(round(perc*100, 1), '%'), vjust=-0.5))+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(legend.position="none")

df_customer <- count_perc(data, customer_type)
plot_customer <- ggplot(df_customer, aes(x=customer_type, y=n, fill=customer_type))+
  geom_bar(stat='identity')+
  labs(x="Typ rezerwacji", y='Liczba obserwacji', title = "Rozklad gosci wedlug typu rezerwacji")+
  scale_x_discrete(labels=c("Umowa stala", 'Grupy', "Umowa okresowa", "Umowa czesciowo okresowa"))+
  geom_text(aes(label=paste0(round(perc*100, 1), '%'), vjust=-0.5), size=3)+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  #theme(axis.text.x = element_text(size=9, angle=15))+
  theme(legend.position="none")

plot_3 <- ggarrange(plot_meal, plot_segment, plot_room, plot_deposit, plot_customer,  ncol = 3, nrow = 2)
plot_3

#wykres pie dla krajow pochodzenia

#https://r-graph-gallery.com/piechart-ggplot2.html
df_country <- count_perc(data, country)
df_country <- df_country %>% 
  arrange(desc(n)) %>% 
  mutate(country = ifelse(n > 1500, as.character(country), "Other")) %>%  #zeby polaczyc w jedna kategorie oznaczam inne jako Other
  group_by(country) %>%
  summarise(total_n = sum(n), total_perc = sum(perc)) %>% 
  arrange(desc(total_perc)) %>% 
  mutate(ypos = cumsum(total_perc)- 0.5*total_perc)

plot_4 <- ggplot(df_country, aes(x="", y=total_perc, fill=country)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none")+
  geom_text(aes(label = country), color = "white", size=3, position = position_stack(vjust = 0.5))+
  ggtitle("Wykres kolowy krajow pochodzenia gosci")+
  theme(plot.title = element_text(hjust = 0.5))
plot_4

num_data<-select_if(data,is.numeric)
stargazer(as.data.frame(num_data[-c(2,3,4)]), type = "text", summary.stat = c("min", "max", "p25", "median", "p75", "mean", "sd"), title="Statystyki opisowe", digits=1, out="Descriptives.html") #bez dat

ggplot(data, aes(y=adr))+
  geom_boxplot() #outlier
#View(data[data$adr==max(data$adr), ])
plot_adr1 <- ggplot(data[data$adr!=max(data$adr), ], aes(x="", y=adr))+
  geom_boxplot(fill='lightgreen')+
  stat_summary(fun = "mean", geom = "point", shape = "*", size = 6, color = "blue")+
  labs(x='ADR', y='Wartosci', title='Wykres pudelkowy ADR')+
  theme(plot.title = element_text(hjust = 0.5))

#czy adr rozni sie miedzy anulowanymi i zrealizowanymi rezerwacjami?
plot_adr2 <- ggplot(data[data$adr!=max(data$adr), ], aes(x="", y=adr, fill=hotel))+
  geom_boxplot()+
  labs(x='ADR', y='Wartosci')+
  scale_fill_discrete(name = "Rodzaj hotelu", labels = c("Miejski", "Wypoczynkowy"))

ggplot(data, aes(x="", y=lead_time))+
  geom_boxplot()
  
plot_5 <- ggarrange(plot_adr1, plot_adr2, ncol = 2, nrow = 1)
plot_5


count_perc_2 <- function(data, var1, var2){ #funkcja tworzy datasety z liczba gosci grupujac w dwoch wymiarach
  data <- data %>% 
    group_by({{var1}}, {{var2}}) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(perc = n / sum(n))
  return(data)
}

df_years <- count_perc_2(data, arrival_date_year, hotel)
plot_time1 <- ggplot(df_years, aes(x=arrival_date_year, y=n, col=hotel))+
  geom_line()+
  labs(x="Rok", y='Liczba gosci', title = "Liczba gosci w poszczegolnych latach")+
  scale_x_continuous(breaks=seq(2015, 2017))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_discrete(name = "Rodzaj hotelu", labels = c("Miejski", "Wypoczynkowy"))

df_months <- count_perc_2(data, arrival_date_month, hotel)
plot_time2 <- ggplot(df_months, aes(x=arrival_date_month, y=n, col=hotel))+
  geom_line()+
  labs(x="Miesiac", y='Liczba gosci', title = "Liczba gosci w poszczegolnych miesiach")+
  scale_x_continuous(breaks=seq(1,12))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
  

df_days <- count_perc_2(data, arrival_date_day_of_month, hotel)
plot_time3 <- ggplot(df_days, aes(x=arrival_date_day_of_month, y=n, col=hotel))+
  geom_line()+
  labs(x="Dzien w miesiacu", y='Liczba gosci', title = "Liczba gosci w poszczegolnych dniach miesiaca")+
  scale_x_continuous(breaks=seq(1,31))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

plot_6 <- ggarrange(plot_time1, plot_time2, plot_time3,  ncol = 1, nrow = 3)
plot_6


# Analiza wieloczynnikowa -------------------------------------------------

#http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
num_data$is_canceled <- as.numeric(data$is_canceled) #zabieg, zeby dodac Y do zbioru numerycznych
melted_cormat <- melt(cor(num_data[-c(2, 3, 4)])) #bez czasowych
plot_7 <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))+
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Korelacja zmiennych numerycznych")
plot_7

#wplyw lead_time na odwolanie
plot_8 <- ggplot(data, aes(x = lead_time, fill = is_canceled))+
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity")+
  geom_density(aes(color=is_canceled), alpha=0.1)+
  labs(x="Czas realizacji (dni)", y='Gestosc', title = "Histogram czasu realizacji odwolanych i zrealizowanych")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "Odwolanie rezerwacji", labels = c("Nie", "Tak"))+
  scale_colour_discrete(name = "Odwolanie rezerwacji", labels = c("Nie", "Tak"))
plot_8

#czynniki dochodowosci
plot_adr3 <- ggplot(data, aes(x=arrival_date_month, y=adr, fill = hotel)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean")+
  labs(x="Miesiac", y='Srednie ADR', title = "Porownanie dochodowosci w poszczegolnych miesiach")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1,12))+
  scale_fill_discrete(name = "Rodzaj hotelu", labels = c("Miejski", "Wypoczynkowy"))
plot_adr4 <- ggplot(data, aes(x=reserved_room_type, y=adr)) + 
  geom_bar(fill='lightgreen', position = "dodge", stat = "summary", fun = "mean")+
  labs(x="Typ pokoju", y='Srednie ADR', title = "Porownanie dochodowosci wedlug typu pokoja")+
  theme(plot.title = element_text(hjust = 0.5))
plot_9 <- ggarrange(plot_adr3, plot_adr4, ncol=1, nrow=2)
plot_9

#week nights vs hotel i is_canceled (barplot)
df_weekday_1 <- count_perc_2(data, stays_in_week_nights, hotel)
df_weekday_1 <- df_weekday_1 %>% 
  arrange(desc(n)) %>% 
  mutate(stays_in_week_nights = ifelse(stays_in_week_nights < 5, as.character(stays_in_week_nights), "Wiecej niz 4")) %>%  #zeby polaczyc w jedna kategorie oznaczam inne jako Other
  group_by(stays_in_week_nights, hotel) %>%
  summarise(total_n = sum(n), total_perc = sum(perc))
plot_weekday_1 <- ggplot(df_weekday_1, aes(x=stays_in_week_nights, y=total_n, fill=hotel))+
  geom_bar(stat='identity', position='dodge')+
  labs(x="Pobyt w tygodniu (dni)", y='Liczba gosci', title = "Liczba noc w tygodniu wzgledem hoteli")+
  theme_bw()+
  scale_fill_discrete(name = "Rodzaj hotelu", labels = c("Miejski", "Wypoczynkowy"))+
  theme(plot.title = element_text(hjust = 0.5))

df_weekday_2 <- count_perc_2(data, stays_in_week_nights, is_canceled)
df_weekday_2 <- df_weekday_2 %>% 
  arrange(desc(n)) %>% 
  mutate(stays_in_week_nights = ifelse(stays_in_week_nights < 5, as.character(stays_in_week_nights), "Wiecej niz 4")) %>%  #zeby polaczyc w jedna kategorie oznaczam inne jako Other
  group_by(stays_in_week_nights, is_canceled) %>%
  summarise(total_n = sum(n), total_perc = sum(perc))
plot_weekday_2 <- ggplot(df_weekday_2, aes(x=stays_in_week_nights, y=total_n, fill=is_canceled))+
  geom_bar(stat='identity', position='dodge')+
  scale_fill_manual(values = heat.colors(2), name="Odwolanie rezerwacji", labels = c("Nie", "Tak"))+
  labs(x="Pobyt w tygodniu (dni)", y='Liczba gosci', title = "Liczba noc w tygodniu wzgledem odwolania")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

df_weekend_1 <- count_perc_2(data, stays_in_weekend_nights, hotel)
df_weekend_1 <- df_weekend_1 %>% 
  arrange(desc(n)) %>% 
  mutate(stays_in_weekend_nights = ifelse(stays_in_weekend_nights < 5, as.character(stays_in_weekend_nights), "Wiecej niz 4")) %>%  #zeby polaczyc w jedna kategorie oznaczam inne jako Other
  group_by(stays_in_weekend_nights, hotel) %>%
  summarise(total_n = sum(n), total_perc = sum(perc))
plot_weekend_1 <- ggplot(df_weekend_1, aes(x=stays_in_weekend_nights, y=total_n, fill=hotel))+
  geom_bar(stat='identity', position='dodge')+
  labs(x="Pobyt w weekend (dni)", y='Liczba gosci', title = "Liczba noc w weekend wzgledem hoteli")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

df_weekend_2 <- count_perc_2(data, stays_in_weekend_nights, is_canceled)
df_weekend_2 <- df_weekend_2 %>% 
  arrange(desc(n)) %>% 
  mutate(stays_in_weekend_nights = ifelse(stays_in_weekend_nights < 5, as.character(stays_in_weekend_nights), "Wiecej niz 4")) %>%  #zeby polaczyc w jedna kategorie oznaczam inne jako Other
  group_by(stays_in_weekend_nights, is_canceled) %>%
  summarise(total_n = sum(n), total_perc = sum(perc))
plot_weekend_2 <- ggplot(df_weekend_2, aes(x=stays_in_weekend_nights, y=total_n, fill=is_canceled))+
  geom_bar(stat='identity', position='dodge')+
  scale_fill_manual(values = heat.colors(2))+
  labs(x="Pobyt w weekend (dni)", y='Liczba gosci', title = "Liczba noc w weekend wzgledem odwolania")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

plot_10 <- ggarrange(plot_weekday_1, plot_weekday_2, plot_weekend_1, plot_weekend_2, ncol = 2, nrow = 2)
plot_10

#market_segment vs customer type
df_typical_client <- count_perc_2(data, market_segment, customer_type)
supp.labs <- c("Umowa stala", 'Grupy', "Umowa okresowa", "Umowa czesciowo okresowa")
names(supp.labs) <- c("Contract", "Group", "Transient", "Transient-Party")
plot_11 <- ggplot(df_typical_client, aes(x=market_segment, y=n))+
  geom_bar(stat='identity', fill='lightgreen')+
  facet_wrap(~customer_type, scales='free', labeller=labeller(customer_type=supp.labs), ncol=1)+
  labs(x="Segment", y='Liczba obserwacji', title = "Typowy klient hotelu")+
  scale_x_discrete(labels=c("Lotnicy", "Uzupelniajace", "Korpo", "Bezposrednie", "Grupy", "Biura offline", "Biura online"))+
  theme(plot.title = element_text(hjust = 0.5))
plot_11

# Shiny -------------------------------------------------------------------

#przeprowadzona segmentacja na poziomie panstw

#struktura gosci wg kraju
df_hotels_cancellations <- data %>% #dla porownania wartosci z wykresem
  filter(country=='PRT') %>% 
  group_by(hotel, is_canceled) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n))

#ADR w zaleznosci od miesiaca i hotela
df_mean_ADR <- data %>% #dla porownania wartosci z wykresem
  filter(country=='PRT') %>% 
  group_by(arrival_date_month, hotel) %>% 
  summarise(avg_adr = mean(adr))

#market_segment i customer_type
df_market_customer <- data %>% #dla porownania wartosci z wykresem
  filter(country=='PRT') %>% 
  group_by(market_segment, customer_type, is_canceled) %>% 
  count() %>% 
  mutate(perc = n / sum(n))

# Analiza zaawansowana ----------------------------------------------------

data <- data[data$adr!=max(data$adr), ] #outliery wplywaja na oszacowanie regresji
data <- dplyr::select(data, -c(country)) #zbyt duzo panstw (parametrow do szacowania)

data$market_segment <- relevel(data$market_segment, ref = 'Online TA')
data$customer_type <- relevel(data$customer_type, ref = 'Transient')

set.seed(108887)
test_prop <- 0.25
test.set.index <- (runif(nrow(data)) < test_prop)
data.test <- data[test.set.index, ]
data.train <- data[!test.set.index, ]

#Regresja logistyczna 
reg_model <- glm(is_canceled ~ ., 
                  data = data.train, 
                  family = binomial
)
summary(reg_model)
round(exp(coef(reg_model)), 4)

#Wylaczenie nieistotnych
#reg_model_step <- reg_model %>% #dlugie trenowanie, mala zmiana AIC
#  stepAIC(trace = FALSE)
#summary(reg_model_step)

#Drzewo - generowanie regul klasyfikacyjnych
tree_model <- rpart(is_canceled ~ .,
              data = data.train,
              method = "class")
              #control = list(cp=0.03, maxdepth=4))
tree_model$variable.importance
rpart.plot(tree_model, under = FALSE, tweak = 1, fallen.leaves = TRUE)

CM <- list()
CM[["reg_model"]] <- table(ifelse(predict(reg_model, new = data.test, type = "response") > 0.5, 1, 0), data.test$is_canceled)
CM[["tree_model"]] <- table(predict(tree_model, new = data.test, type = "class"), data.test$is_canceled)

EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # inaczej - Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}
sapply(CM, EvaluateModel)

preds <- list()
preds[["reg_model"]] <- as.vector(predict(reg_model, newdata = data.test, type = "response"))
preds[["tree_model"]] <- as.vector(predict(tree_model, newdata = data.test)[, 2])

#AUC
for (i in 1:length(preds)){
  cat(names(preds)[i], ": ", performance(prediction(preds[[i]], data.test$is_canceled), "auc")@y.values[[1]], "\n")
}

#ROC
for (i in 1:length(preds)){
  plot(performance(prediction(preds[[i]], data.test$is_canceled), "tpr", "fpr"), lwd = 2, colorize = F, col = i, add = ifelse(i == 1, FALSE, TRUE)) 
}
abline(coef = c(0, 1), lty = 2, lwd = 0.5)
legend(0.6, 0.4, 
       legend = names(preds),
       col = 1:length(preds), 
       lty = rep(1, length(preds))
)
title(main = "Krzywe ROC", cex.main = 2)




