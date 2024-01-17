## ________________________________________________
## ------------Author: M. Wozniak------------------
## ----KURS: Statystyczna analiza danych part II---
## --------------ROK: 2023/2024--------------------
## ________________________________________________

## UWAGA:
## Dla działania skryptu wymagana jest instalacja 
## dodatkowych pakietów, których nazwy znajdują się 
## obok funkcji library() lub require() w kodzie

## instalacja odbywa się z menu 'Tools' --> 'install packages'
## lub bezpośrednio poprzez kod R: 
# install.packages(c("nazwa_pakietu1", "nazwa_pakietu2", "nazwa_pakietu_n"))


## ------------------------------------
## Generator liczb losowych - przykład
## ------------------------------------

# losowanie liczb z rozkładu jednostajnego
library(purrr)
rdunif(10, a=0, b=10)

# definiujemy początkową wartość dla generatora
# liczb losowych

# set.seed() to ustawianie tzw. "ziarna generatora"
# po inicjalizacji tym samym ziarnem zwraca ten sam ciąg
set.seed(123)
rdunif(10, a=0, b=10)


## ----------------------------
## Transformacja logarytmiczna
## ----------------------------

# wczytuję potrzebne biblioteki
library(dplyr)
library(ggplot2)
library(moments)
library(MASS)
library(patchwork)

# baza danych mammals
?mammals
mammals <- mammals

# histogram dla zmiennej "masa ciała" i "masa mózgu"
p1 <- ggplot(data = mammals, aes(body)) + 
  geom_histogram(col="black", fill="red", alpha=0.5)
p2 <- ggplot(data = mammals, aes(brain)) + 
  geom_histogram(col="black", fill="red", alpha=0.5)

p1+p2

# skośność
skewness(mammals$body)
skewness(mammals$brain)

# wykres rozrzutu dla zmiennych przed log-transformacją 
ggplot(mammals, aes(body, brain)) + geom_point(size=2)


# histogram dla zmiennej "masa ciała" i "masa mózgu" 
# po log-transformacji

p3 <- ggplot(data = mammals, aes(log(body))) + 
  geom_histogram(col="black", fill="red", alpha=0.5)
p4 <- ggplot(data = mammals, aes(log(brain))) + 
  geom_histogram(col="black", fill="red", alpha=0.5)

p3+p4

# skośność
skewness(log(mammals$body))
skewness(log(mammals$brain))

# wykres rozrzutu dla zmiennych po log-transformacji
ggplot(mammals, aes(log(body), log(brain))) + 
  geom_point(size=2)

## -----------------------
## Normalizacja zmiennych
## -----------------------

# normalizacja min-max

# zbiór danych iris (Fisher)
?iris
iris <- iris

# Pierwsze sześć wierszy zbioru
head(iris)

# tworzę funkcję do normalizacji min-max
minmaxnormalize <- function(x)
  { (x - min(x)) / (max(x) - min(x)) }

# rezultaty normalizacji min - max (pierwsze sześć wierszy)
head(minmaxnormalize(iris[,1:4]))

## -------------------------------
## Standaryzacja (z-transformacja)
## -------------------------------

# używam wbudowanej funkcji "scale" na zbiorze iris
head(scale(iris[,1:4]))


## -------------------------------
## Metody klasteryzacji zmiennych
## -------------------------------

# algorytm k-średnich (kmeans)

# wczytuj dane
?USArrests
df <- USArrests

# czyszczenie danych (usuwanie braków danych)
df <- na.omit(df)
head(df)

# transformacja zmiennych
# muszą być numeryczne i wystandaryzowane
df <- scale(df)
head(df)

# wczytuję potrzebne biblioteki
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# klasteryzacja w oparciu o 2 centroidy
k2 <- kmeans(df, centers = 2, nstart = 25)

# wizualizacja
fviz_cluster(k2, data = df)

# klasteryzacja w oparciu o 3,4 i 5 centroidów 
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)
k4
# wizualizacja
p1 <- fviz_cluster(k2, geom = "point", data = df) + 
  ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + 
  ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + 
  ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + 
  ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

## -------------------------
## Drzewo klasyfikacyjne
## -------------------------

# wykorzystamy te same dane co w przypadku algorytmu k-średnich
# macierz dystansu euklidesowego dla obserwacji
d <- dist(df, method = "euclidean")

# metoda Warda (minimalizacja wariancji)
hc5 <- hclust(d, method = "ward.D2" )

# Przycinamy drzewo dla 4 skupień
sub_grp <- cutree(hc5, k = 4)

# wizualizacja rezultatów
plot(hc5, cex = 0.5)
rect.hclust(hc5, k = 4, border = 2:5)

## -------
## DBSCAN
## -------

# wcztuję niezbędne pakiety
library(factoextra)
library(dbscan)
library(fpc)

# wczytuję dane
data("multishapes")
df <- multishapes[, 1:2]

# algorytm DBSCAN
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)

# Wizualizacja rezultatów DBSCAN
plot(db, df, main = "DBSCAN", frame = FALSE, lwd=2)


## -------------------
## Uczenie maszynowe
## -------------------

## UWAGA: wymaga dodatkowych danych znajdujących się w przesłanym pakiecie
## a także określenie ścieżki dostępu do nich w zaznaczonych fragmentach
## kodu poniżej (TODO)

library(terra)

# ---------------------------
# ETAP 1 - pozyskanie danych
# ---------------------------

# wczytuje dane rastrowe
# TODO: adjust your path!
files = list.files("c:/dane/dane/landsat", 
                   pattern = "\\.TIF$", 
                   full.names = TRUE)
landsat = rast(files)
names(landsat) = paste0("B", 1:7)

# TODO: adjust your path!
poly = vect("c:/dane/dane/powiat_sremski.gpkg")

# TODO: adjust your path!
# wczytuje informacje o klasach pokrycia terenu
cat = rast("c:/dane/dane/S2GLC_T33UXT.tif")
leg = read.csv("c:/dane/dane/S2GLC_T33UXT.csv")


## ETAP 2 - przygotowanie danych do analizy

# przycinanie i maskowanie
cat = resample(cat, landsat, method = "near")
landsat = crop(landsat, poly, mask = TRUE)
cat = crop(cat, poly, mask = TRUE)

# skalowanie danych
landsat = landsat * 2.75e-05 - 0.2

# usunięcie wartości odstających
landsat[landsat < 0] = NA
landsat[landsat > 1] = NA

# połączenie kolumn w macierzy (piksele z kategoriami)
# usuniecie braków danych
data = cbind(values(cat), values(landsat)) 
data = as.data.frame(data)
data = na.omit(data)

# usuń piksele reprezentujące klasę chmury
# ! to operator negacji (NOT)
data = data[!data$S2GLC_T33UXT == 0, ]
data = merge(data, leg[, -2], by.x = "S2GLC_T33UXT", 
             by.y = "ID")
data = data[, -1]
colnames(data)[8] = "klasa"
data$klasa = as.factor(data$klasa) 

# wizualizacja klasy pokrycia terenu i landsat
plot(cat)
plotRGB(landsat, r = 4, g = 3, b = 2, scale = 1, 
        stretch = "lin")


# ETAP 3 i 4 - wybór i trenowanie

library("rpart") # model ML
library("rpart.plot") # wizualizacja modelu

# zbiór treningowy
# wielkość próby 70%
n = round(0.7 * nrow(data))

# wylosuj indeksy
trainIndex = sample(nrow(data), size = n) 
# wybierz próbki treningowe
train = data[trainIndex, ] 

# algorytm treningowy
mdl = rpart(klasa ~ ., data = train, method = "class")
prp(mdl, cex=0.6)

# klasyfikacja klas terenu dla danych satelitarnych
pred_map = predict(landsat, mdl, type = "class", 
                   na.rm = TRUE)

# ETAP 5 - ewaluacja
# porównanie klas rzeczywistych z tym co 
# zwrócił model

par(mfrow = c(1,2))
lv = droplevels(pred_map)
lv = levels(lv)
lv = lv[[1]][["class"]] 
col_idx = match(lv, leg$Klasa) 
plot(pred_map, main = "Predykcja klas", 
     col = leg$RGB[col_idx])


levels(cat) = leg[, c(1, 3)]
lv = levels(droplevels(cat))[[1]][["Klasa"]]
col_idx = match(lv, leg$Klasa)
plot(cat, col = leg$RGB[col_idx], 
     main = "Rzeczywiste klasy")


## -----------------------
## Analiza parametryczna
## -----------------------

## -----------------------------
## ---- t-test (dla 2 grup) ----
## -----------------------------

# Załóżmy na przykład, że zmierzyliśmy wagę 18 osób: 
# 9 kobiet (grupa A) i 9 mężczyzn (grupa B). 
# Chcemy wiedzieć, czy średnia waga kobiet (mA) różni 
# się istotnie od wagi mężczyzn (mB).


# Dane w wektorach
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 
                  64.6, 48.4, 48.8, 48.5)

men_weight <- c(67.8, 60, 63.4, 76, 89.4, 
                73.3, 67.3, 61.3, 62.4) 
# Ramka danych
my_data <- NULL
my_data <- data.frame( 
  group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight,  men_weight)
)

# rysunek waga w zależności od grupy (K/M)
library("ggpubr")

ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", 
                                      "#E7B800"),
          ylab = "Weight", xlab = "Groups")

# sprawdzmy normalność rozkładu
ggqqplot(my_data, x = "weight")

# Obliczamy t-test
res <- t.test(women_weight, men_weight)
res

# Wartość p testu wynosi 0.015, czyli jest mniejsza 
# od poziomu istotności (0.05). 
# Możemy więc stwierdzić, że średnia waga mężczyzn 
# różni się istotnie od średniej wagi kobiet

## ----------------------------------------
## ---- ANOVA (dla 3 lub więcej grup) -----
## ----------------------------------------

### ANOVA - wariant jednoczynnikowy ###
?PlantGrowth
data <- PlantGrowth

# sprawdźmy podstawowe założenia
# normalność rozkładu - porównujemy dane empiryczne
# z losowymi liczbami z teoretycznego rozkładu normalnego 
library("ggpubr")
ggqqplot(data, x = "weight")

# wizualizacja + obserwacje odstające
ggboxplot(data, x = "group", y = "weight",
          color = "group", palette = 
            c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Waga rośliny", xlab = "Warunki")

# test ANOVA
res.aov <- aov(weight ~ group, data = data)

# podsumowanie wyników
summary(res.aov)

# Wartość p testu wynosi 0.016, czyli jest mniejsza 
# od poziomu istotności (0.05). 
# Możemy więc stwierdzić, że średnia waga roślin różni się 
# istotnie od grupy


### ANOVA - wariant dwuczynnikowy ###

data("jobsatisfaction", package = "datarium")

ggqqplot(jobsatisfaction, x = "score")

ggboxplot(
  jobsatisfaction, x = "gender", y = "score",
  color = "education_level", palette = "jco"
)

res.aov2 <- aov(score ~ gender + education_level, 
                data = jobsatisfaction)
summary(res.aov2)


## ------------------------------------- ##
## -----------Regresja prosta----------- ##
## ------------------------------------- ##

library(ggpubr)

# Wykorzystamy zbiór danych marketingowych 
# [pakiet datarium]. 
# Zawiera on dane dot trzech mediów reklamowych 
# (youtube, facebook i gazeta) razem z wielkością sprzedaży. 

# Load the data
data("marketing", package = "datarium")

# Budujemy model do przewidywania sprzedaży na podstawie 
# budżetu reklamowego wydanego na reklamę w youtube
# tzw. wykres kwantyl-kwantyl
ggqqplot(marketing, x = "sales")
ggqqplot(marketing, x = "youtube")
hist(marketing$youtube)


# budujemy model liniowy (linear model)
model <- lm(sales ~ youtube, data = marketing)
model

# Przed zastosowaniem oszacowanego modelu do 
# przewidywania 
# przyszłej sprzedaży musimy upewnić się, że model 
# ten jest 
# statystycznie istotny, tzn:
# - istnieje związek pomiędzy zmienną zależną i niezależną
# - zbudowany model dobrze pasuje do naszych danych

# zacznijmy od przyjrzenia sie składnikom resztowym równania
library(broom)
model.diag.metrics <- augment(model)
ggplot(model.diag.metrics, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = youtube, yend = .fitted), 
               color = "red", size = 0.3)

# sprawdźmy pozostałe informacje o modelu
options(scipen=999)
summary(model)


## -------------------------------------------------------------------------

# Widzimy 6 składników tabeli wynikowej, w tym:
  
# Call. Pokazuje wywołanie funkcji użytej do 
# obliczenia modelu regresji.

# Reszty (Residuals). Zapewnia podgląd rozkładu reszt, 
# które z definicji mają średnią zero. 
# Dlatego mediana nie powinna być daleka od zera, 
# a minimum i maksimum powinny mieć mniej 
# więcej równą wartość bezwzględną.

# Współczynniki (Coefficients). Pokazuje współczynniki 
# beta regresji i ich istotność statystyczną. 
# Zmienne predykcyjne, które są istotnie związane 
# ze zmienną wynikową, są oznaczone gwiazdkami.

# Resztkowy błąd standardowy (RSE), R-kwadrat (R2) i 
# statystyka F są metrykami, które są używane do 
# sprawdzenia, jak dobrze model pasuje do naszych danych.

## ---------------------------------------------------------------------------

# Kluczowa w interpretacji modelu jest tabela 
# "Coefficients". Zawiera ona informacje dotyczące 
# potencjalnego błędu oszacowania,
# a także parametry mówiące o istotności zmiennych 
# (statystykę t i wartość p)
# im mniejsza wartość p, a większa t, tym 
# istotność jest większa


# H0: współczynniki są równe zero 
# (tzn. nie ma związku między x i y)
# H1: współczynniki nie są równe zeru 
# (tzn. istnieje jakaś zależność między x i y)
# Jeżeli wartość p < 0.05 odrzucamy H0 i przyjmujemy H1

## ---------------------------------------------------------------------------

# Dalsza diagnostyka modelu obejmuje analizę 
# dopasowania:

# Błąd standardowy reszt modelu (RSE)
# Współczynnik R2 i skorygowany R2 
# (zawiera się w przedziale 0-1)
# Statystyka F (ogólna istotność modelu)

## ---------------------------------------------------------------------------

# prognoza na podstawie oszacowanego modelu
newdata = data.frame(youtube=c(15, 30, 300))
predict(model, newdata)

## ------------------------------ ##
## ------Regresja wieloraka------ ##
## ------------------------------ ##

# Chcemy zbudować model szacujący sprzedaż na podstawie 
# budżetu reklamowego zainwestowanego w youtube, 
# facebook i gazetę, jak poniżej:


model2 <- lm(sales ~ youtube + facebook + newspaper, 
             data = marketing)
summary(model2)


# Interpretacja przebiega podobnie jak w przypadku 
# regresji prostej
# Zacznijmy od sprawdzenia dopasowania modelu 
# (statystyka F, R2 i p wartość)
# oraz wartości współczynników

# Widać, że zmiany w budżecie reklamowym 
# youtube i facebook są istotnie związane 
# ze zmianami w sprzedaży, natomiast zmiany 
# w budżecie gazetowym nie są istotnie związane 
# ze sprzedażą.

model3  <- lm(sales ~ youtube + facebook, data = marketing)
summary(model3)

# W modelu z youtube i facebook, skorygowany R2 = 0.89, 
# co oznacza, że 89% wariancji sprzedaży można 
# przewidzieć na podstawie budżetów reklamowych youtube i facebook.


## -------------------------------- ##
## ------Regresja logistyczna------ ##
## -------------------------------- ##

# Pakiety
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs

# Wczytujemy dane (potrzebny pakiet ISLR)
default <- as_tibble(ISLR::Default)

# próbujemy zaklasyfikować klienta do grupy 
# wysokiego lub niskiego ryzyka nie spłacania pożyczki 
# na podstawie salda zadłużenia

# podzielimy nasze dane na treningowe (60%) i testowe (40%), 
# abyśmy mogli ocenić, jak dobrze nasz model radzi sobie na 
# zbiorze danych poza próbą.
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(default), 
                 replace = T, prob = c(0.6, 0.4))

# zbiór "treningowy"
train <- default[sample, ]
# zbiór "testowy"
test <- default[!sample, ]



## ------------------------------------
## -- Regresja logistyczna prosta -----
## ------------------------------------

# Funkcja glm dopasowuje uogólnione modele liniowe, 
# klasę modeli, która obejmuje regresję logistyczną. 
# Składnia funkcji glm jest podobna do składni lm, 
# z wyjątkiem tego, że musimy przekazać argument 
# "rodzina = dwumianowy", aby uruchomić regresję 
# logistyczną, a nie inny typ uogólnionego modelu 
# liniowego.

# bycie dłużnikiem na podstawie pozostałego 
# salda do spłaty
model1 <- glm(default ~ balance, 
              family = "binomial", data = train)


# staramy się znaleźć takie β, że wstawienie 
# tych szacunków do modelu da da liczbę 
# bliską jedności dla wszystkich osób, które wywiązały 
# się ze zobowiązań i liczbę bliską zeru dla wszystkich osób, 
# które tego nie zrobiły.

# narysujmy dopasowaną funkcję
default %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Dopasowany model regresji logistycznej") +
  xlab("Saldo zadłużenia") +
  ylab("Prawdopodobieństwo zaprzestania spłat")

# dalsza diagnostyka modelu
options(scipen = 999)
summary(model1)

# Podobnie jak w przypadku regresji liniowej 
# możemy ocenić model używając podsumowania 
# (podobne do tego w lm); 
# Jednak szczegóły dotyczące dobroci 
# dopasowania różnią się.

# Odchylenie rezydualne jest analogiczne do 
# sumy kwadratów reszt w regresji liniowej 
# i jest miarą braku dopasowania do danych. 
# Celem jest, aby  odchylenie rezydualne 
# było jak najniższe; 

# Po oszacowaniu współczynników można w prosty 
# sposób obliczyć prawdopodobieństwo 
# niewywiązania się z płatności 
# dla dowolnego salda karty kredytowej. 


# Model 2. bycie dłużnikiem na podstawie 
# bycie studentem
model2 <- glm(default ~ student, 
              family = "binomial", 
              data = train)
summary(model2)

# Współczynnik związany ze zmienną 
# "student = tak" jest dodatni, a związana 
# z nim wartość p jest statystycznie istotna. 
# Wskazuje to, że studenci mają wyższe 
# prawdopodobieństwo niewykonania 
# zobowiązania niż osoby niebędące studentami.


## ----------------------------------------
## ---- Regresja logistyczna wieloraka ----
## ----------------------------------------

# dopasujmy model, który przewiduje 
# prawdopodobieństwo braku spłaty w oparciu 
# o zmienne dotyczące salda, dochodu 
# (w tysiącach dolarów) i statusu studenta.

model3 <- glm(default ~ balance + student + 
                income, 
              family = "binomial", 
              data = train)

summary(model3)

# współczynnik dla zmiennej student jest 
# ujemny, co wskazuje, że studenci mają 
# mniejsze prawdopodobieństwo niewywiązania 
# się ze zobowiązań niż osoby niebędące 
# studentami. Z kolei współczynnik dla zmiennej
# student w modelu 2, wskazywał, że studenci 
#cmają większe prawdopodobieństwo 
# niewywiązania się ze zobowiązań. Dlaczego?

boxplot(balance~student, data = default)

# Studenci mają tendencję do posiadania 
# wyższego poziomu zadłużenia, co wiąże się 
# z wyższym prawdopodobieństwem 
# niewywiązania się z płatności.

# Jednak prawdopodobieństwo niewywiązania 
# się ze zobowiązań przez pojedynczego studenta
# z danym saldem na karcie kredytowej 
# jest niższe niż w przypadku osoby niebędącej 
# studentem z takim samym saldem na karcie kredytowej.

# Student jest więc mniej ryzykowny niż osoba 
# niebędąca studentem 
# z takim samym saldem na karcie kredytowej!

## -------------------------------------------
## Ocena dopasowania i zdolności predykcyjnych
## -------------------------------------------

# wskaźnik dopasowania pseudo-R2 (0-1, powyżej 0.4 b.dobre dopasowanie)
# wymagany pakiet "pscl"
pscl::pR2(model1)["McFadden"]
pscl::pR2(model2)["McFadden"]
pscl::pR2(model3)["McFadden"]

# predykcja na zbiorze testowym
test.predicted.m1 <- predict(model1, 
                             newdata = test, 
                             type = "response")
test.predicted.m2 <- predict(model2, 
                             newdata = test, 
                             type = "response")
test.predicted.m3 <- predict(model3, 
                             newdata = test, 
                             type = "response")

# sprawdzamy trafność przewidywań modeli 1- 3 na zbiorze testowym
list(
  model1 = table(test$default, test.predicted.m1 > 0.5) 
  %>% prop.table() %>% round(3),
  model2 = table(test$default, test.predicted.m2 > 0.5) 
  %>% prop.table() %>% round(3),
  model3 = table(test$default, test.predicted.m3 > 0.5) 
  %>% prop.table() %>% round(3)
)

# prawdziwy pozytywny (prawy dolny kwadrant): 
# są to przypadki, w których przewidzieliśmy, 
# że klient nie wywiąże się z umowy i tak się 
# stało.

# prawdziwy negatywy (górny lewy kwadrant): 
# Przewidywaliśmy brak zaległości i klient 
# nie zalegał

# fałszywy pozytywy (prawy górny kwadrant): 
# Przewidywaliśmy, że będzie zalegał, 
# ale w rzeczywistości nie zalegał 
# (Znany również jako "błąd typu I").

# Fałszywy negatywy (dolny lewy kwadrant): 
# Przewidywaliśmy, że nie będzie zalegał, 
# ale nie wywiązali się z umowy. 
# (Znany również jako "błąd typu II").


# Wyniki pokazują, że model1 i model3 są 
# bardzo podobne. 
# 96% przewidywanych obserwacji 
# to prawdziwe negatywy, 
# a około 1% to prawdziwe pozytywy.

# Oba modele mają 
# błąd typu II mniejszy niż 3%; 
# Oba modele mają błąd typu I mniejszy niż 1%;

# Model 2 dokładnie przewiduje klientów, 
# którzy nie wywiązują się 
# ze swoich zobowiązań (97%), ale nigdy 
# nie przewiduje klientów, 
# którzy nie wywiązują się ze swoich zobowiązań


## -----------------------------------------------------------------------------


## ------------------------------------
## ---- Analiza szeregów czasowych ----
## ------------------------------------

#  Dane dotyczące cen głównych indeksów EU
?EuStockMarkets
EuStockMarkets <- EuStockMarkets

# zajmiemy się indeksem DAX (pierwsza kolumna)
tsData <- EuStockMarkets[, 1]

# dekompozycja szeregu; funkcja "decompose" 
# wpisz type = "additive" dla komponentów
# addytywnych

decomposedRes <- decompose(tsData, type="mult")

# rysunek komponentów
plot(decomposedRes)


## -------------------
## --- Model ARIMA ---
## -------------------

library(forecast)
# Air Passengers’ Dataset
?AirPassengers
data("AirPassengers")

# tworzymy szereg czasowy; dane są częstotliwości miesięcznej,
# stąd "frequency=12"
tsdata <- ts(AirPassengers, frequency = 12) 

# dekompozycja
ddata <- decompose(tsdata, "multiplicative")
plot(ddata)

# dopasowujemy model ARIMA przy pomocy
# funkcji "auto.arima"
ARIMA.model <- auto.arima(AirPassengers)
summary(ARIMA.model)

# prognoza dla kolejnych 120 miesięcy
ARIMA.forecast <- forecast(ARIMA.model, h=120)

# rysunek dla danych empircznych i prognozowanych
# wraz z przedziałem błędów
plot(ARIMA.forecast,main=
"Prognoza liczby pasażerów ARIMA(2,1,1)")

# diagnostyka modelu może obejmować zestawienie błędów
# predykcyjnych (Mean Error, Root Mean Square Error, 
# Mean Percentage Error, Mean Absolute Percentage Error)
# oraz ocenę istotności modelu (test Boxa-Ljunga)









