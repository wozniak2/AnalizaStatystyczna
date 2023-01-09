## ________________________________________________
## ------------Author: M. Wozniak------------------
## ----KURS: Statystyczna analiza danych part I----
## --------------ROK: 2022/2023--------------------
## ________________________________________________


## UWAGA:
## Dla działania skryptu wymagana jest instalacja 
## dodatkowych pakietów, których nazwy znajdują się 
## obok funkcji library() lub require() w kodzie

## instalacja odbywa się z menu 'Tools' --> 'install packages'
## lub bezpośrednio poprzez kod R: 
# install.packages(c("nazwa_pakietu1", "nazwa_pakietu2", "nazwa_pakietu_n"))


## ----------------------------
## ---- STATYSTYKI OPISOWE ----
## ----------------------------


## ------------------------------------
## Histogram
## ------------------------------------

# wczytujemy bibliotekę tidyverse
library(tidyverse)

# informacje o zbiorze "txhousing"
?txhousing
txhousing <- txhousing

# rysujemy histogram
ggplot(txhousing, aes(median)) +
  geom_histogram(col="black", bins=15,
                 alpha = 0.2, fill="purple")

# wyłączenie zapisu naukowego
options(scipen=999)

## -------------------------------------------------------
## wykres pudełkowy (box plot)
## -------------------------------------------------------

#informacje o zbiorze "mtcars"
?mtcars

# informacje o pierwszych 6 wierszach zbioru
head(mtcars)

# rysujemy wykres pudełkowy dla liczby cylindrów i mpg
ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + 
  geom_boxplot(fill="gold", alpha=0.1) + 
  xlab("cyl") +
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))


##------------------------------------------------------
## wykres linowy
##------------------------------------------------------

# wybieramy potrzebne biblioteki 
library(babynames) # ramka danych 'babynames'
library(dplyr)
library(hrbrthemes)
library(viridis)

# baza babynames
?babynames

# Wybieramy 3 imiona żeńskie
imiona <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")

# Wykres
imiona %>%
  ggplot(aes(x=year, y=n, group=name, color=name)) +
  geom_line(size=1.2) +
  geom_point(col="black") +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Popularnosc imion dzieci w USA") +
  ylab("Liczba urodzonych dzieci")


## -----------------------------------------------------
## tablice częstości
## -----------------------------------------------------

# tablica częstości dla cylindrów
library(epiDisplay)

# info o zbiorze "mtcars"
?mtcars
mtcars <- mtcars
head(mtcars)

# tworze tablice częstości + wykres słupkowy dla zmiennej liczba cylindrów
tab1(mtcars$cyl, sort.group = "decreasing", cum.percent = TRUE, 
     col = c("lightblue", "lightgreen", "purple"), 
     main = "Wizualizacja tablicy czestosci dla liczby cylindrow") %>%
  kable()


## ---------------------------------------------------
## Tablice krzyżowe
## ---------------------------------------------------

# wczytuje potrzebne biblioteki
library(pollster)
library(dplyr)
library(knitr)

# informacje o zbiorze "Illinois"
?illinois

# Zbiór danych Illinois - tablica krzyżowa dla zmiennych płeć i rasa
crosstab(df = illinois, x = sex, y = raceethnic, weight = weight) %>%
  kable()

# wizualizacja tablicy krzyżowej dla zmiennych płeć i rasa
crosstab(df = illinois, x = sex, y = raceethnic, weight = weight, format = "long") %>%
  ggplot(aes(x = raceethnic, y = pct, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("rasa") +
  ylab("czestosc w %") +
  labs(title = "Rasa wedlug plci")


## -------------------------------------
## Wariancja i odchylenie standardowe
## -------------------------------------

#info o zbiorze danych ChickWeight
?ChickWeight
ChickWeight <- ChickWeight

# obliczamy wskaźniki
var(ChickWeight$weight) # wariancja

sd(ChickWeight$weight) # odchylenie standardowe

mean(ChickWeight$weight) # średnia arytmetyczna


## ----------------------------
## Skośność i kurtoza
## ----------------------------

## Skośność

# ustawienia rysowania
par(mfrow=c(2,2))
# parametr generatora liczb losowych
set.seed(5)

# generowanie danych + rysunek
x = rnorm(1000, 0,1)
hist(x, main="Zbior x", freq=FALSE)
lines(density(x), col='red', lwd=3)
abline(v = c(mean(x),median(x)),  col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

# generowanie danych + rysunek
y = rexp(1000,1)
hist(y, main="Zbior y", freq=FALSE)
lines(density(y), col='red', lwd=3)
abline(v = c(mean(y),median(y)),  col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

# generowanie danych + rysunek
z= rbeta(10000,5,2)
hist(z, main="Zbior z", freq=FALSE)
lines(density(z), col='red', lwd=3)
abline(v = c(mean(z),median(z)),  col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

# wczytuję biblioteke moments
library(moments)

# obliczam skośność
skewness(x)
skewness(y)
skewness(z)

# obliczam kurtoze
kurtosis(x)
kurtosis(y)
kurtosis(z)

## współczynnik Giniego

# wczytujemy potrzebne biblioteki
library(ineq)
library(tidyverse)

# info o zbiorze
?txhousing
txhousing <- txhousing

# krzywa Lorenza
dev.off()
plot(Lc(txhousing$median),col="darkred",lwd=2)

# indeks Giniego
ineq(txhousing$median,type="Gini")


## ---------------------------------
## Miary zależności i bliskości
## ---------------------------------

## 1) Współczynnik korelacji Persona

# zbiór mtcars
?mtcars
head(mtcars)

# korelacja liniowa
cor(mtcars$hp, mtcars$mpg)

# i rysunek
plot(mtcars$hp, mtcars$mpg, lwd=3)
abline(lm(mpg ~ hp, data = mtcars), col="red", lwd=2)

## -----------------------------------------
## 2) Test chi-kwadrat

# wczytuję potrzebne biblioteki
library(epiDisplay)
library(knitr)

# zbiór Cars93
?Cars93
Cars93 <- Cars93

# wybieram zmienne do testu
car.data <- data.frame(Cars93$AirBags, Cars93$Type)

# potrzebuję tabeli częstości
car.data.tab <- table(car.data)

print(car.data.tab) %>%
  kable()

# obliczam wartość statystyki testowej (korelacji Pearsona)
chisq.test(car.data.tab)

## ------------------------------------------
## 3) Współczynnik korelacji rang Spearmana

cor.test(x=as.numeric(Cars93$AirBags), 
         y=as.numeric(Cars93$Type), 
         method = 'spearman')


## -----------------------------------------
## 4) autokorelacja

# tworzę dane
x <- c(22, 24, 25, 25, 28, 29, 34, 37, 40, 44, 
       51, 48, 47, 50, 51)

# wczytuję potrzebną bibliotekę
library(tseries)

#rysuję wykres autokorelacji dla zmiennej
acf(x)
#obliczam współczynniki autokorelacji
acf(x, pl=FALSE)

## ------------------------------------------
## autokorelacja przestrzenna - I Morana

# wczytuję potrzebne biblioteki
library(raster)
library(sf)
library(spdep)
library(tmap)
library(knitr)

# Wczytuję plik z danymi geoprzestrzennymi
s <- readRDS(url("https://github.com/mgimond/Data/raw/gh-pages/Exercises/nhme.rds"))

# wyłączam notację wykładniczą
options(scipen = 999)

# wyświetlam dane
head(s) %>%
  kable()

# wykres pudełkowy dla dochodu
boxplot(s$Income, horizontal = TRUE)

# rysują mapę dla dla dochodu
tm_shape(s) + tm_fill(col="Income", style="quantile", n=8, palette="Greens") +
  tm_polygons(border.col='grey', alpha = 0.8) +
  tm_legend(outside=F) +
  tm_layout(title = "Dochód w hrabstwach w stanie New Hempshire i Maine")

# tworzę matrycę wag przestrzennych; typ - królowa
nb <- poly2nb(s, queen=TRUE)

# koordynaty do rysunku matrycy wag
coords <- st_coordinates(st_centroid(st_geometry(s)))

# i rysunek matrycy wag przestrznnych
par(mar=c(1,1,1,1))
plot(s$geometry, lwd=1.5)
par(new=TRUE)
plot(nb, coords, lwd=2.5, col = "red")

# przydzielam wagi poszczególnym połączeniem. 
# W tym przypadku niech będą one równe
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[2]

# test I Morana
# H0: dochód jest losowo rozmieszczony w hrabstwach (brak korelacji przestrzennej)
# H1: istnieje korelacja przestrzenna, podobne wartości sąsiadują ze sobą
moran.test(s$Income,lw, alternative="greater")

