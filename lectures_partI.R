## ________________________________________________
## ------------Author: M. Wozniak------------------
## -------KURS: Analiza statystyczna (part I)------
## --------------ROK: 2023/2024--------------------
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

# wyłączenie zapisu naukowego
options(scipen=999)

# rysujemy histogram
ggplot(txhousing, aes(median)) +
  geom_histogram(col="black",
                 alpha = 0.2, fill="purple") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))

# Formuła Scotta
3.5 * 25 / 1000^(1/3)

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
# The United States Social Security Administration
?babynames

# Wybieramy 3 imiona żeńskie
imiona <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")

# Wykres
imiona %>%
  ggplot(aes(x=year, y=n, group=name, color=name)) +
  geom_line(size=1.2) +
  geom_point(col="black", alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Popularnosc imion dzieci w USA") +
  ylab("Liczba urodzonych dzieci") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


## -----------------------------------------------------
## tablice częstości
## -----------------------------------------------------

# tablica częstości dla cylindrów
library(epiDisplay)
library(knitr)

# info o zbiorze "mtcars"
?mtcars
mtcars <- mtcars
head(mtcars)

# tworze tablice częstości + wykres słupkowy dla zmiennej liczba cylindrów
tab1(mtcars$cyl, sort.group = "decreasing", cum.percent = TRUE, 
     col = c("lightblue", "lightgreen", "purple"), 
     main = "Wizualizacja tablicy czestosci dla liczby cylindrow") %>%
  kable()

# base R
table(mtcars$cyl)
barplot(table(mtcars$cyl))

## ---------------------------------------------------
## Tablice krzyżowe
## ---------------------------------------------------

# wczytuje potrzebne biblioteki
library(pollster)
library(dplyr)
library(knitr)

# informacje o zbiorze "Illinois"
?illinois
illinois <- illinois

# Zbiór danych Illinois - tablica krzyżowa dla zmiennych płeć i rasa
# tablica dwudzielna
crosstab(df = illinois, x = sex, y = raceethnic, weight = weight) %>%
  kable()


# wizualizacja tablicy krzyżowej dla zmiennych płeć i rasa
crosstab(df = illinois, x = sex, y = raceethnic, weight = weight, 
         format = "long") %>%
  ggplot(aes(x = raceethnic, y = pct, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("rasa") +
  ylab("czestość w %") +
  labs(title = "Rasa według płci")

# inny przykład
# with(airquality,
#     table(Ozone > 80, Month))


# ---------------------------------
# SREDNIA Arytmetyczna
# ---------------------------------

zbior <- c(2,2,3,3,4,4,5,5,6,6)

sum(zbior)/length(zbior)
mean(zbior)

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

## -------------------------------
## współczynnik Giniego
## -------------------------------

# wczytujemy potrzebne biblioteki
library(ineq)
library(tidyverse)

# info o zbiorze
?txhousing
txhousing <- txhousing

# krzywa Lorenza
dev.off()
plot(Lc(txhousing$median),col="darkred",lwd=2, 
     main = "Krzywa Loranza")

# indeks Giniego
ineq(txhousing$median,type="Gini")


## ---------------------------
## Percentyle
## ---------------------------

# wektor 100 losowych liczb od 0 do 500
dane <- runif(100, 0, 500)
print(dane)

# percentyl 32, 57. 98
quantile(dane, c(.32, .50, .98))
median(dane)

## ----------------------------
## Skośność i kurtoza
## ----------------------------

## Skośność

# ustawienia rysowania
par(mfrow=c(2,2))

# generowanie danych + rysunek (base R)
x = rnorm(1000, 0,1)
hist(x, main="Zbior x", freq=FALSE)
lines(density(x), col='red', lwd=3)
abline(v = c(mean(x),median(x)),  
       col=c("green", "blue"), lty=c(2,2), 
       lwd=c(3, 3))

# generowanie danych + rysunek
y = rexp(1000,1)
hist(y, main="Zbior y", freq=FALSE)
lines(density(y), col='red', lwd=3)
abline(v = c(mean(y),median(y)),  
       col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

# generowanie danych + rysunek
z= rbeta(10000,5,2)
hist(z, main="Zbior z", freq=FALSE)
lines(density(z), col='red', lwd=3)
abline(v = c(mean(z),median(z)),
       col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

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


## ---------------------------------
## Miary zależności i bliskości
## ---------------------------------

## -----------------------------------------
## 1) Współczynnik korelacji Persona
## -----------------------------------------

# zbiór mtcars
?mtcars
head(mtcars)
mtcars <- mtcars

# korelacja liniowa
cor(mtcars$hp, mtcars$mpg)

# i rysunek
plot(mtcars$hp, mtcars$mpg, lwd=3)
abline(lm(mpg ~ hp, data = mtcars), col="red", 
       lwd=2)

## -----------------------------------------
## 2) Test chi-kwadrat
## -----------------------------------------

# wczytuję potrzebne biblioteki
library(epiDisplay)
library(knitr)

# zbiór Cars93
?Cars93
Cars93 <- Cars93

# wybieram zmienne do testu
car.data <- data.frame(Cars93$AirBags, 
                       Cars93$Type)

# potrzebuję tabeli krzyżowej
car.data.tab <- table(car.data)

print(car.data.tab) %>%
  kable()

# obliczam wartość statystyki testowej chi-kwadrat
chisq.test(car.data.tab)

## Inny przykład: wymyślone dane w tablicy częstości
dane.wybory <- matrix(c(120, 90, 40, 110, 95, 45), 
                      ncol=3, byrow=TRUE)
colnames(dane.wybory) <- c("PIS","KO","IIIDroga")
rownames(dane.wybory) <- c("Kobieta","Mezczyzna")

# musze miec tabele czestosci do testu
# as.table
dane.wybory <- as.table(dane.wybory)

print(dane.wybory) %>%
  kable()

# obliczam wartość statystyki testowej chi-kwadrat
chisq.test(dane.wybory)


## ------------------------------------------
## 3) Współczynnik korelacji rang Spearmana
## -----------------------------------------

library(epiDisplay)
library(knitr)
# zbiór Cars93
?Cars93
Cars93 <- Cars93
head(Cars93)

# czy istnieje korelacja pomiędzy 
# typem auta a liczbą poduszek powietrznych?
print(car.data.tab) %>%
  kable()

#obliczam wartość statystyki testowej rho Spearmana
cor.test(x=as.numeric(Cars93$AirBags), 
         y=as.numeric(Cars93$Type), 
         method = 'spearman')


# korelacja liniowa i rho Spearmana
cor(mtcars$hp, mtcars$mpg) # Pearson
cor.test(mtcars$hp, mtcars$mpg,  
         method = "spearman")


## -----------------------------------------
## 4) autokorelacja
## -----------------------------------------

# wczytuję potrzebne biblioteki
library(tseries)
require(astsa)

# tworzę dane x i y
x <- c(22, 24, 25, 25, 28, 29, 34, 37, 40, 44, 
       51, 48, 47, 50, 51)
y <- runif(n=100, min=1, max=20)

# rysuję dane
dev.off()
par(mfrow=c(2,1))
plot(x, type="l")
plot(y, type="l")


#rysuję wykres autokorelacji dla zmiennych
acf(x)
acf(y)
#obliczam współczynniki autokorelacji
acf(x, pl=FALSE)
acf(y, pl=FALSE)


# global temperature
dev.off()
?globtemp
plot(globtemp, xlab="Year", ylab="Wahania", 
     main="Wahania temperatury globalnej, 
     1880-2015")
acf(globtemp)
#obliczam współczynniki autokorelacji
acf(globtemp, pl=FALSE)

## ------------------------------------------
## autokorelacja przestrzenna - I Morana
## ------------------------------------------

# wczytuję potrzebne biblioteki
library(raster)
library(sf)
library(spdep)
library(tmap)
library(knitr)

# Wczytuję plik z danymi geoprzestrzennymi
s <- readRDS(url("https://github.com/mgimond/Data/raw/gh-pages/Exercises/nhme.rds"))

# system odniesienia
st_crs(s) = 26919


# wyłączam notację wykładniczą
options(scipen = 999)

# wyświetlam dane
head(s) %>%
  kable()

# kolumna geometry
plot(s$geometry)

# wykres pudełkowy dla dochodu
boxplot(s$Income ~ s$STATE_NAME, horizontal = F)

# rysują mapę dla dla dochodu
tm_shape(s) + tm_fill(col="Income", style="quantile", n=8, palette="Greens") +
  tm_polygons(border.col='black', alpha = 0.8) +
  tm_layout(title = "Dochód w hrabstwach w stanie New Hempshire i Maine", frame = FALSE)

# tworzę matrycę wag przestrzennych; typ - królowa
nb <- poly2nb(s, queen=TRUE)
nb
nb[3] # Kennebec (Maine)

# sąsiedzi hrabstwa Kennebec
s$NAME[c(1,5,7,8,20,24)]

# rysunek: hrabstwa + matryca
par(mar=c(1,1,1,1))
plot(s$geometry, lwd=1.5)

# dodajmy Kennebec
plot(s$geometry[3], add=TRUE, lwd=2.5, col="green")
# i jego sześciu sąsiadów
plot(s$geometry[c(1,5,7,8,20,24)], add=TRUE, lwd=2.5, col="lightblue")

# no i matryca wag przestrzennych dla całości
coords <- st_coordinates(st_centroid(st_geometry(s)))
plot(nb, coords, add=TRUE, lwd=2.5, col = "red")

# przydzielam wagi poszczególnym połączeniem. 
# W tym przypadku niech będą one równe
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# Jakie wagi będą mieć linki dla Kennebec?
lw$weights[3]

# test I Morana
# H0: dochód jest losowo rozmieszczony w hrabstwach (brak korelacji)
# H1: istnieje korelacja przestrzenna, podobne wartości sąsiadują ze sobą
moran.test(s$Income, lw)

