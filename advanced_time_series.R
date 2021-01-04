# Wstęp 

# Celem badania jest analiza porównawcza ryzyka rozumianego jako oszacowanie funkcji warunkowej wariancji w modelach klasy GARCH. 
# Zbudowano portfel składający się z czterech kryptowalut, w którym każdego dnia udziały procentowe każdej z nich są proporcjonalne 
# do ich kapitalizacji rynkowej oraz przeprowadzono analizę porównawczą oszacowań funkcji warunkowej wariancji w okresie in-sample oraz 
# oszacowań wartości narażonej na ryzyko uzyskanych za pomocą rozpatrywanych modeli GARCH w okresie out-of-sample. 

# Dane modelu

# Dane potrzebne do przeprowadzenia badania pochodzą z serwisu https://coinmarketcap.com/. 
# Wybrano do analizy notowania kursów: tron, cosmos, neo, waves, nano. 
# Próba pochodzi z okresu od stycznia do sierpnia.

# Import i wstępna obróbka danych

library(lmtest)
library(xts)
library(RCurl)
library(rlist)
library(urca)
library(fBasics)
library(dygraphs)
library(tseries)
library(car)
library(rugarch)
library(fGarch)
library(timeSeries)
library.path <- .libPaths("/Library/Frameworks/R.framework/Versions/3.6/Resources/library")


scrapData <- function(x){
  
  # function to scrap the OHLC data from 
  # www.coinmarketcap.com
  
  # load packages
  library(XML)
  library(RCurl)
  library(rlist)
  
  # set locale to English
  Sys.setlocale("LC_TIME", "C")
  
  # set url
  theurl <- getURL(
    paste0("https://coinmarketcap.com/currencies/", 
           x, 
           "/historical-data/?start=20130428&end=21000101"),
    .opts = list(ssl.verifypeer = FALSE))
  
  # read html source
  tables <- readHTMLTable(theurl, stringsAsFactors = FALSE)
  
  # clean the list object
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  
  # unlist the list
  table <- tables[[1]]
  
  # rename variables
  table$Open <- table$`Open*`
  table$`Open*` <- NULL
  table$Close <- table$`Close**`
  table$`Close*` <- NULL
  
  # convert characters to numericals
  table$Open      <- as.numeric(gsub(",", "", table$Open))
  table$High      <- as.numeric(gsub(",", "", table$High))
  table$Low       <- as.numeric(gsub(",", "", table$Low))
  table$Close     <- as.numeric(gsub(",", "", table$Close))
  table$Volume    <- as.numeric(gsub(",", "", table$Volume))
  table$MarketCap <- as.numeric(gsub(",", "", table[, "Market Cap"]))
  table[, "Market Cap"] <- NULL
  
  # convert the date from chr into Date
  table$Date <- as.Date(table$Date, format = "%b %d, %Y")
  
  # sort the data  
  table <- table[order(as.numeric(table$Date)), ]
  table <- table[, c("Date", "Open", "High", "Low", "Close", 
                     "Volume",  "MarketCap")]
  
  return(table)
}

####################################################################################################

# function call
library(tidyverse)

# tron, neo, waves, nano: 
tron <- scrapData("tron")
summary(tron)
tron %>% as.tibble %>% tail

neo <- scrapData("neo")
summary(neo)
neo %>% as.tibble %>% tail

waves <- scrapData("waves")
summary(waves)
waves %>% as.tibble %>% tail

nano <- scrapData("nano")
summary(nano)
nano %>% as.tibble %>% tail

###################################################################################################

tron$Date<- as.Date(tron$Date)
tron <- tron[, c("Date", "Close")]
colnames(tron) <- c("Date", "tron_closure")

neo$Date<- as.Date(neo$Date)
neo <- neo[, c("Date", "Close")]
colnames(neo) <- c("Date", "neo_closure")

waves$Date<- as.Date(waves$Date)
waves <- waves[, c("Date", "Close")]
colnames(waves) <- c("Date", "waves_closure")

nano$Date<- as.Date(nano$Date)
nano <- nano[, c("Date", "Close")]
colnames(nano) <- c("Date", "nano_closure")

tron <- tron[tron$Date >= "2019-01-01", ]
neo <- neo[neo$Date >= "2019-01-01", ]
waves <- waves[waves$Date >= "2019-01-01", ]
nano <- nano[nano$Date >= "2019-01-01", ]

library(xts)
tron$rtron <- diff.xts(log(tron$tron))
neo$rneo <- diff.xts(log(neo$neo))
waves$rwaves <- diff.xts(log(waves$waves))
nano$rnano <- diff.xts(log(nano$nano))

# Następnym krokiem było utworzenie portfela oraz wstępna analiza zgromadzonych danych.

df <- data.frame(tron$Date, tron$rtron, neo$rneo, waves$rwaves, nano$rnano)
colnames(df) <- c("Date", "tron", "neo", "waves", "nano")


df$portfel <- rowSums(df[,c(2:5)])
portfel <- df[,c(1,6)]

colnames(portfel) <- c("Date", "zwroty")

# 1. Wykres gęstości zwrotów poszczególnych kryptowalut:

#install.packages("reshape",repos = "http://cran.us.r-project.org")
library(reshape)
molten.data <- melt(df,
                    id = c("Date"))
ggplot(molten.data,
       aes(x = value, fill = variable)) +
  geom_density(alpha = .3) +
  xlab ("") + ylab("gęstość") +
  ggtitle("Gęstości zwrotów poszczególnych kryptowalut")

# 2. Badanie niestacjonarność poszczególnych szeregów czasowych.

# tron, neo, waves, nano 

par(mfrow = c(2, 1))
plot(tron$Date, tron$rtron,
     type = "l", col="red", lwd = 1,
     main = "zwroty TRON")
plot(tron$Date, tron$tron,
     type = "l", col = "black", lwd = 1,
     main = "notowania TRON")
par(mfrow = c(1, 1))

par(mfrow = c(2, 1))
plot(neo$Date, neo$rneo,
     type = "l", col="red", lwd = 1,
     main = "zwroty NEO")
plot(neo$Date, neo$neo,
     type = "l", col = "black", lwd = 1,
     main = "notowania NEO")
par(mfrow = c(1, 1))

par(mfrow = c(2, 1))
plot(waves$Date, waves$rwaves,
     type = "l", col="red", lwd = 1,
     main = "zwroty WAVES")
plot(waves$Date, waves$waves,
     type = "l", col = "black", lwd = 1,
     main = "notowania WAVES")
par(mfrow = c(1, 1))

par(mfrow = c(2, 1))
plot(nano$Date, nano$rnano,
     type = "l", col="red", lwd = 1,
     main = "zwroty NANO")
plot(nano$Date, nano$nano,
     type = "l", col = "black", lwd = 1,
     main = "notowania NANO")
par(mfrow = c(1, 1))

# Na podstawie wykresów trudno stwierdzić stacjonarność szeregów. Wydaje się, że średnia może mieć stałą wartość oczekiwaną, natomiast ciężko to powiedzieć o wariancji.
# 
# 2. Testowanie niestacjonarność poszczególnych szeregów czasowych.

testdf <- function(variable, adf_order) {
  results_adf <- data.frame(order = -1,
                            adf = 0,
                            p_adf = "",
                            bgodfrey = 0, p_bg = 0)
  variable <- variable[!is.na(variable)]
  
  for (order in 0:adf_order) {
    df.test_ <- ur.df(variable, type = c("drift"), lags = order)
    df_ <- df.test_@teststat[1]
    df_crit <- df.test_@cval[1, ]
    df_crit <- (df_ < df_crit) * 1
    p_adf <- ifelse(sum(df_crit) == 0,
                    ">10pct",
                    paste("<",
                          names(df_crit)[min(which(df_crit == 1))],
                          sep = "")
    )
    
    resids_ <- df.test_@testreg$residuals
    bgtest_ <- bgtest(resids_ ~ 1, order = 1)
    bgodfrey <- bgtest_$statistic
    names(bgodfrey) <- NULL
    p_bg <- bgtest_$p.value
    
    results_adf <- rbind(results_adf,
                         data.frame(order = order,
                                    adf = df_,
                                    p_adf = p_adf,
                                    bgodfrey = bgodfrey,
                                    p_bg = p_bg)
    )
  }
  
  results_adf <- results_adf[results_adf$order >= 0, ]
  
  plot(variable,
       type = "l",
       col = "blue",
       lwd = 2,
       main = "Plot of the examined variable")
  
  return(results_adf)
}

# tron, neo, waves, nano

df.test <- ur.df(tron$tron, type = c("drift"), lags = 0)
summary(df.test)

df.test2 <- ur.df(neo$neo, type = c("drift"), lags = 0)
summary(df.test2)

df.test3 <- ur.df(waves$waves, type = c("drift"), lags = 0)
summary(df.test3)

df.test4 <- ur.df(nano$nano, type = c("drift"), lags = 0)
summary(df.test4)

# P-value testu jest większe od 5% w każdym przypadku. Zatem brak jest podstaw do odrzucenia H0 o niestacjonarności szeregów.

# Analiza portfela zrównoważonego

# 1. Wykres zwrotów z portfela.

plot(portfel$Date, portfel$zwroty,
     type = "l", col="blue", lwd = 1,
     main = "zwroty z portfela")


# 2. Testowanie normalności rozkładu zwrotów.

#histogram zwrotów z portfela
```{r}
library(fGarch)
bstats <- basicStats(portfel$zwroty)
knitr::kable(as.matrix(bstats), digits = 2)

hist(portfel$zwroty, prob = T, breaks = 40)
curve(dnorm(x, mean = mean(portfel$zwroty, na.rm = T),
            sd  = sd(portfel$zwroty, na.rm = T)),
      col = "blue", lwd = 2, add = TRUE)

# test jarque- bera

jarque.bera.test <-
  function(x)
  {
    if((NCOL(x) > 1) || is.data.frame(x))
      stop("x is not a vector or univariate time series")
    if(any(is.na(x)))
      stop("NAs in x")
    DNAME <- deparse(substitute(x))
    n <- length(x)
    m1 <- sum(x)/n
    m2 <- sum((x-m1)^2)/n
    m3 <- sum((x-m1)^3)/n
    m4 <- sum((x-m1)^4)/n
    b1 <- (m3/m2^(3/2))^2
    b2 <- (m4/m2^2)
    STATISTIC <- n*b1/6+n*(b2-3)^2/24
    PVAL <- 1 - pchisq(STATISTIC,df = 2)
    PARAMETER <- 2
    METHOD <- "Jarque Bera Test"
    names(STATISTIC) <- "X-squared"
    names(PARAMETER) <- "df"
    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = METHOD,
                   data.name = DNAME),
              class = "htest")
  }
jarque.bera.test(na.omit(portfel$zwroty))
durbinWatsonTest(lm(portfel$zwroty ~ 1),
                 max.lag = 5)

# Badanie normalności reszt - test Jarque Bera
# Badanie autokorelacji stóp zwrotów- statystyki Durbina-Watsona
# 
# Odrzucamy założenie o  normalności reszt.
# P-value dla wszystkich opóźnień przekracza poziom istotności 5%, brak podstaw do odrzucenia H0 o braku autokorelacji zwrotów.
# 
# 3. Testowanie występowania efektów ARCH

ArchTest <- function (x, lags=12, demean = FALSE) 
{
  # Capture name of x for documentation in the output  
  xName <- deparse(substitute(x))
  # 
  x <- as.vector(x)
  if(demean) x <- scale(x, center = TRUE, scale = FALSE)
  #  
  lags <- lags + 1
  mat <- embed(x^2, lags)
  arch.lm <- summary(lm(mat[, 1] ~ mat[, -1]))
  STATISTIC <- arch.lm$r.squared * length(resid(arch.lm))
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- lags - 1
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "ARCH LM-test;  Null hypothesis:  no ARCH effects"
  result <- list(statistic = STATISTIC, parameter = PARAMETER, 
                 p.value = PVAL, method = METHOD, data.name =
                   xName)
  class(result) <- "htest"
  return(result)
}
ArchTest(portfel$zwroty, lags = 5)

# Odrzucamy H0 o braku efektów ARCH.
# 
# 4. Sprawdzenie, czy ACF reszt i ich kwadratów zachowują się jak biały szum.
# 
# Wykresy ACF dla zwrotów i kwadratów zwrotów.

acf(portfel$zwroty, lag.max = 36, na.action = na.pass,
    col = "blue", lwd = 7,
    main = "Wykres ACF zwrotów portfela")

acf(portfel$zwroty^2, lag.max = 100, na.action = na.pass,
    col = "blue", lwd = 7,
    main = "Wykres ACF kwadratów zwrotów portfela")

# Na podstawie wykresów wnioskujemy, że nie zachowują się jak biały szum.
# 
# 5. Badanie autokorelacji zmiennej
# 
# Wykres wartości ACF dla zwrotów:

acf(portfel$zwroty, lag.max = 36, na.action = na.pass,
    ylim = c(-0.4, 0.4),
    col = "blue", lwd = 5,
    main = "ACF zwrotów ")

# Badana zmienna podlega autokorelacji.

# Estymacja modelów GARCH w celu wyboru odpowiedniego modelu

# Tworzymy próbkę ograniczoną do pewnego okresu czasu. 
# Generujemy GARCH o różnych rzędach p i q:

portfel_probka_in <- portfel[portfel$Date <= as.Date("2019-05-01"), ]
portfel_probka_in <- portfel_probka_in[as.Date("2019-03-01") <= portfel_probka_in$Date, ]

library(fGarch)
spec_11 <- ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(1, 1)),
                      mean.model = list(armaOrder = c(0, 0),
                                        include.mean = T),
                      distribution.model = "norm")

garch_11 <- ugarchfit(spec = spec_11,
                      data = na.omit(portfel_probka_in$zwroty))


spec_10 <- ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(1, 0)),
                      mean.model = list(armaOrder = c(0, 0),
                                        include.mean = T),
                      distribution.model = "norm")

garch_10 <- ugarchfit(spec = spec_10,
                      data = na.omit(portfel_probka_in$zwroty))



spec_20 <- ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(2, 0)),
                      mean.model = list(armaOrder = c(0, 0),
                                        include.mean = T),
                      distribution.model = "norm")

garch_20 <- ugarchfit(spec = spec_20,
                      data = na.omit(portfel_probka_in$zwroty))


spec_21 <- ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(2, 1)),
                      mean.model = list(armaOrder = c(0, 0),
                                        include.mean = T),
                      distribution.model = "norm")

garch_21 <- ugarchfit(spec = spec_21,
                      data = na.omit(portfel_probka_in$zwroty))


spec_30 <- ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(3, 0)),
                      mean.model = list(armaOrder = c(0, 0),
                                        include.mean = T),
                      distribution.model = "norm")

garch_30 <- ugarchfit(spec = spec_30,
                      data = na.omit(portfel_probka_in$zwroty))

spec_31 <- ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(3, 1)),
                      mean.model = list(armaOrder = c(0, 0),
                                        include.mean = T),
                      distribution.model = "norm")

garch_31 <- ugarchfit(spec = spec_31,
                      data = na.omit(portfel_probka_in$zwroty))

garch_11
garch_10
garch_20
garch_21
garch_30
garch_31


# Dokonujemy porównania modeli uwzględniając następujące kryteria: AIC, BIC, Shibata oraz Hannan-Quinna.

# -garch_31
# Akaike       -1.01848
# Bayes        -0.81263
# Shibata      -1.03510
# Hannan-Quinn -0.93766
# 
# -garch_30
# Akaike       -1.03836
# Bayes        -0.86682
# Shibata      -1.05012
# Hannan-Quinn -0.97101
# 
# 
# -garch_21
# Akaike       -1.05007
# Bayes        -0.87853
# Shibata      -1.06183
# Hannan-Quinn -0.98272
# 
# -garch_20
# Akaike       -1.06840
# Bayes        -0.93116
# Shibata      -1.07607
# Hannan-Quinn -1.01452
# 
# -garch_10
# Akaike       5.6438
# Bayes        5.7467
# Shibata      5.6394
# Hannan-Quinn 5.6842
# 
# -garch_11
# Akaike       -1.08164
# Bayes        -0.94441
# Shibata      -1.08932
# Hannan-Quinn -1.02776

# Najniższe kryteria przyjmuje model GARCH(1,1).

garch_11

# Testy wskazują na to, że nie występuje autokorelacja reszt i kwadratów reszt wystandaryzowanych, ani efekty ARCH.
# 
# Następnie utworzono wykresy funkcji autokorelacji dla kwadratów reszt:

plot(garch_11, which = 10)
plot(garch_20, which = 10)
plot(garch_10, which = 10)
plot(garch_21, which = 10)
plot(garch_30, which = 10)
plot(garch_31, which = 10)
```
# Wypustki są nieistotne, ponieważ nie przekraczają obszaru krytycznego. Zatem nie występuje autokorelacja reszt.
# 
# Kolejnym krokiem jest obliczenie Value-At_Risk w in sample i out of sample oraz porównanie kwantyli empirycznych ze standardowym z rozkładu normalnego.

portfel_probka_in$zwrotystd <- (portfel_probka_in$zwroty - mean(portfel_probka_in$zwroty, na.rm=T)) /
  sd(portfel_probka_in$zwroty ,na.rm = T)
tail(portfel_probka_in)

basicStats(portfel_probka_in$zwrotystd)

q01 <- quantile(portfel_probka_in$zwrotystd, 0.01, na.rm = T)
q01
qnorm(0.01, 0, 1)

str(garch_11)
garch_11@fit$sigma
portfel_probka_in$VaR <- q01 * garch_11@fit$sigma
tail(portfel_probka_in)

# Kwantyle nie równią się od siebie w znaczącym stopniu.
# 
# Wykres zwrotów i wartości Value-At_Risk przedstawia się w następujący sposób:

plot(portfel_probka_in$Date, portfel_probka_in$zwroty, col = "red", lwd = 1, type = 'l',
     ylim = c(-2,2))
abline(h = 0, lty = 2)
lines(portfel_probka_in$Date, portfel_probka_in$VaR, type = 'l', col = "green")

sum(portfel_probka_in$zwroty < portfel_probka_in$VaR) / length(portfel_probka_in$VaR)

# Straty przekroczyły zakładany poziom VaR w 1,6% przypadków.
# 
# Natomiast dla out-of-sample:

#1-dniowa prognoza warunkowego odchylenia standardowego
sigma.forecast1 <- ugarchforecast(garch_11, n.ahead = 1)
str(sigma.forecast1)
str(sigma.forecast1@forecast)
sigma.forecast1@forecast$sigmaFor
sigma.forecast1_2 <- sigma.forecast1@forecast$sigmaFor[1, 1]

# 1-dniowy VaR:
q01 * sigma.forecast1_2

portfel$obs<-1:length(portfel$zwroty)
start  <- portfel$obs[portfel$Date == as.Date("2019-03-02")]
finish <- portfel$obs[portfel$Date == as.Date("2019-06-01")]
portfel1 <-portfel[start:finish, ]
VaR1 <- rep(NA, times = finish - start + 1)

time1 <- Sys.time()
for (k in start:finish) {
  tmp.data <- portfel[portfel$obs <= (k - 1), ]
  tmp.data <- tmp.data[as.Date("2019-01-02") <= tmp.data$Date, ]
  tmp.data$zwrotystd <- (tmp.data$zwroty - mean(tmp.data$zwroty, na.rm = T)) /
    sd(tmp.data$zwroty, na.rm = T)
  q01 <- quantile(tmp.data$zwrotystd, 0.01, na.rm = T)
  spec10 <- ugarchspec(variance.model = list(model = "sGARCH",
                                             garchOrder = c(1, 0)),
                       mean.model = list(armaOrder = c(0, 0),
                                         include.mean = T),
                       distribution.model = "norm")
  tmp.garch10 <- ugarchfit(spec = spec_10, data = na.omit(portfel$zwroty))
  sigma.forecast1  <- ugarchforecast(tmp.garch10, n.ahead = 1)
  sigma.forecast12 <- sigma.forecast1@forecast$sigmaFor[1, 1]
  VaR1[k - start + 1] <- q01 * sigma.forecast12
}
time2 <- Sys.time()

time2 - time1
portfel1$VaR1 <- VaR1

plot(portfel1$Date, portfel1$zwroty, col = "red", lwd = 1, type = 'l',
     ylim = c(-1, 1))
abline(h = 0, lty = 2)
lines(portfel1$Date, portfel1$VaR1, type = 'l', col = "green")

sum(portfel1$zwroty < portfel1$VaR1) / length(portfel1$VaR1)

# Straty przekroczyły zakładany poziom VaR w 0% przypadków.
# 
# Szacowanie modelu tGARCH:

tspec_10 <- ugarchspec(variance.model = list(model = "fGARCH",
                                             garchOrder = c(1, 0),
                                             submodel = "TGARCH"),
                       mean.model = list(armaOrder = c(0, 0),
                                         include.mean = F),
                       distribution.model = "norm")

k.tgarch_10 <- ugarchfit(spec = tspec_10, data = na.omit(portfel_probka_in$zwroty))


tspec_11 <- ugarchspec(variance.model = list(model = "fGARCH",
                                             garchOrder = c(1, 1),
                                             submodel = "TGARCH"),
                       mean.model = list(armaOrder = c(0, 0),
                                         include.mean = F),
                       distribution.model = "norm")

k.tgarch_11 <- ugarchfit(spec = tspec_11, data = na.omit(portfel_probka_in$zwroty))


tspec_21 <- ugarchspec(variance.model = list(model = "fGARCH",
                                             garchOrder = c(2, 1),
                                             submodel = "TGARCH"),
                       mean.model = list(armaOrder = c(0, 0),
                                         include.mean = F),
                       distribution.model = "norm")

k.tgarch_21 <- ugarchfit(spec = tspec_21, data = na.omit(portfel_probka_in$zwroty))


tspec_30 <- ugarchspec(variance.model = list(model = "fGARCH",
                                             garchOrder = c(3, 0),
                                             submodel = "TGARCH"),
                       mean.model = list(armaOrder = c(0, 0),
                                         include.mean = F),
                       distribution.model = "norm")

k.tgarch_30 <- ugarchfit(spec = tspec_30, data = na.omit(portfel_probka_in$zwroty))

k.tgarch_10
k.tgarch_11
k.tgarch_21
k.tgarch_30

# k.tgarch_10 
# Akaike       103.39
# Bayes        103.50
# Shibata      103.39
# Hannan-Quinn 103.43
# 
# k.tgarch_11 
# Akaike       -1.1490
# Bayes        -1.0117
# Shibata      -1.1567
# Hannan-Quinn -1.0951
# 
# k.tgarch_21
# Akaike       -1.08632
# Bayes        -0.88047
# Shibata      -1.10294
# Hannan-Quinn -1.00550
# 
# k.tgarch_30
# Akaike       -0.96328
# Bayes        -0.72312
# Shibata      -0.98549
# Hannan-Quinn -0.86899

# Na podstawie kryteriów informacyjnych wybieramy model tGARCH(1,1).
# 
# Wykres funkcji autokorelacji dla tGARCH(1,1):

plot(k.tgarch_11, which = 10)

# Wypustki są nieistotne, ponieważ nie przekraczają obszaru krytycznego. Zatem nie występuje autokorelacja reszt.
# 
# Kolejnym krokiem jest obliczenie Value-At_Risk w in sample i out of sample oraz porównanie kwantyli empirycznych ze standardowym z rozkładu normalnego.

portfel_probka_in$zwrotystd2 <- (portfel_probka_in$zwroty - mean(portfel_probka_in$zwroty, na.rm=T)) /
  sd(portfel_probka_in$zwroty ,na.rm = T)
tail(portfel_probka_in)

basicStats(portfel_probka_in$zwrotystd2)

q01 <- quantile(portfel_probka_in$zwrotystd2, 0.01, na.rm = T)
q01

qnorm(0.01, 0, 1)

str(k.tgarch_11)
k.tgarch_11@fit$sigma

portfel_probka_in$VaR2 <- q01 * k.tgarch_11@fit$sigma
tail(portfel_probka_in)

# Wykres zwrotów i VaR.

plot(portfel_probka_in$Date, portfel_probka_in$zwroty, col = "red", lwd = 1, type = 'l',
     ylim = c(-2,2))
abline(h = 0, lty = 2)
lines(portfel_probka_in$Date, portfel_probka_in$VaR2, type = 'l', col = "green")

sum(portfel_probka_in$zwroty < portfel_probka_in$VaR2) / length(portfel_probka_in$VaR2)

# W 1,6% przypadkach straty przekroczyły zakładany poziom VaR.
# 
# Natomiast dla out of sample:

sigma.forecast2 <- ugarchforecast(k.tgarch_11, n.ahead = 1)

str(sigma.forecast2)
str(sigma.forecast2@forecast)
sigma.forecast2@forecast$sigmaFor

sigma.forecast22 <- sigma.forecast2@forecast$sigmaFor[1, 1]

# 1-dniowy VaR
q01 * sigma.forecast22

portfel$obs<-1:length(portfel$zwroty)
start  <- portfel$obs[portfel$Date == as.Date("2019-03-02")]
finish <- portfel$obs[portfel$Date == as.Date("2019-06-01")]
portfel4 <-portfel[start:finish, ]
VaR <- rep(NA, times = finish - start + 1)

time1 <- Sys.time()
for (k in start:finish) {
  tmp.data <- portfel[portfel$obs <= (k - 1), ]
  tmp.data <- tmp.data[as.Date("2019-01-02") <= tmp.data$Date, ]
  tmp.data$zwrotystd <- (tmp.data$zwroty - mean(tmp.data$zwroty, na.rm = T)) /
    sd(tmp.data$zwroty, na.rm = T)
  q01 <- quantile(tmp.data$zwrotystd, 0.01, na.rm = T)
  tspec11 <- ugarchspec(variance.model = list(model = "fGARCH",
                                              garchOrder = c(2, 1),
                                              submodel = "TGARCH"),
                        mean.model = list(armaOrder = c(0, 0),
                                          include.mean = F),
                        distribution.model = "norm")
  tmp.k.tgarch11 <- ugarchfit(spec = tspec_11, data = na.omit(portfel$zwroty))
  sigma.forecast2  <- ugarchforecast(tmp.k.tgarch11, n.ahead = 1)
  sigma.forecast22 <- sigma.forecast2@forecast$sigmaFor[1, 1]
  VaR[k - start + 1] <- q01 * sigma.forecast22
}
time2 <- Sys.time()

time2 - time1

portfel4$VaR <- VaR

plot(portfel4$Date, portfel4$zwroty, col = "red", lwd = 1, type = 'l',
     ylim = c(-1, 1))
abline(h = 0, lty = 2)
lines(portfel4$Date, portfel4$VaR, type = 'l', col = "green")


# Straty przekroczyły zakładany poziom VaR w 0% przypadków.
# 
# Prognozy warunkowej wariancji są do siebie zbliżone.
# 
# Kolejnym krokiem było oszacowanie modelu eGARCH.

espec_11 = ugarchspec(variance.model = list(model ="eGARCH",
                                            garchOrder = c(1, 1)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = F),
                      distribution.model = "norm")

egarch_11 <- ugarchfit(spec = espec_11, data = na.omit(portfel_probka_in$zwroty))


espec_12 = ugarchspec(variance.model = list(model ="eGARCH",
                                            garchOrder = c(1, 2)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = F),
                      distribution.model = "norm")

egarch_12 <- ugarchfit(spec = espec_12, data = na.omit(portfel_probka_in$zwroty))


espec_21 = ugarchspec(variance.model = list(model ="eGARCH",
                                            garchOrder = c(2, 1)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = F),
                      distribution.model = "norm")

egarch_21 <- ugarchfit(spec = espec_21, data = na.omit(portfel_probka_in$zwroty))


espec_13 = ugarchspec(variance.model = list(model ="eGARCH",
                                            garchOrder = c(1, 3)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = F),
                      distribution.model = "norm")

egarch_13 <- ugarchfit(spec = espec_13, data = na.omit(portfel_probka_in$zwroty))


espec_31 = ugarchspec(variance.model = list(model ="eGARCH",
                                            garchOrder = c(3, 1)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = F),
                      distribution.model = "norm")

egarch_31 <- ugarchfit(spec = espec_31, data = na.omit(portfel_probka_in$zwroty))

egarch_11
egarch_12
egarch_21
egarch_13
egarch_31

# egarch_11
# Akaike       -1.1560
# Bayes        -1.0188
# Shibata      -1.1637
# Hannan-Quinn -1.1022
# 
# egarch_12
# Akaike       -1.2221
# Bayes        -1.0505
# Shibata      -1.2338
# Hannan-Quinn -1.1547
# 
# egarch_21
# Akaike       -1.08707
# Bayes        -0.88122
# Shibata      -1.10369
# Hannan-Quinn -1.00625
# 
# egarch_13
# Akaike       -1.2526
# Bayes        -1.0467
# Shibata      -1.2692
# Hannan-Quinn -1.1717
# 
# egarch_31
# Akaike       -1.4033
# Bayes        -1.1288
# Shibata      -1.4318
# Hannan-Quinn -1.2955

# Na podstawie kryteriów informacyjnych wybieramy model eGARCH(3,1).
# 
# Wykres funkcji autokorelacji na kwadratach zwrotów:

plot(egarch_31, which = 10)

# Wypustki są nieistotne, ponieważ nie przekraczają obszaru krytycznego. Zatem nie występuje autokorelacja reszt.
# 
# Kolejnym krokiem jest obliczenie Value-At_Risk w in sample i out of sample oraz porównanie kwantyli empirycznych ze standardowym z rozkładu normalnego.

portfel_probka_in$zwrotystd3 <- (portfel_probka_in$zwroty - mean(portfel_probka_in$zwroty, na.rm=T)) /
  sd(portfel_probka_in$zwroty ,na.rm = T)
tail(portfel_probka_in)

basicStats(portfel_probka_in$zwrotystd3)

q01 <- quantile(portfel_probka_in$zwrotystd3, 0.01, na.rm = T)
q01

qnorm(0.01, 0, 1)

str(egarch_11)
egarch_11@fit$sigma

portfel_probka_in$VaR3 <- q01 * egarch_11@fit$sigma
tail(portfel_probka_in)

# wykres zwrotów i var
plot(portfel_probka_in$Date, portfel_probka_in$zwroty, col = "red", lwd = 1, type = 'l',
     ylim = c(-2,2))
abline(h = 0, lty = 2)
lines(portfel_probka_in$Date, portfel_probka_in$VaR3, type = 'l', col = "green")

a =sum(portfel_probka_in$zwroty < portfel_probka_in$VaR3) / length(portfel_probka_in$VaR3)
a

# W 0% przypadków straty przekroczyły zakładany poziom VaR.
# 
# VaR dla out of sample:

sigma.forecast3 <- ugarchforecast(egarch_11, n.ahead = 1)

str(sigma.forecast3)
str(sigma.forecast3@forecast)
sigma.forecast3@forecast$sigmaFor

sigma.forecast33 <- sigma.forecast3@forecast$sigmaFor[1, 1]

# 1-dniowy VaR:
q01 * sigma.forecast33

portfel$obs<-1:length(portfel$zwroty)
start  <- portfel$obs[portfel$Date == as.Date("2019-03-02")]
finish <- portfel$obs[portfel$Date == as.Date("2019-06-01")]
portfel3 <-portfel[start:finish, ]
VaR3 <- rep(NA, times = finish - start + 1)

time1 <- Sys.time()
for (k in start:finish) {
  tmp.data <- portfel[portfel$obs <= (k - 1), ]
  tmp.data <- tmp.data[as.Date("2019-01-02") <= tmp.data$Date, ]
  tmp.data$zwrotystd <- (tmp.data$zwroty - mean(tmp.data$zwroty, na.rm = T)) /
    sd(tmp.data$zwroty, na.rm = T)
  q01 <- quantile(tmp.data$zwrotystd, 0.01, na.rm = T)
  espec11 = ugarchspec(variance.model = list(model ="eGARCH",
                                             garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(0, 0), include.mean = F),
                       distribution.model = "norm")
  tmp.egarch_13 <- ugarchfit(spec = espec_13, data = na.omit(portfel$zwroty))
  sigma.forecast3  <- ugarchforecast(tmp.egarch_13, n.ahead = 1)
  sigma.forecast33 <- sigma.forecast3@forecast$sigmaFor[1, 1]
  VaR3[k - start + 1] <- q01 * sigma.forecast33
}
time2 <- Sys.time()

time2 - time1

portfel3$VaR3 <- VaR3

head(portfel3)
tail(portfel3)

plot(portfel3$Date, portfel3$zwroty, col = "red", lwd = 1, type = 'l',
     ylim = c(-1, 1))
abline(h = 0, lty = 2)
lines(portfel3$Date, portfel3$VaR3, type = 'l', col = "green")


b = sum(portfel3$zwroty < portfel3$VaR3) / length(portfel3$VaR3)
b

# W 0% przypadków straty przekroczyły zakładany poziom VaR.
# 
# Porównanie oszacowań wybranych modeli

garch_11 
k.tgarch_11 
egarch_31

# Spośród wybranych modeli najmniejszą wartość kryteriów informacyjnych ma eGARCH(3,1).

#Prognozowanie wariancji bezwarunkowej dla eGARCH(3,1)

egarch_31 <- garchFit(formula = ~ garch(3, 1),
                      data = na.omit(portfel$zwroty),
                      include.mean = F,
                      cond.dist = "norm",
                      trace = F)
egarch_31@fit$par

var_uncond <- egarch_31@fit$par[1] / (1 - egarch_31@fit$par[2]
                                      - egarch_31@fit$par[3])

names(var_uncond) <- "unconditional variance egarch"
var_uncond

fore31 <- predict(egarch_31, n.ahead = 20)
head(fore31)

plot(fore31[, 3] ^ 2, type = "l")
abline(h = var_uncond, col = "red", lty = 2)
title(main = "Warunkowa i bezwarunkowa wariancja zwrotów eGARCH(3,1)")

# Wykres wariancji out of sample GARCH(1,1)

spec_11 <- ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(1, 1)),
                      mean.model = list(armaOrder = c(0, 0),
                                        include.mean = T),
                      distribution.model = "norm")

garch_11 <- ugarchfit(spec = spec_11,
                      data = na.omit(portfel$zwroty))
garch_11

plot(ugarchforecast(garch_11, n.ahead=20), which = 3)

# Wykres wariancji out of sample eGARCH(3,1)

espec_31 = ugarchspec(variance.model = list(model ="eGARCH",
                                            garchOrder = c(3, 1)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = F),
                      distribution.model = "norm")

egarch_31 <- ugarchfit(spec = espec_31, data = na.omit(portfel$zwroty))
egarch_31
str(egarch_31)
plot(ugarchforecast(egarch_31, n.ahead=20), which = 3)


# Wykres wariancji out of sample tGARCH(1,1)

tspec_11 <- ugarchspec(variance.model = list(model = "fGARCH",
                                             garchOrder = c(1, 1),
                                             submodel = "TGARCH"),
                       mean.model = list(armaOrder = c(0, 0),
                                         include.mean = F),
                       distribution.model = "norm")

k.tgarch_11 <- ugarchfit(spec = tspec_11, data = na.omit(portfel$zwroty))
k.tgarch_11
plot(ugarchforecast(k.tgarch_11, n.ahead=20), which = 3)

# Najlepszym z powyższych modeli jest model eGARCH(3,1). Wszystkie oszacowane parametry modelu są istotne. 