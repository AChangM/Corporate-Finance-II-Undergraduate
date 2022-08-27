#====================================================
#=== UNIVERSIDAD NACIONAL DE INGENIERIA - FIEECS ====
#====================================================
## Finanzas Corporativas II - MSc Alfonso Chang M. ##
#====================================================
# U2: Intr. a la Teoria del Portafolio de Inversiones
#====================================================
# Estudio de Caso
#====================================================

#Portfolio

### Paqueteria
if(!require("pacman")) install.packages("pacman")
p_load("PerformanceAnalytics","quadprog","xts")
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
p_load("IntroCompFinR","readxl","tidyverse")

#### Generando la Base de Datos
library("quantmod") 

getSymbols.yahoo('MSFT',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date', 
                 from='2015-01-01', to='2019-12-31', 
                 periodicity = "monthly")

getSymbols.yahoo('SHI',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date',
                 from='2015-01-01', to='2019-12-31', 
                 periodicity = "monthly")

getSymbols.yahoo('GE',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date',
                 from='2015-01-01', to='2019-12-31', 
                 periodicity = "monthly")

getSymbols.yahoo('TSN',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date',
                 from='2015-01-01', to='2019-12-31',
                 periodicity = "monthly")

getSymbols.yahoo('ZM',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date',
                 from='2015-01-01', to='2019-12-31',
                 periodicity = "monthly")

getSymbols.yahoo('AAPL',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date',
                 from='2015-01-01', to='2019-12-31',
                 periodicity = "monthly")

getSymbols.yahoo('SBUX',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date',
                 from='2015-01-01', to='2019-12-31',
                 periodicity = "monthly")

getSymbols.yahoo('TLSA',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date',
                 from='2015-01-01', to='2019-12-31',
                 periodicity = "monthly")

getSymbols.yahoo('GOOG',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date',
                 from='2015-01-01', to='2019-12-31',
                 periodicity = "monthly")

getSymbols.yahoo('AMZN',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date',
                 from='2015-01-01', to='2019-12-31',
                 periodicity = "monthly")

getSymbols.yahoo('INTC',env=.GlobalEnv,
                 return.class = 'xts',
                 index.class = 'Date',
                 from='2015-01-01', to='2019-12-31',
                 periodicity = "monthly")


#====================================================
# Calculando retornos mensuales:
MSFT.ret <- diff(log(MSFT$MSFT.Adjusted))
SHI.ret <- diff(log(SHI$SHI.Adjusted))
GE.ret <- diff(log(GE$GE.Adjusted))
TSN.ret <- diff(log(TSN$TSN.Adjusted))
ZM.ret <- diff(log(ZM$ZM.Adjusted))
AAPL.ret <- diff(log(AAPL$AAPL.Adjusted))
SBUX.ret <- diff(log(SBUX$SBUX.Adjusted))
TLSA.ret <- diff(log(TLSA$TLSA.Adjusted))
GOOG.ret <- diff(log(GOOG$GOOG.Adjusted))
AMZN.ret <- diff(log(AMZN$AMZN.Adjusted))
INTC.ret <- diff(log(INTC$INTC.Adjusted))

MSFT.M <- MSFT.ret['2015-02-01/2019-12-31']
SHI.M <- SHI.ret['2015-02-01/2019-12-31']
GE.M <- GE.ret['2015-02-01/2019-12-31']
TSN.M <- TSN.ret['2015-02-01/2019-12-31']
ZM.M <- ZM.ret['2015-02-01/2019-12-31']
AAPL.M <- AAPL.ret['2015-02-01/2019-12-31']
SBUX.M <- SBUX.ret['2015-02-01/2019-12-31']
TLSA.M <- TLSA.ret['2015-02-01/2019-12-31']
GOOG.M <- GOOG.ret['2015-02-01/2019-12-31']
AMZN.M <- AMZN.ret['2015-02-01/2019-12-31']
INTC.M <- INTC.ret['2015-02-01/2019-12-31']

#====================================================
# Consolidando todo en un solo objeto xts #
datamensual <- na.omit(merge(TSN.ret, SBUX.ret,GOOG.ret),join='inner')
head(datamensual)
tail(datamensual)

#====================================================
# Promedio
mean <- apply(datamensual[2:9], 2 , function(x) mean(x))
# Desviación Estandar
sd <- apply(datamensual[2:9], 2 , function(x) sd(x))
# Covarianza
cov <- cov(datamensual[2:9])

#====================================================
# graficando el trade-off riesgo-retorno
g1 <- ggplot(mapping = aes(sd, mean, label = c("TSN","SBUX","GOOG"))) + geom_point()
g1 <- g1 + geom_text(hjust = 0, vjust = 0)
g1 <- g1 + scale_y_continuous(breaks = seq(-.04, 0.4, by = 0.02),limits = c(-.04,0.08))
g1 <- g1 + scale_x_continuous(breaks = seq(0, 0.2, by = 0.02),limits = c(0,0.2))
g1 <- g1 + theme_bw() + xlab("Riesgo") + ylab("Retorno")
g1 <- g1 + ggtitle("Balance Riesgo-Retorno", subtitle = "Cuatro activos riesgosos")
g1

#====================================================
# construimos los pesos
weights <- rep(1,3)/3
# construimos el portfolio
getPortfolio(mean, cov, weights)

#====================================================
# Calculando el Portafolio de Mínima Varianza
#====================================================
# construimos el objeto
globalmin <- globalMin.portfolio(mean, cov, shorts = TRUE)
# vemos el objeto en la consola
globalmin


#====================================================
# graficando el Portafolio de Mínima Varianza
g2 <- ggplot() + geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "1"))
g2 <- g2 + geom_point(mapping = aes(sd, mean, color = "2"))
g2 <- g2 + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),limits = c(0,0.06))
g2 <- g2 + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0,0.2))
g2 <- g2 + scale_color_manual("", values = c("green", "red"), labels = c("Min Var.", "Stocks 1"))
g2 <- g2 + theme_bw() + xlab("Riesgo") + ylab("Retorno")
g2 <- g2 + ggtitle("Balance Riesgo-Retorno", subtitle = "Tres activos riesgosos & minima varianza")
g2

#====================================================
# retorno igual a Nordstrom
port.nods <- efficient.portfolio(mean, cov, mean[1], shorts = TRUE)
# retorno igual a Starbucks
port.sbux <- efficient.portfolio(mean, cov, mean[2], shorts = TRUE)
# retorno igual a Microsoft
port.msft <- efficient.portfolio(mean, cov, mean[3], shorts = TRUE)
# construimos objeto con los retornos y desviaciones estandar
mean.2 <- c(port.nods$er, port.sbux$er, port.msft$er)
sd.2 <- c(port.nods$sd, port.sbux$sd, port.msft$sd)


#====================================================
# graficando el Portafolio de Mínima Varianza sujeto a un retorno objetivo

g3 <- ggplot() + geom_point(mapping = aes(sd, mean, color = "1"))
g3 <- g3 + geom_point(mapping = aes(sd.2, mean.2, color = "2"))
g3 <- g3 + geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "3"))
g3 <- g3 + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),limits = c(0,0.06))
g3 <- g3 + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0,0.2))
g3 <- g3 + scale_color_manual("", values = c("blue", "red", "green"), labels = c("Stocks 1",
                                                                                 "Stocks 2",
                                                                                 "Min var."))
g3 <- g3 + theme_bw() + xlab("Riesgo") + ylab("Retorno")
g3 <- g3 + ggtitle("Trade-off Riesgo-Retorno", subtitle = "Tres activos riesgosos & minima varianza")
g3

#====================================================
# Portafolio Tangente

# Tasa libre de riesgo
risk_free <- 0.005

# Portafolio tangente
port.tang <- tangency.portfolio(mean, cov, risk_free, shorts = TRUE)

#sharpe ratio
sharpe.ratio <- (port.tang$er - risk_free)/port.tang$sd


#====================================================
# Graficando Portafolio Tangente

g4 <- ggplot() + geom_point(mapping = aes(sd, mean, color = "1"))
g4 <- g4 + geom_point(mapping = aes(sd.2, mean.2, color = "2"))
g4 <- g4 + geom_point(mapping = aes(port.tang$sd, port.tang$er, color = "3"))
g4 <- g4 + geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "4"))
g4 <- g4 + geom_abline(intercept = risk_free, slope = sharpe.ratio)
g4 <- g4 + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),limits = c(0,0.06))
g3 <- g4 + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0,0.2))
g4 <- g4 + scale_color_manual("", values = c("blue", "red", "orange", "green"),labels = c("Stocks 1",
                                                                                          "Stocks 2",
                                                                                          "Tang. Port",
                                                                                          "Min var."))
g4 <- g4 + theme_bw() + xlab("Riesgo") + ylab("Retorno")
g4 <- g4 + ggtitle("Trade-off Riesgo-Retorno", subtitle = "Tres activos riesgosos & minima varianza")
g4

#====================================================
# Frontera Eficiente

eff.front.short <- efficient.frontier(mean, cov, nport = 25, alpha.min = -2,
                                      alpha.max = 1.5, shorts = TRUE)
eff.front.short

g5 <- ggplot() + geom_point(mapping = aes(eff.front.short$sd, eff.front.short$er, color = "1"))
g5 <- g5 + geom_point(mapping = aes(sd, mean, color = "2"))
g5 <- g5 + geom_point(mapping = aes(sd.2, mean.2, color = "3"))
g5 <- g5 + geom_point(mapping = aes(port.tang$sd, port.tang$er, color = "4"))
g5 <- g5 + geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "5"))
g5 <- g5 + geom_abline(intercept = risk_free, slope = sharpe.ratio)
g5 <- g5 + scale_y_continuous(breaks = seq(0,0.2, by = 0.01), limits = c(0,0.08))
g5 <- g5 + scale_x_continuous(breaks = seq(0,0.2, by = 0.02), limits = c(0,0.2))
g5 <- g5 + scale_color_manual("", values = c("black","blue", "red", "orange", "green"),
                              labels = c("Frontera", "Stocks 1", "Stocks 2", "Tang. Port","Min var."))
g5 <- g5 + theme_bw() + xlab("Riesgo") + ylab("Retorno")
g5 <- g5 + ggtitle("Trade-off Riesgo-Retorno", subtitle = "Tres activos riesgosos & minima varianza")
g5



