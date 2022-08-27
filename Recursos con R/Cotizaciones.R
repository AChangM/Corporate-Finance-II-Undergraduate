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

setwd("D:/Inkapitales - Academico/009_UNIVERSIDADES/Academico UNI/Finanzas II - 2021I/Unidad 2 Teoria de Portafolio/BaseDatos")

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
#ZM.M <- ZM.ret['2015-02-01/2019-12-31']
AAPL.M <- AAPL.ret['2015-02-01/2019-12-31']
SBUX.M <- SBUX.ret['2015-02-01/2019-12-31']
#TLSA.M <- TLSA.ret['2015-02-01/2019-12-31']
GOOG.M <- GOOG.ret['2015-02-01/2019-12-31']
AMZN.M <- AMZN.ret['2015-02-01/2019-12-31']
INTC.M <- INTC.ret['2015-02-01/2019-12-31']

datamensual <- na.omit(merge(MSFT.M, SHI.M, GE.M, TSN.M, AAPL.M, SBUX.M, GOOG.M, AMZN.M, INTC.M),join='inner')
head(datamensual)
tail(datamensual)

write.csv(datamensual, file="datamensual.csv")
write.table(datamensual, file = "datamensual1.csv", row.names=F, sep = ",")
write.zoo(datamensual, file = "datamensual2.csv", sep=",")
