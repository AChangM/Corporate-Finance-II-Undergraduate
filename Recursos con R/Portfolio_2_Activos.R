#====================================================
#=== UNIVERSIDAD NACIONAL DE INGENIERIA - FIEECS ====
#====================================================
## Finanzas Corporativas II - MSc Alfonso Chang M. ##
#====================================================
# U2: Intr. a la Teoria del Portafolio de Inversiones
#====================================================
# Estudio de Caso
#====================================================

#=================================================================
# Unidad 2: Introducciion a la Teoria de Portafolio de Inversiones
#=================================================================
# Estudio de Caso
#=================================================================
# Instalando paquetes y activandolo

install.packages("quantmod")
library(quantmod)
#=================================================================
# Descargando precios de acciones de:
getSymbols.yahoo('MSFT',env=.GlobalEnv,return.class = 'xts',index.class = 'Date',from='2009-01-01', to='2019-12-31',periodicity = "monthly")
getSymbols.yahoo('SHI',env=.GlobalEnv,return.class = 'xts',index.class = 'Date',from='2009-01-01', to='2019-12-31',periodicity = "monthly")
getSymbols.yahoo('GE',env=.GlobalEnv,return.class = 'xts',index.class = 'Date',from='2009-01-01', to='2019-12-31',periodicity = "monthly")
getSymbols.yahoo('TSN',env=.GlobalEnv,return.class = 'xts',index.class = 'Date',from='2009-01-01', to='2019-12-31',periodicity = "monthly")

#=================================================================
# Calculando retornos mensuales:
MSFT.ret <- diff(log(MSFT$MSFT.Adjusted))*100
SHI.ret <- diff(log(SHI$SHI.Adjusted))*100
GE.ret <- diff(log(GE$GE.Adjusted))*100
TSN.ret <- diff(log(TSN$TSN.Adjusted))*100

MSFT.M <- MSFT.ret['2009-02-01/2019-12-31']
SHI.M <- SHI.ret['2009-02-01/2019-12-31']
GE.M <- GE.ret['2009-02-01/2019-12-31']
TSN.M <- TSN.ret['2009-02-01/2019-12-31']

#=================================================================
# Consolidando todo en un solo objeto xts
datamensual <- na.omit(merge(MSFT.M,SHI.M,GE.M,TSN.M), join='inner')
#=================================================================
# Calculando los diversos valores
# Matriz de Varianzas y Covarianzas
s=cov(datamensual)
# Retornos Esperados
Er=matrix(c(mean(MSFT.M),mean(SHI.M),mean(GE.M),mean(TSN.M)),nrow=4,ncol = 1)
# Constante , A.K.A. Tasa Libre de Riesgo # rf = 4% (anual) equivalente a c=0.003274
c=0.003274
# Retornos Esperados menos Constante
Erc=Er-c
#=================================================================
# Calculando la Frontera Eficiente.
# Paso 1:
pesosx=solve(s,diag(1,nrow=dim(s)))%*%Er/sum(solve(s,diag(1,nrow=dim(s)))%*%Er)
pesosy=solve(s,diag(1,nrow=dim(s)))%*%Erc/sum(solve(s,diag(1,nrow=dim(s)))%*%Erc)
# Cálculo de los retornos esperados de los dos portafolios
Ex=t(pesosx)%*%Er
Ey=t(pesosy)%*%Er
# Cálculo de la Varianza de los dos portafolios
varx=t(pesosx)%*%s%*%pesosx
vary=t(pesosy)%*%s%*%pesosy
# Cálculo de la Covarianza entre los dos portafolios
covxy=t(pesosx)%*%s%*%pesosy

# Paso 2:
# Simulando Pesos de los portafolios x y y
a=matrix(seq(from=-1, to=3,by=0.2),nrow = length(seq(from=-1, to=3,by=0.2)),ncol = 1)
b=matrix(a-1,nrow=length(a),ncol = 1)
# Retorno Esperado y Desviacion Estandar de Portafolio Eficiente
Erp=a%*%Ex+b%*%Ey
sdp=sqrt((a^2)%*%varx+(b^2)%*%vary+2*a*b%*%covxy)
#Graficando Frontera Eficiente
plot(sdp,Erp)