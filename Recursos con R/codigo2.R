#===============================================
# CONSTRUCCION DE PORTAFOLIO , CML, FRONTERA EFICIENTE, RATIO DE SHARPE, ETC
#===============================================
# Instalar y activar paquetes
install.packages("Ecdat")
install.packages("quadprog")
library(Ecdat)
library(quadprog)
#===============================================

# Seleccionando el directorio de trabajo,
# No olvidar cambiar \ por /
setwd("D:/Usuario- Alfonso/Desktop/FinCorpII")
CRSPday = read.csv("CRSPday.csv", header = T)
# Esta base de datos contiene datos diarios de:
# General Electric Company (GE)
# International Business Machines Corporation (IBM)
# Exxon Mobil Corporation (XOM), que para los anos considerados el ticker era MOBIL
# CRSP Indice ponderado por valor, incluidos dividendos. 
# CRSP es el Center for Research in Security Prices de la Universidad de Chicago


#===============================================
# Vemos los datos
View(CRSPday)
# Seleccionamos las columnas donde estan los retornos de los acciones,
# de las primeras 3 acciones
# y multiplicamos por 100 para convertir en porcentajes
R = 100*CRSPday[,4:6] 

# Creamos el vector con los retornos promedio
# aplicamos, en el  objeto R, a las columnas, la funcion mean.
# Nota: objeto margin en la funcion apply, solo toma dos valores:
# si margin=1, la funcion que sigue se aplica a FILAS
# si margin=2, la funcion que sigue se aplica a COLUMNAS

# Por eso, usamos "2"
mean_vect = apply(R,2,mean)

# Calculamos la mat#riz de covarianzas
cov_mat = cov(R)

# Calculamos matriz de desviaciones estandar
sd_vect = sqrt(diag(cov_mat))

# Creando la matriz de restricciones: una columna de puros 1, 
# y la segunda columna con los retornos promedios

Amat = cbind(rep(1,3),mean_vect) 

# conjunto de 300 valores objetivo posibles
muP = seq(.05,.14,length=300)


# para el rendimiento esperado de la cartera

# Para configurar el almacenamiento de las desviaciones estandar del portafolio
sdP = muP 

# almacenamiento para pesos de cartera

weights = matrix(0,nrow=300,ncol=3)


#===============================================
#===============================================
#===============================================
# PARA Encontrar las carteras optimas para cada rendimiento esperado objetivo
# Para ello usamos un for
# el indice para iterar va desde 1 hasta el ultimo de lo simulado (300)
# bvec = c(1,muP[i]) es un vector de contraste
# en el objeto return, se usa la funcion solve.QP,
# para resolver un problema de programacion cuadratica

# Esta rutina implementa el metodo dual de Goldfarb e Idnani (1982, 1983) 
# para resolver problemas de programacion cuadratica de la forma:

# min(-d^T b + 1/2 b^T D b) con las restricciones: A^T b >= b_0


#Entre las partes de esta funcion:

# Dmat: matriz que aparece en la funcion cuadratica a ser minimizada
# dvec: vector que aparece en la funcion cuadratica a ser minimizada
# Amat: matriz que define las restricciones bajo las cuales queremos minimizar la funcion cuadratica.
# bvec: vector que contiene los valores de b_0 (por defecto es cero).
# meq: las primeras restricciones de meq se tratan como restricciones de igualdad, todas 
#        ademas como restricciones de desigualdad (el valor predeterminado es 0).



for (i in 1:length(muP))
{
  bvec = c(1,muP[i])  # constraint vector
  result =
    solve.QP(Dmat=2*cov_mat,dvec=rep(0,3),Amat=Amat,bvec=bvec,meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] <- result$solution
}

# Graficando lo anterior:

#x11()
plot(sdP,muP,type="l",xlim=c(1.0,2.5),ylim=c(0.076,0.3),lty=3, lwd = .5)



#===============================================
# la frontera eficiente (y carteras ineficientes
# debajo de la cartera de min var)


mufree = 3.2/253 # valor de entrada de la tasa de interes libre de riesgo
# 253 dias utiles en un anho tipico
#===============================================
# Anhadiendo puntos al grafico
# cex es tama?o del simbolo, 4 significa 4 veces mas grande lo normal
# pch es el tipo de simbolo, le pondremos asterisco

points(0,mufree,cex=4,pch=25)  # mostrar activo libre de riesgo

#===============================================
# Calculando el RATIO DE SHARPE

sharpe =( muP-mufree)/sdP

# Encontrando el m?ximo valor de RATIO DE SHARPE
ind = (sharpe == max(sharpe))
options(digits=3)

weights[ind,] #  imprimir los pesos de la cartera de tangencia


#===============================================
# Graficando la CML

lines(c(0,2),mufree+c(0,2)*(muP[ind]-mufree)/sdP[ind],lwd=2,lty=1, col = "red")
# lwd: ancho relativo de l?nea
# lty: tipo de l?nea (s?lo puntos, l?neas entrecortadas, etc. Varia entre 1,2,3,4,5 y 6)

#===============================================
# Mostrar linea de carteras optimas
points(sdP[ind],muP[ind],cex=4,pch="*") # Mostrar portafolio de tangencia


#===============================================
# Encontrando el portafolio de MINIMA VARIANZA
ind2 = (sdP == min(sdP)) # Encontrando la minima desviaci?n estandar
ind3 = (muP > muP[ind2]) # Seleccionando los retornos esperados mayores a ese portafolio
#===============================================
# Graficando la FRONTERA EFICIENTE
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=3, col = "red")


#===============================================
# Anadiendo leyendas al grafico

text(sd_vect[1],mean_vect[1],"GE",cex=1.15)
text(sd_vect[2],mean_vect[2],"IBM",cex=1.15)
text(sd_vect[3],mean_vect[3],"Mobil",cex=1.15)

#===============================================
w = seq(0, 1.5,len = 500)

covjm = cov(R[,3], data.matrix(R) %*% weights[ind])

mup2 = w * as.numeric (weights[ind] %*% mean_vect) + (1 - w) * mean_vect[3]

sdp2 = sqrt(w^2 * sdP[ind]^2 + (1 - w)^2 * sd_vect[3]^2 + 2 * w * (1 - w) * as.numeric (covjm))

lines(sdp2, mup2, lwd = 3, col = "purple")

legend("topleft", c("CML", "Frontera Eficiente","Portafolios de tangencia y MOBIL","Portafolio de Tangencia"),
       col = c("blue","red","purple","black"),
       lty = c(1,1,1,NA), lwd = c(4,3,3,NA), pch = c(NA,NA,NA,"*"), pt.cex = c(1,1,1,4)  )
