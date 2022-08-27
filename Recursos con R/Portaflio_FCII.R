#====================================================
#=== UNIVERSIDAD NACIONAL DE INGENIERIA - FIEECS ====
#====================================================
## Finanzas Corporativas II - MSc Alfonso Chang M. ##
#====================================================
# U2: Intr. a la Teoria del Portafolio de Inversiones
#====================================================
# Estudio de Caso
#====================================================

#=============================================================
# CONSTRUCCION DE PORTAFOLIO , CML, FRONTERA EFICIENTE, RATIO DE SHARPE, ETC
#=============================================================
# Instalar y activar paquetes

#install.packages("Ecdat")
library(Ecdat)
#install.packages("quadprog")
library(quadprog)
#===============================================

#### Parte I ####

setwd("D:/Usuario- Alfonso/Desktop/Portfolio_21Set21")
CRSPday = read.csv("CRSPday.csv", header = T)

View(CRSPday)

R = 100 * CRSPday[ ,4:6]

# Calcular la "media" de retornos
# Aplicar a la base de datos "R", el método "mean"
# 1: aplicar (el método) a Filas
# 2: aplicar (el método) a Columnas

mean_vect = apply(R, 2, mean)

#Calcular la matriz de covarianzas
cov_mat = cov(R)

#Calcular las desviaciones estándar
sd_vect = sqrt(diag(cov_mat))

# Creando la matriz de restricciones: una columna de puros 1, 
# y la segunda columna con los retornos promedios

Amat = cbind(rep(1,3),mean_vect)

#Conjunto de 300 valores posibles
muP = seq(0.05 , .14, length=300)

sdP = muP

weights = matrix(0, nrow = 300, ncol = 3)

#### Parte II ####

#===============================================
# Para encontrar las carteras para cada rendimiento esperado objetivo
# Usamos un estructura FOR 
# el indice para iterar va desde 1 hasta el ultimo de lo simulado (300)
# bvec = c(1,muP[i]) es un vector de contraste
# en el objeto return, se usa la funcion solve.QP,
# para resolver un problema de programaci?n cuadr?tica

# Esta rutina implementa el m?todo dual de Goldfarb e Idnani (1982, 1983) 
# para resolver problemas de programaci?n cuadr?tica de la forma:

# min(-d^T b + 1/2 b^T D b) con las restricciones: A^T b >= b_0


#Entre las partes de esta función:

# Dmat: matriz que aparece en la función cuadrática a ser minimizada
# dvec: vector que aparece en la funci?n cuadr?tica a ser minimizada
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
plot(sdP,muP,type="l",xlim=c(1.0,2.5),ylim=c(0.076,0.3),lty=3, lwd = .5)


#### Parte III ####

#===============================================
# la frontera eficiente (y carteras ineficientes
# debajo de la cartera de min var)

mufree = 1.3/253

#===============================================
# Anhadiendo puntos al grafico
# cex es tama?o del simbolo, 4 significa 4 veces mas grande lo normal
# pch es el tipo de simbolo, le pondremos asterisco

points(0,mufree,cex=4,pch=25)  # mostrar activo libre de riesgo

#===============================================
# Calculando el RATIO DE SHARPE

sharpe =(muP-mufree)/sdP

# Encontrando el máximo valor de RATIO DE SHARPE
ind = (sharpe == max(sharpe))
options(digits=3)

weights[ind,] #  imprimir los pesos de la cartera de tangencia


#===============================================
# Graficando la CML

lines(c(0,2),mufree+c(0,2)*(muP[ind]-mufree)/sdP[ind],lwd=4,lty=1, col = "blue")
# lwd: ancho relativo de línea
# lty: tipo de línea (sólo puntos, líneas entrecortadas, etc. Varia entre 1,2,3,4,5 y 6)

#===============================================
# Mostrar linea de carteras optimas
points(sdP[ind],muP[ind],cex=2,pch="*") # Mostrar portafolio de tangencia

#===============================================
# Encontrando el portafolio de MINIMA VARIANZA
ind2 = (sdP == min(sdP)) # Encontrando la minima desviación estandar
ind3 = (muP > muP[ind2]) # Seleccionando los retornos esperados mayores a ese portafolio

#===============================================
# Graficando la FRONTERA EFICIENTE
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=3, col = "red")


#===============================================
# Añadiendo leyendas al grafico

text(sd_vect[1],mean_vect[1],"GE",cex=1.15)
text(sd_vect[2],mean_vect[2],"IBM",cex=1.15)
text(sd_vect[3],mean_vect[3],"Mobil",cex=1.15)

#===============================================
w = seq(0, 1.5,len = 500)

covjm = cov(R[,3], data.matrix(R)%*%weights[ind])

mup2 = w * as.numeric (weights[ind] %*% mean_vect) + (1 - w) * mean_vect[3]

sdp2 = sqrt(w^2 * sdP[ind]^2 + (1 - w)^2 * sd_vect[3]^2 + 2 * w * (1 - w) * as.numeric (covjm))

lines(sdp2, mup2, lwd = 3, col = "purple")

legend("topleft", c("CML", "Frontera Eficiente","Portafolios de tangencia y MOBIL","Portafolio de Tangencia"),
       col = c("blue","red","purple","black"),
       lty = c(1,1,1,NA), lwd = c(4,3,3,NA), pch = c(NA,NA,NA,"*"), pt.cex = c(1,1,1,4)  )


#### Bibliografía ####
### Ruppert & Matteson (2015). Statistics and Data Analysis for Financial 
### Engineering with R examples. 2nd Ed. Springer. 






