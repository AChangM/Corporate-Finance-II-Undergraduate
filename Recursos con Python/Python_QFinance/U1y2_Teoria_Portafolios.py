# -*- coding: utf-8 -*-
"""
Created on Wed Mar  2 15:26:33 2022

@author: Alfonso Chang. MSc
"""

import datetime as dt
import pandas as pd
import numpy as np
import yfinance as yf
import matplotlib.pyplot as plt
from scipy.stats import skew, kurtosis, chi2, linregress


#### === 1. Cotizaciones (Precios) y Retornos === ####
risky_assets = ['GOOG', 'MSFT', 'BHP', 'PFE', 'MELI']  # Tickers: 'IBM', 'GE', 'PFE', 'MELI', 'BHP'
market_asset = ['^GSPC']
risk_free = 0.035

# Si quieren setear fechas fijas
# start_date = '2015-01-01'
# end_date = '2021-12-31'

# Si quieren setear fechas dinamicas
end_date = dt.datetime.now()
start_date = dt.date(end_date.year - 6, end_date.month, end_date.day)

N = len(risky_assets)

data_frame = yf.download(risky_assets , start=start_date, end=end_date, adjusted=True) # + market_asset
returns = 100 * data_frame['Adj Close'].pct_change().dropna()
returns = np.round(returns, 4)
returns.head()

plt.figure(figsize =(12, 10))
returns.plot(subplots = True, title = f'Retornos de acciones: De {start_date} al {end_date}')
plt.xticks(rotation=0)
# plt.show()
plt.tight_layout()


#### === 2. Portafolios === ####

# Calculando los estadígrafos que usaremos
mean_returns = returns.mean()

# Generamos una matriz con números aleatorios de 1000 filas y 5 columnas
matrix = np.random.rand(10000,5)

# Convertimos la matriz en un data frame
matrix_df = pd.DataFrame(matrix, columns = returns.columns)
#matrix_df

matrix_sum = matrix_df.sum(axis = 1)
#matrix_sum

# Calculamos los pesos de los portafolios
weights  = matrix_df.divide(matrix_sum, axis ="rows")
#weights

# Transponemos
weights_T = np.transpose(weights)
#weights_T

# Calculamos el retorno de portafolio por cada simulación
portfolio_return = np.dot(weights, mean_returns)*12

# Calulamos la Matriz de Varianzas-Covarianzas
cov_mat = returns.cov()*12
#cov_mat

# Estimamos el Riesgo de Portafolio en el set de 1000 ports
portfolio_risk = []
for i in range(weights.shape[0]):
    risk = np.sqrt(np.dot(weights.iloc[i,:], np.dot(cov_mat , weights_T.iloc[:,i])))
    portfolio_risk.append(risk)

portfolio_risk = np.array(portfolio_risk)    
portfolio_sharpe_ratio = (portfolio_return - risk_free)/portfolio_risk     # Estimar ratio de sharpe de cada portafolio
    
portfolio_results_df = pd.DataFrame({'Returns': portfolio_return,
                                 'Volatility': portfolio_risk,
                                 'Ratio_sharpe': portfolio_sharpe_ratio})

# Graficamos el entorno Media-Varianza de Markowitz
plt.figure(figsize = (10,8))
plt.scatter(portfolio_risk, portfolio_return, c = portfolio_sharpe_ratio, alpha= 0.8)
plt.colorbar(label = 'Sharpe Ratio')
plt.title(f'Entorno Media-Varianza {risky_assets}')
plt.xlabel("Riesgo de Portafolio (Standard Deviation)")
plt.ylabel("Retorno de Portafolio")
plt.show()






