# -*- coding: utf-8 -*-
"""
Created on Thu Jun 2 15:26:33 2022

@author: Alfonso Chang, MSc FE
"""

import datetime as dt
import pandas as pd
import numpy as np
import yfinance as yf
import matplotlib.pyplot as plt
from scipy.stats import skew, kurtosis, chi2, linregress


# === 1. Cotizaciones (Precios) y Retornos ===
risky_assets = ['BHP', 'GOOG', 'MELI', 'MSFT', 'BAC']  # Tickers: 'BHP', 'GOOG', 'MELI', 'MSFT', 'PFE', 'BBVA' , 'AAPL', 'BAC'
market_asset = '^GSPC' # '^GSPC', '^IXIC', '^DJI', 'SPY' 
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
returns.tail()

# Graficar los retornos de los activos
plt.figure(figsize =(12, 10))
returns.plot(subplots = True, title = f'Retornos de acciones: De {start_date} al {end_date}')
plt.xticks(rotation=0)
# plt.show()
plt.tight_layout()


# === 2. Portafolios ===

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

# Calculamos el retorno de portafolio por cada simulación y se anualiza
portfolio_return = np.dot(weights, mean_returns)*252

# Calulamos la Matriz de Varianzas-Covarianzas
cov_mat = returns.cov()*252
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

# Imprimiendo el Sharpe maximo

print(' ')
print('=============================')
print( 'Max Ratio Sharpe is ' + str(np.round(max(portfolio_results_df['Ratio_sharpe']), 4)))
print('=============================')


# === 3. Capital Asset Pricing Model (base) ===

'''
Preparar los activos subyacentes al CAPM: (i) Activo representativo del 
mercado (^GSPC), (ii) del rendimiento risk-free (T-Bills/T-Bonds), (iii) el
activo a analizar:
'''

''' (i) '''
market_prices = yf.download(market_asset , start=start_date, end=end_date, adjusted=True) 
market_returns = market_prices['Adj Close'].pct_change().dropna()*100

mean_market_returns = market_returns.mean()

''' (ii) '''
rf = 0.03 / 252 # Annual


''' (iii) '''
''' Cualquier activo riesgoso: p.e. alguno de los de la sección 2. '''

ticker = 'BAC'  # Tickers: 'BHP', 'GOOG', 'MELI', 'MSFT', 'PFE', 'BBVA', 'BAC'

asset = returns[ticker]
asset_str = "Real returns " + ticker

data_frame_prices = data_frame['Adj Close']

# Graficando precios en series de tiempo
plt.figure(figsize=(10, 8))
plt.plot(data_frame_prices[ticker], c='blue', label=str(ticker))
plt.title(f'Precios reales en series de tiempo de {ticker}')
plt.xlabel('Tiempo')
plt.ylabel('Precios')
plt.legend(loc='upper left')
plt.show()

plt.figure(figsize=(10, 8))
plt.plot(market_prices['Adj Close'], c='red', label=str(market_asset))
plt.title(f'Precios reales en series de tiempo de {market_asset}')
plt.xlabel('Tiempo')
plt.ylabel('Precios')
plt.legend(loc='upper left')
plt.show()

# Grafico conjunto
plt.figure(figsize=(10, 8))
plt.title(f'Precios reales en series de tiempo de {ticker} y {market_asset}')
plt.xlabel('Tiempo')
plt.ylabel('Precios')
ax = plt.gca()
ax1 = market_prices['Adj Close'].plot(kind='line', x='date', ax=ax, label=market_asset)
ax2 = data_frame_prices[ticker].plot(kind='line', x='date', ax=ax, secondary_y=True, label=ticker)
ax1.legend(loc=2)
ax2.legend(loc=1)
plt.show()


# Graficando retornos en series de tiempo
plt.figure()
plt.title('Asset & Market Returns')
plt.plot(asset, label=str(ticker)+' Returns')
plt.plot(market_returns, c='red', label=str(market_asset)+' Returns')
plt.legend(loc='upper left')
plt.show()

# Calculando los momentos de la variable aleatoria y generando un
# test de normalidad: Jarque-Vera
asset_mean = asset.mean()
asset_std = asset.std()
asset_skewness = asset.skew()
asset_kurtosis = asset.kurtosis()  # Excess kurtosis
asset_jb_stat = len(asset)/6 * (asset_skewness**2 + 1/4*asset_kurtosis**2)
asset_pvalue = 1 - chi2.cdf(asset_jb_stat, df=2)
asset_is_normal = (asset_pvalue > 0.05)
asset_sharpe = asset_mean / asset_std * np.sqrt(252)  # Sharpe anualizado

# Print asset metrics
digits = 4

print(' ')
print('=============================')
print(asset_str)
print('=============================')
print('Mean is ' + str(np.round(asset_mean, digits)))
print('Std Dev is ' + str(np.round(asset_std, digits)))
print('Skewness is ' + str(np.round(asset_skewness, digits)))
print('Kurtosis is ' + str(np.round(asset_kurtosis, digits)))
print('JB statistic is ' + str(np.round(asset_jb_stat, digits)))
print('p value is ' + str(asset_pvalue))
print('is normal ' + str(asset_is_normal))
print('Sharpe ratio is ' + str(np.round(asset_sharpe, digits)))

str_xlabel_1 = 'Mean ' + str(np.round(asset_mean, digits))\
        + ' | Std Dev ' + str(np.round(asset_std, digits))\
        + ' | Skewness ' + str(np.round(asset_skewness, digits))\
        + ' | Kurtosis ' + str(np.round(asset_kurtosis, digits))
       

str_xlabel_2 = ' JB statistic ' + str(np.round(asset_jb_stat, digits-2))\
        + ' | p value ' + str(np.round(asset_pvalue, digits))\
        + ' | is normal ' + str(asset_is_normal)\
        + ' | Sharpe ratio ' + str(np.round(asset_sharpe, digits))   

# Graficar el histograma de retornos

asset_description = 'Market Data: ' + str(ticker)

plt.figure()
plt.hist(asset, bins=100)
plt.title('Histograma ' + asset_description)
plt.xlabel(str_xlabel_1 + '\n' + str_xlabel_2)
plt.show()

# Regresión entre el activo representativo del mercado (Market_returns = S&P500) &
# el activo que quiero analizar (Risky Asset = 'el que escoja')

# Regresión Lineal
slope, intercept, r_value, p_value, std_err = linregress(market_returns, asset)

beta = np.round(slope, digits)
alpha = np.round(intercept, digits)
p_value = np.round(p_value, digits)
null_hypotesis = p_value > 0.05  # p_value < 0.05 --> se rechaza Hipotesis Nula
r_value = np.round(r_value, digits)  # coeficiente de correlacion
r_squared = np.round(r_value**2, digits)  # porcentaje de la varianza del activo explicada por el mercado
predictor_linreg = alpha + beta * market_returns

# Imprimir datos relevantes

print('=============================================')
print('Estimación de parámetros del CAPM estimado:')
print('============================================')
print('Beta is ' + str(np.round(slope, digits)))
print('Alpha is ' + str(np.round(intercept, digits)))
print('p value is ' + str(np.round(p_value, digits)))
print('Null hyotesis is ' + str(p_value > 0.05))
print('Correl is ' + str(np.round(r_value, digits)))
print('R-squared is ' + str(np.round(r_value**2, digits)))


# Graficando la Regresión Lineal // forma de Scatter plot

str_title_capm1 = 'CAPM: ' + str(ticker) + ' Vs. ' + str(market_asset)
str_titel_capm2 = 'Nube de retornos' + '\n'\
    + 'Regresion lineal entre Ticker ' + str(ticker) + ' y el Mercado ' + str(market_asset) + '\n'\
    + 'alpha (intercept) ' + str(alpha) + ' | beta (slope) ' + str(beta) + '\n'\
    + 'p-value ' + str(p_value) + ' | hipotesis nula ' + str(null_hypotesis) + '\n'\
    + 'r-value (correl) ' + str(r_value) + ' | r-squared ' + str(r_squared)

plt.figure()
plt.title(str_title_capm1)
plt.scatter(market_returns, asset)
plt.plot(market_returns, predictor_linreg, color='red')
plt.ylabel(ticker)
plt.xlabel(market_asset + '\n' + '\n' +
           str_titel_capm2)
plt.grid()
plt.show()

########################
########## Fin #########
########################










