#====================================================
#=== UNIVERSIDAD NACIONAL DE INGENIERIA - FIEECS ====
#====================================================
## Finanzas Corporativas II - MSc Alfonso Chang M. ##
#====================================================
# U2: Intr. a la Teoria del Portafolio de Inversiones


#===========================================================
# Descargar Tickets desde Yahoo Finance y grabarlo en *csv
#===========================================================



library(quantmod)
getSymbols.yahoo('^GSPC',
                 env=.GlobalEnv, 
                 return.class = 'xts', 
                 index.class = 'Date',
                 from='2016-06-01', 
                 to='2021-07-05',
                 periodicity = "monthly")

#write.csv(GSPC, file = 'D:/Usuario- Alfonso/Desktop/15052021/GSPC.csv')

write.zoo(GSPC, file = 'D:/Usuario- Alfonso/Desktop/GSPC.csv',
          index.name = "Date",row.names = FALSE, col.names = TRUE, sep= ",")


getSymbols.yahoo('PPL',
                 env=.GlobalEnv, 
                 return.class = 'xts', 
                 index.class = 'Date',
                 from='2016-06-01', 
                 to='2021-07-05',
                 periodicity = "monthly")

#write.csv(GSPC, file = 'D:/Usuario- Alfonso/Desktop/15052021/GSPC.csv')

write.zoo(PPL, file = 'D:/Usuario- Alfonso/Desktop/PPL.csv',
          index.name = "Date",row.names = FALSE, col.names = TRUE, sep= ",")
