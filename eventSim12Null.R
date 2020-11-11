# simstudy Generando secuencias

ini<- Sys.time()


set.seed(1234567)


# #dd12Null20 <- genMarkov(n = 5000, transMat = tmatrix12Null, 
# #                chainLen = 20, wide = TRUE)
# dd12Null30 <- genMarkov(n = 20000, transMat = tmatrix12N, 
#                        chainLen = 30, wide = TRUE)
# dd12Null40 <- genMarkov(n = 7500, transMat = tmatrix12N, 
#                        chainLen = 40, wide = TRUE)
# dd12Null50 <- genMarkov(n = 5006, transMat = tmatrix12N, 
#                        chainLen = 50, wide = TRUE)
dd12Null100 <- genMarkov(n = 5006, transMat = tmatrix12N, 
                       chainLen = 100, wide = TRUE)
dd12Null200 <- genMarkov(n = 5000, transMat = tmatrix12N, 
                       chainLen = 200, wide = TRUE)
dd12Null500 <- genMarkov(n = 5000, transMat = tmatrix12N, 
                        chainLen = 500, wide = TRUE)

#dd12Null20<- dd12Null20[,-1]
# dd12Null30<- dd12Null30[,-1]
# dd12Null40<- dd12Null40[,-1]
#dd12Null50<- dd12Null50[,-1]
dd12Null100<- dd12Null100[,-1]
dd12Null200<- dd12Null200[,-1]
dd12Null500<- dd12Null500[,-1]


               
# dd12Null50<- dd12Null50[-c(635, 1616, 1711, 1759, 1889, 2069, 2869,
#                            4388, 4935),]
# dd12Null100<- dd12Null100[-c(635, 1616, 1711, 1759, 1889, 2069, 2869,
#                              4388, 4935),]
# dd12Null200<- dd12Null200[-c(635, 1616, 1711, 1759, 1889, 2069,
#                              2869, 4388, 4935),]
# dd12Null500<- dd12Null500[-c(635, 1616, 1711, 1759, 1889, 2069,
#                              2869, 4388, 4935),]

# Cálculo de las matrices de transición

# #mt12Null20<- alply(dd12Null20,1,transi)
# mt12Null30<- alply(dd12Null30,1,transi)
# mt12Null40<- alply(dd12Null40,1,transi)
#mt12Null50<- alply(dd12Null50,1,transi)
mt12Null100<- alply(dd12Null100,1,transi)
mt12Null200<- alply(dd12Null200,1,transi)
mt12Null500<- alply(dd12Null500,1,transi)

# Cálculo de los residuales


#residT12Null20<- lapply(mt12Null20,resid2)
# residT12Null30<- lapply(mt12Null30,resid2)
# residT12Null40<- lapply(mt12Null40,resid2)
#residT12Null50<- lapply(mt12Null50,resid2)
residT12Null100<- lapply(mt12Null100,resid2)
residT12Null200<- lapply(mt12Null200,resid2)
residT12Null500<- lapply(mt12Null500,resid2)

# v12N30<- excepciones(residT12Null30,12)
# v12N40<- excepciones(residT12Null40,12)
#v12N50<- excepciones(residT12Null50,12)
v12N100<- excepciones(residT12Null100,12)
v12N200<- excepciones(residT12Null200,12)
v12N500<- excepciones(residT12Null500,12)

# length(v12N30)
# length(v12N40)
#length(v12N50)
length(v12N100)
length(v12N200)
length(v12N500)

# residT12Null30<- residT12Null30[-v12N30]
# residT12Null40<- residT12Null40[-v12N40]
# residT12Null50<- residT12Null40[-v12N50]
residT12Null100<- residT12Null100[-v12N100]
#residT12Null200<- residT12Null30[-v12N200]


# Cálculo de errores modelo nulo

#erroresT12Null20<- lapply(residT12Null20,erroresNull)
# erroresT12Null30<- lapply(residT12Null30,erroresNull)
# erroresT12Null40<- lapply(residT12Null40,erroresNull)
# erroresT12Null50<- lapply(residT12Null50,erroresNull)
erroresT12Null100<- lapply(residT12Null100,erroresNull)
erroresT12Null200<- lapply(residT12Null200,erroresNull)
erroresT12Null500<- lapply(residT12Null500,erroresNull)

# Porcentaje de errores

#perroresT12Null20<- sapply(erroresT12Null20,perror)
# perroresT12Null30<- sapply(erroresT12Null30,perror)
# perroresT12Null40<- sapply(erroresT12Null40,perror)
# perroresT12Null50<- sapply(erroresT12Null50,perror)
perroresT12Null100<- sapply(erroresT12Null100,perror)
perroresT12Null200<- sapply(erroresT12Null200,perror)
perroresT12Null500<- sapply(erroresT12Null500,perror)


# Media

#media12Null20<- mean(perroresT12Null20,na.rm=T)
# media12Null30<- mean(perroresT12Null30,na.rm=T)
# media12Null40<- mean(perroresT12Null40,na.rm=T)
# media12Null50<- mean(perroresT12Null50,na.rm=T)
media12Null100<- mean(perroresT12Null100,na.rm=T)
media12Null200<- mean(perroresT12Null200,na.rm=T)
media12Null500<- mean(perroresT12Null500,na.rm=T)

# dt

#dt12Null20<- sd(perroresT12Null20,na.rm=T)
# dt12Null30<- sd(perroresT12Null30,na.rm=T)
# dt12Null40<- sd(perroresT12Null40,na.rm=T)
# dt12Null50<- sd(perroresT12Null50,na.rm=T)
dt12Null100<- sd(perroresT12Null100,na.rm=T)
dt12Null200<- sd(perroresT12Null200,na.rm=T)
dt12Null500<- sd(perroresT12Null500,na.rm=T)

media12Null<- c(NA,NA,NA,media12Null100,media12Null200,media12Null500)

dt12Null<- c(NA,NA,NA,dt12Null100,dt12Null200,dt12Null500)

res12Null<- data.frame(media12Null, dt12Null)
row.names(res12Null)<- c('30','40','50','100','200','500')
round(res12Null,2)

fin<- Sys.time()

fin-ini

