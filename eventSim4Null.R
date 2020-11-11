# simstudy Generando secuencias

ini<- Sys.time()


set.seed(1234567)


#dd4Null20 <- genMarkov(n = 5000, transMat = tmatrix4Null, 
#                chainLen = 20, wide = TRUE)
dd4Null30 <- genMarkov(n = 5005, transMat = tmatrix4Null, 
                       chainLen = 30, wide = TRUE)
dd4Null40 <- genMarkov(n = 5000, transMat = tmatrix4Null, 
                       chainLen = 40, wide = TRUE)
dd4Null50 <- genMarkov(n = 5000, transMat = tmatrix4Null, 
                       chainLen = 50, wide = TRUE)
dd4Null100 <- genMarkov(n = 5000, transMat = tmatrix4Null, 
                       chainLen = 100, wide = TRUE)
dd4Null200 <- genMarkov(n = 5000, transMat = tmatrix4Null, 
                       chainLen = 200, wide = TRUE)
dd4Null500 <- genMarkov(n = 5000, transMat = tmatrix4Null, 
                        chainLen = 500, wide = TRUE)

#dd4Null20<- dd4Null20[,-1]
dd4Null30<- dd4Null30[,-1]
dd4Null40<- dd4Null40[,-1]
dd4Null50<- dd4Null50[,-1]
dd4Null100<- dd4Null100[,-1]
dd4Null200<- dd4Null200[,-1]
dd4Null500<- dd4Null500[,-1]

# Cálculo de las matrices de transición

#mt4Null20<- alply(dd4Null20,1,transi)
mt4Null30<- alply(dd4Null30,1,transi)
mt4Null40<- alply(dd4Null40,1,transi)
mt4Null50<- alply(dd4Null50,1,transi)
mt4Null100<- alply(dd4Null100,1,transi)
mt4Null200<- alply(dd4Null200,1,transi)
mt4Null500<- alply(dd4Null500,1,transi)

# Cálculo de los residuales


#residT4Null20<- lapply(mt4Null20,resid2)
residT4Null30<- lapply(mt4Null30,resid2)
residT4Null40<- lapply(mt4Null40,resid2)
residT4Null50<- lapply(mt4Null50,resid2)
residT4Null100<- lapply(mt4Null100,resid2)
residT4Null200<- lapply(mt4Null200,resid2)
residT4Null500<- lapply(mt4Null500,resid2)

vN30<- excepciones(residT4Null30,4)
v40<- excepciones(residT4Null40,4)
residT4Null30<- residT4Null30[-vN30]

# Cálculo de errores modelo nulo

#erroresT4Null20<- lapply(residT4Null20,erroresNull)
erroresT4Null30<- lapply(residT4Null30,erroresNull)
erroresT4Null40<- lapply(residT4Null40,erroresNull)
erroresT4Null50<- lapply(residT4Null50,erroresNull)
erroresT4Null100<- lapply(residT4Null100,erroresNull)
erroresT4Null200<- lapply(residT4Null200,erroresNull)
erroresT4Null500<- lapply(residT4Null500,erroresNull)

# Porcentaje de errores

#perroresT4Null20<- sapply(erroresT4Null20,perror)
perroresT4Null30<- sapply(erroresT4Null30,perror)
perroresT4Null40<- sapply(erroresT4Null40,perror)
perroresT4Null50<- sapply(erroresT4Null50,perror)
perroresT4Null100<- sapply(erroresT4Null100,perror)
perroresT4Null200<- sapply(erroresT4Null200,perror)
perroresT4Null500<- sapply(erroresT4Null500,perror)


# Media

#media4Null20<- mean(perroresT4Null20,na.rm=T)
media4Null30<- mean(perroresT4Null30,na.rm=T)
media4Null40<- mean(perroresT4Null40,na.rm=T)
media4Null50<- mean(perroresT4Null50,na.rm=T)
media4Null100<- mean(perroresT4Null100,na.rm=T)
media4Null200<- mean(perroresT4Null200,na.rm=T)
media4Null500<- mean(perroresT4Null500,na.rm=T)

# dt

#dt4Null20<- sd(perroresT4Null20,na.rm=T)
dt4Null30<- sd(perroresT4Null30,na.rm=T)
dt4Null40<- sd(perroresT4Null40,na.rm=T)
dt4Null50<- sd(perroresT4Null50,na.rm=T)
dt4Null100<- sd(perroresT4Null100,na.rm=T)
dt4Null200<- sd(perroresT4Null200,na.rm=T)
dt4Null500<- sd(perroresT4Null500,na.rm=T)

media4Null<- c(media4Null30,
          media4Null40,media4Null50,
          media4Null100,media4Null200,media4Null500)

dt4Null<- c(dt4Null30,
       dt4Null40,dt4Null50,
       dt4Null100,dt4Null200,dt4Null500)

res4Null<- data.frame(media4Null, dt4Null)
row.names(res4Null)<- c('30','40','50','100','200','500')
round(res4Null,2)

fin<- Sys.time()

fin-ini

