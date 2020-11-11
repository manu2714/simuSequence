# simstudy Generando secuencias

ini<- Sys.time()


set.seed(1234567)


# #dd8Null20 <- genMarkov(n = 5000, transMat = tmatrix8Null, 
# #                chainLen = 20, wide = TRUE)
dd8Null30 <- genMarkov(n = 6306, transMat = tmatrix8N, 
                       chainLen = 30, wide = TRUE)
dd8Null40 <- genMarkov(n = 5345, transMat = tmatrix8N, 
                       chainLen = 40, wide = TRUE)
dd8Null50 <- genMarkov(n = 5109, transMat = tmatrix8N, 
                       chainLen = 50, wide = TRUE)
dd8Null100 <- genMarkov(n = 5000, transMat = tmatrix8N, 
                       chainLen = 100, wide = TRUE)
dd8Null200 <- genMarkov(n = 5000, transMat = tmatrix8N, 
                       chainLen = 200, wide = TRUE)
dd8Null500 <- genMarkov(n = 5000, transMat = tmatrix8N, 
                        chainLen = 500, wide = TRUE)

#dd8Null20<- dd8Null20[,-1]
dd8Null30<- dd8Null30[,-1]
dd8Null40<- dd8Null40[,-1]
dd8Null50<- dd8Null50[,-1]
dd8Null100<- dd8Null100[,-1]
dd8Null200<- dd8Null200[,-1]
dd8Null500<- dd8Null500[,-1]

# Cálculo de las matrices de transición

#mt8Null20<- alply(dd8Null20,1,transi)
mt8Null30<- alply(dd8Null30,1,transi)
mt8Null40<- alply(dd8Null40,1,transi)
mt8Null50<- alply(dd8Null50,1,transi)
mt8Null100<- alply(dd8Null100,1,transi)
mt8Null200<- alply(dd8Null200,1,transi)
mt8Null500<- alply(dd8Null500,1,transi)

# Cálculo de los residuales


#residT8Null20<- lapply(mt8Null20,resid2)
residT8Null30<- lapply(mt8Null30,resid2)
residT8Null40<- lapply(mt8Null40,resid2)
residT8Null50<- lapply(mt8Null50,resid2)
residT8Null100<- lapply(mt8Null100,resid2)
residT8Null200<- lapply(mt8Null200,resid2)
residT8Null500<- lapply(mt8Null500,resid2)

v8N30<- excepciones(residT8Null30,8)
v8N40<- excepciones(residT8Null40,8)
v8N50<- excepciones(residT8Null50,8)
v8N100<- excepciones(residT8Null100,8)
v8N200<- excepciones(residT8Null100,8)
v8N500<- excepciones(residT8Null100,8)

residT8Null30<- residT8Null30[-v8N30]
residT8Null40<- residT8Null30[-v8N40]
residT8Null50<- residT8Null30[-v8N50]



# Cálculo de errores modelo nulo

#erroresT8Null20<- lapply(residT8Null20,erroresNull)
erroresT8Null30<- lapply(residT8Null30,erroresNull)
erroresT8Null40<- lapply(residT8Null40,erroresNull)
erroresT8Null50<- lapply(residT8Null50,erroresNull)
erroresT8Null100<- lapply(residT8Null100,erroresNull)
erroresT8Null200<- lapply(residT8Null200,erroresNull)
erroresT8Null500<- lapply(residT8Null500,erroresNull)

# Porcentaje de errores

#perroresT8Null20<- sapply(erroresT8Null20,perror)
perroresT8Null30<- sapply(erroresT8Null30,perror)
perroresT8Null40<- sapply(erroresT8Null40,perror)
perroresT8Null50<- sapply(erroresT8Null50,perror)
perroresT8Null100<- sapply(erroresT8Null100,perror)
perroresT8Null200<- sapply(erroresT8Null200,perror)
perroresT8Null500<- sapply(erroresT8Null500,perror)


# Media

#media8Null20<- mean(perroresT8Null20,na.rm=T)
media8Null30<- mean(perroresT8Null30,na.rm=T)
media8Null40<- mean(perroresT8Null40,na.rm=T)
media8Null50<- mean(perroresT8Null50,na.rm=T)
media8Null100<- mean(perroresT8Null100,na.rm=T)
media8Null200<- mean(perroresT8Null200,na.rm=T)
media8Null500<- mean(perroresT8Null500,na.rm=T)

# dt

#dt8Null20<- sd(perroresT8Null20,na.rm=T)
dt8Null30<- sd(perroresT8Null30,na.rm=T)
dt8Null40<- sd(perroresT8Null40,na.rm=T)
dt8Null50<- sd(perroresT8Null50,na.rm=T)
dt8Null100<- sd(perroresT8Null100,na.rm=T)
dt8Null200<- sd(perroresT8Null200,na.rm=T)
dt8Null500<- sd(perroresT8Null500,na.rm=T)

media8Null<- c(media8Null30,
          media8Null40,media8Null50,
          media8Null100,media8Null200,media8Null500)

dt8Null<- c(dt8Null30,
       dt8Null40,dt8Null50,
       dt8Null100,dt8Null200,dt8Null500)

res8Null<- data.frame(media8Null, dt8Null)
row.names(res8Null)<- c('30','40','50','100','200','500')
round(res8Null,2)

fin<- Sys.time()

fin-ini

