# simstudy Generando secuencias

options(error=recover)

ini<- Sys.time()

set.seed(1234567)


#dd4B20 <- genMarkov(n = 5000, transMat = tmatrix4B, 
#               chainLen = 20, wide = TRUE)
dd4B30 <- genMarkov(n = 5010, transMat = tmatrix4B, 
                       chainLen = 30, wide = TRUE)
dd4B40 <- genMarkov(n = 5000, transMat = tmatrix4B, 
                       chainLen = 40, wide = TRUE)
dd4B50 <- genMarkov(n = 5000, transMat = tmatrix4B, 
                       chainLen = 50, wide = TRUE)
dd4B100 <- genMarkov(n = 5000, transMat = tmatrix4B, 
                       chainLen = 100, wide = TRUE)
dd4B200 <- genMarkov(n = 5000, transMat = tmatrix4B, 
                       chainLen = 200, wide = TRUE)
dd4B500 <- genMarkov(n = 5000, transMat = tmatrix4B, 
                     chainLen = 500, wide = TRUE)

#dd4B20<- dd4B20[,-1]
dd4B30<- dd4B30[,-1]
dd4B40<- dd4B40[,-1]
dd4B50<- dd4B50[,-1]
dd4B100<- dd4B100[,-1]
dd4B200<- dd4B200[,-1]
dd4B500<- dd4B500[,-1]

# Cálculo de las matrices de transición

#mt4B20<- alply(dd4B20,1,transi)
mt4B30<- alply(dd4B30,1,transi)
mt4B40<- alply(dd4B40,1,transi)
mt4B50<- alply(dd4B50,1,transi)
mt4B100<- alply(dd4B100,1,transi)
mt4B200<- alply(dd4B200,1,transi)
mt4B500<- alply(dd4B500,1,transi)


# Cálculo de los residuales

#residT4B20<- lapply(mt4B20,resid2)
residT4B30<- lapply(mt4B30,resid2)
residT4B40<- lapply(mt4B40,resid2)
residT4B50<- lapply(mt4B50,resid2)
residT4B100<- lapply(mt4B100,resid2)
residT4B200<- lapply(mt4B200,resid2)
residT4B500<- lapply(mt4B500,resid2)


# Cálculo de errores modelo con efecto mayor que azar

#v20<- excepciones(residT4B20,4)
vB30<- excepciones(residT4B30,4)
#v40<- excepciones(residT4B40,4)
#residT4B20<- residT4B20[-v20]
residT4B30<- residT4B30[-vB30]
#residT4B40<- residT4B30[-v40]
#erroresT4B20<- lapply(residT4B20,FUN=erroresSig2,k=4, fila=2,col=3)
erroresT4B30<- lapply(residT4B30,FUN=erroresSig2,k=4,fila=2,col=3)
erroresT4B40<- lapply(residT4B40,FUN=erroresSig2,k=4,fila=2,col=3)
erroresT4B50<- lapply(residT4B50,FUN=erroresSig2,k=4,fila=2,col=3)
erroresT4B100<- lapply(residT4B100,FUN=erroresSig2,k=4,fila=2,col=3)
erroresT4B200<- lapply(residT4B200,FUN=erroresSig2,k=4,fila=2,col=3)
erroresT4B500<- lapply(residT4B500,FUN=erroresSig2,k=4,fila=2,col=3)

 # Porcentaje de errores totales
 
#perroresT4B20<- sapply(erroresT4B20,et)
perroresT4B30<- sapply(erroresT4B30,et)
perroresT4B40<- sapply(erroresT4B40,et)
perroresT4B50<- sapply(erroresT4B50,et)
perroresT4B100<- sapply(erroresT4B100,et)
perroresT4B200<- sapply(erroresT4B200,et)
perroresT4B500<- sapply(erroresT4B500,et)

# Media errores totales

#mediaet4B20<- mean(perroresT4B20,na.rm=T)
mediaET4B30<- mean(perroresT4B30,na.rm=T)
mediaET4B40<- mean(perroresT4B40,na.rm=T)
mediaET4B50<- mean(perroresT4B50,na.rm=T)
mediaET4B100<- mean(perroresT4B100,na.rm=T)
mediaET4B200<- mean(perroresT4B200,na.rm=T)
mediaET4B500<- mean(perroresT4B500,na.rm=T)

# dt errores totales

#dtET4B20<- sd(perroresT4B20,na.rm=T)
dtET4B30<- sd(perroresT4B30,na.rm=T)
dtET4B40<- sd(perroresT4B40,na.rm=T)
dtET4B50<- sd(perroresT4B50,na.rm=T)
dtET4B100<- sd(perroresT4B100,na.rm=T)
dtET4B200<- sd(perroresT4B200,na.rm=T)
dtET4B500<- sd(perroresT4B500,na.rm=T)

mediaET4B<- c(mediaET4B30,mediaET4B40,mediaET4B50,
          mediaET4B100,mediaET4B200,mediaET4B500)

dtET4B<- c(dtET4B30,dtET4B40,dtET4B50,
           dtET4B100,dtET4B200,dtET4B500)

resET4B<- data.frame(mediaET4B, dtET4B)
row.names(resET4B)<-c('30','40','50','100','200','500')



# Porcentaje de errores casilla significativa

#perroresCS4B20<- sapply(erroresT4B20,ecs)
perroresCS4B30<- sapply(erroresT4B30,ecs)
perroresCS4B40<- sapply(erroresT4B40,ecs)
perroresCS4B50<- sapply(erroresT4B50,ecs)
perroresCS4B100<- sapply(erroresT4B100,ecs)
perroresCS4B200<- sapply(erroresT4B200,ecs)
perroresCS4B500<- sapply(erroresT4B500,ecs)

# Media errores casilla significativa

#mediaECS4B20<- mean(perroresCS4B20,na.rm=T)
mediaECS4B30<- mean(perroresCS4B30,na.rm=T)
mediaECS4B40<- mean(perroresCS4B40,na.rm=T)
mediaECS4B50<- mean(perroresCS4B50,na.rm=T)
mediaECS4B100<- mean(perroresCS4B100,na.rm=T)
mediaECS4B200<- mean(perroresCS4B200,na.rm=T)
mediaECS4B500<- mean(perroresCS4B500,na.rm=T)

# dt errores casilla significativa

#dtECS4B20<- sd(perroresCS4B20,na.rm=T)
dtECS4B30<- sd(perroresCS4B30,na.rm=T)
dtECS4B40<- sd(perroresCS4B40,na.rm=T)
dtECS4B50<- sd(perroresCS4B50,na.rm=T)
dtECS4B100<- sd(perroresCS4B100,na.rm=T)
dtECS4B200<- sd(perroresCS4B200,na.rm=T)
dtECS4B500<- sd(perroresCS4B500,na.rm=T)

mediaECS4B<- c(mediaECS4B30,mediaECS4B40,mediaECS4B50,
              mediaECS4B100,mediaECS4B200,mediaECS4B500)

dtECS4B<- c(dtECS4B30,dtECS4B40,dtECS4B50,dtECS4B100,
            dtECS4B200,dtECS4B500)

resECS4B<- data.frame(mediaECS4B, dtECS4B)
row.names(resECS4B)<-c('30','40','50','100','200','500')



# Porcentaje de errores fila significativa

#perroresFS4B20<- sapply(erroresT4B20,efs)
perroresFS4B30<- sapply(erroresT4B30,efs)
perroresFS4B40<- sapply(erroresT4B40,efs)
perroresFS4B50<- sapply(erroresT4B50,efs)
perroresFS4B100<- sapply(erroresT4B100,efs)
perroresFS4B200<- sapply(erroresT4B200,efs)
perroresFS4B500<- sapply(erroresT4B500,efs)

# Media errores fila significativa

#mediaEFS4B20<- mean(perroreFS4B20,na.rm=T)
mediaEFS4B30<- mean(perroresFS4B30,na.rm=T)
mediaEFS4B40<- mean(perroresFS4B40,na.rm=T)
mediaEFS4B50<- mean(perroresFS4B50,na.rm=T)
mediaEFS4B100<- mean(perroresFS4B100,na.rm=T)
mediaEFS4B200<- mean(perroresFS4B200,na.rm=T)
mediaEFS4B500<- mean(perroresFS4B500,na.rm=T)

# dt errores fila significativa

#dtEFS4B20<- sd(perroresFS4B20,na.rm=T)
dtEFS4B30<- sd(perroresFS4B30,na.rm=T)
dtEFS4B40<- sd(perroresFS4B40,na.rm=T)
dtEFS4B50<- sd(perroresFS4B50,na.rm=T)
dtEFS4B100<- sd(perroresFS4B100,na.rm=T)
dtEFS4B200<- sd(perroresFS4B200,na.rm=T)
dtEFS4B500<- sd(perroresFS4B500,na.rm=T)

mediaEFS4B<- c(mediaEFS4B30,mediaEFS4B40,mediaEFS4B50,
               mediaEFS4B100,mediaEFS4B200,mediaEFS4B500)

dtEFS4B<- c(dtEFS4B30,dtEFS4B40,dtEFS4B50,dtEFS4B100,
            dtEFS4B200,dtEFS4B500)

resEFS4B<- data.frame(mediaEFS4B, dtEFS4B)
row.names(resET4B)<-c('30','40','50','100','200','500')


# Porcentaje de errores fila NO significativa

#perroreFNS4B20<- sapply(erroresT4B20,efns)
perroresFNS4B30<- sapply(erroresT4B30,efns)
perroresFNS4B40<- sapply(erroresT4B40,efns)
perroresFNS4B50<- sapply(erroresT4B50,efns)
perroresFNS4B100<- sapply(erroresT4B100,efns)
perroresFNS4B200<- sapply(erroresT4B200,efns)
perroresFNS4B500<- sapply(erroresT4B500,efns)

# Media errores fila NO significativa

#mediaEFNS4B20<- mean(perroreFNS4B20,na.rm=T)
mediaEFNS4B30<- mean(perroresFNS4B30,na.rm=T)
mediaEFNS4B40<- mean(perroresFNS4B40,na.rm=T)
mediaEFNS4B50<- mean(perroresFNS4B50,na.rm=T)
mediaEFNS4B100<- mean(perroresFNS4B100,na.rm=T)
mediaEFNS4B200<- mean(perroresFNS4B200,na.rm=T)
mediaEFNS4B500<- mean(perroresFNS4B500,na.rm=T)

# dt errores fila NO significativa

#dtEFNS4B20<- sd(perroresFNS4B20,na.rm=T)
dtEFNS4B30<- sd(perroresFNS4B30,na.rm=T)
dtEFNS4B40<- sd(perroresFNS4B40,na.rm=T)
dtEFNS4B50<- sd(perroresFNS4B50,na.rm=T)
dtEFNS4B100<- sd(perroresFNS4B100,na.rm=T)
dtEFNS4B200<- sd(perroresFNS4B200,na.rm=T)
dtEFNS4B500<- sd(perroresFNS4B500,na.rm=T)

mediaEFNS4B<- c(mediaEFNS4B30,mediaEFNS4B40,mediaEFNS4B50,
               mediaEFNS4B100,mediaEFNS4B200,mediaEFNS4B500)

dtEFNS4B<- c(dtEFNS4B30,dtEFNS4B40,
             dtEFNS4B50,dtEFNS4B100,dtEFNS4B200,
             dtEFNS4B500)

resEFNS4B<- data.frame(mediaEFNS4B, dtEFNS4B)
row.names(resET4B)<-c('30','40','50','100','200','500')



fin<- Sys.time()

fin-ini

