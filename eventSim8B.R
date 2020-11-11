# simstudy Generando secuencias

options(error=recover)

ini<- Sys.time()

set.seed(1234567)


#dd8B20 <- genMarkov(n = 5000, transMat = tmatrix8B, 
#               chainLen = 20, wide = TRUE)
dd8B30 <- genMarkov(n = 6023, transMat = tmatrix8B, 
                       chainLen = 30, wide = TRUE)
dd8B40 <- genMarkov(n = 5249, transMat = tmatrix8B, 
                       chainLen = 40, wide = TRUE)
dd8B50 <- genMarkov(n = 5080, transMat = tmatrix8B, 
                       chainLen = 50, wide = TRUE)
dd8B100 <- genMarkov(n = 5000, transMat = tmatrix8B, 
                       chainLen = 100, wide = TRUE)
dd8B200 <- genMarkov(n = 5000, transMat = tmatrix8B, 
                       chainLen = 200, wide = TRUE)
dd8B500 <- genMarkov(n = 5000, transMat = tmatrix8B, 
                     chainLen = 500, wide = TRUE)

#dd8B20<- dd8B20[,-1]
dd8B30<- dd8B30[,-1]
dd8B40<- dd8B40[,-1]
dd8B50<- dd8B50[,-1]
dd8B100<- dd8B100[,-1]
dd8B200<- dd8B200[,-1]
dd8B500<- dd8B500[,-1]

# Cálculo de las matrices de transición

#mt8B20<- alply(dd8B20,1,transi)
mt8B30<- alply(dd8B30,1,transi)
mt8B40<- alply(dd8B40,1,transi)
mt8B50<- alply(dd8B50,1,transi)
mt8B100<- alply(dd8B100,1,transi)
mt8B200<- alply(dd8B200,1,transi)
mt8B500<- alply(dd8B500,1,transi)


# Cálculo de los residuales

#residT8B20<- lapply(mt8B20,resid2)
residT8B30<- lapply(mt8B30,resid2)
residT8B40<- lapply(mt8B40,resid2)
residT8B50<- lapply(mt8B50,resid2)
residT8B100<- lapply(mt8B100,resid2)
residT8B200<- lapply(mt8B200,resid2)
residT8B500<- lapply(mt8B500,resid2)


# Cálculo de errores modelo con efecto mayor que azar

#v20<- excepciones(residT8B20,8)
v8B30<- excepciones(residT8B30,8)
v8B40<- excepciones(residT8B40,8)
v8B50<- excepciones(residT8B50,8)
v8B100<- excepciones(residT8B100,8)
v8B200<- excepciones(residT8B200,8)
v8B500<- excepciones(residT8B500,8)

#residT8B20<- residT8B20[-v20]
residT8B30<- residT8B30[-v8B30]
residT8B40<- residT8B30[-v8B40]
residT8B50<- residT8B30[-v8B50]

#erroresT8B20<- lapply(residT8B20,FUN=erroresSigA,k=4, fila=2,col=3)
erroresT8B30<- lapply(residT8B30,FUN=erroresSig2,k=8,fila=6,col=3)
erroresT8B40<- lapply(residT8B40,FUN=erroresSig2,k=8,fila=6,col=3)
erroresT8B50<- lapply(residT8B50,FUN=erroresSig2,k=8,fila=6,col=3)
erroresT8B100<- lapply(residT8B100,FUN=erroresSig2,k=8,fila=6,col=3)
erroresT8B200<- lapply(residT8B200,FUN=erroresSig2,k=8,fila=6,col=3)
erroresT8B500<- lapply(residT8B500,FUN=erroresSig2,k=8,fila=6,col=3)

 # Porcentaje de errores totales
 
#perroresT8B20<- sapply(erroresT8B20,et)
perroresT8B30<- sapply(erroresT8B30,et)
perroresT8B40<- sapply(erroresT8B40,et)
perroresT8B50<- sapply(erroresT8B50,et)
perroresT8B100<- sapply(erroresT8B100,et)
perroresT8B200<- sapply(erroresT8B200,et)
perroresT8B500<- sapply(erroresT8B500,et)

# Media errores totales

#mediaet8B20<- mean(perroresT8B20,na.rm=T)

mediaET8B30<- mean(perroresT8B30,na.rm=T)
mediaET8B40<- mean(perroresT8B40,na.rm=T)
mediaET8B50<- mean(perroresT8B50,na.rm=T)
mediaET8B100<- mean(perroresT8B100,na.rm=T)
mediaET8B200<- mean(perroresT8B200,na.rm=T)
mediaET8B500<- mean(perroresT8B500,na.rm=T)

# dt errores totales

#dtET8B20<- sd(perroresT8B20,na.rm=T)
dtET8B30<- sd(perroresT8B30,na.rm=T)
dtET8B40<- sd(perroresT8B40,na.rm=T)
dtET8B50<- sd(perroresT8B50,na.rm=T)
dtET8B100<- sd(perroresT8B100,na.rm=T)
dtET8B200<- sd(perroresT8B200,na.rm=T)
dtET8B500<- sd(perroresT8B500,na.rm=T)

mediaET8B<- c(mediaET8B30,mediaET8B40,mediaET8B50,
          mediaET8B100,mediaET8B200,mediaET8B500)

dtET8B<- c(dtET8B30,dtET8B40,dtET8B50,
           dtET8B100,dtET8B200,dtET8B500)

resET8B<- data.frame(mediaET8B, dtET8B)
row.names(resET8B)<-c('30','40','50','100','200','500')



# Porcentaje de errores casilla significativa

#perroresCS8B20<- sapply(erroresT8B20,ecs)
perroresCS8B30<- sapply(erroresT8B30,ecs)
perroresCS8B40<- sapply(erroresT8B40,ecs)
perroresCS8B50<- sapply(erroresT8B50,ecs)
perroresCS8B100<- sapply(erroresT8B100,ecs)
perroresCS8B200<- sapply(erroresT8B200,ecs)
perroresCS8B500<- sapply(erroresT8B500,ecs)

# Media errores casilla significativa

#mediaECS8B20<- mean(perroresCS8B20,na.rm=T)
mediaECS8B30<- mean(perroresCS8B30,na.rm=T)
mediaECS8B40<- mean(perroresCS8B40,na.rm=T)
mediaECS8B50<- mean(perroresCS8B50,na.rm=T)
mediaECS8B100<- mean(perroresCS8B100,na.rm=T)
mediaECS8B200<- mean(perroresCS8B200,na.rm=T)
mediaECS8B500<- mean(perroresCS8B500,na.rm=T)

# dt errores casilla significativa

#dtECS8B20<- sd(perroresCS8B20,na.rm=T)
dtECS8B30<- sd(perroresCS8B30,na.rm=T)
dtECS8B40<- sd(perroresCS8B40,na.rm=T)
dtECS8B50<- sd(perroresCS8B50,na.rm=T)
dtECS8B100<- sd(perroresCS8B100,na.rm=T)
dtECS8B200<- sd(perroresCS8B200,na.rm=T)
dtECS8B500<- sd(perroresCS8B500,na.rm=T)

mediaECS8B<- c(mediaECS8B30,mediaECS8B40,mediaECS8B50,
              mediaECS8B100,mediaECS8B200,mediaECS8B500)

dtECS8B<- c(dtECS8B30,dtECS8B40,dtECS8B50,dtECS8B100,
            dtECS8B200,dtECS8B500)

resECS8B<- data.frame(mediaECS8B, dtECS8B)
row.names(resECS8B)<-c('30','40','50','100','200','500')



# Porcentaje de errores fila significativa

#perroresFS8B20<- sapply(erroresT8B20,efs)
perroresFS8B30<- sapply(erroresT8B30,efs)
perroresFS8B40<- sapply(erroresT8B40,efs)
perroresFS8B50<- sapply(erroresT8B50,efs)
perroresFS8B100<- sapply(erroresT8B100,efs)
perroresFS8B200<- sapply(erroresT8B200,efs)
perroresFS8B500<- sapply(erroresT8B500,efs)

# Media errores fila significativa

#mediaEFS8B20<- mean(perroreFS8B20,na.rm=T)
mediaEFS8B30<- mean(perroresFS8B30,na.rm=T)
mediaEFS8B40<- mean(perroresFS8B40,na.rm=T)
mediaEFS8B50<- mean(perroresFS8B50,na.rm=T)
mediaEFS8B100<- mean(perroresFS8B100,na.rm=T)
mediaEFS8B200<- mean(perroresFS8B200,na.rm=T)
mediaEFS8B500<- mean(perroresFS8B500,na.rm=T)

# dt errores fila significativa

#dtEFS8B20<- sd(perroresFS8B20,na.rm=T)
dtEFS8B30<- sd(perroresFS8B30,na.rm=T)
dtEFS8B40<- sd(perroresFS8B40,na.rm=T)
dtEFS8B50<- sd(perroresFS8B50,na.rm=T)
dtEFS8B100<- sd(perroresFS8B100,na.rm=T)
dtEFS8B200<- sd(perroresFS8B200,na.rm=T)
dtEFS8B500<- sd(perroresFS8B500,na.rm=T)

mediaEFS8B<- c(mediaEFS8B30,mediaEFS8B40,mediaEFS8B50,
               mediaEFS8B100,mediaEFS8B200,mediaEFS8B500)

dtEFS8B<- c(dtEFS8B30,dtEFS8B40,dtEFS8B50,dtEFS8B100,
            dtEFS8B200,dtEFS8B500)

resEFS8B<- data.frame(mediaEFS8B, dtEFS8B)
row.names(resET8B)<-c('30','40','50','100','200','500')


# Porcentaje de errores fila NO significativa

#perroreFNS8B20<- sapply(erroresT8B20,efns)
perroresFNS8B30<- sapply(erroresT8B30,efns)
perroresFNS8B40<- sapply(erroresT8B40,efns)
perroresFNS8B50<- sapply(erroresT8B50,efns)
perroresFNS8B100<- sapply(erroresT8B100,efns)
perroresFNS8B200<- sapply(erroresT8B200,efns)
perroresFNS8B500<- sapply(erroresT8B500,efns)

# Media errores fila NO significativa

#mediaEFNS8B20<- mean(perroreFNS8B20,na.rm=T)
mediaEFNS8B30<- mean(perroresFNS8B30,na.rm=T)
mediaEFNS8B40<- mean(perroresFNS8B40,na.rm=T)
mediaEFNS8B50<- mean(perroresFNS8B50,na.rm=T)
mediaEFNS8B100<- mean(perroresFNS8B100,na.rm=T)
mediaEFNS8B200<- mean(perroresFNS8B200,na.rm=T)
mediaEFNS8B500<- mean(perroresFNS8B500,na.rm=T)

# dt errores fila NO significativa

#dtEFNS8B20<- sd(perroresFNS8B20,na.rm=T)
dtEFNS8B30<- sd(perroresFNS8B30,na.rm=T)
dtEFNS8B40<- sd(perroresFNS8B40,na.rm=T)
dtEFNS8B50<- sd(perroresFNS8B50,na.rm=T)
dtEFNS8B100<- sd(perroresFNS8B100,na.rm=T)
dtEFNS8B200<- sd(perroresFNS8B200,na.rm=T)
dtEFNS8B500<- sd(perroresFNS8B500,na.rm=T)

mediaEFNS8B<- c(mediaEFNS8B30,mediaEFNS8B40,mediaEFNS8B50,
               mediaEFNS8B100,mediaEFNS8B200,mediaEFNS8B500)

dtEFNS8B<- c(dtEFNS8B30,dtEFNS8B40,
             dtEFNS8B50,dtEFNS8B100,dtEFNS8B200,
             dtEFNS8B500)

resEFNS8B<- data.frame(mediaEFNS8B, dtEFNS8B)
row.names(resET8B)<-c('30','40','50','100','200','500')

round(resET8B,2)
round(resECS8B,2)
round(resEFS8B,2)
round(resEFNS8B,2)


fin<- Sys.time()

fin-ini

