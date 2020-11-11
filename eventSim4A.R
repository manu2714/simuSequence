# simstudy Generando secuencias

options(error=recover)

ini<- Sys.time()

set.seed(1234567)


#dd4A20 <- genMarkov(n = 5000, transMat = tmatrix4A, 
#               chainLen = 20, wide = TRUE)
dd4A30 <- genMarkov(n = 5003, transMat = tmatrix4A, 
                       chainLen = 30, wide = TRUE)
dd4A40 <- genMarkov(n = 5001, transMat = tmatrix4A, 
                       chainLen = 40, wide = TRUE)
dd4A50 <- genMarkov(n = 5000, transMat = tmatrix4A, 
                       chainLen = 50, wide = TRUE)
dd4A100 <- genMarkov(n = 5000, transMat = tmatrix4A, 
                       chainLen = 100, wide = TRUE)
dd4A200 <- genMarkov(n = 5000, transMat = tmatrix4A, 
                       chainLen = 200, wide = TRUE)
dd4A500 <- genMarkov(n = 5000, transMat = tmatrix4A, 
                     chainLen = 500, wide = TRUE)

#dd4A20<- dd4A20[,-1]
dd4A30<- dd4A30[,-1]
dd4A40<- dd4A40[,-1]
dd4A50<- dd4A50[,-1]
dd4A100<- dd4A100[,-1]
dd4A200<- dd4A200[,-1]
dd4A500<- dd4A500[,-1]

# Cálculo de las matrices de transición

#mt4A20<- alply(dd4A20,1,transi)
mt4A30<- alply(dd4A30,1,transi)
mt4A40<- alply(dd4A40,1,transi)
mt4A50<- alply(dd4A50,1,transi)
mt4A100<- alply(dd4A100,1,transi)
mt4A200<- alply(dd4A200,1,transi)
mt4A500<- alply(dd4A500,1,transi)


# Cálculo de los residuales

#residT4A20<- lapply(mt4A20,resid2)
residT4A30<- lapply(mt4A30,resid2)
residT4A40<- lapply(mt4A40,resid2)
residT4A50<- lapply(mt4A50,resid2)
residT4A100<- lapply(mt4A100,resid2)
residT4A200<- lapply(mt4A200,resid2)
residT4A500<- lapply(mt4A500,resid2)


# Cálculo de errores modelo con efecto mayor que azar

#v20<- excepciones(residT4A20,4)
vA30<- excepciones(residT4A30,4)
vA40<- excepciones(residT4A40,4)
#residT4A20<- residT4A20[-v20]
residT4A30<- residT4A30[-vA30]
residT4A40<- residT4A30[-vA40]
#erroresT4A20<- lapply(residT4A20,FUN=erroresSigA,k=4, fila=2,col=3)
erroresT4A30<- lapply(residT4A30,FUN=erroresSig2,k=4,fila=2,col=3)
erroresT4A40<- lapply(residT4A40,FUN=erroresSig2,k=4,fila=2,col=3)
erroresT4A50<- lapply(residT4A50,FUN=erroresSig2,k=4,fila=2,col=3)
erroresT4A100<- lapply(residT4A100,FUN=erroresSig2,k=4,fila=2,col=3)
erroresT4A200<- lapply(residT4A200,FUN=erroresSig2,k=4,fila=2,col=3)
erroresT4A500<- lapply(residT4A500,FUN=erroresSig2,k=4,fila=2,col=3)

 # Porcentaje de errores totales
 
#perroresT4A20<- sapply(erroresT4A20,et)
perroresT4A30<- sapply(erroresT4A30,et)
perroresT4A40<- sapply(erroresT4A40,et)
perroresT4A50<- sapply(erroresT4A50,et)
perroresT4A100<- sapply(erroresT4A100,et)
perroresT4A200<- sapply(erroresT4A200,et)
perroresT4A500<- sapply(erroresT4A500,et)

# Media errores totales

#mediaet4A20<- mean(perroresT4A20,na.rm=T)

mediaET4A30<- mean(perroresT4A30,na.rm=T)
mediaET4A40<- mean(perroresT4A40,na.rm=T)
mediaET4A50<- mean(perroresT4A50,na.rm=T)
mediaET4A100<- mean(perroresT4A100,na.rm=T)
mediaET4A200<- mean(perroresT4A200,na.rm=T)
mediaET4A500<- mean(perroresT4A500,na.rm=T)

# dt errores totales

#dtET4A20<- sd(perroresT4A20,na.rm=T)
dtET4A30<- sd(perroresT4A30,na.rm=T)
dtET4A40<- sd(perroresT4A40,na.rm=T)
dtET4A50<- sd(perroresT4A50,na.rm=T)
dtET4A100<- sd(perroresT4A100,na.rm=T)
dtET4A200<- sd(perroresT4A200,na.rm=T)
dtET4A500<- sd(perroresT4A500,na.rm=T)

mediaET4A<- c(mediaET4A30,mediaET4A40,mediaET4A50,
          mediaET4A100,mediaET4A200,mediaET4A500)

dtET4A<- c(dtET4A30,dtET4A40,dtET4A50,
           dtET4A100,dtET4A200,dtET4A500)

resET4A<- data.frame(mediaET4A, dtET4A)
row.names(resET4A)<-c('30','40','50','100','200','500')



# Porcentaje de errores casilla significativa

#perroresCS4A20<- sapply(erroresT4A20,ecs)
perroresCS4A30<- sapply(erroresT4A30,ecs)
perroresCS4A40<- sapply(erroresT4A40,ecs)
perroresCS4A50<- sapply(erroresT4A50,ecs)
perroresCS4A100<- sapply(erroresT4A100,ecs)
perroresCS4A200<- sapply(erroresT4A200,ecs)
perroresCS4A500<- sapply(erroresT4A500,ecs)

# Media errores casilla significativa

#mediaECS4A20<- mean(perroresCS4A20,na.rm=T)
mediaECS4A30<- mean(perroresCS4A30,na.rm=T)
mediaECS4A40<- mean(perroresCS4A40,na.rm=T)
mediaECS4A50<- mean(perroresCS4A50,na.rm=T)
mediaECS4A100<- mean(perroresCS4A100,na.rm=T)
mediaECS4A200<- mean(perroresCS4A200,na.rm=T)
mediaECS4A500<- mean(perroresCS4A500,na.rm=T)

# dt errores casilla significativa

#dtECS4A20<- sd(perroresCS4A20,na.rm=T)
dtECS4A30<- sd(perroresCS4A30,na.rm=T)
dtECS4A40<- sd(perroresCS4A40,na.rm=T)
dtECS4A50<- sd(perroresCS4A50,na.rm=T)
dtECS4A100<- sd(perroresCS4A100,na.rm=T)
dtECS4A200<- sd(perroresCS4A200,na.rm=T)
dtECS4A500<- sd(perroresCS4A500,na.rm=T)

mediaECS4A<- c(mediaECS4A30,mediaECS4A40,mediaECS4A50,
              mediaECS4A100,mediaECS4A200,mediaECS4A500)

dtECS4A<- c(dtECS4A30,dtECS4A40,dtECS4A50,dtECS4A100,
            dtECS4A200,dtECS4A500)

resECS4A<- data.frame(mediaECS4A, dtECS4A)
row.names(resECS4A)<-c('30','40','50','100','200','500')



# Porcentaje de errores fila significativa

#perroresFS4A20<- sapply(erroresT4A20,efs)
perroresFS4A30<- sapply(erroresT4A30,efs)
perroresFS4A40<- sapply(erroresT4A40,efs)
perroresFS4A50<- sapply(erroresT4A50,efs)
perroresFS4A100<- sapply(erroresT4A100,efs)
perroresFS4A200<- sapply(erroresT4A200,efs)
perroresFS4A500<- sapply(erroresT4A500,efs)

# Media errores fila significativa

#mediaEFS4A20<- mean(perroreFS4A20,na.rm=T)
mediaEFS4A30<- mean(perroresFS4A30,na.rm=T)
mediaEFS4A40<- mean(perroresFS4A40,na.rm=T)
mediaEFS4A50<- mean(perroresFS4A50,na.rm=T)
mediaEFS4A100<- mean(perroresFS4A100,na.rm=T)
mediaEFS4A200<- mean(perroresFS4A200,na.rm=T)
mediaEFS4A500<- mean(perroresFS4A500,na.rm=T)

# dt errores fila significativa

#dtEFS4A20<- sd(perroresFS4A20,na.rm=T)
dtEFS4A30<- sd(perroresFS4A30,na.rm=T)
dtEFS4A40<- sd(perroresFS4A40,na.rm=T)
dtEFS4A50<- sd(perroresFS4A50,na.rm=T)
dtEFS4A100<- sd(perroresFS4A100,na.rm=T)
dtEFS4A200<- sd(perroresFS4A200,na.rm=T)
dtEFS4A500<- sd(perroresFS4A500,na.rm=T)

mediaEFS4A<- c(mediaEFS4A30,mediaEFS4A40,mediaEFS4A50,
               mediaEFS4A100,mediaEFS4A200,mediaEFS4A500)

dtEFS4A<- c(dtEFS4A30,dtEFS4A40,dtEFS4A50,dtEFS4A100,
            dtEFS4A200,dtEFS4A500)

resEFS4A<- data.frame(mediaEFS4A, dtEFS4A)
row.names(resET4A)<-c('30','40','50','100','200','500')


# Porcentaje de errores fila NO significativa

#perroreFNS4A20<- sapply(erroresT4A20,efns)
perroresFNS4A30<- sapply(erroresT4A30,efns)
perroresFNS4A40<- sapply(erroresT4A40,efns)
perroresFNS4A50<- sapply(erroresT4A50,efns)
perroresFNS4A100<- sapply(erroresT4A100,efns)
perroresFNS4A200<- sapply(erroresT4A200,efns)
perroresFNS4A500<- sapply(erroresT4A500,efns)

# Media errores fila NO significativa

#mediaEFNS4A20<- mean(perroreFNS4A20,na.rm=T)
mediaEFNS4A30<- mean(perroresFNS4A30,na.rm=T)
mediaEFNS4A40<- mean(perroresFNS4A40,na.rm=T)
mediaEFNS4A50<- mean(perroresFNS4A50,na.rm=T)
mediaEFNS4A100<- mean(perroresFNS4A100,na.rm=T)
mediaEFNS4A200<- mean(perroresFNS4A200,na.rm=T)
mediaEFNS4A500<- mean(perroresFNS4A500,na.rm=T)

# dt errores fila NO significativa

#dtEFNS4A20<- sd(perroresFNS4A20,na.rm=T)
dtEFNS4A30<- sd(perroresFNS4A30,na.rm=T)
dtEFNS4A40<- sd(perroresFNS4A40,na.rm=T)
dtEFNS4A50<- sd(perroresFNS4A50,na.rm=T)
dtEFNS4A100<- sd(perroresFNS4A100,na.rm=T)
dtEFNS4A200<- sd(perroresFNS4A200,na.rm=T)
dtEFNS4A500<- sd(perroresFNS4A500,na.rm=T)

mediaEFNS4A<- c(mediaEFNS4A30,mediaEFNS4A40,mediaEFNS4A50,
               mediaEFNS4A100,mediaEFNS4A200,mediaEFNS4A500)

dtEFNS4A<- c(dtEFNS4A30,dtEFNS4A40,
             dtEFNS4A50,dtEFNS4A100,dtEFNS4A200,
             dtEFNS4A500)

resEFNS4A<- data.frame(mediaEFNS4A, dtEFNS4A)
row.names(resET4A)<-c('30','40','50','100','200','500')



fin<- Sys.time()

fin-ini

