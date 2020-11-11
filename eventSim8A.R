# simstudy Generando secuencias

options(error=recover)

ini<- Sys.time()

set.seed(1234567)


#dd8A20 <- genMarkov(n = 5000, transMat = tmatrix8A, 
#               chainLen = 20, wide = TRUE)
dd8A30 <- genMarkov(n = 5798, transMat = tmatrix8A, 
                       chainLen = 30, wide = TRUE)
dd8A40 <- genMarkov(n = 5207, transMat = tmatrix8A, 
                       chainLen = 40, wide = TRUE)
dd8A50 <- genMarkov(n = 5070, transMat = tmatrix8A, 
                       chainLen = 50, wide = TRUE)
dd8A100 <- genMarkov(n = 5000, transMat = tmatrix8A, 
                       chainLen = 100, wide = TRUE)
dd8A200 <- genMarkov(n = 5000, transMat = tmatrix8A, 
                       chainLen = 200, wide = TRUE)
dd8A500 <- genMarkov(n = 5000, transMat = tmatrix8A, 
                     chainLen = 500, wide = TRUE)

#dd8A20<- dd8A20[,-1]
dd8A30<- dd8A30[,-1]
dd8A40<- dd8A40[,-1]
dd8A50<- dd8A50[,-1]
dd8A100<- dd8A100[,-1]
dd8A200<- dd8A200[,-1]
dd8A500<- dd8A500[,-1]

# Cálculo de las matrices de transición

#mt8A20<- alply(dd8A20,1,transi)
mt8A30<- alply(dd8A30,1,transi)
mt8A40<- alply(dd8A40,1,transi)
mt8A50<- alply(dd8A50,1,transi)
mt8A100<- alply(dd8A100,1,transi)
mt8A200<- alply(dd8A200,1,transi)
mt8A500<- alply(dd8A500,1,transi)


# Cálculo de los residuales

#residT8A20<- lapply(mt8A20,resid2)
residT8A30<- lapply(mt8A30,resid2)
residT8A40<- lapply(mt8A40,resid2)
residT8A50<- lapply(mt8A50,resid2)
residT8A100<- lapply(mt8A100,resid2)
residT8A200<- lapply(mt8A200,resid2)
residT8A500<- lapply(mt8A500,resid2)


# Cálculo de errores modelo con efecto mayor que azar

#v20<- excepciones(residT8A20,8)
v8A30<- excepciones(residT8A30,8)
v8A40<- excepciones(residT8A40,8)
v8A50<- excepciones(residT8A50,8)
v8A100<- excepciones(residT8A100,8)
v8A200<- excepciones(residT8A200,8)
v8A500<- excepciones(residT8A500,8)

#residT8A20<- residT8A20[-v20]
residT8A30<- residT8A30[-v8A30]
residT8A40<- residT8A30[-v8A40]
residT8A50<- residT8A30[-v8A50]

#erroresT8A20<- lapply(residT8A20,FUN=erroresSigA,k=4, fila=2,col=3)
erroresT8A30<- lapply(residT8A30,FUN=erroresSig2,k=8,fila=6,col=3)
erroresT8A40<- lapply(residT8A40,FUN=erroresSig2,k=8,fila=6,col=3)
erroresT8A50<- lapply(residT8A50,FUN=erroresSig2,k=8,fila=6,col=3)
erroresT8A100<- lapply(residT8A100,FUN=erroresSig2,k=8,fila=6,col=3)
erroresT8A200<- lapply(residT8A200,FUN=erroresSig2,k=8,fila=6,col=3)
erroresT8A500<- lapply(residT8A500,FUN=erroresSig2,k=8,fila=6,col=3)

 # Porcentaje de errores totales
 
#perroresT8A20<- sapply(erroresT8A20,et)
perroresT8A30<- sapply(erroresT8A30,et)
perroresT8A40<- sapply(erroresT8A40,et)
perroresT8A50<- sapply(erroresT8A50,et)
perroresT8A100<- sapply(erroresT8A100,et)
perroresT8A200<- sapply(erroresT8A200,et)
perroresT8A500<- sapply(erroresT8A500,et)

# Media errores totales

#mediaet8A20<- mean(perroresT8A20,na.rm=T)

mediaET8A30<- mean(perroresT8A30,na.rm=T)
mediaET8A40<- mean(perroresT8A40,na.rm=T)
mediaET8A50<- mean(perroresT8A50,na.rm=T)
mediaET8A100<- mean(perroresT8A100,na.rm=T)
mediaET8A200<- mean(perroresT8A200,na.rm=T)
mediaET8A500<- mean(perroresT8A500,na.rm=T)

# dt errores totales

#dtET8A20<- sd(perroresT8A20,na.rm=T)
dtET8A30<- sd(perroresT8A30,na.rm=T)
dtET8A40<- sd(perroresT8A40,na.rm=T)
dtET8A50<- sd(perroresT8A50,na.rm=T)
dtET8A100<- sd(perroresT8A100,na.rm=T)
dtET8A200<- sd(perroresT8A200,na.rm=T)
dtET8A500<- sd(perroresT8A500,na.rm=T)

mediaET8A<- c(mediaET8A30,mediaET8A40,mediaET8A50,
          mediaET8A100,mediaET8A200,mediaET8A500)

dtET8A<- c(dtET8A30,dtET8A40,dtET8A50,
           dtET8A100,dtET8A200,dtET8A500)

resET8A<- data.frame(mediaET8A, dtET8A)
row.names(resET8A)<-c('30','40','50','100','200','500')



# Porcentaje de errores casilla significativa

#perroresCS8A20<- sapply(erroresT8A20,ecs)
perroresCS8A30<- sapply(erroresT8A30,ecs)
perroresCS8A40<- sapply(erroresT8A40,ecs)
perroresCS8A50<- sapply(erroresT8A50,ecs)
perroresCS8A100<- sapply(erroresT8A100,ecs)
perroresCS8A200<- sapply(erroresT8A200,ecs)
perroresCS8A500<- sapply(erroresT8A500,ecs)

# Media errores casilla significativa

#mediaECS8A20<- mean(perroresCS8A20,na.rm=T)
mediaECS8A30<- mean(perroresCS8A30,na.rm=T)
mediaECS8A40<- mean(perroresCS8A40,na.rm=T)
mediaECS8A50<- mean(perroresCS8A50,na.rm=T)
mediaECS8A100<- mean(perroresCS8A100,na.rm=T)
mediaECS8A200<- mean(perroresCS8A200,na.rm=T)
mediaECS8A500<- mean(perroresCS8A500,na.rm=T)

# dt errores casilla significativa

#dtECS8A20<- sd(perroresCS8A20,na.rm=T)
dtECS8A30<- sd(perroresCS8A30,na.rm=T)
dtECS8A40<- sd(perroresCS8A40,na.rm=T)
dtECS8A50<- sd(perroresCS8A50,na.rm=T)
dtECS8A100<- sd(perroresCS8A100,na.rm=T)
dtECS8A200<- sd(perroresCS8A200,na.rm=T)
dtECS8A500<- sd(perroresCS8A500,na.rm=T)

mediaECS8A<- c(mediaECS8A30,mediaECS8A40,mediaECS8A50,
              mediaECS8A100,mediaECS8A200,mediaECS8A500)

dtECS8A<- c(dtECS8A30,dtECS8A40,dtECS8A50,dtECS8A100,
            dtECS8A200,dtECS8A500)

resECS8A<- data.frame(mediaECS8A, dtECS8A)
row.names(resECS8A)<-c('30','40','50','100','200','500')



# Porcentaje de errores fila significativa

#perroresFS8A20<- sapply(erroresT8A20,efs)
perroresFS8A30<- sapply(erroresT8A30,efs)
perroresFS8A40<- sapply(erroresT8A40,efs)
perroresFS8A50<- sapply(erroresT8A50,efs)
perroresFS8A100<- sapply(erroresT8A100,efs)
perroresFS8A200<- sapply(erroresT8A200,efs)
perroresFS8A500<- sapply(erroresT8A500,efs)

# Media errores fila significativa

#mediaEFS8A20<- mean(perroreFS8A20,na.rm=T)
mediaEFS8A30<- mean(perroresFS8A30,na.rm=T)
mediaEFS8A40<- mean(perroresFS8A40,na.rm=T)
mediaEFS8A50<- mean(perroresFS8A50,na.rm=T)
mediaEFS8A100<- mean(perroresFS8A100,na.rm=T)
mediaEFS8A200<- mean(perroresFS8A200,na.rm=T)
mediaEFS8A500<- mean(perroresFS8A500,na.rm=T)

# dt errores fila significativa

#dtEFS8A20<- sd(perroresFS8A20,na.rm=T)
dtEFS8A30<- sd(perroresFS8A30,na.rm=T)
dtEFS8A40<- sd(perroresFS8A40,na.rm=T)
dtEFS8A50<- sd(perroresFS8A50,na.rm=T)
dtEFS8A100<- sd(perroresFS8A100,na.rm=T)
dtEFS8A200<- sd(perroresFS8A200,na.rm=T)
dtEFS8A500<- sd(perroresFS8A500,na.rm=T)

mediaEFS8A<- c(mediaEFS8A30,mediaEFS8A40,mediaEFS8A50,
               mediaEFS8A100,mediaEFS8A200,mediaEFS8A500)

dtEFS8A<- c(dtEFS8A30,dtEFS8A40,dtEFS8A50,dtEFS8A100,
            dtEFS8A200,dtEFS8A500)

resEFS8A<- data.frame(mediaEFS8A, dtEFS8A)
row.names(resET8A)<-c('30','40','50','100','200','500')


# Porcentaje de errores fila NO significativa

#perroreFNS8A20<- sapply(erroresT8A20,efns)
perroresFNS8A30<- sapply(erroresT8A30,efns)
perroresFNS8A40<- sapply(erroresT8A40,efns)
perroresFNS8A50<- sapply(erroresT8A50,efns)
perroresFNS8A100<- sapply(erroresT8A100,efns)
perroresFNS8A200<- sapply(erroresT8A200,efns)
perroresFNS8A500<- sapply(erroresT8A500,efns)

# Media errores fila NO significativa

#mediaEFNS8A20<- mean(perroreFNS8A20,na.rm=T)
mediaEFNS8A30<- mean(perroresFNS8A30,na.rm=T)
mediaEFNS8A40<- mean(perroresFNS8A40,na.rm=T)
mediaEFNS8A50<- mean(perroresFNS8A50,na.rm=T)
mediaEFNS8A100<- mean(perroresFNS8A100,na.rm=T)
mediaEFNS8A200<- mean(perroresFNS8A200,na.rm=T)
mediaEFNS8A500<- mean(perroresFNS8A500,na.rm=T)

# dt errores fila NO significativa

#dtEFNS8A20<- sd(perroresFNS8A20,na.rm=T)
dtEFNS8A30<- sd(perroresFNS8A30,na.rm=T)
dtEFNS8A40<- sd(perroresFNS8A40,na.rm=T)
dtEFNS8A50<- sd(perroresFNS8A50,na.rm=T)
dtEFNS8A100<- sd(perroresFNS8A100,na.rm=T)
dtEFNS8A200<- sd(perroresFNS8A200,na.rm=T)
dtEFNS8A500<- sd(perroresFNS8A500,na.rm=T)

mediaEFNS8A<- c(mediaEFNS8A30,mediaEFNS8A40,mediaEFNS8A50,
               mediaEFNS8A100,mediaEFNS8A200,mediaEFNS8A500)

dtEFNS8A<- c(dtEFNS8A30,dtEFNS8A40,
             dtEFNS8A50,dtEFNS8A100,dtEFNS8A200,
             dtEFNS8A500)

resEFNS8A<- data.frame(mediaEFNS8A, dtEFNS8A)
row.names(resET8A)<-c('30','40','50','100','200','500')

round(resET8A,2)
round(resECS8A,2)
round(resEFS8A,2)
round(resEFNS8A,2)


fin<- Sys.time()

fin-ini

