
resET<- data.frame(media4Null,mediaET4A, mediaET4B,
                   media8Null,mediaET8A, mediaET8B,
                   media12Null,mediaET12A, mediaET12B)
row.names(resET)<-c('30','40','50','100','200','500')
round(resET,2)



resECS<- data.frame(mediaECS4A, mediaECS4B,
                    mediaECS8A, mediaECS8B,
                    mediaECS12A, mediaECS12B)

row.names(resECS)<-c('30','40','50','100','200','500')
round(resECS,2)

resEFS<- data.frame(mediaEFS4A, mediaEFS4B,
                    mediaEFS8A, mediaEFS8B,
                    mediaEFS12A, mediaEFS12B)

row.names(resEFS)<-c('30','40','50','100','200','500')
round(resEFS,2)

resEFNS<- data.frame(mediaEFNS4A, mediaEFNS4B,
                     mediaEFNS8A, mediaEFNS8B,
                     mediaEFNS12A, mediaEFNS12B)
row.names(resEFNS)<-c('30','40','50','100','200','500')
round(resEFNS,2)



ET<-c(media4Null,mediaET4A, mediaET4B,
      media8Null,mediaET8A, mediaET8B,
      media12Null,mediaET12A, mediaET12B)

ECS<-c(mediaECS4A, mediaECS4B,
       mediaECS8A, mediaECS8B,
       mediaECS12A, mediaECS12B) 
EFS<- c(mediaEFS4A, mediaEFS4B,
        mediaEFS8A, mediaEFS8B,
        mediaEFS12A, mediaEFS12B)

EFNS<- c(mediaEFNS4A, mediaEFNS4B,
         mediaEFNS8A, mediaEFNS8B,
         mediaEFNS12A, mediaEFNS12B)


# Gráfico de errores totales

codes<- factor(c(1,2,3),labels= c('4','8','12'),
                ordered=F)
codes<-  rep(codes,each=18)
efecto<- factor(c(1,2,3),labels= c('N','A','B' ))
efecto<-  rep(efecto,each=6)
condicion<- rep(efecto,3)

cond<-rep(c('4N','4A','4B',
            '8N','8A','8B',
            '12N','12A','12B'),each=6)

long <- factor(c(1,2,3,4,5,6),labels= c('30','40','50',
                                      '100','200','500')
               ,ordered=T)
long<-  rep(long,9)

grafET<- data.frame(codes,condicion,cond,long,ET)

library(dplyr)

grafET <- grafET %>% 
  mutate_if(is.numeric, round, digits = 3)




# ggplot


theme_update(plot.title = element_text(hjust = 0.5))
gET<- ggplot(grafET, aes(x=long, y=ET,
              group=condicion))+  
geom_line(aes(linetype=condicion),size =1)+ geom_point() 

gET + facet_grid(.~reorder(codes))+
labs(title="Figure 1",
         x= "Sequence length",
         y = "%  of total errors")+
scale_y_continuous(breaks=c(0.05,0.1,0.15,0.20,0.25,0.30))



#gET + facet_grid(reorder(numero)~.)

# Gráfico de errores

cond2<- rep(c('4A','4B',
              '8A','8B',
              '12A','12B'),each=6)

long2 <- rep(c('30','40','50','100','200','500'),6)
codes2<- rep(c('4','8','12'),each=12)
efecto2<- rep(c('A','B' ),each=6)
condicion2<- rep(efecto2,3)

grafDat2<- data.frame(codes2,condicion2,long2,
                      ECS, EFS,EFNS)
grafOTE <- grafDat2 %>% 
  mutate_if(is.numeric, round, digits = 3)

# Gráfico de errores en casilla significativa

theme_update(plot.title = element_text(hjust = 0.5))
gECS<- ggplot(grafDat2, aes(x=reorder(long2), y=ECS,
                         group=condicion2))+  
  geom_line(aes(linetype=condicion2),size =1)+ geom_point() 

gECS + facet_grid(.~reorder(codes2))+
  labs(title="Figure 2",
       x= "Sequence length",
       y = "%  of  CS errors")+
  scale_y_continuous(limits=c(0.00,0.05))

# Gráfico de errores en filas significativas

theme_update(plot.title = element_text(hjust = 0.5))
gEFS<- ggplot(grafDat2, aes(x=reorder(long2), y=EFS,
                  group=reorder(condicion2))) + 
  geom_line(aes(linetype=condicion2),size =1)+ 
  geom_point()
  

gEFS + facet_grid(.~reorder(codes2))+
  labs(title="Figure 3",
       x= "Sequence length",
       y = "%  of  Significant row errors")+
      labs(linetype = "Condition")+
  scale_y_continuous(breaks=c(0.05,0.1,0.15,0.18))

  
# Gráfico de errores en filas no significativas

gEFNS<- theme_update(plot.title = element_text(hjust = 0.5))
gEFNS<- ggplot(grafDat2, aes(x=reorder(long2), y=EFNS,
                            group=reorder(condicion2))) + 
  geom_line(aes(linetype=condicion2),size =1)+ 
  geom_point()


gEFNS + facet_grid(.~reorder(codes2))+
  labs(title="Figure 4",
       x= "Sequence length",
       y = "%  of non Significant rows errors")+
  labs(linetype = "Condition")+
  scale_y_continuous(breaks=c(0.05,0.1,0.15,0.20,0.25,0.28))


# Múltiples gráficos

mul<-ggarrange(gET,gECS,gEFS,gEFNS,
          nrow=1,ncol=1)


mul

