setwd("/home/alumno3/rladies")
library(dplyr)
library(ggplot2)
library(tidyr)

usr <- read.csv("./data/tr_endutih_usuario_anual_2020.csv")

#P7_13 Uso redes sociales
#P7_14_1 -P7_14_10
####-----####
#Preguntas sobre uso de redes sociales
p_redes <- paste("P7_14_",1:10, sep="")
#Redes sociales a alas que refieren las preguntas
redes <- c("Facebook","Twitter","Instagram", "LinkedIn", "Snapchat","Whatsapp","Youtube","Pinterest", "Messenger","Tumblr")

#Filtro para quedarse unicamente con instancias que representen a mujeres
mujeresRS <- filter(usr, (SEXO==2) & (P7_13==1))[,c("FAC_PER",p_redes)]
rm(usr)#Liberamos espacio
View(mujeresRS)

#Resumen de uso por cada red social
total_usr <- function(p){
  return(sum(filter(mujeresRS, mujeresRS[,p]==1)$FAC_PER))
}


#Se aplica la funcion para obtener el total de usuarias para cada red social por cada 100 000 mujeres
resumen <- ldply(p_redes,total_usr)/1e5
names(resumen) <- c("total") #Se renombra el atributo

#Se agregael nombre de la red social
resumen$red <- redes
resumen$red <- factor(resumen$red, levels = resumen$red)#Mantener el orden
View(resumen)


#Colores https://brandcolors.net/
colores <- c("#1877f2","#1da1f2","#e1306c", "#0077b5", "#fffc00", "#25d366", "#ff0000","#e60023","#0084ff","#35465c")

#Plot final
plot_final <- ggplot(data=resumen, mapping = aes(x=red,y=total,fill=red)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks=seq(0,360,by=50)) +
  labs(title="Usuarias de redes sociales",subtitle = "(cada 100 000 mujeres)", 
       x="Red Social", y="Número de usuarias",
       caption = "Fuente: Elaboración propia con datos de la ENDUTIH(2020)")+
  scale_fill_manual(values=colores) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90), legend.position="none")

#Guardar el plot
## PDF
ggsave("GraficaUsrMX.pdf",
       plot=plot_final,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 600,
       device = "pdf"
)
##SVG
ggsave("GraficaUsrMX.svg",
       plot=plot_final,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 600,
       device = "svg"
)
##PNG
ggsave("GraficaUsrMX.png",
       plot=plot_final,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 600,
       device = "png"
)
