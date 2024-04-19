"Religión en Chile y la confianza en el sistema político"
"Valentina Veas Camarada"

## Análisis descriptivo

## cargar paquetes
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven, summarytools,
               kableExtra, ggplot2, sjPlot, tidyverse)

## Cargar base de datos
lapop_2023 = read_dta("~/Downloads/r-analisis-estadistico/r-analisis-estadistico/Input/CHL_2023_LAPOP_AmericasBarometer_v1.0_w.dta")

## selección de variables ----
find_var(data = lapop_2023,"Confianza")
find_var(data = lapop_2023,"Religión")

proc_data = lapop_2023 %>% select(b13, #confianza en el congreso
                                  b21, #confianza en los partidos políticos
                                  b21a, #confianza en el presidente
                                  b31, #confaza en la corte suprema de justicia
                                  b32, #confianza en su municipalidad
                                  b6, #apoyo al sistema político
                                  b2, #respeto a las instituciones políticas
                                  q3cn, #religion
                                  q1tc_r, #género
                                  )


##Procesamiento de variables ----
#este procesamiento de variables es una operacionalización de las variables 
#para una mejor compresión de estas. Esto se traduce en un proceso que conlleva distintos pasos:
  
#descriptivo general (para revisar la variable)

#Recodificación de la variable

#Etiquetado

#descriptivo final

##Variables de confianza ----
#En esta encuesta las variables de confianza tienen un nivel de medición ordinal
#que considera los valores: 1 = nada; 7 = mucho

frq(proc_data$b13)
frq(proc_data$b21)
frq(proc_data$b21a)
frq(proc_data$b31)
frq(proc_data$b32)

#En este caso la variable no será recodificada porque no presenta problemas que compliquen
#el posterior analisis, por lo tanto se continúa con el etiquetado.

proc_data = proc_data %>% rename("conf_congreso"=b13, "conf_partpol"=b21, "conf_presidente"=b21a, "conf_cortsup"=b31, "conf_mun"=b32)

#Etiquetas de las variables
proc_data$conf_congreso = set_label(x = proc_data$conf_congreso,label = "Confianza: congreso")
get_label(proc_data$conf_congreso)
proc_data$conf_partpol = set_label(x = proc_data$conf_partpol,label = "Confianza: partidos políticos")
get_label(proc_data$conf_partpol)

proc_data$conf_presidente = set_label(x = proc_data$conf_presidente,label = "Confianza: Presidente")
get_label(proc_data$conf_presidente)

proc_data$conf_cortsup = set_label(x = proc_data$conf_cortsup,label = "Confianza: Corte Suprema")
get_label(proc_data$conf_cortsup)

proc_data$conf_mun = set_label(x = proc_data$conf_mun,label = "Confianza: municipalidad")
get_label(proc_data$conf_mun)

##Indice sumativo de confianza total en instituciones
proc_data$conf_inst <- (proc_data$conf_congreso+proc_data$conf_partpol+proc_data$conf_presidente+proc_data$conf_cortsup+proc_data$conf_mun)
summary(proc_data$conf_inst)
frq(proc_data$conf_inst)
#para este caso consideraremos como nada de confianza en las instituciones
#recodificación de esta variable

proc_data$conf_inst = recode(proc_data$conf_inst, "5=5; 6=5; 7=5; 8=5; 9=5; 10=5;
                                11=11; 12=11; 13=11; 14=11; 15=11; 16=11;
                                17=17; 18=17; 19=17; 20=17; 21=17; 22=17;
                                23=23; 24=23; 25=23; 26=23; 27=23; 28=23; 29=23;
                                30=30; 31=30; 32=30; 33=30; 34=30; 35=30")

proc_data$conf_inst = set_labels(proc_data$conf_inst, labels=c("nada de confianza"=5,
                                          "poca confianza"=11,
                                          "algo de confianza"=17,
                                          "mucha confianza"=23,
                                          "absoluta confianza"=30))

proc_data$conf_inst <- factor(proc_data$conf_inst,
                             labels = c("nada de confianza", "poca confianza", "algo de confianza",
                                        "mucha confianza", "absoluta confianza"),
                             levels = c(5, 11, 17, 23, 30))

#Etiquetas
proc_data$conf_inst  <- set_label(x = proc_data$conf_inst, label = "confianza en instituciones")

##Revisión de los cambios

frq(proc_data$conf_congreso)
frq(proc_data$conf_partpol)
frq(proc_data$conf_presidente)
frq(proc_data$conf_cortsup)
frq(proc_data$conf_mun)
frq(proc_data$conf_inst)
##Variables respeto y apoyo al sistema y las instituciones políticas ----
#Estas variables presentan la misma clasificación que las anteriores.
#Los valores de la variable van de 1 a 7, donde 1 = nada; 7 = mucho

frq(proc_data$b2)
frq(proc_data$b6)

#En este caso la variable no será recodificada porque no presenta problemas que compliquen
#el posterior analisis, por lo tanto se continúa con el etiquetado.

proc_data = proc_data %>% rename("respeto_istpol"=b2, "apoyo_sistpol"=b6)

#Etiquetas de las variables
proc_data$respeto_istpol = set_label(x = proc_data$respeto_istpol,label = "Respeto a las instituciones políticas")
get_label(proc_data$respeto_istpol)

proc_data$apoyo_sistpol = set_label(x = proc_data$apoyo_sistpol,label = "Apoyo a las instituciones políticas")
get_label(proc_data$apoyo_sistpol)

##Variable religión ----
frq(proc_data$q3cn)

proc_data = proc_data %>% rename("religion"=q3cn)
#Esta variable presenta muchas categorías distintas, esto con el objetivo de no perder información
#de parte de los encuestados, pero en cierto modo hace incomodo el análisis, es por eso que se recodificará.

proc_data$religion = recode(proc_data$religion, "1=1; 2=1; 5=1; 4=4; 7=7; 3=7; 77=7; 11=11")

proc_data$religion = set_labels(proc_data$religion, labels=c("religión cristiana"=1,
                                                               "creyente no religioso"=4,
                                                               "religión no cristiana"=7,
                                                               "no creyente"=11))
proc_data$religion <- factor(proc_data$religion,
                             labels = c("religión cristiana", "creyente no religioso", "religión no cristiana", "no creyente"),
                             levels = c(1, 4, 7, 11))
frq(proc_data$religion)

##Variable de género ----
frq(proc_data$q1tc_r)

#Al revisar el descriptivo se presenta solo un caso que no se identifica ni como hombre ni como mujer, 
#es por esta razón que al no representar una proporción considerable de casos se recodificará la variable
#para eliminar esta categoría. Además bajo las etiquetas de las categorías Hombre/masculino y Mujer/femenino, 
#se opta por la denominación binaria entre hombre y mujer porque la pregunta está construída de manera en que esa
#es la categoría de respuesta que aparece primero en lugar de masculino/femenino, sin embargo apuntan a lo mismo.
#También se cambian los valores que representan cada categoría de esta manera: 0 = Hombre; 1 = Mujer.

proc_data$q1tc_r = recode(proc_data$q1tc_r, "c(3)=NA")
proc_data = proc_data %>% rename("genero"=q1tc_r)

proc_data$genero <- car::recode(proc_data$genero, "1=0;2=1")

#Etiquetas de la variable
proc_data$genero <- factor(proc_data$genero,
                           labels=c( "Hombre",
                                     "Mujer"),
                           levels=c(0,1))

frq(proc_data$genero)


## Base de datos procesada para el analisis

proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

save(proc_data, file = "~/Downloads/r-analisis-estadistico/r-analisis-estadistico/Input/lapop_2023_proc.RData")

##Gráficos y tablas
#librerias para tablas y gráficos
pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2) # Para la mayoría de los gráficos

## Tabla descriptiva ----
sjmisc::descr(proc_data, show = c("label","range", "mean", "sd", "NA.prc", "n"))%>% kable(.,"markdown")

#respaldo base de datos
proc_data_original <-proc_data
dim(proc_data)
sum(is.na(proc_data))
#Eliminar NA
proc_data =na.omit(proc_data)
dim(proc_data)

#Gráficos ----
grafico1 = sjPlot::plot_stackfrq(dplyr::select(proc_data, conf_congreso,
                                                conf_partpol,
                                                conf_presidente,
                                                conf_cortsup),
                                  title = "Confianza en instituciones políticas") +
  theme(legend.position="bottom")
grafico1

frq(proc_data$religion)
grafico2 <- sjPlot::plot_stackfrq(dplyr::select(proc_data, religion),
                                  title = "religiones") +
  theme(legend.position="bottom")

grafico2

colores_personalizados = c("#16A085", "#1ABC9C", "#76D7C4", "#A3E4D7")

grafico3 = proc_data %>%
  ggplot(aes(x = conf_inst, fill = religion)) + 
  geom_bar() +
  xlab("Confianza en instituciones") +
  ylab("Cantidad") + 
  labs(fill = "Religión") +
  scale_fill_manual(labels = c('Cristiana', 'Creyente no religioso', 'Religión no cristiana', 'No creyente'),
                    values = colores_personalizados)

grafico3
