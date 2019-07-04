rm(list=ls())
setwd("D:/psycho-test-master/data/raw")
library(readr)
library(tidyr)
library(dplyr)
library(TAM)
test_data <- read_csv("test_data.csv")

#Generamos el dataframe de preguntas con sus respuestas para el a\~no 2018
data_tam<-test_data %>% filter(year==2018) %>% spread(key=idQuestion, value=isCorrect)

#########Inciso 1

#Generamos el modelo Rasch
modeloRasch<-data_tam %>% select(-one_of("year", "campusName", "idUser_int")) %>% tam()

#Extraemos las dificultades y discriminaciones de cada pregunta
pregunta<-modeloRasch$item_irt$item
dificultad<-modeloRasch$item_irt$beta
discriminacion<-modeloRasch$item_irt$alpha

#generamos un data frame con las dificultades y discriminaciones de cada pregunta
incisoUno<-data.frame(pregunta, dificultad, discriminacion)

############ Inciso 2
campusA<-test_data %>% filter(year==2018 & campusName=="Campus A") %>% spread(key=idQuestion, value=isCorrect) %>% select(-one_of("year", "campusName", "idUser_int")) %>% tam()
campusB<-test_data %>% filter(year==2018  & campusName=="Campus B") %>% spread(key=idQuestion, value=isCorrect) %>% select(-one_of("year", "campusName", "idUser_int")) %>% tam()
campusC<-test_data %>% filter(year==2018 & campusName=="Campus C") %>% spread(key=idQuestion, value=isCorrect) %>% select(-one_of("year", "campusName", "idUser_int")) %>% tam()

#Extraemos
thetaA<-tam.wle(campusA)$theta
thetaB<-tam.wle(campusB)$theta
thetaC<-tam.wle(campusC)$theta

#Generando el ranking
ranking<-function(habilidad){
  temp<-sum(habilidad[habilidad>0])/length(habilidad)
  temp
}

ranking2<-function(habilidad){
  temp<-sum(habilidad>0)/sum(habilidad[habilidad>0])
  temp
}

ranking(thetaA)
ranking(thetaB)
ranking(thetaC)

ranking2(thetaA)
ranking2(thetaB)
ranking2(thetaC)

############ Inciso 3
idUsuarioA<-data_tam %>% filter(campusName=="Campus A") %>% select(idUser_int) %>% c()
idUsuarioB<-data_tam %>% filter(campusName=="Campus B") %>% select(idUser_int) %>% c()
idUsuarioC<-data_tam %>% filter(campusName=="Campus C") %>% select(idUser_int) %>% c()

#Generamos el modelo de habilidades considerando el identificador de cada alumno
thetaA2<-tam.wle(campusA, pid=idUsuarioA)
thetaB2<-tam.wle(campusB, pid=idUsuarioB)
thetaC2<-tam.wle(campusC, pid=idUsuarioC)

rankCampA<- data.frame(thetaA2$idUser_int, thetaA2$theta) %>% arrange(desc(thetaA2$theta))
rankCampB<- data.frame(thetaB2$idUser_int, thetaB2$theta) %>% arrange(desc(thetaB2$theta))
rankCampC<- data.frame(thetaC2$idUser_int, thetaC2$theta) %>% arrange(desc(thetaC2$theta))


########## Inciso 4
#Generamos el modelo para todas las escuelas en 2019
data_tam2019<-test_data %>% filter(year==2019) %>% spread(key=idQuestion, value=isCorrect)
modeloRasch2019<-data_tam %>% select(-one_of("year", "campusName", "idUser_int")) %>% tam()

#Calculamos los niveles de habilidad para ambos a\~nos
theta2019<-tam.wle(modeloRasch2019)$theta
theta2018<-tam.wle(modeloRasch)$theta

ranking(theta2019)
ranking(theta2018)

ranking2(theta2019)
ranking2(theta2018)

#Generamos los modelos para cada escuela en 2019
campusA2019<-test_data %>% filter(year==2019 & campusName=="Campus A") %>% spread(key=idQuestion, value=isCorrect) %>% select(-one_of("year", "campusName", "idUser_int")) %>% tam()
campusB2019<-test_data %>% filter(year==2019  & campusName=="Campus B") %>% spread(key=idQuestion, value=isCorrect) %>% select(-one_of("year", "campusName", "idUser_int")) %>% tam()
campusC2019<-test_data %>% filter(year==2019 & campusName=="Campus C") %>% spread(key=idQuestion, value=isCorrect) %>% select(-one_of("year", "campusName", "idUser_int")) %>% tam()

thetaA2019<-tam.wle(campusA2019)$theta
thetaB2019<-tam.wle(campusB2019)$theta
thetaC2019<-tam.wle(campusC2019)$theta

#Calculamos los rankings para 2019 de cada escuela
ranking(thetaA2019)
ranking(thetaB2019)
ranking(thetaC2019)

ranking2(thetaA2019)
ranking2(thetaB2019)
ranking2(thetaC2019)
