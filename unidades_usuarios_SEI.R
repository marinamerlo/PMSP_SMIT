#Filtro de usuários do SEI por unidade

setwd("C:\\Users\\d841255\\Desktop")
library(tidyverse)
library(data.table)
library(stringr)

#lista de usuários extraída da base do SEI
dados <- fread("usuario.csv")

#fazendo o filtro pelo nome da unidade que usuário tem permissão
dados_DEMAP <- dados %>%
filter(str_detect(unidades_permissao, "DEMAP"))

#salvando os dados
write.table(dados_DEMAP, "usuarios_unidades_DEMAP.csv", row.names = F, sep = ";", dec =",")
