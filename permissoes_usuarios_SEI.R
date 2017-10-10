#Script para filtrar os usuários que ainda precisam ser cadastrados no SEI a partir da lista de login

library(dplyr)
library(stringr)
library(readxl)

#lista de usuários cadastrados no SEI
#extração necessária na base do SEI
sei <- read_excel("C:/Users/d841255/Desktop/dpgi_seitreinamento.xlsx")

#lista de usuários a cadastrar
#providenciado pela unidade
usuarios <- read_excel("C:/Users/d841255/Desktop/dgpi_usuarios.xls")

#tira os espaços em branco antes e depois do string
sei <- sei %>%
  mutate(usuario = str_trim(usuario))

#exclui da lista de usuários a cadastrar aqueles que já estão cadastrados
sei_cadastrar <- anti_join(usuarios, sei, by ="usuario")

glimpse(sei_cadastrar)
