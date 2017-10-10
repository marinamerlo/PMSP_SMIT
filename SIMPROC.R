#Códigos utilizando o pacote Tidyverse para preparar as planilhas mais comuns nas análises de diagnóstico de migração do SEI e de desempenho das secretarias

setwd("C:\\Users\\d841255\\Desktop")

library(readr)
library(dplyr)
library(data.table)
library(tidyverse)
library(stringr)


##extração do SIMPROC com órgão e unidade na mesma variável + quantitativo de processos mês a mês
dados <- read_excel("C:/Users/d841255/Desktop/proc2017.xlsx")

dados <- dados %>%
  separate(Órgão, into = c("orgao", "unidade"), "/")%>% #separa o órgão da unidade
  select(-unidade) %>% #joga fora a variável com a unidade
  group_by(orgao, Assunto, Subassunto) %>% #agrupa por órgão e processo
  summarise_each(funs(sum)) %>% #faz a soma dos quantitativos mês a mês pelo agrupamento
  arrange(desc(Total)) #ordena pelo total
  
#salvando a planilha  
write.table(dados, "processos2017_orgao.csv", sep = ";", fileEncoding ="latin1", row.names = F)


#filtrando por um órgão específico
dados_smg <- dados %>%
  filter(str_detect(orgao, "SMG"))
  
#salvando a planilha  
write.table(dados_smg, "processos2017SMG.csv", sep = ";", fileEncoding ="latin1", row.names = F)


#agrupando só pelos tipos de processos mais autuados
processos2017_semorgao <- dados %>%
  select(-orgao) %>%
  group_by(Assunto, Subassunto) %>%
  summarise_each(funs(sum))

#salvando a planilha  
write.table(processos2017_semorgao, "processos2017_semorgao.csv", sep = ";", fileEncoding ="latin1", row.names = F)
