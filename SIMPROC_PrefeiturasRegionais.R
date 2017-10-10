#script para extrair os dados de autuação das prefeituras regionais, a partir de arquivo de excel em que os dados de cada PR estava numa planilha diferente
#também tem o preparo de um banco com os dados da autuação no SISACOE

setwd("C:\\Users\\d841255\\Desktop\\SIMPROC")

library(tidyverse)
library(readxl)
library(data.table)

#vendo quais são os arquivos na pasta
list.files(file.path(getwd()))

#função para extrair todas as planilhas do arquivo
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

#aplicando a função no arquivo das subprefeituras. É um workbook com 32 planilhas, uma pra cada subprefeitura
mysheets <- read_excel_allsheets("subprefeituras_v2.xlsx")

#transformando todas as planilhas em um data frame único
subpref <- Map(as.data.frame, mysheets)
subpref <- rbindlist(subpref)

#vendo como ficou
glimpse(subpref)

#renomeando as colunas problemáticas
subpref <- subpref %>%
  rename(orgao = Órgão,
         n2016 = `Total 2016`,
         n2017 = `Total 2017`) 
  
#vendo o total de processos por subprefeitura e ordenando pelo total de 2016
subpref_total <- subpref %>%
  group_by(orgao) %>%
  summarise(total_2017 = sum(n2017, na.rm = TRUE),
            total_2016 = sum(n2016, na.rm = TRUE)) %>%
  arrange(desc(total_2016))

#criando um ranking das 10 maiores 
subpref_total <- head(subpref_total, 10)

#adicionando o total por subprefeitura no banco
subpref <- left_join(subpref, subpref_total)
subpref <- subpref %>%
  mutate(pct_2016 = (n2016/total_2016)*100,
         pct_2017 = (n2017/total_2017)*100)

#criando um subset do banco total somente com as 10 maiores subprefeituras
subpref_top10 <- subpref %>%
  semi_join(subpref_total, by="orgao") %>%
  arrange(desc(n2016))

#selecionando os 10 processos mais autuados por órgão
top10 <- data.table(subpref_top10, key="orgao")
top10 <- top10[, head(.SD, 10), by=orgao]



#### análises SISACOE #####

sisacoe <- d %>%
  group_by(Assunto) %>%
  summarise(total_2017 = sum(n2017, na.rm = TRUE),
            total_2016 = sum(n2016, na.rm = TRUE)) %>%
  arrange(desc(total_2016))
  

assuntos <- as.data.frame(table(top10$Assunto))
subassuntos <- as.data.frame(table(top10$Subassunto))
motivos <- as.data.frame(table(top10$Motivo))


top10 <- top10 %>%
  mutate(processo = ifelse(grepl('AUTO DE', Motivo), "AUTOS", "OUTROS"))%>%
  mutate(processo = ifelse(grepl('CERTIDAO', Motivo), "CERTIDAO", processo))%>%
  mutate(processo = ifelse(grepl('CADAN', Motivo), "CADAN", processo))%>%
  mutate(processo = ifelse(grepl('COMUNICACAO', Motivo), "COMUNICACAO", processo))
processos <- as.data.frame(table(top10$processo))


migracao <- fread("subpref_migracao.csv")

top10 <- top10 %>%
  left_join(migracao)

#salvando banco com os 10 processos mais autados para as 10 maiores subprefeituras
write.table(top10, "top10supref.csv", sep = ";", dec = ",", fileEncoding ="latin1", row.names = F)

#fazendo o agrupamento por assunto e subassunto

top10_processos <- subpref %>%
  group_by(Assunto, Subassunto) %>%
  summarise(total_2017 = sum(n2017, na.rm = TRUE),
            total_2016 = sum(n2016, na.rm = TRUE))          
         
write.table(top10_processos, "top10processos.csv", sep = ";", dec = ",", fileEncoding ="latin1", row.names = F)  
