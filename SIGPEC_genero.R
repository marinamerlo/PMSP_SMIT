#http://dados.prefeitura.sp.gov.br/dataset/servidores-ativos-da-prefeitura

## referências teóricas

#http://repositorio.ipea.gov.br/bitstream/11058/4004/1/mt42_completo.pdf

#http://repositorio.enap.gov.br/bitstream/1/1422/1/2006%20Vol.57%2cn.4%20Kerr.pdf

#http://www.jstor.org/stable/976730?seq=1#page_scan_tab_contents

#http://onlinelibrary.wiley.com/doi/10.1111/j.1467-8500.1987.tb01439.x/full

#http://www.tandfonline.com/doi/abs/10.1080/21565503.2017.1403932?journalCode=rpgi20

#https://www.google.com.br/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&ved=0ahUKEwjm-ObxyunXAhXIG5AKHQMkAkQQFggyMAE&url=http%3A%2F%2Fwww.observatoriodegenero.gov.br%2Fmenu%2Fpublicacoes%2Foutros-artigos-e-publicacoes%2Fgenero-raca-e-competencias-de-direcao-no-servico-publico-federal%2Fat_download%2Ffile&usg=AOvVaw2sG0yqGcQsMkDeztz7k0q4

#http://ipea.gov.br/agencia/images/stories/PDFs/relatoriopesquisa/120601_relatorio_mulhereshomens_das.pdf

#alguns dados pra extrair a partir do que diz a literatura:


#cargos comissionados mais altos 
#cargos de coordenação




setwd("C:\\Users\\d841255\\Desktop\\SIMPROC")

library(readr)
library(dplyr)
library(data.table)
library(tidyverse)
library(stringr)
library(readxl)

list.files()

funcativo <- fread("verificadoativos14-11-2017.csv")

glimpse(funcativo)

table(funcativo$SEXO)



funcmulher <- funcativo %>%
  filter(SEXO == "F") %>%
  group_by(SECRET_PREFREG) %>%
  summarise(n_mulheres_secr = n())
  

funchomens <- funcativo %>%
  filter(SEXO == "M") %>%
  group_by(SECRET_PREFREG) %>%
  summarise(n_homens_secr = n())
  
functotal <- funcativo %>%
  group_by(SECRET_PREFREG) %>%
  summarise(n_secr = n())



funcativo <- funcativo %>%
  mutate(n_total = n()) %>%
  left_join(funcmulher, by = "SECRET_PREFREG") %>%
  left_join(funchomens, by = "SECRET_PREFREG") %>%
  left_join(functotal, by = "SECRET_PREFREG") %>%
  mutate(pct_sec = n_mulheres_secr / n_secr)


funcsec <- funcativo%>%
  filter(!str_detect(SECRET_PREFREG, "PREFEITURA REGIONAL")) %>%
  filter(!SECRET_PREFREG =="")
 
func_pct <- funcativo %>%
  select(SECRET_PREFREG, pct_sec)%>%
  distinct() %>% 
  filter(!SECRET_PREFREG =="") %>%
  filter(!str_detect(SECRET_PREFREG, "PREFEITURA REGIONAL")) %>%
  arrange(desc(pct_sec))


func_buroc <- funcativo %>%
  filter(!str_detect(SECRET_PREFREG, "PREFEITURA REGIONAL")) %>%
  filter(!SECRET_PREFREG =="") %>%
  filter(!SUBGRUPO =="DOCENTE")

table(funcativo$REF_CARGO_BAS)

g <- ggplot(func_buroc, aes(x = reorder(SECRET_PREFREG, pct_sec), y= pct_sec)) + 
  geom_bar(stat = "identity", aes(fill = SEXO), position = "fill") +
  theme_minimal() +
  scale_fill_manual(name = "Gênero", values = c("maroon4", "gray")) +
  geom_hline(yintercept = 0.5) +
  labs(title ="Composição de sexo por Secretaria", x = "Secretaria", y = "% de mulheres") + 
  theme(axis.text.x=element_text(angle=50, hjust=1), 
        legend.position = "bottom") +
  coord_flip() +
  scale_y_reverse(breaks = c(0, 0.25, 0.5, 0.75, 1),
                  labels = c("100%","75%","50%", "25%", "0%"))

g
