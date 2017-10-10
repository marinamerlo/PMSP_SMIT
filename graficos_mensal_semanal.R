#Script para os gráficos dos processos administrativos da PMSP autuado no SEI e em papel para acompanhamento da migração

setwd("C:\\Users\\d841255\\Desktop\\teste")
library(tidyverse)
library(ggrepel)
library(data.table)

#semanal
#extrair da planilha gerada toda segunda feira e salvar numa a parte
semanal <- read_delim("C:/Users/d841255/Desktop/teste/semanal.csv", ";", 
                        escape_double = FALSE, col_types = cols(pct = col_character()), trim_ws = TRUE)

semanal <- semanal %>% 
  mutate(pct = gsub(",", ".", pct)) %>% 
  mutate(pct = as.numeric(pct)) %>%
  mutate(crono = as.numeric(crono))

colour <- c("#40b8d0", "#b2d183")

ggplot(data=semanal, 
       aes(x= reorder(data, crono), 
           y=pct, 
           group=tipo, 
           color=tipo)) +
  geom_line(size=2, 
            lineend= "round") +
  scale_y_continuous(limits = c(35, 65)) +
  xlab("Período") + 
  ylab("Porcentagem de processos autuados") +
  ggtitle("Proporção semanal de processos físicos/eletrônicos") + 
  scale_colour_manual(name = "", 
                      values= colour, 
                      labels=c("Porcentagem de processos eletrônicos", "Porcentagem de processos em papel")) +
  theme(axis.text.x=element_text(angle=30, 
                                 hjust=1, 
                                 size=8)) + 
   geom_label_repel(aes(label = pct_label), 
                    show.legend=F)+
  theme_bw() +
  theme(legend.box.background = element_rect(),
        legend.position = c(.95, .25),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.margin = margin(3, 3, 3, 3),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank())
ggsave("semanal_26052017.png", width = 10, height = 5)

#mensal

mensal <- fread("mensal.csv")

mensal_label <- mensal %>%
  select(data, pct_label) %>%
  filter(data == "mai/17")

mensal <- mensal %>% 
  mutate(pct = gsub(",", ".", pct)) %>% 
  mutate(pct = as.numeric(pct)) %>%
  mutate(crono = as.numeric(crono))

colour <- c("deepskyblue2", "yellowgreen")

library(ggrepel)

ggplot(data=mensal, 
       aes(x= reorder(data, crono), 
           y=pct, 
           group=tipo, 
           color=tipo,
           label = pct_label)) +
  geom_line(size=2, 
            lineend= "round") +
  #xlab("Período") + 
  ylab("Porcentagem de processos autuados") +
  ggtitle("Proporção mensal de processos físicos/eletrônicos") + 
   scale_color_manual(name = "", 
                      values= colour, 
                      labels=c("Porcentagem de processos eletrônicos", "Porcentagem de processos em papel")) +
  theme(axis.text.x=element_text(angle=30, 
                                 hjust=1, 
                                 size=8)) + 
  geom_label(data=mensal[mensal$data=="mai/17"|mensal$data=="dez/16"|mensal$data=="mai/16"|mensal$data=="jan/16",], 
             show.legend=F, size=3.5)+
  theme_minimal() + 
  theme(#legend.box.background = element_rect(),
        legend.position = "bottom",
        legend.justification = c("bottom"),
        #legend.box.just = "right",
        #legend.box.margin = margin(3, 3, 3, 3),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.direction = "horizontal",
        #panel.grid.major.x = element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(angle=40, hjust=1),
        axis.title.x=element_blank())
ggsave("mensal_26052017.png", width = 10, height = 5)
