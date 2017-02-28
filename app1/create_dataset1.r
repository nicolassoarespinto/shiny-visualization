#setwd("C:/Users/b2826073/Desktop/EBC/Moeda")
setwd("..")


library(dplyr)
library(tidyr)
library(xlsx)
library(lazyeval)
library(ggplot2)
library(scales)
library(tidyr)

raw_file<- read.xlsx("raw_data/mc10_moeda_manual_1901_2000.xls", sheetIndex = 1, encoding = "UTF-8", startRow = 3, stringsAsFactors = FALSE)



names(raw_file) <- raw_file[1,]
names(raw_file)[1]<- "ano"

user_names <- raw_file[3:dim(raw_file)[1],]

names(raw_file)<- c("ano", "periodo","papel_moeda_emitido","caixa_autoridades","caixa_bancos", "papel_moeda_pp")

raw_file<- mutate(raw_file, moeda = ifelse(is.na(caixa_bancos), ifelse(is.na(periodo), caixa_autoridades, periodo), NA)) 




fill(raw_file, moeda, .direction = "down") %>% 
  filter(!is.na(periodo)) %>% 
  filter(!is.na(caixa_autoridades)) %>%
  fill(ano) -> data_table1

data_table1$periodo <- data_table1$periodo %>% as.numeric %>% as.Date(origin = as.Date("1899-12-30"))




a<- data_table1 %>% filter(!is.na(ano)) %>% gather(key = key, value = value, caixa_bancos, caixa_autoridades, papel_moeda_pp,papel_moeda_emitido)

a$value <- as.numeric(a$value)

a <- a %>% mutate( var=  a$value/lag(a$value) - 1)

write.table(a, "data/data_app1.csv", sep = ";")

#############################

g<- ggplot(a, 
       aes( x = periodo, y = value, color = moeda)) + 
  geom_path(group = "key")  + facet_wrap("key")

#############################

g + scale_y_continuous(labels = comma)

