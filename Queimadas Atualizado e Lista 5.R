require(dplyr)
library(readr)
library(stringr)
require(tidyverse)
library(tibble)
require(lubridate)



queimadasdt1 = readr::read_csv("Dataset_FireWatch_Brazil_Q1_2024(1).csv")
queimadasdt2 = readr::read_csv("Dataset_FireWatch_Brazil_Q2_2024(1).csv")
queimadasdt3 = readr::read_csv("Dataset_FireWatch_Brazil_Q3_2024(1).csv")
view(queimadasdt3)
view(queimadasdt2)
view(queimadasdt1)
queimadasdt = rbind(queimadasdt1,queimadasdt2,queimadasdt3)
view(queimadasdt)

#1)

#a)Utilizando o banco de dados Queimadas, crie uma tabela com a contagem de incêndios por estado.

incendio_estados = queimadasdt %>%
  group_by(estado) %>%
  summarise(count = n()) 
view(incendio_estados)

#b)Qual o número de queimadas por região?

sul = c("PARANÁ", "RIO GRANDE DO SUL", "SANTA CATARINA")
suldeste = c("SÃO PAULO","RIO DE JANEIRO","MINAS GERAIS","ESPÍRITO SANTO")
centrooeste = c("GOIÁS","MATO GROSSO","MATO GROSSO DO SUL","DISTRITO FEDERAL")
norte = c("ACRE","AMAPÁ","AMAZONAS","PARA","RÔRAIMA","TOCANTINS","RONDÔNIA")
nordeste = c("PERNAMBUCO","BAHIA","SERGÍPE","ALAGOAS","CEARÁ","MARANHÃO","PARAÍBA","RIO GRANDE DO NORTE","PIAUÍ")

#região sul
n_queimadas_sul = queimadasdt %>%
  group_by(estado) %>%
  filter(estado %in% sul)%>%
  summarise(count = n())
view(n_queimadas_sul)

#região suldeste
n_queimadas_suldeste = queimadasdt %>%
  group_by(estado) %>%
  filter(estado %in% suldeste)%>%
  summarise(count = n())
view(n_queimadas_suldeste)

#região centrooeste
n_queimadas_centro_oeste = queimadasdt %>%
  group_by(estado) %>%
  filter(estado %in% centrooeste)%>%
  summarise(count = n())
view(n_queimadas_centro_oeste)

#região norte
n_queimadas_norte = queimadasdt %>%
  group_by(estado) %>%
  filter(estado %in% norte)%>%
  summarise(count = n())
view(n_queimadas_norte)
ncol(n_qu)
#região nordeste
n_queimadas_nordeste = queimadasdt %>%
  group_by(estado) %>%
  filter(estado %in% nordeste)%>%
  summarise(count = n())
view(n_queimadas_nordeste)

#c)Para a região com maior número de queimadas, identifique: 
#a. A cidade com maior número de queimadas.
#b. A data com maior número de queimadas.
#c. O mês com maior número de queimadas.
max_centro_oeste = sum(n_queimadas_centro_oeste$count)
max_norte = sum(n_queimadas_norte$count)
max_nordeste = sum(n_queimadas_nordeste$count)
max_suldeste = sum(n_queimadas_suldeste$count)
max_sul = sum(n_queimadas_sul$count)

max_centro_oeste 
max_norte 
max_nordeste 
max_suldeste
max_sul 

#maior npumero de queimadas na região nordeste

#a. A cidade com maior número de queimadas.

queimadas_nordeste = queimadasdt %>%
  group_by(estado) %>%
  filter(estado %in% nordeste) 

municipio_queimadas = queimadas_nordeste %>%
  group_by(municipio) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1)
view(municipio_queimadas)

#b.A data com maior número de queimadas.

municipio_queimadas = queimadas_nordeste %>%
  group_by(data) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1)
view(municipio_queimadas)

#c. O mês com maior número de queimadas.

data_queimadas = queimadas_nordeste %>%
  mutate(data = month(as.Date(data, format = "%Y-%m-%d"))) %>%  
  group_by(data) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
view(data_queimadas)


#4)Quantas cidades tiveram risco de fogo acima de 90% no mês de julho?

cidade_risc_90 = queimadasdt %>%
  filter(avg_risco_fogo > 90) %>%
  group_by(municipio) %>%
  summarise(count = n()) 
nrow(cidade_risc_90)

#5)Qual a média de risco de fogo por estado?

media_risc_estado = queimadasdt %>%
  group_by(estado) %>%
  summarise(media = mean(avg_risco_fogo))
view(media_risc_estado)





require(data.table)
require(tidyverse)

dados <- read.csv("C:\\Users\\levan\\Documents\\Aulas em R\\chocolate.csv.gz")

paises_produz <- dados %>% 
  summarise(num_paises = n_distinct(local_compania))

paises_produz

tres_ingredientes <- dados %>% 
  filter(as.numeric(str_extract(ingredientes, "^\\d+")) >= 3) %>% 
  summarise(num_chocolates = n())

tres_ingredientes


cinco_ingredientes <- dados %>% 
  filter(as.numeric(str_extract(ingredientes, "^\\d+")) == 5) %>% 
  summarise(num_ingredientes = n())

cinco_ingredientes


quatro_caracteristicas <- dados %>% 
  mutate(num_caracteristicas = str_count(caracteristicas, ",") + 1) %>% 
  filter(num_caracteristicas >= 4) %>% 
  summarise(num_chocolates = n())


quatro_caracteristicas


sal_composicao <- dados %>% 
  filter(str_detect(ingredientes, "Sa")) %>% 
  summarise(num_sal = n())

sal_composicao 


baunilha_composicao <- dados %>% 
  filter(str_detect(ingredientes, "V")) %>% 
  summarise(num_baunilha = n())

baunilha_composicao


banilha_lecitina <- dados %>% 
  filter(str_detect(ingredientes, "V") & str_detect(ingredientes, "L")) %>% 
  summarise(num_composicao = n())

banilha_lecitina




art <- fread("C:\\Users\\levan\\Documents\\Aulas em R\\Art.csv.gz")
moma <- fread("C:\\Users\\levan\\Documents\\Aulas em R\\Art_Moma.csv.gz")


moma_media_ano <- moma %>% 
  group_by(year) %>% 
  summarise(media_ano = mean(moma_count_to_year)) %>% 
  arrange(desc(media_ano))

print(moma_media_ano, n = 22)

whitney_media_ano <- moma %>% 
  group_by(year) %>% 
  summarise(media_ano = mean(whitney_count_to_year)) %>% 
  arrange(desc(media_ano))

print(whitney_media_ano, n = 22)


juncao <- moma %>%
  inner_join(art %>% select(artist_unique_id, artist_race_nwi, artist_name),
             by = c("artist_unique_id" = "artist_unique_id"))


moma_media_ano_nwi <- juncao %>% 
  filter(artist_race_nwi == "Non-White") %>% 
  group_by(year) %>% 
  summarise(media_ano = mean(moma_count_to_year)) %>% 
  arrange(desc(media_ano))

print(moma_media_ano_nwi, n = 22)


whitney_media_ano_nwi <- juncao %>% 
  filter(artist_race_nwi == "Non-White") %>% 
  group_by(year) %>% 
  summarise(media_ano = mean(whitney_count_to_year)) %>% 
  arrange(desc(media_ano))

print(whitney_media_ano_nwi)



quatro_artistas <- juncao %>%
  group_by(artist_name) %>% 
  summarise(mais_apresenta = sum(moma_count_to_year)) %>% 
  arrange(desc(mais_apresenta)) %>% 
  slice_head(n = 4)


quatro_artistas



genero_artistas <- art %>% 
  group_by(artist_gender) %>% 
  summarise(genero = n()) 

genero_artistas



nacionalidades_top <- art %>% 
  group_by(artist_nationality) %>% 
  summarise(nacionalidades = n()) %>% 
  arrange(desc(nacionalidades)) %>% 
  slice_head(n = 5)

nacionalidades_top


cada_livro_moma <- juncao %>% 
  filter(moma_count_to_year > 0) %>% 
  group_by(book) %>% 
  summarise(apresenta_moma = n_distinct(artist_name)) %>% 
  arrange(desc(apresenta_moma))

cada_livro_moma


cada_livro_whitney <- juncao %>% 
  filter(whitney_count_to_year > 0) %>% 
  group_by(book) %>% 
  summarise(apresenta_whitney = n_distinct(artist_name)) %>%
  arrange(desc(apresenta_whitney))

cada_livro_whitney              



media_espaco <- juncao %>% 
  group_by(artist_name) %>% 
  summarise(media_espaco = mean(space_ratio_per_page_total)) %>% 
  arrange(desc(media_espaco))

media_espaco






refugiados_pais <- fread("C:\\Users\\levan\\Documents\\Aulas em R\\refugiados_pais.csv.gz")
refugiados <- fread("C:\\Users\\levan\\Documents\\Aulas em R\\refugiados.csv.gz")


juncao_refugiados <- refugiados %>%
  inner_join(refugiados_pais, by = c("id_origem" = "id")) %>%
  inner_join(refugiados_pais, by = c("id_destino" = "id"), 
             suffix = c("_origem", "_destino"))

media_refugiados <- juncao_refugiados %>% 
  group_by(nome_origem) %>% 
  summarise(media_pais = mean(refugiados)) %>% 
  arrange(desc(media_pais))

media_refugiados


afega_90 <- juncao_refugiados %>% 
  filter(ano == "1990") %>% 
  filter(nome_origem == "Afeganistão") %>% 
  summarise(total = n())

afega_90