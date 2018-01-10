


## SEMANA 2 ####

library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

# BASIC DATA MANIPULATION ####
# Manipulação basicas de dados #

ext_tracks_file <- "http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/data/ebtrk_atlc_1988_2015.txt"


ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

# Read the file in from its url
ext_tracks <- read_fwf(ext_tracks_file, 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")


library(httr)
meso_url <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py/"
denver <- GET(url = meso_url,
              query = list(station = "DEN",
                           data = "sped",
                           year1 = "2016",
                           month1 = "6",
                           day1 = "1",
                           year2 = "2016",
                           month2 = "6",
                           day2 = "30",
                           tz = "America/Denver",
                           format = "comma")) %>%
  content() %>% 
  read_csv(skip = 5, na = "M") 

denver %>% slice(1:3)

##  PIPING ####

katrina <- filter(ext_tracks, storm_name == "KATRINA")
katrina_reduced <- select(katrina, month, day, hour, max_wind)
head(katrina_reduced, 3)

# Construindo o mesmo filtro  sem contruir novos objetos de dados,
# otmizanto tempo e espaço de memória.

head(select(filter(ext_tracks, storm_name == "KATRINA"),
            month, day, hour, max_wind), 3)

# Com o uso do piping evita a reatribuição do quadro de dados a cada etapa realizada
# assim, com o piping pode-se enviar a saída da útima função para a próxima função como
# o primeiro argumento.

ext_tracks %>%
  filter(storm_name == "KATRINA") %>%
  select(month, day, hour, max_wind) %>%
  head(3)


# SUMARISE DATA ####
# Resumindo Dados #

## Os pacotes tidyr e dplyr contém inumera funções no que se diz respeito a 
## limpeza de dados, começanado com a summarize .

ext_tracks %>%
  summarize(n_obs = n(),
            worst_wind = max(max_wind),
            worst_pressure = min(min_pressure))

## Podemos usar funções própias dentro da função de resumo(summarize),
## supondo que queiras corigir alguma coluna/varaivél que esteja mal dentro
## da base.


knots_to_mph <- function(knots){
  mph <- 1.152 * knots
}

ext_tracks %>%
  summarize(n_obs = n(),
            worst_wind = knots_to_mph(max(max_wind)),
            worst_pressure = min(min_pressure))


## Agrupando dados por uma determinada coluna/variável, ou seja,
## interesse particular em alguma caracterista da variável.

ext_tracks %>%
  group_by(storm_name, year) %>%
  head()

#Source: local data frame [6 x 29]
#Groups: storm_name, year [1]

ext_tracks %>%
  group_by(storm_name, year) %>%
  summarize(n_obs = n(),
            worst_wind = max(max_wind),
            worst_pressure = min(min_pressure))

## group_by/summarize é uma combinação que pode ser bem utíl
## se quiser fazer de resumos de dados.

library(ggplot2)
ext_tracks %>%
  group_by(storm_name) %>%
  summarize(worst_wind = max(max_wind)) %>%
  ggplot(aes(x = worst_wind)) + geom_histogram() 


# Você não pode fazer alterações em uma variável que está sendo usada 
# para agrupar um quadro de dados. Se você tentar, você receberá o erro 
# Erro: não pode modificar a variável de agrupamento. Se você receber esse 
# erro, use a função desagrupar para remover o agrupamento dentro de um 
# quadro de dados e, em seguida, você poderá mutar qualquer uma das 
# variáveis nos dados.


# SELECING AND FILERING DATASETS ####
# Selecionando e filtrando conjuntos de dados #

# Para criação de sub conjuntos de dados, realizando filtros de certas variaveis,
# ou filtrar algumas linhas de dados, para essas ações pode ser feita usando o 
# dplyr com a função filter e select.


ext_tracks %>%
  select(storm_name, month, day, hour, year, latitude, longitude, max_wind)


## função starts_with, para selecionar variáveis que iniciam com,
## strings iguais.



ext_tracks %>%
  select(storm_name, latitude, longitude, starts_with("radius_34"))


## Funções Similares ##
## ends_with: Select all columns that end with a certain string 
  ## (for example, select(ext_tracks, ends_with("ne")) to get all 
  ## the wind radii for the northeast quadrant of a storm for the 
  ## hurricane example data) 

## contains: Select all columns that include a certain string 
  ##(select(ext_tracks, contains("34")) to get all wind radii for 
  ## 34-knot winds)

## matches: Select all columns that match a certain relative expression 
  ## (select(ext_tracks, matches("_[0-9][0-9]_")) to get all columns 
  ## where the column name includes two numbers between two underscores,
  ## a pattern that matches all of the wind radii columns).


## Opeadores Lógicos do R.

# Operator	        Meaning  	                Example
#  ==	              Equals	           storm_name == KATRINA
#  !=	           Does not equal	        min_pressure != 0
#  >	            Greater than	            latitude > 25
#  >=	        Greater than or equal to	  max_wind >= 160
#  <	             Less than	            min_pressure < 900
#  <=	          Less than or equal to	  distance_to_land <= 0
# %in%	           Included in	        storm_name %in% c("KATRINA", "ANDREW")
# is.na()	      Is a missing value	        is.na(radius_34_ne)



head(ext_tracks$hour)

head(ext_tracks$hour == "00")


ext_tracks %>% 
  select(storm_name, hour, max_wind) %>%
  head(9)

ext_tracks %>%
  select(storm_name, hour, max_wind) %>%
  filter(hour == "00") %>%
  head(3)


ext_tracks %>%
  group_by(storm_name, year) %>%
  summarize(worst_wind = max(max_wind)) %>%
  filter(worst_wind >= 160)

ext_tracks %>%
  select(storm_name, month, day, hour, latitude, longitude, max_wind) %>%
  filter(storm_name == "ANDREW" & max_wind >= 137) 



# ADDING, CHANGING OR RENAMING COLUMNS ####
# Adicionando colunas e fazendo mudanças de colunas ja existes
# em data frames.

# Dados copa do mundo #

library(faraway)
data(worldcup)


# Este conjunto de dados tem observações do jogador, incluindo a equipe do jogador, 
# posição, quantidade de tempo jogado nesta Copa do Mundo, e número de tiros, passes, 
# tackles e salva. Este conjunto de dados atualmente não é arrumado, pois possui uma 
# das variáveis (nomes dos jogadores) como rownames, e não como uma coluna do quadro 
# de dados. Você pode usar a função mutar para mover os nomes dos jogadores para sua 
# própria coluna:


worldcup <- worldcup %>%
  mutate(player_name = rownames(worldcup))

worldcup %>% slice(1:3)


# Você também pode usar mutate em coordenação com group_by para criar novas colunas
# que dão resumos dentro de determinadas janelas dos dados. Por exemplo, o código a 
# seguir irá adicionar uma coluna com o número médio de fotos para a posição do jogador 
# adicionada como uma nova coluna. Embora este código esteja resumindo os dados 
# originais para gerar os valores nesta coluna, mutate irá adicionar esses valores 
# de resumo repetidos ao conjunto de dados original por grupo, em vez de retornar um 
# quadro de dados com uma única linha para cada uma das variáveis de agrupamento 
# (tente substituir mutate com resumir neste código para se certificar de que compreende
# a diferença).

worldcup <- worldcup %>% 
  group_by(Position) %>%
  mutate(ave_shots = mean(Shots)) %>%
  ungroup()

worldcup %>% slice(1:3)



# Se houver uma coluna que você deseja renomear, mas não mudar, você pode usar a 
# função de renomear. Por exemplo:

worldcup %>% 
  rename(Name = player_name) %>%
  slice(1:3)


# SPREADING AND GATHERING DATA ####
# Separando e Juntando Dados #

# O pacote tidyr inclui funções para transferir um quadro de dados entre formatos 
# longos e amplos. [Definir formatos de dados longos e amplos] Na seção sobre dados 
# arrumados, mostramos um exemplo que usou recolher para converter dados em um formato 
# arrumado. Os dados são primeiro em um formato descomprometido:




























