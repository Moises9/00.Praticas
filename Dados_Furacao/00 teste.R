


## SEMANA 2 ####

library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

# BASIC DATA MANIPULATION ####
# Manipula��o basicas de dados #

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
# otmizanto tempo e espa�o de mem�ria.

head(select(filter(ext_tracks, storm_name == "KATRINA"),
            month, day, hour, max_wind), 3)

# Com o uso do piping evita a reatribui��o do quadro de dados a cada etapa realizada
# assim, com o piping pode-se enviar a sa�da da �tima fun��o para a pr�xima fun��o como
# o primeiro argumento.

ext_tracks %>%
  filter(storm_name == "KATRINA") %>%
  select(month, day, hour, max_wind) %>%
  head(3)


# SUMARISE DATA ####
# Resumindo Dados #

## Os pacotes tidyr e dplyr cont�m inumera fun��es no que se diz respeito a 
## limpeza de dados, come�anado com a summarize .

ext_tracks %>%
  summarize(n_obs = n(),
            worst_wind = max(max_wind),
            worst_pressure = min(min_pressure))

## Podemos usar fun��es pr�pias dentro da fun��o de resumo(summarize),
## supondo que queiras corigir alguma coluna/varaiv�l que esteja mal dentro
## da base.


knots_to_mph <- function(knots){
  mph <- 1.152 * knots
}

ext_tracks %>%
  summarize(n_obs = n(),
            worst_wind = knots_to_mph(max(max_wind)),
            worst_pressure = min(min_pressure))


## Agrupando dados por uma determinada coluna/vari�vel, ou seja,
## interesse particular em alguma caracterista da vari�vel.

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

## group_by/summarize � uma combina��o que pode ser bem ut�l
## se quiser fazer de resumos de dados.

library(ggplot2)
ext_tracks %>%
  group_by(storm_name) %>%
  summarize(worst_wind = max(max_wind)) %>%
  ggplot(aes(x = worst_wind)) + geom_histogram() 


# Voc� n�o pode fazer altera��es em uma vari�vel que est� sendo usada 
# para agrupar um quadro de dados. Se voc� tentar, voc� receber� o erro 
# Erro: n�o pode modificar a vari�vel de agrupamento. Se voc� receber esse 
# erro, use a fun��o desagrupar para remover o agrupamento dentro de um 
# quadro de dados e, em seguida, voc� poder� mutar qualquer uma das 
# vari�veis nos dados.


# SELECING AND FILERING DATASETS ####
# Selecionando e filtrando conjuntos de dados #

# Para cria��o de sub conjuntos de dados, realizando filtros de certas variaveis,
# ou filtrar algumas linhas de dados, para essas a��es pode ser feita usando o 
# dplyr com a fun��o filter e select.


ext_tracks %>%
  select(storm_name, month, day, hour, year, latitude, longitude, max_wind)


## fun��o starts_with, para selecionar vari�veis que iniciam com,
## strings iguais.



ext_tracks %>%
  select(storm_name, latitude, longitude, starts_with("radius_34"))


## Fun��es Similares ##
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


## Opeadores L�gicos do R.

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
# Adicionando colunas e fazendo mudan�as de colunas ja existes
# em data frames.

# Dados copa do mundo #

library(faraway)
data(worldcup)


# Este conjunto de dados tem observa��es do jogador, incluindo a equipe do jogador, 
# posi��o, quantidade de tempo jogado nesta Copa do Mundo, e n�mero de tiros, passes, 
# tackles e salva. Este conjunto de dados atualmente n�o � arrumado, pois possui uma 
# das vari�veis (nomes dos jogadores) como rownames, e n�o como uma coluna do quadro 
# de dados. Voc� pode usar a fun��o mutar para mover os nomes dos jogadores para sua 
# pr�pria coluna:


worldcup <- worldcup %>%
  mutate(player_name = rownames(worldcup))

worldcup %>% slice(1:3)


# Voc� tamb�m pode usar mutate em coordena��o com group_by para criar novas colunas
# que d�o resumos dentro de determinadas janelas dos dados. Por exemplo, o c�digo a 
# seguir ir� adicionar uma coluna com o n�mero m�dio de fotos para a posi��o do jogador 
# adicionada como uma nova coluna. Embora este c�digo esteja resumindo os dados 
# originais para gerar os valores nesta coluna, mutate ir� adicionar esses valores 
# de resumo repetidos ao conjunto de dados original por grupo, em vez de retornar um 
# quadro de dados com uma �nica linha para cada uma das vari�veis de agrupamento 
# (tente substituir mutate com resumir neste c�digo para se certificar de que compreende
# a diferen�a).

worldcup <- worldcup %>% 
  group_by(Position) %>%
  mutate(ave_shots = mean(Shots)) %>%
  ungroup()

worldcup %>% slice(1:3)



# Se houver uma coluna que voc� deseja renomear, mas n�o mudar, voc� pode usar a 
# fun��o de renomear. Por exemplo:

worldcup %>% 
  rename(Name = player_name) %>%
  slice(1:3)


# SPREADING AND GATHERING DATA ####
# Separando e Juntando Dados #

# O pacote tidyr inclui fun��es para transferir um quadro de dados entre formatos 
# longos e amplos. [Definir formatos de dados longos e amplos] Na se��o sobre dados 
# arrumados, mostramos um exemplo que usou recolher para converter dados em um formato 
# arrumado. Os dados s�o primeiro em um formato descomprometido:



























