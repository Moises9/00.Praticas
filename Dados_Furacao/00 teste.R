


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

data("VADeaths")
head(VADeaths)




# Depois de mudar as categorias de idade de nomes de linha para uma vari�vel (o que pode 
# ser feito com a fun��o de muta��o), o problema-chave com a limpeza dos dados � que as 
# vari�veis de urbano / rural e masculino / feminino n�o est�o em suas pr�prias colunas,
# mas sim s�o incorporados na estrutura das colunas. Para corrigir isso, voc� pode usar 
# a fun��o reunir, para coletar valores espalhados por v�rias colunas em uma �nica coluna,
# com os nomes das colunas reunidos em uma coluna "chave". Ao reunir, exclua as colunas 
# que voc� n�o quer "reunidas" (neste caso), incluindo os nomes das colunas com o sinal 
# menos na fun��o de recolha. Por exemplo:


data("VADeaths")
library(tidyr)

# Move age from row names into a column
# Mover idade dos nomes das linhas para uma coluna

VADeaths  <- VADeaths %>%
  tbl_df() %>%
  mutate(age = row.names(VADeaths)) 
head(VADeaths)


# Gather everything but age to tidy data
# Re�na tudo, exceto idade, para arrumar dados

VADeaths %>%
  gather(key = key, value = death_rate, -age)


# Mesmo que seus dados estejam em um formato arrumado, reunir � ocasionalmente �til,
# para juntar dados juntos para tirar proveito de facetas ou tra�ar parcelas separadas
# com base em uma vari�vel de agrupamento. Por exemplo, se voc� quiser tra�ar a rela��o
# entre o tempo que um jogador jogou na Copa do Mundo e seu n�mero de salvamentos, 
# tackles e tiros, com um gr�fico separado para cada posi��o (Figura 1.2), voc� pode
# usar reunir para puxe todos os n�meros de salva, toque e dispare em uma �nica coluna
# (N�mero) e use facetas para tra��-los como gr�ficos separados

library(tidyr)
library(ggplot2)
worldcup %>%
  select(Position, Time, Shots, Tackles, Saves) %>% 
  gather(Type, Number, -Position, -Time) %>%
  ggplot(aes(x = Time, y = Number)) + 
  geom_point() + 
  facet_grid(Type ~ Position)


# A fun��o de propaga��o � menos comum para arrumar dados. No entanto, pode ser 
# �til para criar tabelas de resumo. Por exemplo, se voc� quisesse imprimir uma 
# tabela do n�mero m�dio e intervalo de passes por posi��o para as quatro melhores 
# equipes nesta Copa do Mundo (Espanha, Holanda, Uruguai e Alemanha), voc� poderia 
# executar:

library(knitr)

# Summarize the data to create the summary statistics you want
# Resumir os dados para criar as estat�sticas de resumo desejadas


wc_table <- worldcup %>% 
  filter(Team %in% c("Spain", "Netherlands", "Uruguay", "Germany")) %>%
  select(Team, Position, Passes) %>%
  group_by(Team, Position) %>%
  summarize(ave_passes = mean(Passes),
            min_passes = min(Passes),
            max_passes = max(Passes),
            pass_summary = paste0(round(ave_passes), " (", 
                                  min_passes, ", ",
                                  max_passes, ")")) %>%
  select(Team, Position, pass_summary)

# What the data looks like before using `spread`
# Como os dados se parecem antes de usar `spread`

wc_table

# Observe neste exemplo como a propaga��o foi usada no final da seq��ncia de c�digo
# para converter os dados resumidos em uma forma que ofere�a uma melhor apresenta��o
# tabular para um relat�rio. Na chamada de propaga��o, primeiro especifique o nome 
# da coluna a ser usada para os novos nomes das colunas (Posi��o neste exemplo) e 
# especifique a coluna a ser usada para os valores das c�lulas (pass_summary aqui).

# Neste c�digo, usei a fun��o kable do pacote knitr para criar a tabela de resumo em 
# um formato de tabela e n�o como sa�da R b�sica. Esta fun��o � muito �til para formatar
# tabelas b�sicas em documentos de redirecionamento R. Para tabelas mais complexas, 
# confira os pacotes pander e xtable.


# MERGING DATASETS ####

# Muitas vezes, voc� ter� dados em dois conjuntos de dados separados que voc� gostaria
# de combinar com base em uma vari�vel comum ou vari�veis. Por exemplo, para os dados 
# de exemplo da Copa do Mundo que usamos, seria interessante adicionar uma coluna com 
# a finalidade da equipe de cada jogador. N�s inclu�mos dados com essa informa��o em 
# um arquivo chamado "team_standings.csv", que pode ser lido no R objectteam_standings
# com a chamada :.


team_standings <- read_csv("data/team_standings.csv")
team_standings %>% slice(1:3)



# Este quadro de dados tem uma observa��o por equipe e os nomes das equipes s�o consistentes
# com os nomes das equipes no quadro de dados do Worldcup.

# Voc� pode usar as diferentes fun��es da fam�lia * _join para mesclar esses dados de equipe
# com as estat�sticas do jogador no quadro worldcupdata. Depois de fazer isso, voc� pode usar
# outras ferramentas de limpeza de dados do dplyr para puxar rapidamente e explorar partes
# interessantes do conjunto de dados. Os principais argumentos para as fun��es * _join s�o
# os nomes de objeto dos dois quadros de dados para se juntar e por, que especifica quais 
# vari�veis usar para corresponder as observa��es dos dois quadros de dados.

# Existem v�rias fun��es na fam�lia * _join. Essas fun��es combinam dois quadros de dados;
# eles diferem em como eles lidam com observa��es que existem em um, mas n�o em ambos os 
# quadros de dados. Aqui est�o as quatro fun��es desta fam�lia que voc� provavelmente usar�
# mais frequentemente:


# Fun��o O que ele inclui no quadro de dados mesclado-------------------
 
# left_join : Inclui todas as observa��es no quadro de dados � esquerda, 
# independentemente de haver ou n�o uma correspond�ncia no quadro de dados certo

# right_join: Inclui todas as observa��es no quadro de dados correto, 
# independentemente de haver ou n�o uma correspond�ncia no quadro de dados � esquerda
 
# inner_join: Inclui apenas observa��es que est�o em ambos os quadros de dados
 
# full_join: Inclui todas as observa��es de ambos os quadros de dados

# Nesta tabela, o quadro de dados "esquerdo" refere-se � primeira entrada de quadro de 
# dados na chamada * _join, enquanto o quadro de dados "direito" se refere � segunda 
# entrada de quadro de dados na fun��o. Por exemplo, na chamada

left_join(world_cup, team_standings, by = "Team")


# O quadro de dados world_cup � o quadro de dados "� esquerda" e o quadro de dados 
# team_standings � o quadro de dados "correto". Portanto, o uso de left_join incluiria
# todas as linhas da World_cup, independentemente de o jogador ter ou n�o uma equipe 
# listada em time_journal, enquanto a right_join incluiria todas as linhas do team_standings, 
# independentemente de haver ou n�o jogadores dessa equipe no world_cup.

# Lembre-se de que, se voc� estiver usando a tubula��o, o primeiro quadro de dados
# ("� esquerda" para essas fun��es) �, por padr�o, o quadro de dados criado pelo c�digo
# logo antes do tubo. Quando voc� mescla os quadros de dados como uma etapa no c�digo 
# de canal, portanto, a moldura de dados "esquerda" � a canalizada na fun��o, enquanto
# o quadro de dados "direito" � o indicado na chamada de fun��o * _join.
 
# Como um exemplo de fus�o, digamos que voc� deseja criar uma tabela dos 5 melhores
# jogadores por tiros no objetivo, bem como a posi��o final para cada uma dessas equipes
# de jogadores, usando os dados do worldcup e team_standings. Voc� pode fazer isso executando:

data(worldcup)
worldcup %>% 
  mutate(Name = rownames(worldcup),
         Team = as.character(Team)) %>%
  select(Name, Position, Shots, Team) %>%
  arrange(desc(Shots)) %>%
  slice(1:5) %>%
  left_join(team_standings, by = "Team") %>% # Merge in team standings
  rename("Team Standing" = Standing) %>%
  kable()


# Al�m da fus�o neste c�digo, h� algumas outras coisas interessantes para apontar:
  
# O c�digo usa a fun��o as.character dentro de uma chamada mutada para alterar o nome 
# da equipe de um fator para um personagem no quadro de dados do mundo. Ao combinar dois
# quadros de dados, � mais seguro se a coluna que voc� est� usando para mesclar tem a
# mesma classe em cada quadro de dados. A coluna "Equipe" � uma classe de caracteres
# no quadro de dados theteam_standings, mas uma classe de fatores no quadro worldcupdata,
# ent�o essa chamada converte essa coluna para uma classe de personagem na worldcup.
# A fun��o left_join ainda executar� uma mesclagem se voc� n�o incluir essa chamada,
# mas lan�ar� um aviso de que � coagir a coluna na worldcup para um vetor de caracteres.
# Geralmente, � mais seguro faz�-lo explicitamente.
 
# Ele usa a fun��o de sele��o para remover colunas em que n�o nos interessamos e tamb�m 
# colocar as colunas que queremos manter na ordem que gostar�amos para a mesa final.
 
# Ele usa arranjos seguido de uma fatia para retirar os 5 melhores jogadores e orden�-los
# por n�mero de tiros.
 
# Para um dos nomes das colunas, queremos usar "Team Standing" em vez do nome
# da coluna atual "Standing". Este c�digo usa renomear no final para fazer essa
# altera��o logo antes de criar a tabela. Voc� tamb�m pode usar o argumento 
# col.names na fun��o kable para personalizar todos os nomes das colunas na 
# tabela final, mas essa chamada renomeada � uma solu��o r�pida, pois queremos
# apenas mudar um nome de coluna.


# WORKING WITH DATES, TIMES, TIMES ZONES ----------------------------------

# Os objetivos de aprendizagem para esta se��o s�o:
  
# Transforme dados n�o arrumados em dados arrumados
 
# Manipular e transformar uma variedade de tipos de dados, incluindo datas, horas e 
# dados de texto.

 
# R tem classes de objetos especiais para datas e data-�pocas. Muitas vezes, vale a 
# pena converter uma coluna em um quadro de dados para um desses tipos de objetos especiais, 
# porque voc� pode fazer algumas coisas muito �teis com objetos de data ou data-hora, 
# incluindo retirar o m�s ou dia da semana das observa��es em o objeto, ou determinar a
# diferen�a de tempo entre dois valores.
 
# Muitos dos exemplos desta se��o usam o objeto ext_tracks carregado anteriormente no livro.
# Se voc� precisar recarregar isso, voc� pode usar o seguinte c�digo para faz�-lo:


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


