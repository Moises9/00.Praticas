


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

data("VADeaths")
head(VADeaths)




# Depois de mudar as categorias de idade de nomes de linha para uma variável (o que pode 
# ser feito com a função de mutação), o problema-chave com a limpeza dos dados é que as 
# variáveis de urbano / rural e masculino / feminino não estão em suas próprias colunas,
# mas sim são incorporados na estrutura das colunas. Para corrigir isso, você pode usar 
# a função reunir, para coletar valores espalhados por várias colunas em uma única coluna,
# com os nomes das colunas reunidos em uma coluna "chave". Ao reunir, exclua as colunas 
# que você não quer "reunidas" (neste caso), incluindo os nomes das colunas com o sinal 
# menos na função de recolha. Por exemplo:


data("VADeaths")
library(tidyr)

# Move age from row names into a column
# Mover idade dos nomes das linhas para uma coluna

VADeaths  <- VADeaths %>%
  tbl_df() %>%
  mutate(age = row.names(VADeaths)) 
head(VADeaths)


# Gather everything but age to tidy data
# Reúna tudo, exceto idade, para arrumar dados

VADeaths %>%
  gather(key = key, value = death_rate, -age)


# Mesmo que seus dados estejam em um formato arrumado, reunir é ocasionalmente útil,
# para juntar dados juntos para tirar proveito de facetas ou traçar parcelas separadas
# com base em uma variável de agrupamento. Por exemplo, se você quiser traçar a relação
# entre o tempo que um jogador jogou na Copa do Mundo e seu número de salvamentos, 
# tackles e tiros, com um gráfico separado para cada posição (Figura 1.2), você pode
# usar reunir para puxe todos os números de salva, toque e dispare em uma única coluna
# (Número) e use facetas para traçá-los como gráficos separados

library(tidyr)
library(ggplot2)
worldcup %>%
  select(Position, Time, Shots, Tackles, Saves) %>% 
  gather(Type, Number, -Position, -Time) %>%
  ggplot(aes(x = Time, y = Number)) + 
  geom_point() + 
  facet_grid(Type ~ Position)


# A função de propagação é menos comum para arrumar dados. No entanto, pode ser 
# útil para criar tabelas de resumo. Por exemplo, se você quisesse imprimir uma 
# tabela do número médio e intervalo de passes por posição para as quatro melhores 
# equipes nesta Copa do Mundo (Espanha, Holanda, Uruguai e Alemanha), você poderia 
# executar:

library(knitr)

# Summarize the data to create the summary statistics you want
# Resumir os dados para criar as estatísticas de resumo desejadas


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

# Observe neste exemplo como a propagação foi usada no final da seqüência de código
# para converter os dados resumidos em uma forma que ofereça uma melhor apresentação
# tabular para um relatório. Na chamada de propagação, primeiro especifique o nome 
# da coluna a ser usada para os novos nomes das colunas (Posição neste exemplo) e 
# especifique a coluna a ser usada para os valores das células (pass_summary aqui).

# Neste código, usei a função kable do pacote knitr para criar a tabela de resumo em 
# um formato de tabela e não como saída R básica. Esta função é muito útil para formatar
# tabelas básicas em documentos de redirecionamento R. Para tabelas mais complexas, 
# confira os pacotes pander e xtable.


# MERGING DATASETS ####

# Muitas vezes, você terá dados em dois conjuntos de dados separados que você gostaria
# de combinar com base em uma variável comum ou variáveis. Por exemplo, para os dados 
# de exemplo da Copa do Mundo que usamos, seria interessante adicionar uma coluna com 
# a finalidade da equipe de cada jogador. Nós incluímos dados com essa informação em 
# um arquivo chamado "team_standings.csv", que pode ser lido no R objectteam_standings
# com a chamada :.


team_standings <- read_csv("data/team_standings.csv")
team_standings %>% slice(1:3)



# Este quadro de dados tem uma observação por equipe e os nomes das equipes são consistentes
# com os nomes das equipes no quadro de dados do Worldcup.

# Você pode usar as diferentes funções da família * _join para mesclar esses dados de equipe
# com as estatísticas do jogador no quadro worldcupdata. Depois de fazer isso, você pode usar
# outras ferramentas de limpeza de dados do dplyr para puxar rapidamente e explorar partes
# interessantes do conjunto de dados. Os principais argumentos para as funções * _join são
# os nomes de objeto dos dois quadros de dados para se juntar e por, que especifica quais 
# variáveis usar para corresponder as observações dos dois quadros de dados.

# Existem várias funções na família * _join. Essas funções combinam dois quadros de dados;
# eles diferem em como eles lidam com observações que existem em um, mas não em ambos os 
# quadros de dados. Aqui estão as quatro funções desta família que você provavelmente usará
# mais frequentemente:


# Função O que ele inclui no quadro de dados mesclado-------------------
 
# left_join : Inclui todas as observações no quadro de dados à esquerda, 
# independentemente de haver ou não uma correspondência no quadro de dados certo

# right_join: Inclui todas as observações no quadro de dados correto, 
# independentemente de haver ou não uma correspondência no quadro de dados à esquerda
 
# inner_join: Inclui apenas observações que estão em ambos os quadros de dados
 
# full_join: Inclui todas as observações de ambos os quadros de dados

# Nesta tabela, o quadro de dados "esquerdo" refere-se à primeira entrada de quadro de 
# dados na chamada * _join, enquanto o quadro de dados "direito" se refere à segunda 
# entrada de quadro de dados na função. Por exemplo, na chamada

left_join(world_cup, team_standings, by = "Team")


# O quadro de dados world_cup é o quadro de dados "à esquerda" e o quadro de dados 
# team_standings é o quadro de dados "correto". Portanto, o uso de left_join incluiria
# todas as linhas da World_cup, independentemente de o jogador ter ou não uma equipe 
# listada em time_journal, enquanto a right_join incluiria todas as linhas do team_standings, 
# independentemente de haver ou não jogadores dessa equipe no world_cup.

# Lembre-se de que, se você estiver usando a tubulação, o primeiro quadro de dados
# ("à esquerda" para essas funções) é, por padrão, o quadro de dados criado pelo código
# logo antes do tubo. Quando você mescla os quadros de dados como uma etapa no código 
# de canal, portanto, a moldura de dados "esquerda" é a canalizada na função, enquanto
# o quadro de dados "direito" é o indicado na chamada de função * _join.
 
# Como um exemplo de fusão, digamos que você deseja criar uma tabela dos 5 melhores
# jogadores por tiros no objetivo, bem como a posição final para cada uma dessas equipes
# de jogadores, usando os dados do worldcup e team_standings. Você pode fazer isso executando:

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


# Além da fusão neste código, há algumas outras coisas interessantes para apontar:
  
# O código usa a função as.character dentro de uma chamada mutada para alterar o nome 
# da equipe de um fator para um personagem no quadro de dados do mundo. Ao combinar dois
# quadros de dados, é mais seguro se a coluna que você está usando para mesclar tem a
# mesma classe em cada quadro de dados. A coluna "Equipe" é uma classe de caracteres
# no quadro de dados theteam_standings, mas uma classe de fatores no quadro worldcupdata,
# então essa chamada converte essa coluna para uma classe de personagem na worldcup.
# A função left_join ainda executará uma mesclagem se você não incluir essa chamada,
# mas lançará um aviso de que é coagir a coluna na worldcup para um vetor de caracteres.
# Geralmente, é mais seguro fazê-lo explicitamente.
 
# Ele usa a função de seleção para remover colunas em que não nos interessamos e também 
# colocar as colunas que queremos manter na ordem que gostaríamos para a mesa final.
 
# Ele usa arranjos seguido de uma fatia para retirar os 5 melhores jogadores e ordená-los
# por número de tiros.
 
# Para um dos nomes das colunas, queremos usar "Team Standing" em vez do nome
# da coluna atual "Standing". Este código usa renomear no final para fazer essa
# alteração logo antes de criar a tabela. Você também pode usar o argumento 
# col.names na função kable para personalizar todos os nomes das colunas na 
# tabela final, mas essa chamada renomeada é uma solução rápida, pois queremos
# apenas mudar um nome de coluna.


# WORKING WITH DATES, TIMES, TIMES ZONES ----------------------------------

# Os objetivos de aprendizagem para esta seção são:
  
# Transforme dados não arrumados em dados arrumados
 
# Manipular e transformar uma variedade de tipos de dados, incluindo datas, horas e 
# dados de texto.

 
# R tem classes de objetos especiais para datas e data-épocas. Muitas vezes, vale a 
# pena converter uma coluna em um quadro de dados para um desses tipos de objetos especiais, 
# porque você pode fazer algumas coisas muito úteis com objetos de data ou data-hora, 
# incluindo retirar o mês ou dia da semana das observações em o objeto, ou determinar a
# diferença de tempo entre dois valores.
 
# Muitos dos exemplos desta seção usam o objeto ext_tracks carregado anteriormente no livro.
# Se você precisar recarregar isso, você pode usar o seguinte código para fazê-lo:


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


