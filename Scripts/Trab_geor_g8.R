#Trabalho de avaliação da disciplina de Introdução à análise geoespacial com R 
#Docente Maurício Vancine
--------
#Grupo Guabiroba:
#Arthur Setsuo Tahara
#Fracielson da Silva Barbosa
#Paula Ribeiro
#Pedro Henrique Reis
#Thassiana Lacerda Coelho
------
#Packages----------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(here)
library(readxl)

#Carregar diretório------------------------------------------------------------
here::here()

#DATA--------------------------------------------------------------------------
#####
# Add morcegos
{BSite <- readr::read_csv("./dados/tabelas/ATLANTIC_BATS_Study_site.csv",
                           locale = readr::locale(encoding = "latin1")) %>% 
  dplyr::select(`ID`,Latitude, Longitude) %>% 
  dplyr::rename(I=`ID`)


BCap <-  readr::read_csv("./dados/tabelas/Atlantica_bats_cap_corrigido.csv",
                     locale = readr::locale(encoding = "latin1")) %>% 
  dplyr::select(`ID`,Species) %>% 
  dplyr::rename(I=`ID`)

Bats <- dplyr::left_join(BCap,BSite, by="I")}
  
#####
#Add dados mamiferos medio e grandes
Mammals <- readr::read_csv("./dados/tabelas/ATLANTIC_MAMMAL_MID_LARGE _assemblages_and_sites.csv",
                           locale = readr::locale(encoding = "latin1"))

#Add dados aves
Birds <- readr::read_csv("./dados/tabelas/ATLANTIC_BIRDS_quantitative.csv",
                         locale = readr::locale(encoding = "latin1"))

#Add dados primatas
Primates <- read.csv(file="./dados/tabelas/ATLANTIC-PR_Occurrence.csv", sep=";")%>% 
  as_tibble()

#Para small mammals foi necessario unir as duas tabelas
{
SMCap <- readr::read_csv("./dados/tabelas/ATLANTIC_SM_Capture.csv",
                      locale = readr::locale(encoding = "latin1")) %>% 
  dplyr::select(`ID `,Actual_species_name) %>% 
  dplyr::rename(I=`ID `)

SMSite <- readr::read_csv("./dados/tabelas/ATLANTIC_SM_Study_Site.csv",
                       locale = readr::locale(encoding = "latin1")) %>% 
dplyr::select(`ID `,Longitude, Latitude) %>% 
dplyr::rename(I=`ID `)

Small_Mammals <- dplyr::left_join(SMCap,SMSite, by="I")
}


Frugivory <- readr::read_csv("./dados/tabelas/ATLANTIC_frugivory.csv")

#Selecionando colunas de interesses e montando os data.frame

BA <- Bats %>% 
  dplyr::select(Species, Longitude, Latitude) %>% 
  dplyr::mutate(Group="Bats", 
                .before = Species)  
 

SM <- Small_Mammals %>% 
  dplyr::select(Actual_species_name, Longitude, Latitude) %>% 
  dplyr::rename(Species=Actual_species_name) %>% 
  dplyr::mutate(Group ="Small Mammals", 
                 .before = Species)

PR <- Primates %>% 
  dplyr::select(SPECIES, LONGITUDE_X, LATITUDE_Y) %>% 
  dplyr::mutate(Group="Primates", 
                .before = SPECIES) %>% 
  dplyr::rename(Species=SPECIES,
                Longitude=LONGITUDE_X,
                Latitude=LATITUDE_Y)

BI <- Birds %>% 
  dplyr::select(Species, Longitude_x, Latitude_y) %>% 
  dplyr::mutate(Group="Birds", 
                .before = Species) %>% 
  dplyr::rename(Longitude=Longitude_x,
                Latitude=Latitude_y)

MG <- Mammals %>% 
  dplyr::select(Actual_species_Name, Longitude, Latitude) %>% 
  dplyr::rename(Species=Actual_species_Name) %>% 
  dplyr::mutate(Group ="Mammals", 
                .before = Species)

FR <- Frugivory %>% 
  dplyr::select(Frug_Group, Frugivore_Species, Longitude, Latitude) %>% 
  dplyr::rename(Group=Frug_Group,
                Species=Frugivore_Species) %>% 
  tidyr::drop_na()

# Unindo data fauna em um arq só

dados <- dplyr::bind_rows(BI, PR, SM, MG, BA)
dados

# Selecionando apenas as espécies em comum entre "dados" e "FR", assim teremos uma tabela apenas com especies frugivoras.

dados_2 <- dados %>% 
  semi_join(FR, by = "Species") 
dados_2

# adiconando os dados de frugívoros à tabela para distribuição dos pontos ao mapa
dados_3 <- dplyr::bind_rows(FR, dados_2)
dados_3



