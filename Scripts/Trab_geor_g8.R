#Trabalho de avalia√ß√£o da disciplina de Introdu√ß√£o √† an√°lise geoespacial com R 
#Docente Maur√≠cio Vancine
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

#Carregar diret√≥rio------------------------------------------------------------
here::here()

#DATA--------------------------------------------------------------------------

Bats <- read.csv(file="./dados/tabelas/ATLANTIC-PR_Occurrence.csv", sep = ";") %>% 
  as_tibble()

Mammals <- readr::read_csv("./dados/tabelas/ATLANTIC_MAMMAL_MID_LARGE _assemblages_and_sites.csv",
                           locale = readr::locale(encoding = "latin1"))

Birds <- readr::read_csv("./dados/tabelas/ATLANTIC_BIRDS_quantitative.csv",
                         locale = readr::locale(encoding = "latin1"))

Primates <- read.csv(file="./dados/tabelas/ATLANTIC-PR_Occurrence.csv", sep=";")%>% 
  as_tibble()

#Para small mammals foi necess·rio unir as duas tabelas
{
SMCap <- readr::read_csv("./dados/tabelas/ATLANTIC_SM_Capture.csv",
                      locale = readr::locale(encoding = "latin1")) %>% 
  dplyr::select(`ID†`,Actual_species_name) %>% 
  dplyr::rename(I=`ID†`)

SMSite <- readr::read_csv("./dados/tabelas/ATLANTIC_SM_Study_Site.csv",
                       locale = readr::locale(encoding = "latin1")) %>% 
dplyr::select(`ID†`,Longitude, Latitude) %>% 
dplyr::rename(I=`ID†`)

Small_Mammals <- dplyr::left_join(SMCap,SMSite, by="I")
}

Frugivory <- readr::read_csv("./dados/tabelas/ATLANTIC_frugivory.csv")

#Selecionando colunas de interesses e montando os data.frame

BA <- Bats %>% 
  dplyr::select(SPECIES, LONGITUDE_X, LATITUDE_Y) %>% 
  dplyr::mutate(Group="Bats", 
                .before = SPECIES) %>% 
  dplyr::rename(Species=SPECIES,
                Longitude=LONGITUDE_X,
                Latitude=LATITUDE_Y)

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

FR <- Frugivory %>% 
  dplyr::select(Frug_Group, Frugivore_Species, Longitude, Latitude) %>% 
  dplyr::rename(Group=Frug_Group,
                Species=Frugivore_Species) %>% 
  tidyr::drop_na()

#Unindo em um arq s√≥

dados <- dplyr::bind_rows(BI, PR, FR, SM, BA)

