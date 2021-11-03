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

# Add morcegos
Bats <- readxl::read_excel("ATLANTIC_BATS_2020_comp.xlsx") %>% 
  rename(id = ID...1) %>% 
  mutate(Longitude = str_replace(Longitude, ",", ".") %>% as.numeric(),
         Latitude = str_replace(Latitude, ",", ".") %>% as.numeric()) %>% 
  filter(Olsong200r == "Atlantic Forests") %>% 
  select(1, Longitude, Latitude, Anoura.caudifer:Micronycteris.sp.) %>% 
  pivot_longer(cols = Anoura.caudifer:Micronycteris.sp.,
               names_to = "Species",
               values_to = "Abundancy") %>% 
  select(-Abundancy)
Bats


  
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


# Limpeza de dados final
dados_final <- dados %>% 
  semi_join(FR, by = "Species") %>% #Selecionando apenas as espécies em comum entre "dados" e "FR", assim teremos uma tabela apenas com especies frugivoras.
  dplyr::bind_rows(FR) %>%# adiconando os dados de frugívoros à tabela para distribuição dos pontos ao mapa 
  filter(Group != "Amphibians") %>% #Excluindo Amphibios da lista
  distinct(Species, Longitude, Latitude, .keep_all = TRUE)#removendo espécies que aparecem no msm lugar
dados_final

# Exportar os arquivos
write.csv(dados_filtro_fr, file = "~/GitHub/potencial_de_regenerabilidade_x_dispersores/dados/tabelas/dados_filtro_fr.csv") 
write.csv(dados_final, file = "~/GitHub/potencial_de_regenerabilidade_x_dispersores/dados/tabelas/dados_final.csv")  

# importar raster ---------------------------------------------------------


# criam hexagonos ---------------------------------------------------------



# resumir as informacoes --------------------------------------------------



# estatistica -------------------------------------------------------------



# end ---------------------------------------------------------------------


