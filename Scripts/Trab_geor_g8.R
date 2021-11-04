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
library(stringr)
library(raster)
library(sf)
library(tmap)


#Carregar diretório------------------------------------------------------------
here::here()

#DATA--------------------------------------------------------------------------

# Add morcegos
Bats <- readxl::read_excel("ATLANTIC_BATS_2020_comp.xlsx") %>% 
  rename(id = ID...1) %>% 
  mutate(Longitude = str_replace(Longitude, ",", ".") %>% as.numeric(),
         Latitude = str_replace(Latitude, ",", ".") %>% as.numeric()) %>% 
  filter(Olsong200r == "Atlantic Forests") %>% 
  dplyr::select(1, Longitude, Latitude, Anoura.caudifer:Micronycteris.sp.) %>% 
  pivot_longer(cols = Anoura.caudifer:Micronycteris.sp.,
               names_to = "Species",
               values_to = "Abundancy") %>% 
  dplyr::select(-Abundancy)
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
#write.csv(dados_filtro_fr, file = "~/GitHub/potencial_de_regenerabilidade_x_dispersores/dados/tabelas/dados_filtro_fr.csv") 
#write.csv(dados_final, file = "~/GitHub/potencial_de_regenerabilidade_x_dispersores/dados/tabelas/dados_final.csv")  

# importar raster ---------------------------------------------------------

# listar raster
ti <- dir(path = here::here("dados", "raster"), pattern = "map",
          full.names = TRUE) %>% 
  grep(".tif", ., value = TRUE)# Lista apenas os arquivos da pasta com o final TIF
ti

# # importar limites ------------------------------------------------------
lim <- sf::st_read("./dados/vetor/limit_af_lawaf2006_gcs_wgs84.shp") %>% 
  sf::st_transform(crs = 4674)
lim
plot(lim$geometry)

#raster do mapa
raster <- raster::raster("./dados/raster/reg_1000m_sirgas2000.tif")
raster

plot(raster, col = viridis::viridis(10))
plot(lim$geometry, add = TRUE)
 
# Criando vetor com os dados
dados_vetor <- dados_final %>% 
  tidyr::drop_na(Longitude, Latitude) %>% 
  dplyr::filter(Longitude > -1e3) %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4674)
dados_vetor

# filtro das coordenadas
dados_vetor_lim <- dados_vetor[lim, ]
dados_vetor_lim

# corte do raster
raster_lim <- raster %>% 
  raster::crop(lim) %>% 
  raster::mask(lim)
raster_lim

plot(raster_lim, col = viridis::viridis(10))
plot(lim$geometry, add = TRUE)
plot(dados_vetor_lim$geometry, pch = 20, col = "red", add = TRUE)

# criam hexagonos ---------------------------------------------------------

dados_hex <- lim %>% 
  sf::st_make_grid(cellsize = 1, square = FALSE) %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(sf::st_intersects(x = ., y = lim, sparse = FALSE)) %>% 
  st_as_sf()
dados_hex

#sf::sf_write(obj = dados_hex, dsn = here::here("dados", "dados.shp"))

plot(lim$geometry)
plot(dados_hex, add = TRUE)

##Cortar

# lim_hex <- sf::st_intersection(x = lim_albers, y=dados_hex_albers, col="light gray")
# lim_hex_albers <- st_transform(lim_hex, crs = crs(raster))
# plot(lim_hex_albers$geometry)

#Plots
plot(raster_lim, col=viridis::viridis(10))
plot(lim$geometry, add = TRUE)
plot(dados_hex, add = TRUE)
plot(dados_vetor_lim$geometry, pch = 20, col = "red", add = TRUE)

# extracts ----------------------------------------------------------------

dados_hex_id <- dados_hex %>% 
  sf::st_as_sf() %>% 
  dplyr::mutate(id = 1:nrow(.))

dados_hex_reg <- dados_hex_id %>% 
  sf::st_as_sf() %>% 
  dplyr::mutate(reg = raster::extract(x = raster_lim, y = dados_hex, fun = median, na.rm = TRUE))
dados_hex_reg$reg

plot(dados_hex_reg)

ggplot() +
  geom_sf(data = dados_hex_reg, aes(fill = reg)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 15)

# pontos ------------------------------------------------------------------

dados_hex_sp <- dados_hex_id %>% 
  sf::st_join(dados_vetor_lim) %>%
  sf::st_drop_geometry() %>% 
  dplyr::distinct(id, Species, .keep_all = TRUE) %>% 
  tidyr::drop_na(Species) %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(n = n())
dados_hex_sp$n

dados_hex_species <- dplyr::left_join(dados_hex_id, dados_hex_reg) %>% 
  dplyr::mutate(n = ifelse(is.na(n), 0, n))
dados_hex_species

ggplot() +
  geom_sf(data = dados_hex_species, aes(fill = n)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 15)

# join final --------------------------------------------------------------

dados_hex_reg_species <- dplyr::left_join(dados_hex_reg, st_drop_geometry(dados_hex_species))
dados_hex_reg_species

dados_hex_reg_species %>% 
  sf::st_drop_geometry() %>%
  dplyr::filter(n > 0) %>% 
  ggplot(aes(x = sqrt(n), y = reg/100)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_bw(base_size = 15)

summary(glm(reg ~ n, data = dados_hex_reg_species %>% 
      sf::st_drop_geometry() %>% 
      dplyr::mutate(reg = reg / 100), family = binomial))


# Rasterização--------------------------------------------------
# Rasterizar lim_hex_albers
lim_hex_albers_raster <- raster::rasterToPolygons(lim_hex_albers) %>% #não é assim
  sf::st_as_sf()
lim_hex_albers_raster

# Rasterizar dados_vetor_albers
dados_vetor_albers_raster <- raster::rasterToPoints(dados_vetor_albers, spatial = TRUE) %>% #não é assim
  sf::st_as_sf()
dados_vetor_albers_raster


# extrair valores do raster -----------------------------------------------

# raster original
regen <- raster::stack(raster)
regen
plot(regen, col = colorRamps::matlab.like2(100))

# copia
regen.id <- regen
plot(regen, col = colorRamps::matlab.like(100))

# criar um raster com ids das celulas
regen.id[] # valores das celulas

plot(is.na(regen.id)) # na
plot(!is.na(regen.id)) # nao na

regen.id[!is.na(regen.id)] # valores nao na

ncell(regen.id[!is.na(regen.id)]) # numeroes de celulas nao na
seq(regen.id[!is.na(regen.id)]) # seq 1 para o número de celulas

regen.id[!is.na(regen.id)] <- seq(regen.id[!is.na(regen.id)])
plot(regen.id, col = colorRamps::matlab.like2(100))

dados_valores <- dados %>% 
  mutate(oppc = raster::extract(regen.id, dplyr::select(dados, longitude, latitude)))
dados_valores

# verificar
sort(table(dados_valores$oppc))

# valores
dados_valores2 <- dados_valores %>% 
  dplyr::distinct(oppc, .keep_all = TRUE) %>% 
  na.omit
dados_valores2

# verificar
sort(table(dados_valores2$oppc))

# exportar
#exportar dados_valores2#
dados = dados_final.csv
# estatistica -------------------------------------------------------------
m1 <- glm(Regenerablabla~diversidadetotal)
m2<-glm(Regenerabilidade~diversidade por grupo)



# end ---------------------------------------------------------------------



