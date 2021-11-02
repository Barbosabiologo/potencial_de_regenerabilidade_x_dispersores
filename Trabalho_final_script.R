

getwd()
 
dados <- read.table("ATLANTIC_frugivory.csv", header = TRUE,sep = ";", dec = ",")
dados

select(dados$ATLANTIC_frugivory,dados$Frugivore_Species,FALSE)



