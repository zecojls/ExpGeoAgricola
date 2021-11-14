
## Pacotes
library(terra)
library(luna)
library(geodata)

# Listando produtos
prod <- getProducts()
head(prod)

# Listando produtos MODIS
modis <- getProducts("^MOD|^MYD|^MCD")
head(modis)

# MODIS Reflectancia da superficie 500 m
product <- "MOD09A1"
productInfo(product)

# Filtros de datas
start <- "2021-07-01"
end <- "2021-10-31"

# Poligonos de municipios
BR <- gadm("BR", level = 2, path = "dados/vector")
BR

# Ponta Grossa
selecao <- BR$NAME_2 == "Ponta Grossa"
PG <- BR[selecao,]
plot(PG)

# Listando images MODIS disponiveis
mf <- luna::getModis(product, start, end, aoi=PG, download = FALSE)
mf

# Pasta onde sera salvo os arquivos
datadir <- "dados/raster"
dir.create(datadir, showWarnings=FALSE)

# Download das imagens
mfpg <- luna::getModis(product, start, end, aoi=aoi, download=TRUE,
                       path=datadir, username="XXX", password="XXX")
