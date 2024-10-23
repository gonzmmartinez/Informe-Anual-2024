# Limpiar todo
rm(list = ls())

# Librerias
library(openxlsx)
library(tidyverse)
library(ggforce)
library(ggplot2)
library(ggthemes)
library(devtools)
library(rgdal)
library(geogrid)
library(sf)
library(tmap)
library(readxl)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Cargar datos
Data <- read_excel(path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Femicidios 2014-2024 - modificado.xlsx"),
                   sheet="En filas") %>%
  group_by(Provincia) %>%
  summarize(Promedio = mean(Tasa))

# Cargar shape
Mapa_Argentina <- rnaturalearth::ne_states(country = c("argentina"), returnclass = "sf") %>%
  mutate(name = ifelse(name == "Ciudad Autónoma de Buenos Aires", "CABA", name))

# Modificar datos
Data_femicidios <- Data %>%
  rename(name = "Provincia")
Mapa_Argentina <- Mapa_Argentina %>%
  left_join(Data_femicidios, by="name") %>%
  replace(is.na(.), 0)

# Colores
Colores <- colorRampPalette(c("white", "#191717"))(2)

# Grafico
grafico <- ggplot(Mapa_Argentina) +
  geom_sf(color="black", aes(fill=Promedio)) +
  geom_sf_text(aes(label=formatC(round(Promedio,2), big.mark=".", decimal.mark=",")),
               color="black", family="font", size=2, show_guide=FALSE) +
  theme_void() +
  guides(fill = guide_colorbar(theme = theme(legend.frame = element_rect(colour = "black")))) +
  scale_fill_gradient2(name="Tasa de femicidios", low="white", high="#a5549c", midpoint=0.7, limits=c(0.5,2), breaks=seq(0.5,2, by=0.5)) +
  theme(text=element_text(family="font"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.margin = margin(t=0,r=0,b=0,l=-25),
        plot.background = element_rect(fill="white", color=NA),
        legend.position = "right",
        legend.justification = "top",
        legend.margin = margin(t=50, r=0, b=0, l=0),
        legend.title = element_text(size=10, family="font", margin=margin(t=0,r=0,b=10,l=0)),
        legend.text = element_text(size=5, family="font"),
        legend.ticks = element_line(color="black")) +
  guides(color = guide_legend( 
    override.aes=list(shape = 16)))


# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=5, height=8)
ggsave(filename = paste0(filename, ".svg"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=5, height=8)