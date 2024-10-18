# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Crear datos
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Respuestas_unsa.csv")) %>%
  filter(Imputar == "No")

Data <- Raw %>%
  select(P3) %>%
  mutate(P3 = factor(P3)) %>%
  group_by(P3) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup() %>%
  mutate(Label = paste0("<span style='font-size:15pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:10pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                        "</span>"))

Total <- sum(Data$Cantidad)

# Definir colores
Paleta <- c("#e54c7c",
             "#f2904c",
             "#ffd241",
             "#1daa6a",
             "#747264")

Colores <- c("No" = "#f2904c",
             "Sí" = "#e6e8eb")

# Total
Total <- paste0(paste0("<span style='font-size:30pt'>",
                       "Total",
                       "</span><br><span style='font-size:60pt'>**",
                       formatC(sum(Data$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span>"))

# Gráfico1
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=P3)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Total, size=15,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label),
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.box.margin=margin(5,5,5,5),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
ggsave(filename="Generos_al_nacer.png", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=5)
ggsave(filename="Generos_al_nacer.svg", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=7, height=5)