# Limpiar todo
rm(list = ls())

`%nin%` = Negate(`%in%`)

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
  filter(Imputar == "No") %>%
  select(P15)

Data <- Raw %>%
  rename(Tipo = "P15") %>%
  mutate(Tipo = str_to_sentence(Tipo)) %>%
  mutate(Tipo = str_replace_all(Tipo, ";", ",")) %>%
  mutate(Tipo = ifelse(Tipo %nin% c("No atravesaste situaciones de violencia/discriminacion", "No reaccionaste ante estas situaciones", "Ninguna", ""),
                       "Sí", "No")) %>%
  mutate(Tipo = factor(Tipo, levels=c("Sí", "No"))) %>%
  group_by(Tipo) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  group_by(Tipo) %>%
  arrange(Tipo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>% 
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup() %>%
  mutate(Label = paste0("<span style='font-size:20pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:10pt'>(",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                        ")</span>")) %>%
  mutate(xpos = ifelse(Porcentaje <= 10, 4.4, 3.25))

# Definir colores
Colores <- c("#6e3169",
             "#e54c7c",
             "#f2904c",
             "#ffd241",
             "#1daa6a",
             "#747264",
             "#c93131",
             "#e55a3e",
             "#9bc6b7",
             "#266f9b",
             "#2e544d")

Colores <- c("Sí" = "#e55a3e",
             "No" = "#e6e8eb")

# Total
Total <- paste0(paste0("<span style='font-size:20pt'>",
                       "Total",
                       "</span><br><span style='font-size:30pt'>**",
                       formatC(sum(Data$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span>"))

# Gráfico1
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=Tipo)) +
  geom_rect() +
  geom_richtext(aes(x = xpos, y=ymid, label=Label),
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 6)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        plot.margin = margin(t=-150, b=-150, r=0, l=-150),
        legend.box.margin = margin(t=0, r=0, b=0, l=-150),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=6, height=5)
ggsave(filename = paste0(filename, ".svg"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=6, height=5)