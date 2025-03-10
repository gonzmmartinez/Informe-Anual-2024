# Limpiar todo
rm(list = ls())

# Librer?as
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
  select(P22) %>%
  mutate(P22 = factor(P22, levels=c("S�", "No"))) %>%
  group_by(P22) %>%
  arrange(P22) %>%
  summarise(Cantidad = n()) %>%
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

Total <- sum(Data$Cantidad)

# Definir colores
Paleta <- c("#6e3169",
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

Colores <- c("S�" = "#f2904c",
             "No" = "#e6e8eb")

# Gr?fico1
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=P22)) +
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

# Guardar gr�fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=6, height=5)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=6, height=5)