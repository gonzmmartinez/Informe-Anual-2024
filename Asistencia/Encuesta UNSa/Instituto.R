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
  select(P7) %>%
  mutate(P7 = factor(P7)) %>%
  group_by(P7) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup() %>%
  mutate(Label = ifelse(Porcentaje <=5,
                        paste0("<span style='font-size:12pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                               "%**  </span><span style='font-size:10pt'>(",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                               ")</span>"),
                        paste0("<span style='font-size:15pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                               "%**</span><br><span style='font-size:10pt'>(",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                               ")</span>")))

Total <- sum(Data$Cantidad)

# Definir colores
Colores <- c("#e54c7c",
             "#892e4a",
             "#f2904c",
             "#91562e",
             "#1daa6a",
             "#116640",
             "#747264")

# Total
Total <- paste0(paste0("<span style='font-size:30pt'>",
                       "Total",
                       "</span><br><span style='font-size:60pt'>**",
                       formatC(sum(Data$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span>"))

# Gráfico1
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=P7)) +
  geom_rect() +
  geom_richtext(aes(y=ymid, label=Label),
                x = ifelse(Data$Porcentaje <= 5, 4.4, 3.3),
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 6)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  scale_alpha(range = c(0.8, 1), guide = 'none') +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        plot.margin = margin(t=-100, b=-100, r=0, l=-150),
        legend.box.margin = margin(t=0, r=0, b=0, l=-120),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
ggsave(filename="Instituto.png", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=5)
ggsave(filename="Instituto.svg", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=7, height=5)