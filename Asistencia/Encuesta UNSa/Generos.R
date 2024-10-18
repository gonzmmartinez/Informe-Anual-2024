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

Data1 <- Raw %>%
  select(P2) %>%
  group_by(P2) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(P2 = ifelse(Cantidad < 3, "Otro", P2)) %>%
  group_by(P2) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

Levels <- (Data1 %>%
  arrange(Porcentaje))$P2

Data1 <- Data1 %>%
  mutate(P2 = factor(P2, levels=Levels))

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

# Total
Total <- paste0(paste0("<span style='font-size:20pt'>",
                       "Total",
                       "</span><br><span style='font-size:30pt'>**",
                       formatC(sum(Data1$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span>"))

# Gráfico
grafico <- ggplot(Data1, aes(y=P2, x=Porcentaje, fill=Porcentaje)) +
  geom_col() +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")),
            size=5, color = "black", hjust = -0.2, family="font") +
  theme_light() +
  labs(y="Géneros de las personas respondientes") +
  scale_fill_gradient2(low="#6e3169", high="#1daa6a", mid="#266f9b", midpoint=mean(Data1$Porcentaje), labels = function(z) str_wrap(z, width=5)) +
  scale_x_continuous(limits = c(0, max(Data1$Porcentaje + 10)), labels = function(z) paste0(z, "%")) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=10, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

# Guardar gráfico
ggsave(filename="Generos.png", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=7)
ggsave(filename="Generos.svg", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=7, height=7)