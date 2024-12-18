# Limpiar todo
rm(list = ls())

# Librer�as
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
  select(P10)

Data <- data.frame(Sujeto = unlist(strsplit(paste0(Raw$P10, collapse=", "), ", "))) %>%
  group_by(Sujeto) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(Sujeto = ifelse(Cantidad < 3, "Otro", Sujeto)) %>%
  group_by(Sujeto) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

Levels <- (Data %>%
  arrange(Porcentaje))$Sujeto

Data <- Data %>%
  mutate(Sujeto = factor(Sujeto, levels=Levels))

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

# Colores
Colores <- colorRampPalette(c("lightgray", "#c93131"))(3)

# Total
Total <- paste0(paste0("<span style='font-size:20pt'>",
                       "Total",
                       "</span><br><span style='font-size:30pt'>**",
                       formatC(sum(Data$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span>"))

# Gr�fico
grafico <- ggplot(Data, aes(y=Sujeto, x=Porcentaje, fill=Porcentaje)) +
  geom_col() +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")),
            size=5, color = "black", hjust = -0.2, family="font") +
  theme_light() +
  labs(y="Qui�nes realizaron las agresiones verbales") +
  scale_fill_gradient2(low=Colores[1], high=Colores[3], mid=Colores[2], midpoint=mean(Data$Cantidad), labels = function(z) str_wrap(z, width=5)) +
  scale_x_continuous(limits = c(0, max(Data$Porcentaje + 10)), labels = function(z) paste0(z, "%")) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size=10, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

# Guardar gr�fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=7)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=7, height=7)