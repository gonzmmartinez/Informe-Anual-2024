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

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Denuncias_tipo.csv"))

Data1 <- Raw %>%
  filter(A�o == 2024) %>%
  group_by(A�o, Tipo) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Tipo = replace(Tipo, Porcentaje <= 5, "Otras")) %>%
  group_by(A�o, Tipo) %>%
  summarise(Cantidad = sum(Cantidad),
            Porcentaje = sum(Porcentaje)) %>%
  mutate(Label = ifelse(Porcentaje < 10,
                        paste0("<span style='font-size:12pt'>**",round(Porcentaje,1),"%**</span><br><span style='font-size:8pt'>",formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>")),
                        paste0("<span style='font-size:25pt'>**",round(Porcentaje,1),"%**</span><br><span style='font-size:10pt'>",formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>")))) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

Data2 <- Raw %>%
  filter(A�o == 2023) %>%
  group_by(A�o, Tipo) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Tipo = replace(Tipo, Porcentaje <= 5, "Otras")) %>%
  group_by(A�o, Tipo) %>%
  summarise(Cantidad = sum(Cantidad),
            Porcentaje = sum(Porcentaje)) %>%
  mutate(Label = paste0("<span style='font-size:15pt'>**",
                        round(Porcentaje,1),
                        "%**</span><br><span style='font-size:8pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",",
                                "</span>")),
         ypos = (sum(Cantidad) - lag(cumsum(Cantidad), default = 0) - 0.5 * Cantidad)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

# Definir colores
Colores <- c("F�sica" = "#a5549c",
             "Psicol�gica" = "#1daa6a",
             "Simb�lica" = "#f2904c",
             "Otras" = "#747264")
# Gr�fico1
grafico1 <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.25, fill=Tipo)) +
  geom_rect() +
  geom_richtext(aes(x = 3.2, y=ymid, label=Label), size=3,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(name = "Tipo de violencia",
                    values = Colores) +
  labs(title="2.024",
       subtitle = "enero-septiembre") +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font", size=15, face="italic", hjust=0.5),
        legend.title = element_text(family="font", size=10, margin=margin(b=5)),
        legend.text = element_text(size=15),
        legend.box.margin=margin(5,5,5,5),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Gr�fico2
grafico2 <- ggplot(Data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.25, fill=Tipo)) +
  geom_rect() +
  geom_richtext(aes(x = 3.2, y=ymid, label=Label), size=3,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  labs(title="2.023",
       subtitle="Todo el a�o") +
  theme(text=element_text(family="font"),
        legend.position = "none",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font", size=12, face="italic", hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.box.margin=margin(5,5,5,5))

# Layout
grafico <- plot_grid(grafico2, grafico1, ncol=2,
                     rel_widths = c(1,2)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gr�fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=5)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=10, height=5)