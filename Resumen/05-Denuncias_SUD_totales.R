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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Denuncias_SUD_totales.csv")) %>%
  mutate(Tipo = case_when(Tipo == "Penal por violencia de g�nero" ~ "Penal y civil por violencia de g�nero y/o familiar",
                          Tipo == "Penal por violencia familiar" ~ "Penal y civil por violencia de g�nero y/o familiar",
                          Tipo == "Violencia no penal" ~ "Penal y civil por violencia de g�nero y/o familiar",
                          Tipo == "Penal" ~ "Otros delitos penales"))

Data1 <- Raw %>%
  filter(A�o == 2024) %>%
  group_by(Tipo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Tipo = factor(Tipo,
                         levels = c("Penal y civil por violencia de g�nero y/o familiar",
                                    "Otros delitos penales"))) %>%
  arrange(Tipo) %>%
  mutate(Label = paste0("<span style='font-size:10pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:6pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                        "</span>")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

# Definir colores
Colores <- c("Penal y civil por violencia de g�nero y/o familiar" = "#a5549c",
             "Otros delitos penales" = "#1daa6a")

# Total
Total1 <- paste0(paste0("<span style='font-size:20pt'>",
                       "Total",
                       "</span><br><span style='font-size:30pt'> **",
                       formatC(sum(Data1$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span>"))

# Gr�fico1
grafico <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tipo)) +
  geom_rect() +
  geom_richtext(y=2, x=1.5,
                label=Total1, size=6,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=3,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores, name="Tipo de denuncia", labels=function(z) str_wrap(z, width=20)) +
  labs(title="2.024",
       subtitle = str_wrap("enero-septiembre inclusive", 20)) +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font", size=15, face="italic", hjust=0.5, margin=margin(b=-15)),
        legend.title = element_text(family="font", size=12, margin=margin(b=10)),
        legend.text = element_text(size=15),
        legend.box.margin = margin(t=5,b=5,l=5,r=5),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(b=-20),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gr�fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=6, height=4.5)
ggsave(filename = paste0(filename, ".svg"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=6, height=4.5)