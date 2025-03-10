# Limpiar todo
rm(list = ls())

# Librer�as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Requerimientos_911_horas.csv")) %>%
  filter(Accion == "Llamadas", Tipo == "Violencia de g�nero")

Data <- Raw %>%
  group_by(A�o, Hora) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Hora = factor(Hora),
         A�o = factor(formatC(A�o, big.mark=".", decimal.mark=","))) %>%
  group_by(A�o) %>%
  mutate(Porcentaje = Cantidad/sum(Cantidad) * 100) %>%
  ungroup

# Gr�fico
grafico <- ggplot(Data, aes(x="", y=Hora, fill=Porcentaje)) +
  geom_tile() +
  facet_wrap(~A�o, nrow=1) +
  geom_text(aes(label=paste0(round(Porcentaje,1),"%")), family="font", size=3, color="black", alpha=0.7) +
  labs(x="A�o", y="Hora") +
  scale_y_discrete(limits = rev) + 
  scale_fill_gradient2(low="#1daa6a", mid="#FAF99F", high="#a5549c", midpoint=mean(Data$Porcentaje)) +
  theme_light() +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=10, family="font", face="italic"),
        strip.text = element_text(size=15, family="font", face="bold"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, margin = margin(t=0,r=5,b=0,l=0)))

# Guardar gr�fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=10)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=10, height=10)