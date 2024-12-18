# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer�as
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Crear datos
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Requerimientos_911.csv"))

Data <- Raw %>%
  mutate(A�o = as.factor(A�o)) %>%
  filter(A�o %in% c(2023, 2024))

Data2 <- Raw %>%
  mutate(A�o = as.factor(A�o)) %>%
  filter(A�o %ni% c(2023, 2024))

# Colores
Colores <- c("#1daa6a", "#e54c7c")

# Gr�fico
grafico <- ggplot(Data, aes(x=reorder(Mes, Mes_ord), y=Cantidad, group= A�o, color=A�o)) +
  geom_line(data=Data2, color="darkgrey", linewidth=1, linetype=2) +
  annotate(geom="label", x=11, y=c(14689,16529,18053), label=2020:2022, size=5, color="darkgrey", fill="white", family="font") +
  geom_point(size=3) +
  geom_line(linewidth=2) +
  ylim(0,NA) +
  labs(title="",
       x="Mes", y="Cantidad") +
  geom_dl(aes(label = A�o), method = list(cex=1.5, dl.trans(x = x + 0.4), "last.points", fontfamily="font")) +
  theme_light() +
  scale_x_discrete(labels = ~ substr(.x, 1,3)) +
  scale_color_manual(values=Colores[2:1]) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gr�fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=12, height=7)

# Fuente: Elaboraci�n propia a partir de la informaci�n remitida por la Oficina de Violencia Familiar y de G�nero. Corte de Justicia de Salta