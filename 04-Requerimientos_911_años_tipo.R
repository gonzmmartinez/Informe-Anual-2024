# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer�as
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Crear datos
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Requerimientos_911_Accion_Tipo.csv"))

Data <- Raw %>%
  mutate(Accion = factor(Accion, levels=c("Llamadas recibidas por el 911","Intervenciones por agencia policial","Intervenciones conjuntas con agencia SAMEC")),
         Tipo = factor(Tipo, levels=c("Violencia de g�nero", "Violencia familiar en curso", "Violencia familiar hist�rica")))

Totales <- Data %>%
  group_by(Accion) %>%
  summarise(Total = sum(Cantidad))

# Colores
Colores <- c("#7149C6", "#FC2947","#FE6244")

# Definir colores
Colores <- c("Violencia de g�nero" = "#f2904c",
             "Violencia familiar en curso" = "#ec6489",
             "Violencia familiar hist�rica" = "#6e3169")

# Gr�fico
grafico <- ggplot(Data, aes(x=Accion, y=Cantidad, fill=Tipo)) +
  geom_col(position="dodge") +
  geom_text(aes(label=formatC(Cantidad, big.mark=".", decimal.mark=","), group=Tipo), position=position_dodge(width=0.9), vjust=-0.5, size=7, family="font", color="black") +
  annotate(geom="text", x=1:3, y=32000, label=formatC(Totales$Total, big.mark=".", decimal.mark=","), size=15, family="font", fontface="bold") +
  labs(title="",
       x="Requerimiento", y="Cantidad") +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark=",", format="d"),
                     limits = c(0, round(max(Data$Cantidad) * 1.5, -3))) +
  scale_fill_manual(name = str_wrap("Situaci�n motivo del requerimiento", width=20),
                    labels = function(x) str_wrap(x, width = 20), values=Colores) +
  theme_light() +
  theme(text=element_text(family="font"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font"),
        legend.text = element_text(size=12, family="font"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid = element_blank(),
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