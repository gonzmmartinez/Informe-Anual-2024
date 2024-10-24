# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(readxl)

# Función
`%nin%` <- Negate(`%in%`)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Cargar datos
Data <- read_excel(path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Femicidios 2014-2024 - modificado.xlsx"),
                   sheet="En filas") %>%
  mutate(Provincia = factor(Provincia,
                            levels = c("Salta", "CABA", "Buenos Aires", "Catamarca", "Chaco", "Chubut", "Córdoba",
                                       "Corrientes", "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja",
                                       "Mendoza", "Misiones", "Neuquén", "Río Negro", "San Juan", "San Luis",
                                       "Santa Cruz", "Santa Fe", "Santiago del Estero", "Tierra del Fuego", "Tucumán", "Totales"))) %>%
  filter(Provincia %in% c("Salta", "Tucumán", "Entre Ríos", "Misiones", "Corrientes", "Santiago del Estero", "Totales"))

# Datos de referencia para cada gráfico individual
every_facet_data = subset(Data, Provincia == "Totales")

# Datos individuales para cada faceta
individual_facet_data = subset(Data, Provincia != "Totales")
individual_facet_data$facet = individual_facet_data$Provincia

# Combina los datos de referencia con los datos individuales
every_facet_data = merge(every_facet_data, 
                         data.frame(Provincia = "Totales", 
                                    facet = unique(individual_facet_data$facet)))

plot_data = rbind(every_facet_data %>% mutate(Total = "Tasa nacional"),
                  individual_facet_data %>% mutate(Total = "Tasa provincial")) %>%
  mutate(Salta = Provincia == "Salta")

# Gráfico
grafico <- ggplot(plot_data, aes(x = Año, y = Tasa, alpha = Total, color= Salta, linetype=Total)) +
  geom_line(linewidth=1.5, lineend = "round") +
  facet_wrap(~ facet, ncol=3) +
  theme_light() +
  scale_alpha_manual(name="Referencia", values=c(0.3, 1)) +
  scale_color_manual(values=c("black", "#ec6489"), guide="none") +
  scale_linetype_manual(values=c(2,1), guide="none") +
  labs(x = "Año", y = "Tasa de femicidios",
       caption = str_wrap("La cantidad de mujeres por provincia resulta de una proyección de población realiaza por el INDEC",70)) +
  theme(text = element_text(size=15, family="font"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_text(size=20, family="font", margin=margin(t=15,r=0,b=0,l=0)),
        axis.title.y = element_text(size=20, family="font", margin=margin(t=0,r=15,b=0,l=0)),
        panel.grid = element_line(colour = "#F5F5F5"),
        legend.position = "top",
        legend.justification = "right",
        legend.text = element_text(size=15, family="font"))

# Dirección
Dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Graficos/Lineas_provincias_facet.pdf")

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=8)
ggsave(filename = paste0(filename, ".svg"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=12, height=8)