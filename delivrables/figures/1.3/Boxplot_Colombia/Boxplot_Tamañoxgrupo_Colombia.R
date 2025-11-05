
library(readxl)
library(tidyverse)
library(ggplot2)

# === 1) LECTURA DEL ARCHIVO ===
# Cambia 'ruta/del/archivo.xlsx' y 'NombreHoja' por los tuyos
datos <- read_excel("Data_Colombia.xlsx", sheet = "COLOMBIA")

# === 2) MAPEO DE FACTORES ===
factor_map <- tibble(
  Variable = c("A.E","I.N","E.S","R.P","R.S",
               "I.E.C","H.C","M.I","S.M","C.K","LEA",
               "I.P","E.P.P","S.T","D.M","T.A","I.P.M","CIRC","C.I","CREA",
               "MARK","F.M","I.P2","C.I3","R.D","T.S","S.P.S.P"),
  Factor = c(rep("External Factors",5),
             rep("Team",6),
             rep("Innovation Process",9),
             rep("Organization",7))
)

# Paleta pastel para las 4 categorías
color_map <- c(
  "External Factors"   = "#FF9999",  # rojo pastel
  "Team"               = "#99CCFF",  # azul pastel
  "Innovation Process" = "#99FF99",  # verde pastel
  "Organization"       = "#C9A0DC"   # lila pastel
)

# === 3) TRANSFORMAR A FORMATO LARGO Y ORDENAR FACTORES ===
datos_long <- datos %>%
  pivot_longer(
    cols = -c(Company, Size),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  left_join(factor_map, by = "Variable") %>%
  mutate(
    # orden de tamaños y categorías solicitado
    Size   = factor(Size, levels = c("Large","Medium","Small","Micro")),
    Factor = factor(Factor, levels = c("External Factors","Team","Innovation Process","Organization"))
  )

# === 4) BOXPLOT COMBINADO (Categoría x Tamaño) ===
ggplot(datos_long, aes(x = Factor, y = Valor, fill = Factor)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.9) +                              # sin outliers
  stat_summary(fun = mean, geom = "point", shape = 4, size = 1,                # X pequeña = promedio
               color = "#4B4B4B", stroke = 1.2) +
  scale_fill_manual(values = color_map, name = "Category") +
  facet_wrap(~ Size, nrow = 0.5) +                                               # un panel por tamaño
  labs(
    title = "Analysis by size and factor categories - Colombia",
    x = "",
    y = "Value (1-5)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),                     # título centrado
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, color = "white")                          # ligera inclinación para legibilidad
  )

# (Opcional) Guardar
ggsave("boxplot_categoria_por_tamano_Colombia.png", width = 12, height = 5, dpi = 300)

