
library(readxl)
library(tidyverse)
library(ggplot2)
library(here)


# === 1) LECTURA DEL ARCHIVO ===
# Cambia 'ruta/del/archivo.xlsx' y 'NombreHoja' por los tuyos
datos <- read_excel("delivrables/figures/1.3/Boxplot_Colombia/Data_Colombia.xlsx", sheet = "COLOMBIA")

# === 2) MAPEO DE FACTORES Y COLORES ===
factor_map <- tibble(
  Variable = c("A.E","I.N","E.S","R.P","R.S",
               "I.E.C","H.C","M.I","S.M","C.K","LEA",
               "I.P","E.P.P","S.T","D.M","T.A","I.P.M","CIRC","C.I","CREA",
               "MARK","F.M","I.P2","C.I3","R.D","T.S","S.P.S.P")
)

# === 3) AGREGAR PROMEDIO POR FILA (empresa) ===
datos2 <- datos %>%
  mutate(
    PromedioFila = rowMeans(across(all_of(factor_map$Variable)), na.rm = TRUE),
    Size = factor(Size, levels = c("Large","Medium","Small","Micro")) # orden explícito (ajústalo si quieres)
  )

View(datos)

# === 4) PALETA PASTEL POR TAMAÑO (para este gráfico) ===
size_colors <- c(
  "Large"  = "#99CCFF",  # azul pastel
  "Medium" = "#99CCFF",  # durazno pastel
  "Small"  = "#99CCFF",  # lila pastel
  "Micro"  = "#99CCFF"   # verde pastel
)


# === 5) BOXPLOT SOLO PARA EL PROMEDIO POR FILA, AGRUPADO POR TAMAÑO ===
ggplot(datos, aes(x = Size, y = PromedioFila, fill = Size)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.9) +                      # sin outliers (círculos)
  stat_summary(fun = mean, geom = "point", shape = 4, size = 2,        # X pequeña = promedio del grupo
               color = "#4B4B4B", stroke = 1.2) +
  scale_fill_manual(values = size_colors) +
  labs(
    title = "Global sustainable innovation per company size - Colombia",
    x = "Company size",
    y = "Gblobal average",
    fill = "Company size"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),              # título centrado
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

# (Opcional) guardar
ggsave("boxplot_global_Colombia.png", width = 8, height = 5, dpi = 300)


