
library(readxl)
library(tidyverse)

# === 1) LECTURA DEL ARCHIVO ===
# Cambia 'ruta/del/archivo.xlsx' y 'NombreHoja' por los tuyos
datos <- read_excel("Data_Colombia.xlsx", sheet = "COLOMBIA")

# === 2) MAPEO DE FACTORES Y COLORES ===
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

color_map <- c(
  "External Factors" = "#FF9999",     # Rojo pastel
  "Team" = "#99CCFF",                 # Azul pastel
  "Innovation Process" = "#99FF99",   # Verde pastel
  "Organization" = "#C9A0DC"          # Lila pastel
)

# === 3) TRANSFORMAR DATOS ===
datos_long <- datos %>%
  pivot_longer(
    cols = -c(Company, Size),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  left_join(factor_map, by = "Variable") %>%
  mutate(
    # Ordenar variables según el factor_map original
    Variable = factor(Variable, levels = factor_map$Variable),
    Factor = factor(Factor, levels = c("External Factors", "Team", "Innovation Process", "Organization"))
  )

# === 4) FUNCIÓN PARA GRAFICAR UN TAMAÑO ===
plot_por_tamano <- function(tamano) {
  datos_filtrados <- datos_long %>% filter(Size == tamano)
  
  # Boxplot con promedio marcado
  p <- ggplot(datos_filtrados, aes(x = Variable, y = Valor, fill = Factor)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.9) +
    # Promedio (más pequeño, color blanco)
    stat_summary(fun = mean, geom = "point", shape = 4, size = 2, color = "#4B4B4B", stroke = 1.2) +
    scale_fill_manual(values = color_map) +
    labs(
      title = paste("Assessment of factors - Colombia - ", tamano, "companies"),
      x = "Factors",
      y = "Value (1-5)",
      fill = "Category"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major.x = element_blank(),
      legend.box = "vertical"
    )

}

# === 5) EJEMPLOS DE USO ===
# Graficar solo "Large"
plot_por_tamano("Large")
plot_por_tamano("Medium")
plot_por_tamano("Small")
plot_por_tamano("Micro")

# Si quieres guardar las figuras:
ggsave("boxplot_Colombia_Large.png", plot = plot_por_tamano("Large"), width = 10, height = 6)
ggsave("boxplot_Colombia_Medium.png", plot = plot_por_tamano("Medium"), width = 10, height = 6)
ggsave("boxplot_Colombia_Small.png", plot = plot_por_tamano("Small"), width = 10, height = 6)
ggsave("boxplot_Colombia_Micro.png", plot = plot_por_tamano("Micro"), width = 10, height = 6)

