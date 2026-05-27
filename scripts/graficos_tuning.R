# Grafico 1
library(factoextra)
library(ggplot2)
library(dplyr)

set.seed(2026)

fviz_cluster(
  dependencia_clust,
  dependencia_tidy |>
    select(TDD_JOVE, TDD_VEJEZ, RA_POTENCIAL, RA_PADRES),
  
  # Mostrar puntos
  geom = "point",
  
  # Colores de clusters
  palette = c("#B30000", "#4D4D4D", "#808080", "#1A1A1A", "#D9D9D9"),
  
  # Transparencia
  alpha = 0.8,
  
  # Tamaño de puntos
  pointsize = 2.5,
  
  # Elipses
  ellipse.type = "norm",
  ellipse.alpha = 0.15,
  ellipse.level = 0.95,
  
  # Sin etiquetas
  labelsize = 0,
  
  # Tema minimalista
  ggtheme = theme_minimal()
  
) +
  
  labs(
    title = "Clústeres de dependencia demográfica",
    subtitle = "Método K-means clustering",
    x = "Dimensión 1",
    y = "Dimensión 2"
  ) +
  
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 11
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    legend.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
  )
```