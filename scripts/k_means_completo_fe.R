# 1. CARGA DE LIBRERIAS ---------------------------------------------------
library(tidyverse)
library(openxlsx)
library(tidymodels)
library(broom)
library(deckgl)
library(factoextra)


# 2. CARGA DE DATOS -------------------------------------------------------
dependencia <- read.xlsx("data/03.Tasa_Dependencia.xlsx")



# 3. EXPLORACIÓN DE DATOS -------------------------------------------------

dependencia %>%
  group_by(REGION, PROVINCIA) %>% 
  count() %>% arrange((n))

dependencia_tidy <- dependencia %>% 
  select(UBIGEO,
         REGION,
         PROVINCIA,
         DISTRITO,
         TDD_JOVE,
         TDD_VEJEZ,
         RA_POTENCIAL,
         RA_PADRES
  ) %>% 
  mutate(
    across(where(is.numeric), ~ as.numeric(scale(.)))
  )

dependencia_clust <- kmeans(select(dependencia_tidy, -c("UBIGEO", "REGION", "PROVINCIA", "DISTRITO")),
                            centers = 5
                            )
summary(dependencia_clust)

# 4. ELEGIR EL NÚMERO DE CLUSTERS -----------------------------------------

fviz_cluster(dependencia_clust, dependencia_tidy |> select(TDD_JOVE, TDD_VEJEZ, RA_POTENCIAL, RA_PADRES))
fviz_nbclust(dependencia_tidy |> select(TDD_JOVE, TDD_VEJEZ, RA_POTENCIAL, RA_PADRES), kmeans, method = "wss")
fviz_nbclust(dependencia_tidy |> select(TDD_JOVE, TDD_VEJEZ, RA_POTENCIAL, RA_PADRES), kmeans, method = "gap_stat")
# k = 3
# 4.1 IMPLEMENTAR LA AGRUPACIÓN EN CLUSTERS --------------------------------
  
set.seed(123)
k_means <- kmeans(dependencia_tidy |> select(TDD_JOVE, TDD_VEJEZ, RA_POTENCIAL, RA_PADRES),
                  centers = 5,  
                  nstart  = 20,
                  )


# 4.2 Visualizar los clusters
fviz_cluster(k_means,
             data = dependencia_tidy |> select(TDD_JOVE, TDD_VEJEZ, RA_POTENCIAL, RA_PADRES),
             geom = "point",
             palette = c("#C1121F", "#8D99AE", "#000000", "#B1A7A6", "#D90429"),
             ggtheme = theme_minimal(),
             repel = TRUE
             ) +
  labs(
    title = "Perú: Clusters de Dependencia Demográfica, 2025",
    subtitle = "Segementación de distritos según indicadores de dependencia",
    caption = "Elaboración: Dirección de Población - MIMP"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#333333"),
    plot.subtitle = element_text(size = 12, color = "#666666"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "#444444"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 9, color = "#666666", face = "italic", hjust = 0)
    
  )

# 4.2 AGREGAR LOS CLUSTER AL DATASET --------------------------------------
dependencia_cluster_fe <- dependencia_tidy %>%
  select(UBIGEO) %>%
  mutate(CLUSTER = k_means$cluster)

# Asignar nombres a los clusters
dependencia_cluster_fe <- dependencia_cluster_fe %>%
  mutate(CLUSTER_NAME = case_when(
    CLUSTER == 1 ~ "Moderada carga por vejez",
    CLUSTER == 2 ~ "Moderada carga por juventud, con poblacion activa",
    CLUSTER == 3 ~ "Dependencia moderada, equilibrado",
    CLUSTER == 4 ~ "Muy alta carga por juventud, fuerte poblacion activa",
    CLUSTER == 5 ~ "Muy alta vejez con débil soporte"
  ))

# Agregar al dataset original

dependencia_tidy_clust_fe <- dependencia%>% left_join(
  dependencia_cluster_fe, by = "UBIGEO"
) 

dependencia_tidy_clust_fe %>% 
  select(UBIGEO, PROVINCIA, DISTRITO, TDD_JOVE, TDD_VEJEZ, RA_POTENCIAL, RA_PADRES, CLUSTER_NAME) |> 
  group_by(CLUSTER_NAME) %>% summarise(
    CANTID = n_distinct(UBIGEO),
    TD_JOV = mean(TDD_JOVE),
    TD_VEJ = mean(TDD_VEJEZ),
    TD_POT = mean(RA_POTENCIAL),
    TD_PAD = mean(RA_PADRES),
  ) 


# 5. VISUALIZACIÓN 3D CON PLOTLY -----------------------------------------
library(plotly)

# Crear un data frame con las variables principales y los clusters
dependencia_plot <- dependencia_tidy_clust_fe %>% 
  mutate(CLUSTER_NAME = factor(CLUSTER_NAME))

# Gráfico 3D interactivo
fig <- plot_ly(
  data = dependencia_plot,
  x = ~TDD_JOVE,
  y = ~TDD_VEJEZ,
  z = ~RA_PADRES,
  color = ~CLUSTER_NAME,
  colors = c("#FF595E", "#FFCA3A", "#8AC926", "#1982C4", "#6A4C93"),
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4, opacity = 0.8)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Dependencia Juvenil"),
      yaxis = list(title = "Dependencia de Vejez"),
      zaxis = list(title = "Relación de apoyo a los padres")
    ),
    title = "Clústeres de Dependencia Demográfica - Gráfico 3D"
  )

fig





dependencia_tidy_clust_fe%>% write.xlsx("salidas/depedencia_cluster_fe.xlsx")






