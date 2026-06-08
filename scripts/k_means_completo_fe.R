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
             palette = c("#6b2328", "#de6a73", "#f5d2d5", "#e68f96", "#4b1217"),
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

# 6. MAPA DE CLUSTERS -----------------------------------------------------
library(sf)
shp_peru <- read_sf("data/Peru_Distrital.geojson")
clus_peru <- dependencia_tidy_clust_fe

clus_shp_peru <- shp_peru |> left_join(clus_peru,by = c("UBIGEO"="UBIGEO"))

paleta_clusters_info <- c(
  "1" = "#6b2328",
  "2" = "#e68f96",
  "3" = "#f5d2d5",
  "4" = "#de6a73",
  "5" = "#4b1217"
)


clus_shp_peru |> ggplot() +
  geom_sf(aes(fill = factor(CLUSTER)),
          color = "white",
          linewidth = 0.01
          ) +
  scale_fill_manual(
            values = paleta_clusters_info,
            name = "Cluster") +
  labs(
    title = stringr::str_trim("Perú: Clusters de Dependencia Demográfica"),
    subtitle = "(n = 1,891)",
    caption = "Fuente: MINSA. Repositorio Único Nacional de Información en Salud (REUNIS)*.\nElaboración: Denis Rodríguez (www.denis-rodriguez.com)")+
  guides(fill=guide_legend(
    direction = "vertical",
    keyheight = unit(4, "mm"),
    keywidth = unit(6, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    reverse = F,
    label.position = "right"
  ))+ 
  # geom_text(
  # data = cents |> filter(Tasa_incidencia >= 240), aes(coords.x1, coords.x2, label = paste0(NOMBPROV, "\n", round(Tasa_incidencia, 1))
  # ),
  # size = 3,
  # fontface = "bold",
  # color = "#000000",
  # family = "georg") +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    legend.position = "right",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=22, color='#05204d', hjust=0.3, vjust=1, face = "bold"),
    plot.subtitle = element_text(size=18, color='#4b1217', hjust=0.5, vjust=-1, face = "bold"),
    plot.caption = element_text(size=11, color="grey60", hjust=0.0, vjust=-1, lineheight = 0.8),
    axis.title.x = element_text(size=18, color="grey20", hjust=0.5, vjust=-6),
    legend.text = element_text(size=12, color="grey20"),
    legend.title = element_text(size=12, color="grey20"),
    strip.text = element_text(size=12),
    plot.margin = unit(c(t=1, r=-2, b=1, l=-2),"lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  labs(x ="") -> map_nive_dist_clust
 
ggsave(plot = map_nive_dist_clust, "imagenes/map_nive_dist_clus.png", width = 2500, height = 2000, units = "px")


dependencia_tidy_clust_fe%>% write.xlsx("salidas/depedencia_cluster_fe.xlsx")





