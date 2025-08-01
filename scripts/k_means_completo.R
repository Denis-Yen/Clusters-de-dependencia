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
         TASA_BONO
  ) %>% 
  mutate(
    across(where(is.numeric), ~ as.numeric(scale(.)))
  )



# 4. IMPLEMENTAR LA AGRUPACIÓN EN CLUSTERS --------------------------------
dependencia_clust <- kmeans(select(dependencia_tidy, -c("UBIGEO", "REGION", "PROVINCIA", "DISTRITO")),
                            centers = 5
)
summary(dependencia_clust)

# Encontrar los centros de los clusters
tidy(dependencia_clust)

# Representar las dimensiones
augment(dependencia_clust, dependencia_tidy) %>% 
  ggplot(aes(TDD_VEJEZ, TASA_BONO, color = .cluster)) +
  geom_point()

fviz_cluster(dependencia_clust, dependencia_tidy |> select(TDD_JOVE, TDD_VEJEZ, TASA_BONO))
fviz_nbclust(dependencia_tidy |> select(TDD_JOVE, TDD_VEJEZ, TASA_BONO), kmeans, method = "gap_stat")
fviz_nbclust(dependencia_tidy |> select(TDD_JOVE, TDD_VEJEZ, TASA_BONO), kmeans, method = "wss")

# 4.1 ELEGIR EL NÚMERO DE CLUSTERS ----------------------------------------

kclusts <- 
  tibble(k = 1:10) %>% 
  mutate(
    kclust = map(k, ~kmeans(select(dependencia_tidy, -c("UBIGEO", "REGION", "PROVINCIA", "DISTRITO")), .x)),
    glanced = map(kclust, glance)
  )

kclusts %>% 
  unnest(cols = c(glanced)) %>% 
  ggplot(aes(k, tot.withinss))+
  geom_line(alpha = 0.5, size = 1.2, color = "midnightblue") +
  geom_point(size = 2, color = "midnightblue")



# 4.2 AGREGAR LOS CLUSTER AL DATASET --------------------------------------
dependencia_cluster <- dependencia_tidy %>%
  select(UBIGEO) %>%
  mutate(cluster = dependencia_clust$cluster)

# Asignar nombres a los clusters
dependencia_cluster <- dependencia_cluster %>%
  mutate(cluster_nombre = case_when(
    cluster == 1 ~ "Depedencia juvenil moderada",
    cluster == 2 ~ "Alta Dependencia por envejecimiento",
    cluster == 3 ~ "Bono demográfico activo",
    cluster == 4 ~ "Transición al envejecimiento",
    cluster == 5 ~ "Alta Depedencia por juventud"
  ))

# Agregar al dataset original

dependencia_tidy_clust <- dependencia%>% left_join(
  dependencia_cluster, by = "UBIGEO"
) 

dependencia_tidy_clust %>% 
  group_by(cluster_nombre) %>% count()



dependencia_tidy_clust%>% write.csv("salidas/depedencia_cluster.csv")






