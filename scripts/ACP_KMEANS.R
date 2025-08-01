# 1. CARGA DE LIBRERIAS ---------------------------------------------------
library(tidyverse)
library(openxlsx)
library(tidymodels)
library(broom)
library(factoextra)
library(deckgl)
library(funModeling)
library(tidytext)
library(psych)



# 2. CARGA DE DATOS -------------------------------------------------------
dependencia <- read.xlsx("data/03.Tasa_Dependencia.xlsx")
dependencia %>% df_status()


dependencia_tidy <- dependencia %>% 
  select(UBIGEO,
         REGION,
         PROVINCIA,
         DISTRITO,
         TDD_JOVE,
         TDD_VEJEZ,
         IND_ENVEJ,
         RA_POTENCIAL,
         RA_PADRES,
         TASA_BONO
  ) %>% 
  mutate(
    across(where(is.numeric), ~ (. - min(.)) / (max(.) - min(.)))
  )

KMO(select(dependencia_tidy,-c("UBIGEO", "PROVINCIA", "REGION", "DISTRITO")))

# 3. ANÁLISIS DE COMPONENTES PRINCIPALES ----------------------------------

pca_rot <- principal(
  select(dependencia_tidy,-c("UBIGEO", "PROVINCIA", "REGION", "DISTRITO")),
  nfactors = 2,
  rotate = "varimax",
  method = "principal")

print(pca_rot, digits = 2, sort = TRUE)

# extraer los scores
scores_df <- as.data.frame(pca_rot$scores)


# 4. IMPLEMENTAR LA AGRUPACIÓN EN CLUSTERS --------------------------------
set.seed(123)
clust_kmeans <- kmeans(scores_df, centers = 3)

# Encontrar los centros de los clusters
tidy(clust_kmeans)

# Unir clusters con la infromación geográfica

dependencia_cluster <- dependencia_tidy %>%
  select(UBIGEO, REGION, PROVINCIA, DISTRITO) %>%
  bind_cols(scores_df) %>%
  mutate(cluster = clust_kmeans$cluster)

# Centorides de cada cluster
library(broom)
tidy(clust_kmeans)

# VIsualizar
ggplot(dependencia_cluster, aes(x = RC1, y = RC2, color = as.factor(cluster))) +
  geom_point(size = 2) +
  labs(title = "Clústeres sobre factores rotados (PCA Varimax)",
       x = "Factor 1 (RC1)", y = "Factor 2 (RC2)", color = "Cluster") +
  theme_minimal()


dependencia_cluster %>%
  count(REGION, cluster) %>%
  tidyr::pivot_wider(names_from = cluster, values_from = n, values_fill = 0)






# numero oprtio de clustrs 
set.seed(123)
wss <- map_dbl(1:10, function(k) {
  kmeans(scores_df, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- tibble(k = 1:10, wss = wss)

 
ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(title = "Método del Codo", x = "Número de clusters (k)", y = "Suma total de cuadrados intra-cluster") +
  theme_minimal()


# recueprr las varibale originales

dependencia_tidy_final <- dependencia_tidy %>%
  left_join(
    dependencia_cluster %>% select(UBIGEO, RC1, RC2, cluster),
    by = "UBIGEO"
  ) 











