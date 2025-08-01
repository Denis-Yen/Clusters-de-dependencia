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


fviz_cluster(k_means, data = dependencia_tidy |> select(TDD_JOVE, TDD_VEJEZ, RA_POTENCIAL, RA_PADRES))

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



dependencia_tidy_clust_fe%>% write.xlsx("salidas/depedencia_cluster_fe.xlsx")






