# 1. CARGA DE LIBRERIAS ---------------------------------------------------
library(tidyverse)
library(openxlsx)
library(tidymodels)
library(broom)
library(deckgl)


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
         RA_PADRES,
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
  ggplot(aes(IND_ENVEJ, RA_PADRES, color = .cluster)) +
  geom_point()

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
