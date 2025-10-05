# 🧭 Clusters de Dependencia Demográfica  
**Un estudio de análisis de clustering en R para identificar distritos con mayor dependencia demográfica en el Perú**

---

## 📘 Descripción general  

Este proyecto aplica técnicas de **aprendizaje no supervisado (clustering)** mediante el algoritmo **K-Means** para identificar **agrupaciones de distritos según su nivel de dependencia demográfica**.  
El objetivo es **detectar patrones territoriales** relacionados con la estructura etaria y la carga demográfica, para apoyar el **análisis poblacional y la planificación territorial** en el marco de las políticas públicas de población y desarrollo.

El análisis se desarrolló íntegramente en **R**, utilizando los paquetes del ecosistema **tidyverse**, **factoextra**, **cluster** y **sf** para la manipulación, modelado y visualización espacial de los datos.

---

## 🎯 Objetivos del estudio  

- Identificar **grupos homogéneos de distritos** según sus indicadores de dependencia demográfica (infantil, adultos mayores y total).  
- Explorar la **distribución territorial de la dependencia demográfica** y su relación con variables socioeconómicas complementarias.  
- Generar **mapas y visualizaciones** que faciliten la interpretación de los patrones territoriales y la toma de decisiones basadas en evidencia.  

---

## 🧩 Metodología  

### 1. Preparación de datos  
- Fuentes: **Censos Nacionales de Población y Vivienda (INEI)** y estimaciones demográficas oficiales.  
- Procesamiento y limpieza de datos mediante el paquete **dplyr**.  
- Cálculo de tasas de dependencia infantil, de adultos mayores y total.  
- Estandarización de variables para asegurar comparabilidad entre distritos.  

### 2. Análisis de Clustering  
- Determinación del número óptimo de clusters mediante el **método del codo (Elbow Method)** y el **índice de silueta (Silhouette Score)**.  
- Aplicación del algoritmo **K-Means** con múltiples iteraciones para optimizar la estabilidad de los resultados.  
- Evaluación de la homogeneidad interna y separación entre grupos.  

### 3. Visualización y análisis  
- Gráficos de dispersión y centroides con **factoextra**.  
- Mapas temáticos de los clusters por distrito utilizando **sf** y **ggplot2**.  
- Interpretación de los grupos y caracterización de los perfiles territoriales.  

---

## 🧮 Paquetes utilizados  

```r
library(tidyverse)
library(cluster)
library(factoextra)
library(sf)
library(ggplot2)
library(scales)
library(readr)
