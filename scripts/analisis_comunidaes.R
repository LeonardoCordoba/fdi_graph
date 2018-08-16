rm(list = ls())
library(igraph)
library(dplyr)
df_promedio_path <- "df_promedio.csv"
df_promedio <- read.csv(df_promedio_path)
paises_path <- "df_promedio.csv"
paises <- read.csv(paises_path)
paises <- unlist(paises)

# Número y modularidad de comunidades por rango -----------------------------------------
#rango_completo <- c(10, 16, 22, 28, 34, 40, 46, 52, 58, 64, 70, 76, 82, 88, 94, 100, 106, 112, 118, 124, 130, 136, 142, 148, 154, 160, 166, 172, 178, 184, 190, 196, 202, 208, 214, 220, 226, 232, 238)
rango_completo <- c(94)
#cluster_sg_mod <- c()
n_comunidades <- c()
rango <- c()

#df_promedio <- df_promedio %>% filter(Reporting.country %in% paises & Partner.country %in% paises)
# Comunidades solo esta implementado para graficos no dirigidos

for (r in rango_completo) {
  print(r)
  df_i <- df_promedio %>% filter(Value > r)
  grafo <- graph_from_data_frame(df_i[,c(1,2)], directed = T)
  cluster_sg <- cluster_spinglass(grafo, weights = NULL) 
#  cluster_sg_mod <- c(cluster_sg_mod, modularity(grafo, cluster_sg$membership))
  n_comunidades <- c(n_comunidades,length(unique(cluster_sg$membership)))
  rango <- c(rango,r)
}  

resultados <- data.frame(rango = rango, n_comunidades = n_comunidades)

library(ggplot2)

#resultados %>% ggplot(aes(y = modularidad, x = rango)) + geom_point()
#resultados %>% ggplot(aes(y = n_comunidades, x = rango)) + geom_point()

# Estadísticas  --------
# Se analiza la cantidad de veces que el grafo recableado da una modularidad menor y una
# cantidad de comunidades menor

rango_completo <- c(10, 16, 22, 28, 34, 40, 46, 52, 58, 64, 70, 76, 82, 88, 94, 100, 106, 112, 118, 124, 130, 136, 142, 148, 154, 160, 166, 172, 178, 184, 190, 196, 202, 208, 214, 220, 226, 232, 238)
cluster_sg_mod <- c()
n_comunidades <- c()
rango <- c()
mod_azar_mnr_real <- c()
nc_azar_mnr_real <- c()

df_promedio <- df_promedio %>% filter(Reporting.country %in% paises & Partner.country %in% paises)

  for (r in 10) {
    df_i <- df_promedio %>% filter(Value > r)
    grafo <- graph_from_data_frame(df_i[,c(1,2)], directed = T)
    cluster_sg <- cluster_spinglass(grafo, weights = NULL) 
    cluster_sg_mod <- c(cluster_sg_mod, modularity(grafo, cluster_sg$membership))
    n_comunidades <- c(n_comunidades,length(unique(cluster_sg$membership)))
    rango <- c(rango,r)
    
    mod_azar <- vector(mode="numeric", length=10)
    n_comunidades_azar <- vector(mode="numeric", length=10)

    for(i in 1:10){
    
      recableado <- rewire(grafo, with = keeping_degseq(loops = F, niter = 71))
      mod_azar[i] <- modularity(recableado, cluster_sg$membership)
      inicio <- Sys.time()
      cluster_sg_azar <- cluster_spinglass(recableado, weights = NULL) 
      n_comunidades_azar[i] <- length(unique(cluster_sg_azar$membership))
    }
    
    table_mod <- sum((mod_azar < cluster_sg_mod)==TRUE) 
    table_nc <- sum((n_comunidades_azar < n_comunidades)==TRUE)
    
    mod_azar_mnr_real <- c(mod_azar_mnr_real,table_mod)
    nc_azar_mnr_real <- c(nc_azar_mnr_real,table_nc)
}



compare(clust_2003$membership, clust_2004$membership)


grafo1 <- graph_from_data_frame(prueba[,c(1,2)], directed = T)


prueba_infomap <- infomap.community(grafo1)

# walktrap y edge_betweenness generan demasiados grupos
walktrap <- cluster_walktrap(grafo1)
edge_bet <- cluster_edge_betweenness(grafo1)
# No termina mas
#optimal <- cluster_optimal(grafo1)

#infomap genera un solo grupo

# label prop genera una cantidad acorde de grupos pero demasiado concentrados en uno solo
label_prop <- cluster_label_prop(grafo1)

# por eso, se elige spinglass
spinglass <- cluster_spinglass(grafo1)

compare(clust_2003$membership, clust_2004$membership)

table(label_prop$membership)

prueba = data.frame(pais = spinglass$names, grupo = spinglass$membership)
prueba

prueba <- prueba %>% mutate(grupo_2 = grupo * 2)

  prueba_infomap$membership

gr <- get.adjacency(grafo1, sparse = T)

heatmap(gr)



