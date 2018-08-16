rm(list = ls())
library(igraph)
library(dplyr)
library(RColorBrewer)

df_2003 <- read.csv("df_dirigido_2003.csv")
df_2004 <- read.csv("df_dirigido_2004.csv")
df_2005 <- read.csv("df_dirigido_2005.csv")
df_2006 <- read.csv("df_dirigido_2006.csv")
df_2007 <- read.csv("df_dirigido_2007.csv")
df_2008 <- read.csv("df_dirigido_2008.csv")
df_2009 <- read.csv("df_dirigido_2009.csv")
df_2010 <- read.csv("df_dirigido_2010.csv")
df_2011 <- read.csv("df_dirigido_2011.csv")
df_2012 <- read.csv("df_dirigido_2012.csv")
df_2013 <- read.csv("df_dirigido_2013.csv")

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

paises_path <- "paises.csv"
paises <- read.csv(paises_path, header = F)
paises <- unlist(paises)
paises <- paises[1:29]
length(paises)
df_list = list(df_2003, df_2004, df_2005, df_2006, df_2007, df_2008, df_2009, df_2010, df_2011, df_2012, df_2013)

## Falta filtar los pa[ises]
plot_grafo <- function(df){
  
  df <- df %>% filter((Reporting.country %in% paises) & (Partner.country %in% paises))

  rango_completo <- 94
  #cluster_sg_mod <- c()
  n_comunidades <- c()
  
  #df_promedio <- df_promedio %>% filter(Reporting.country %in% paises & Partner.country %in% paises)
  # Comunidades solo esta implementado para graficos no dirigidos
  df_i <- df %>% filter(Value > rango_completo) 
  value_df <- df_i %>% group_by(Reporting.country) %>% summarise(Total_value = sum(Value)) %>% arrange(Reporting.country)
  #df_i <- df_i %>% mutate(weight = Value)
  grafo <- graph_from_data_frame(df_i[,c(2,3,4)], directed = T)
  cluster_sg <- cluster_spinglass(grafo, weights = ) 
  
  colors <- list("lightgreen","blue","red","brown")
  
  l1 <- layout_in_circle(grafo)
  l2 <- layout_nicely(grafo)
  l1 <- norm_coords(l1, ymin=-1, ymax=1, xmin=-1, xmax=1)
  l2 <- norm_coords(l2, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
#  deg <- degree(grafo, mode="all")

  coul = brewer.pal(length(unique(membership(cluster_sg))), "Set2") 
  my_color=coul[unname(membership(cluster_sg))]
  
  par(bg="grey20")
  
  lab.locs <- radian.rescale(x=1:vcount(grafo), direction=-1, start=0)
  
  plot(grafo, edge.arrow.size = log(df_i$Value)*0.0005, edge.width = df_i$Value*0.0001, vertex.label.font	= 2,
       edge.curved = -.3, edge.color = "grey", mark.groups = NULL, vertex.shape = "circle", vertex.label.color=my_color,
       layout = l1, vertex.size = log(value_df$Total_value)**2/13, vertex.label.dist = 1, vertex.color = "grey44",
       vertex.label.cex	= log(value_df$Total_value)**2/100, vertex.label.degree=lab.locs)
  
  plot(grafo, edge.arrow.size = log(df_i$Value)*0.0005, edge.width = df_i$Value*0.0001, vertex.label.font	= 2,
       edge.curved = -.3, edge.color = "grey", mark.groups = NULL, vertex.shape = "circle", vertex.label.color=my_color,
       layout = l2, vertex.size = log(value_df$Total_value)**2/13, vertex.label.dist = 1, vertex.color = "grey44",
       vertex.label.cex	= log(value_df$Total_value)**2/100, vertex.label.degree=lab.locs)
}

plot_grafo(df_2008)
paises

