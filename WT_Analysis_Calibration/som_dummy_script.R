library(transformeR)

load("pca_matrix.RData")

som_grid <- somgrid(xdim = 5, ydim = 1 , topo = "rectangular")

som_model <- som(pca.matrix, grid = som_grid)

save(som_model,file = "som.RData")

q("no")


##############################
k <- 5
size <- c()
for (i in c(1:k)) {
  size <- c(size,length(which(som_model$unit.classif == i)))
}

load("C:/Users/usuario/Desktop/TRMM-Calibration/Data/pp_reanalysis.RData")

lista <- lapply(1:k, function(x) subsetDimension(pp_reanalysis, dimension = "time", indices = which(som_model$unit.classif == x)) %>% climatology())

kmedias.pp <- bindGrid(lista, dimension = "member", skip.temporal.check = TRUE)

spatialPlot(kmedias.pp,backdrop.theme = 'countries', rev.color = T, as.table = T, names.attr = paste("Days in cluster", 1:k, ":",size[1:k]))