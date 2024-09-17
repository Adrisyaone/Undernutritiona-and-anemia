# mapping in R
require(rgdal)
library("sf")
library(sp)
library(ggplot2)
library(dplyr)
library("ggspatial")
library(raster)


# District wise Map files------------


# Mesh construction
library(INLA)

coo <- cbind(cluster$LONGNUM, cluster$LATNUM)
mesh <- inla.mesh.2d(
  loc = coo, max.edge = c(0.1, 5),
  cutoff = 0.01
)

# plot mesh and points
plot(mesh)
points(coo, col="Red")


# Building SPDE model on mesh
spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)


# index sets
indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)


# projection matrix
A <- inla.spde.make.A(mesh = mesh, loc = coo)




# model formula
formula<-y~0+b0+
