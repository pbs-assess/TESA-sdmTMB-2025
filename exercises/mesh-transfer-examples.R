library(sdmTMB)

mesh0 <- make_mesh(pcod, c("X", "Y"), cutoff = 10)
mesh <- make_mesh(pcod, c("X", "Y"), mesh = mesh0$mesh)

mesh0 <- make_mesh(qcs_grid, c("X", "Y"), cutoff = 10)
mesh <- make_mesh(pcod, c("X", "Y"), mesh = mesh0$mesh)

fmesher_mesh <- fmesher::fm_mesh_2d_inla(
  loc = cbind(pcod$X, pcod$Y), # coordinates
  max.edge = c(25, 50), # max triangle edge length; inner and outer meshes
  offset = c(5, 25),  # inner and outer border widths
  cutoff = 5 # minimum triangle edge length
)
mesh <- make_mesh(dat, c("X", "Y"), mesh = fmesher_mesh)


