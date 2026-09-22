library(bnlearn)

data("asia")


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("RBGL")
BiocManager::install("Rgraphviz")
BiocManager::install("gRain")

# Q1

dag1 <- hc(asia, score="bde", iss=1)
dag2 <- hc(asia, score="bde", iss=25)

all.equal(cpdag(dag1), cpdag(dag2))

# criterion 1: same adjacencies?
arcs(dag1)
arcs(dag2)


# criterion 2: same unshielded colliders?
vstructs(dag1)
vstructs(dag2)

# not equivalent!!!!


plot(dag1, main = "iss=1")
plot(dag2, main = "iss=25")


# Q2



