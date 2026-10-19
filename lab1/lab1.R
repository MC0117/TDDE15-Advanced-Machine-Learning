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


classify <- function(fitted, evidence_vars){
  g <- compile(as.grain(fitted))
  m <- sapply(test[, evidence_vars], as.character)
  sapply(1:nrow(test), function(i)
    names(which.max(querygrain(setEvidence(g, evidence_vars, m[i,]), nodes="S")$S)))
}

classify <- function(fitted, evidence_vars){
  g <- compile(as.grain(fitted))
  m <- sapply(test[, evidence_vars], as.character)
  sapply(1:nrow(test), function(i)
    names(which.max(querygrain(setEvidence(g, evidence_vars, m[i,]), nodes="S")$S)))
}

vars <- setdiff(names(asia), "S")

# Q2: learned structure, all evidence
dag       <- hc(train, score="bde", iss=5)
fit_hc    <- bn.fit(dag, train)
pred_hc   <- classify(fit_hc, vars)

# Q2b: true structure
true_dag  <- model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")
fit_true  <- bn.fit(true_dag, train)
pred_true <- classify(fit_true, vars)

# Q3: learned structure, Markov blanket only
# mb_vars   <- mb(dag, "S")
pred_mb   <- classify(fit_hc, mb_vars)

# Q4: naive Bayes
nb_dag    <- model2network("[S][A|S][T|S][L|S][B|S][E|S][X|S][D|S]")
fit_nb    <- bn.fit(nb_dag, train)
pred_nb   <- classify(fit_nb, vars)

for(p in list(hc=pred_hc, true=pred_true, mb=pred_mb, nb=pred_nb)) print(table(p, test$S))

c(hc   = mean(pred_hc   == test$S),
  true = mean(pred_true == test$S),
  mb   = mean(pred_mb   == test$S),
  nb   = mean(pred_nb   == test$S))

