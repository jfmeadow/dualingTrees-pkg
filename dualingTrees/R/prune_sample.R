## Modified prune.sample function from picante
## This version takes vector instead of comm matrix
prune_sample <- function (vect, phylo) {
  treeTaxa <- phylo$tip.label
  sampleTaxa <- vect
  trimTaxa <- dplyr::setdiff(treeTaxa, sampleTaxa)
  if (length(trimTaxa) > 0)
    ape::drop.tip(phylo, trimTaxa)
  else phylo
}
