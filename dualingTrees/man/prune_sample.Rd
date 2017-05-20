\name{prune_sample}
\alias{prune_sample}
\title{prune_sample}
\usage{
prune_sample(vect, phylo)
}
\arguments{
\item{vect}{a vector of strings corresponding to the tips to keep in \code{phylo}}
\item{phylo}{The tree that will be trimmed according to \code{vect}}
}

\description{
Prunes tips and other baggage from a phylo object based on a vector of strings that correspond to the tips of the tree to be kept. The trimming vector should be a subset of the tips in the tree.

This is a modified copy of the \code{prune.sample()} function in the \code{picante} package. The only difference is that this function accepts a vector instead of a community matrix.

}
\examples{
\dontrun{
phy <- ape::rtree(5, tip.label = letters[1:5])
phy_trim <- prune_sample(phy, letters[1:4])
}
}
