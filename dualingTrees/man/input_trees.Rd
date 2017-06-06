\name{input_trees}
\alias{input_trees}
\title{input_trees}

\description{
The mandatory precursor to the \code{plot_trees()} function. This function takes
each component of raw data for the visualization and combines them into a list that will be processed by the plotting function.


}

\usage{
input_trees(x_tree = NULL,
            y_tree = NULL,
            x_key = NULL,
            y_key = NULL,
            response = NULL,
            response_type = NULL,
            response_x_bars = NULL,
            response_y_bars = NULL,
            count = FALSE,
            x_lab_cutoff = NULL,
            y_lab_cutoff = NULL,
            x_node_labs = NULL,
            y_node_labs = NULL,
            bubble_scale = 5,
            bubble_transform = NULL,
            pn_color_cutoff = 0)

}

\arguments{

  \item{x_tree}{
  A phylo object (from the ape package) that will be plotted on the x-margin of the interaction matrix. }
  \item{y_tree}{
  A phylo object (from the ape package) that will be plotted on the y-margin of the interaction matrix. }
  \item{x_key}{
  A vector of names that perfectly match to some or all of the tip labels in the x_tree. If the tree has tips that are not represented in the matrix, they will be pruned away. These must correspond to the y_key names and the responses. }
  \item{y_key}{
  A vector of names that perfectly match to some or all of the tip labels in the y_tree. These must correspond to the x_key names and the responses. }
  \item{response}{
  A vector of names that perfectly match to some or all of the tip labels in the y_tree. These must correspond to the x_key names and the y_key names. The most obvious way to make sure these line up is to keep them all in the same data.frame where each row is an interaction with a x_key, a y_key, and a response. These can be pre-summarized by x/y combination (i.e., a single row corresponds to a single resulting bubble, and each x/y combination only occurs once) or they can be pre-summarized (each combination might occur multiple times when the combinations are replicated but the responses are unique). In the latter case, the summarization happens within this function automatically. }
  \item{response_type}{
  A categorical variable indicated for each response. For example, when using this package to visualize mycorrhizal data (such as the example datasets included in the package), these categories might be 'EM' and 'AM' for ectomycorrhizal and arbuscular mycorrhizal, respectively. If left NULL (default), then no types are colored and no legend is drawn for types. }
  \item{response_x_bars}{ Optionally supply a data.frame for margin bars. Note the visualization is designed to lead viewers to assume that bars summarize the bubbles. Use this with caution. Should be a single column vector of numbers, one corresponding to every tip in the respective tree. row.names of the data.frame should perfectly match the tree tip names, but can be in any order. }
  \item{response_y_bars}{ Optionally supply a data.frame for margin bars. Note the visualization is designed to lead viewers to assume that bars summarize the bubbles. Use this with caution. Should be a single column vector of numbers, one corresponding to every tip in the respective tree. row.names of the data.frame should perfectly match the tree tip names, but can be in any order. }
  \item{count}{
  Indicate whether you want the bubbles to represent the response or simply the number of replicates within each combination. If TRUE, response is ignored. }
  \item{x_lab_cutoff}{Not currently implemented. }
  \item{y_lab_cutoff}{
  Labels for the barplot (y-axis) above this cutoff are plotted. If a number too low is picked, the labels will be quite crowded. }
  \item{x_node_labs}{Not currently implemented. }
  \item{y_node_labs}{Optional string of names corresponding to the named internal nodes of the y-axis tree. These will be shown at the node location on the y-tree. }
  \item{bubble_scale}{Scaling for the bubble size. If bubbles are too overlappy, decrease this. }
  \item{bubble_transform}{Additional scaleing input. Chose between \code{'log'} or \code{'sqrt'}. Bubbles are log scaled by default, and using 'log' will apply a second log transformation. }
  \item{pn_color_cutoff}{Cutoff used for positive / negative coloring. Default = 0. The actual colors are supplied as an argument in \code{plot_trees()}.  }


}


\value{
\itemize{
\item{x_tree}{repackaged phylo object}
\item{y_tree}{repackaged phylo object}
\item{x_ave_resp}{summarized response for x-axis barplot}
\item{y_ave_resp}{summarized response for y-axis barplot}
\item{mat}{sparse matrix of responses that will be directly plotted in the center of the figure}
\item{nmat}{sparse matrix of the number of replicate responses}
\item{mat_cex}{response translated to bubble sizes for plotting}
\item{mat_pn}{indicates positive or negative interaction for bubble colors}
\item{full_dat}{repackaged original data}
\item{group_response}{summarized response data}
\item{count}{whether or not to plot count data instead of response}
}
}

\author{James Meadow}

\seealso{
See also \code{plot_trees}
}

\examples{
\dontrun{
trees_list <- input_trees(x_tree = fungal_tree,
                          y_tree = plant_tree,
                          x_key = mycor_ei$fungal_name,
                          y_key = mycor_ei$plant_name,
                          response = mycor_ei$ei,
                          response_type = mycor_ei$mycorrhizae_type,
                          y_lab_cutoff = 1.4,
                          y_node_labs = c('fabaceae', 'betulaceae',
                                          'rosaceae', 'myrtaceae',
                                          'asteraceae', 'poaceae',
                                          'pinaceae'))

}
}
