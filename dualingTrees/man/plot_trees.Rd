
\name{plot_trees}
\alias{plot_trees}
\title{plot_trees}

\description{The plotting function. }

\usage{
plot_trees(trees_list,
           pn_cols = c('cornflowerblue', 'tomato'),
           x_tree_col = 'gray30',
           y_tree_col = 'gray30',
           x_type_cols = 'gray30',
           y_type_cols = 'gray30',
           x_edge_width = 0.5,
           y_edge_width = 2,
           x_tip_width = 2,
           y_tip_width = 2,
           x_bar_axis_offset = 0,
           y_bar_axis_offset = 0,
           x_space = 0,
           y_space = 0,
           x_lab_cex = NULL,
           y_lab_cex = 0.9,
           pdf_filename = NULL,
           png_filename = NULL,
           w_inches = 11,
           h_inches = 11,
           tall_layout = FALSE,
           leg_text_pos = .5,
           leg_title = 'Average Effect Size',
           pn_leg_labels = c('Positive', 'Negative'))

}


\arguments{
\item{trees_list}{Output from \code{input_trees()}. The list contains all elements that will go into the plot except options chosen in this plotting function. }
\item{pn_cols}{Colors for positive/negative values. The same colors are used for bubbles and margin barplots. Default colors are blue and red.  }
\item{x_tree_col}{Color of the tree edges. Either supply a single color or a vector of colors that correspond to the edges in the tree. }
\item{y_tree_col}{Color of the tree edges. Either supply a single color or a vector of colors that correspond to the edges in the tree. }
\item{x_type_cols}{Colors for the short bars at the tree tips. The types correspond to \code{response_type} entered in the \code{input_trees} function. This function will return color choices just to make sure they line up as you expected. If types were entered in the \code{input_trees} function, then colors must be supplied here. }
\item{y_type_cols}{Colors for the short bars at the tree tips. The types correspond to \code{response_type} entered in the \code{input_trees} function. This function will return color choices just to make sure they line up as you expected. If types were entered in the \code{input_trees} function, then colors must be supplied here. }
\item{x_edge_width}{Width of branches on left tree.}
\item{y_edge_width}{Width of branches on top tree.}
\item{x_tip_width}{Width of bars at left tree tips.}
\item{y_tip_width}{Width of bars at top tree tips.}
\item{x_bar_axis_offset}{ How far offet should the axis be from the edge of the barplot? }
\item{y_bar_axis_offset}{How far offet should the axis be from the edge of the barplot? }
\item{x_space}{Extra room for adding names to barplots. A value here of \code{x_space = .5} will add 50\% more room to the edge of the bars for name plotting. }
\item{y_space}{Extra room for adding names to barplots. A value here of \code{x_space = .5} will add 50\% more room to the edge of the bars for name plotting. }
\item{x_lab_cex}{not currently implimented. }
\item{y_lab_cex}{Size of the names plotted at the end of right hand bars. Default = 0.9. }
\item{pdf_filename}{Optional. If supplied, a file will be created. Otherwise it will appear in the R device. }
\item{png_filename}{Optional. If supplied, a file will be created. Otherwise it will appear in the R device. }
\item{w_inches}{Default = 11. Used for both pdf and png creation. }
\item{h_inches}{Default = 11. Used for both pdf and png creation.}
\item{tall_layout}{If TRUE, the layout is adjusted to allow for more of a tall aspect ratio. Works well for large y-tree and small x_tree. Default = FALSE. }
\item{leg_text_pos}{x positioning for the legend title. Ranges between 0 and 1. }
\item{leg_title}{Label text for the legend title. Default = 'Average Effect Size'.  }
\item{pn_leg_labels}{Vector of 2 strings used as legend labels for positive / negative colors. Default = c('Positive', 'Negative') }

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

##############################################################

x_tree_cols <- rep('gray40', length(trees_list$x_tree$edge.length))
x_tree_cols[1:65] <- '#b154a0'
x_tree_cols[66:94] <- '#51ad4f'

##############################################################

plot_trees(trees_list = trees_list,
           x_tree_col = x_tree_cols,
           x_type_cols = c('#51ad4f', '#b154a0'),
           y_type_cols = c('#51ad4f', '#b154a0', '#5a1b1a'))

}
}
