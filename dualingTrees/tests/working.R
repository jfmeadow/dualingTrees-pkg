
library(dualingTrees)

trees_list <- input_trees(x_tree = fungal_tree,
                          y_tree = plant_tree,
                          x_key = mycor_ei$fungal_name,
                          y_key = mycor_ei$plant_name,
                          response = mycor_ei$ei,
                          response_type = mycor_ei$mycorrhizae_type)




##############################################################
## vector for x tree edge colors.
x_tree_cols <- rep('gray40', length(trees_list$x_tree$edge.length))
x_tree_cols[1:65] <- '#b154a0'
x_tree_cols[66:94] <- '#51ad4f'
##############################################################



##############################################################
plot_trees(trees_list = trees_list,
           pn_cols = c('#409ab1', '#dd1d18'),
           x_tree_col = x_tree_cols,
           x_type_cols = c('#51ad4f', '#b154a0'),
           y_type_cols = c('#51ad4f', '#b154a0', '#5a1b1a'),
           x_bar_axis_offset = .5,
           y_bar_axis_offset = .5,
           pdf_filename = NULL)
