

test_that('plot_trees runs smoothly', {

  trees_list_test <- input_trees(x_tree = fungal_tree,
                                 y_tree = plant_tree,
                                 x_key = mycor_ei$fungal_name,
                                 y_key = mycor_ei$plant_name,
                                 response = mycor_ei$ei,
                                 response_type = mycor_ei$mycorrhizae_type)

  mycor_AM <-
    mycor_ei %>%
    filter(mycorrhizae_type == 'AM')

  trees_list_test_AM <- input_trees(x_tree = fungal_tree,
                                    y_tree = plant_tree,
                                    x_key = mycor_AM$fungal_name,
                                    y_key = mycor_AM$plant_name,
                                    response = mycor_AM$ei)


  plot_trees(trees_list = trees_list_test,
             x_type_cols = c('#51ad4f', '#b154a0'),
             y_type_cols = c('#51ad4f', '#b154a0', '#5a1b1a'))
  plot_trees(trees_list = trees_list_test_AM)
  expect_error(plot_trees(trees_list_test))

})


