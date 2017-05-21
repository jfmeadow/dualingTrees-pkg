

# data(iv)


test_that('input_trees returns correctly', {

  trees_list_test <- input_trees(x_tree = fungal_tree,
                                 y_tree = plant_tree,
                                 x_key = mycor_ei$fungal_name,
                                 y_key = mycor_ei$plant_name,
                                 response = mycor_ei$ei,
                                 response_type = mycor_ei$mycorrhizae_type)

  expect_equal(length(trees_list_test$x_tree$tip.label), 53)
  expect_equal(length(trees_list_test$y_tree$tip.label), 290)

  mycor_AM <-
    mycor_ei %>%
    filter(mycorrhizae_type == 'AM')

  ## found bug with sorting of type legend when accidentally set.
  trees_list_test_AM <- input_trees(x_tree = fungal_tree,
                                      y_tree = plant_tree,
                                      x_key = mycor_AM$fungal_name,
                                      y_key = mycor_AM$plant_name,
                                      response = mycor_AM$ei)

  expect_equal(length(trees_list_test_AM$x_tree$tip.label), 15)
  expect_equal(length(trees_list_test_AM$y_tree$tip.label), 234)





})


