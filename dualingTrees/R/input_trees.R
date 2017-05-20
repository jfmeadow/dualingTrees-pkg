
input_trees <- function(x_tree = NULL,     # x_tree = f_phylo
                        y_tree = NULL,     # y_tree = p_phylo
                        x_key = NULL,      # x_key = dat$fungalName
                        y_key = NULL,      # y_key = dat$plantName
                        response = NULL,   # response = dat$ei
                        response_type = NULL,        # category to color tree tips
                        response_x_bars = NULL,     # must be df with row.names matching tree tips and col 1 as response
                        response_y_bars = NULL,
                        count = FALSE,
                        x_lab_cutoff = NULL,  # Not currently used. Labels above this cutoff are drawn
                        y_lab_cutoff = NULL,  # Labels above this cutoff are drawn
                        x_node_labs = NULL,   # Not currently used.
                        y_node_labs = NULL,   # vector of names to label as internal nodes. y_node_labs <- c('fabaceae', 'betulaceae', 'rosaceae', 'myrtaceae', 'asteraceae', 'poaceae', 'pinaceae')
                        bubble_scale = 5,     # multiplier for bubble size
                        bubble_transform = NULL) {     # select tranformation for bubble scaling

  ## stupid cran hack.
  . <- combo <- ave_resp <- NULL

  ## How many tips and how big is interaction matrix
  ny <- length(y_tree$tip.label)
  nx <- length(x_tree$tip.label)
  nmat <- matrix(0,
                 nrow = ny,
                 ncol = nx,
                 dimnames = list(y_tree$tip.label,
                                 x_tree$tip.label))

  ## Save another copy for plotting
  mat <- nmat

  ## fill with 1's if a count of interactions is desired response
  if(count) { response <- rep(1, length(y_key)) }

  ## data.frame combining x, y, and response.
  ## create a new combination column.
  full_dat <-
    data.frame(y_key, x_key, response) %>%
    dplyr::mutate(combo = paste(y_key, x_key, sep=' X '))

  ## Take the long form and compress into summary stats
  ## grouped by the combination column
  group_response <-
    full_dat %>%
    dplyr::group_by(combo) %>%
    dplyr::summarize(mean_response = mean(response),
                     max_response = max(response),
                     min_response = min(response))

  ## Lookup table for the number of combinations
  ## not sure why count won't work but whatever.
  combinations <- full_dat$combo %>% table %>% sort %>% rev
  group_response$n_combo <- combinations[group_response$combo]

  ## fill in matrix
  for (i in 1:nrow(group_response)) {
    y_x <- strsplit(group_response$combo[i], split=' X ')[[1]]
    if(y_x[1] %in% row.names(mat)) {
      if(y_x[2] %in% colnames(mat)) {
        mat[y_x[1], y_x[2]] <- group_response$mean_response[i]
        nmat[y_x[1], y_x[2]] <- group_response$n_combo[i]
      }
    }
  }


  # ---------------------------


  ########## STOPPED HERE.


  ## Size of plotted bubbles. Scaled by the multipier and transform input
  if(!is.null(bubble_transform)) {
    if(bubble_transform == 'log') {
      tr <- function(x) x %>% abs %>% log
    } else if(bubble_transform == 'sqrt') {
      tr <- function(x) x %>% abs %>% sqrt
    } else {
      print('Check bubble_transform shoice')
      tr <- function(x) x
    }
  } else { tr <- function(x) x }

  if(count) {
    mat_cex <-
      (nmat+1) %>%
      log %>% tr %>%
      magrittr::multiply_by(bubble_scale)
  } else {
    mat_cex <-
      (abs(mat)+1) %>%
      log %>% tr %>%
      magrittr::multiply_by(bubble_scale)
  }
  if(max(mat_cex) < 2) {
    cat('\nThe bubbles are going to be really small.\n',
        'Consider increasing bubble_scale or\n',
        'choosing a different bubble_transform.')
  }
  ## Positive or negetive for plotting.
  ## set up test? Maybe not necessary
  mat_pn <- mat
  mat_pn[mat > 0] <- 1
  mat_pn[mat < 0] <- -1

  ## Grouped response values for marginal barplots.
  ## Ordered by the names in the matrix.
  y_ave_resp <-
    full_dat %>%
    dplyr::group_by(y_key) %>%     # name of taxon
    dplyr::summarize(
      ave_resp_per_y = mean(response),
      ave_resp_per_y_abs = mean(response) %>% abs) %>%
    data.frame %>%
    magrittr::set_rownames(.[, 1]) %>%    # stupid.
    .[dimnames(mat)[[1]], ]     # reorder to match mat
  y_ave_resp$ave_resp <- NULL   # get mean of means
  for(i in 1:nrow(mat)) {
    mr <- mat[i, ] %>% .[which(. != 0)] %>% mean
    y_ave_resp$ave_resp[i] <- mr
  }
  y_ave_resp$count_resp <- rowSums(nmat)

  ## add optional values for barplots.
  if(!is.null(response_y_bars)) {
    if(all(row.names(response_y_bars) %in% row.names(y_ave_resp))) {
      y_ave_resp$response_y_bars <-
        response_y_bars[row.names(response_y_bars), 1]
    } else {
      stop('Check the format of response_y_bars')
    }
  }

  ## get absolute value and pos/neg coloring indicator
  y_ave_resp %<>%
    dplyr::mutate(
      ave_resp_abs = abs(ave_resp),
      y_pn = dplyr::case_when(
        .$ave_resp > 0 ~ 1,
        .$ave_resp < 0 ~ -1,
        .$ave_resp == 0 ~ 0)) %>%
    magrittr::set_rownames(.[, 1])

  ## get abs and pn for optional bar response
  if(!is.null(response_y_bars)) {
    y_ave_resp %<>%
      dplyr::mutate(
        bars_resp_abs = abs(response_y_bars),
        y_pn = dplyr::case_when(
          .$response_y_bars > 0 ~ 1,
          .$response_y_bars < 0 ~ -1,
          .$response_y_bars == 0 ~ 0)) %>%
      magrittr::set_rownames(.[, 1])

  }



  ## Same for the x-axis tree
  x_ave_resp <-
    full_dat %>%
    dplyr::group_by(x_key) %>%
    dplyr::summarize(ave_resp_per_x = mean(response),
                     ave_resp_abs_per_x = mean(response) %>% abs) %>%
    data.frame %>%
    magrittr::set_rownames(.[, 1]) %>%
    .[dimnames(mat)[[2]], ]
  x_ave_resp$ave_resp <- NULL
  for(i in 1:ncol(mat)) {
    mc <- mat[, i] %>% .[which(. != 0)] %>% mean
    x_ave_resp$ave_resp[i] <- mc
  }
  x_ave_resp$count_resp <- colSums(nmat)

  ## add optional values for barplots.
  if(!is.null(response_x_bars)) {
    if(all(row.names(response_x_bars) %in% row.names(x_ave_resp))) {
      x_ave_resp$response_x_bars <-
        response_x_bars[row.names(response_x_bars), 1]
    } else {
      stop('Check the format of response_x_bars')
    }
  }

  x_ave_resp %<>%
    dplyr::mutate(
      ave_resp_abs = abs(ave_resp),
      x_pn = dplyr::case_when(
        .$ave_resp > 0 ~ 1,
        .$ave_resp < 0 ~ -1,
        .$ave_resp == 0 ~ 0)) %>%
    magrittr::set_rownames(.[, 1])

  ## get abs and pn for optional bar response
  if(!is.null(response_x_bars)) {
    x_ave_resp %<>%
      dplyr::mutate(
        bars_resp_abs = abs(response_x_bars),
        x_pn = dplyr::case_when(
          .$response_x_bars > 0 ~ 1,
          .$response_x_bars < 0 ~ -1,
          .$response_x_bars == 0 ~ 0)) %>%
      magrittr::set_rownames(.[, 1])
  }

  ## Put these categories into the __ave_resp data.frames.
  ## will be used for colors during plotting.
  if(!is.null(response_type)) {
    rt <- table(response_type)
    ## Lazy for now. if someone needs more than 2, fix it.
    if(dim(rt) > 2) {
      stop('Response currently can only have 2 categories.
         If this is not enough, contact the author.')
    }

    ## creat lookup tables
    xt <- table(response_type, x_key)
    yt <- table(response_type, y_key)

    ## Fill in response type column for x
    x_ave_resp$type <- NULL
    for(i in 1:nrow(x_ave_resp)) {
      xi <- x_ave_resp$x_key[i]
      if(all(xt[, xi] > 0)) {
        x_ave_resp$type[i] <- 'Both'
      } else {
        x_ave_resp$type[i] <- row.names(xt)[which(xt[, xi] != 0)]
      }
    }
    ## sort factor levels for color assignment
    if(any(x_ave_resp$type == 'Both')) {
      x_ave_resp$type %<>% factor(levels = c(names(rt), 'Both'))
    } else x_ave_resp$type %<>% factor

    ## Fill in response type column for y
    y_ave_resp$type <- NULL
    for(i in 1:nrow(y_ave_resp)) {
      yi <- y_ave_resp$y_key[i]
      if(all(yt[, yi] > 0)) {
        y_ave_resp$type[i] <- 'Both'
      } else {
        y_ave_resp$type[i] <- row.names(yt)[which(yt[, yi] != 0)]
      }
    }
    ## sort factor levels for color assignment
    if(any(y_ave_resp$type == 'Both')) {
      y_ave_resp$type %<>% factor(levels = c(names(rt), 'Both'))
    } else y_ave_resp$type %<>% factor

    ## Output for user.
    ## This should help to supply color arguments to the plotting function.
    cat('\n\nThese are the x-axis categories: ')
    print(table(x_ave_resp$type))
    cat('\n\nThese are the y-axis categories: ')
    print(table(y_ave_resp$type))

  } else {

    ## Fill in response type column for x
    x_ave_resp$type <- NULL

    ## Fill in response type column for y
    y_ave_resp$type <- NULL

    ## Output for user.
    ## This should help to supply color arguments to the plotting function.
    cat('\n\nNo categories were supplied for response_type. Moving on.  ')
  }

  ## Select top n to name on barplots based on response cutoff.
  ## Those unplotted are acutally just plotted as ''
  if(!is.null(x_lab_cutoff)) {
    x_ave_resp$x_lab <- x_ave_resp$x_key %>% as.character
    x_ave_resp$x_lab[x_ave_resp$ave_resp_abs < x_lab_cutoff] <- ''
  } else { x_ave_resp$x_lab <- '' }

  ## Same for y
  if(!is.null(y_lab_cutoff)) {
    y_ave_resp$y_lab <- y_ave_resp$y_key %>% as.character
    y_ave_resp$y_lab[which(y_ave_resp$ave_resp_abs < y_lab_cutoff)] <- ''
  } else { y_ave_resp$y_lab <- '' }

  ## Select a few internal clades to label.
  if(!is.null(x_node_labs)) {
    x_these <-
      x_node_labs %>%
      paste(collapse='|') %>%
      grep(., x_tree$node.label)
    x_tree$label_nodes <- rep('', x_tree$Nnode)
    x_tree$label_nodes[x_these] <-
      sapply(x_tree$node.label[x_these], FUN=simple_cap)
  }

  if(!is.null(y_node_labs)) {
    y_these <-
      y_node_labs %>%
      paste(collapse='|') %>%
      grep(., y_tree$node.label)
    y_tree$label_nodes <- rep('', y_tree$Nnode)
    y_tree$label_nodes[y_these] <-
      sapply(y_tree$node.label[y_these], FUN=simple_cap)
  }


  OUT <- list(x_tree = x_tree,
              y_tree = y_tree,
              x_ave_resp = x_ave_resp,
              y_ave_resp = y_ave_resp,
              mat = mat,
              nmat = nmat,
              mat_cex = mat_cex,
              mat_pn = mat_pn,
              full_dat = full_dat,
              group_response = group_response,
              count = count,
              bubble_transform = bubble_transform)
  return(OUT)


}


