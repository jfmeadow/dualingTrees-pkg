

## To Do:
##   test with sim data
##   save data. Maybe shuffle values?
##   write tests
##   gh

plot_trees <- function(trees_list,              # trees_list = OUT  # list from input_trees()
                       pn_cols = c('cornflowerblue', 'tomato'),   # colors for positive and negative, resp.
                       x_tree_col = 'gray30',   # must still add in cols for diff groups.
                       y_tree_col = 'gray30',   # must still add in cols for diff groups.
                       x_type_cols = 'gray30',      # vector of length types. see output from input function
                       y_type_cols = 'gray30',      # vector of length types. see output from input function
                       x_bar_axis_offset = 0,
                       y_bar_axis_offset = 0,
                       x_space = 0,
                       y_space = 0,
                       pdf_filename = NULL,     # pdf_filename = 'test.pdf'
                       png_filename = NULL,     # png_filename = 'test.png'
                       w_inches = 11,
                       h_inches = 11,
                       leg_text_pos = .5) {

  ## stupid cran hack.
  . <- NULL

  # require(ape)
  ## unpack trees_list
  x_tree         <- trees_list$x_tree
  y_tree         <- trees_list$y_tree
  x_ave_resp     <- trees_list$x_ave_resp
  y_ave_resp     <- trees_list$y_ave_resp
  mat            <- trees_list$mat
  nmat           <- trees_list$nmat
  mat_cex        <- trees_list$mat_cex
  mat_pn         <- trees_list$mat_pn
  full_dat       <- trees_list$full_dat
  group_response <- trees_list$group_response
  count          <- trees_list$count


  ## colors for positive and negative responses.
  ## test for whether there are two.
  ## output table of colors for user check.
  if(length(pn_cols) != 2) {
    stop('Make sure you have 2 colors as `pn_cols`
          (one for positive and one for negative)')}
  cols_pn <- data.frame(cols=c(pn_cols, 'transparent'),
                        values = c(1, -1, 0),
                        responses = c('positive', 'negative', 'zero'))
  cat('\n\nHere are the positive and negative colors: \n')
  print(cols_pn[, -2])

  ## Colors for x-axis types
  if(!is.null(x_ave_resp$type)) {
    if(!is.null(x_type_cols)) {
      if(length(x_type_cols) == dim(table(x_ave_resp$type))) {
        xtc <- data.frame(types = levels(x_ave_resp$type),
                          cols = x_type_cols)
      } else {
        stop('Make sure you supplied the right number of colors for x types.\n')
      }
    }
    ## assign colors
    x_ave_resp$type_col <- xtc$cols[match(x_ave_resp$type, xtc$types)]
    ## output for user
    cat('\n\nHere are the colors used for x types: \n')
    print(xtc)
    type <- TRUE
  } else type <- FALSE

  ## Colors for y-axis types
  if(!is.null(y_ave_resp$type)) {
    if(!is.null(y_type_cols)) {
      if(length(y_type_cols) == dim(table(y_ave_resp$type))) {
        ytc <- data.frame(types = levels(y_ave_resp$type),
                          cols = y_type_cols)
      } else {
        stop('Make sure you supplied the right number of colors for y types.\n')
      }
    }
    ## assign colors
    y_ave_resp$type_col <- ytc$cols[match(y_ave_resp$type, ytc$types)]
    ## output for user
    cat('\n\nHere are the colors used for y types: \n')
    print(ytc)
    type <- TRUE
  } else type <- FALSE

  if(!is.null(pdf_filename)) {
    grDevices::pdf(pdf_filename,
                   width = w_inches, height = h_inches)
    dev <- TRUE
  } else
    if(!is.null(png_filename)) {
      grDevices::png(png_filename,
                     width = w_inches, height = h_inches,
                     units = 'in', res = 300)
      dev <- TRUE
    } else dev <- FALSE

  layout(matrix(c(1, 2, 3,
                  4, 5, 6,
                  7, 8, 9),
                nrow = 3,
                ncol = 3),
         widths = c(1, 4, 1.5),
         heights = c(1, 6.5, 1.5))


  #########################
  ## 1: blank top left corner
  #########################
  par(mar = c(0, 0, 0, 0))
  blank_plot()


  if(type) {
    ## legend for interaction type colors
    legend('bottomright',
           legend = ytc$types,
           pch = 15,
           col = ytc$cols %>% as.character,
           y.intersp = 0.85,
           pt.cex = 1.6,
           cex = 1.2,
           text.col = 'gray40',
           bg = 'gray99',
           box.col = 'transparent')
  }

  #########################
  ## 2: y_axis tree tree
  #########################
  par(mar = c(0, 0, 0, 0))
  tree_edge_width <- 0.5
  ape::plot.phylo(y_tree,
                  show.tip.label = FALSE,
                  no.margin = TRUE,
                  use.edge.length = FALSE,
                  edge.col = 'gray50',
                  edge.width = tree_edge_width)
  if(!is.null(y_tree$label_nodes)) {
    ape::nodelabels(y_tree$label_nodes,
                    bg = 'white',
                    adj = 1,
                    frame = 'none')
  }
  ## Colored bars at tree tips
  u <- par()$usr
  bx1 <- (u[2] - u[1]) * .9
  bx2 <- u[2]
  # print(bx1); print(bx2)

  if(type) {
    type_cols <-
      y_ave_resp[y_tree$tip.label, 'type_col'] %>%
      as.character
    tree_edge_width <- 2
  } else { type_cols <- y_type_cols }

  segments(rep(bx1, nrow(mat)),
           1:nrow(mat),
           rep(bx2, nrow(mat)),
           1:nrow(mat),
           col = type_cols,
           lwd = tree_edge_width,
           lend = 'square')



  #########################
  ## 3: blank bottom left
  #########################
  blank_plot()

  #########################
  ## 4: top x_axis tree
  #########################
  if(length(x_tree_col) > 1) {
    if(length(x_tree_col) != length(x_tree$edge.length)) {
      stop('Make sure x_tree_col is either length=1 or length=edge.length')
    }
  }

  ape::plot.phylo(x_tree,
                  show.tip.label = FALSE,
                  no.margin = TRUE,
                  direction = 'downwards',
                  edge.color = x_tree_col,
                  edge.width = 2,
                  use.edge.length = FALSE)

  ## Colored bars at tree tips
  u <- par()$usr
  by1 <- u[3] + ((u[4] - u[3]) * .065)
  by2 <- u[3]
  # print(u); print(by1); print(by2)

  if(type) {
    type_cols <-
      x_ave_resp[x_tree$tip.label, 'type_col'] %>%
      as.character
  } else type_cols <- x_type_cols
  segments(1:ncol(mat),
           rep(by1, ncol(mat)),
           1:ncol(mat),
           rep(by2, ncol(mat)),
           col=type_cols,
           lwd = 2,
           lend = 'square')


  #########################
  ## 5: central interaction matrix
  #########################
  par(mar = c(0, 0, 0, 0))
  blank_plot(x.lim = c(1, ncol(mat)),
             y.lim = c(1, nrow(mat)),
             x.axs = 'r',
             y.axs = 'r')
  u <- par()$usr
  rect(u[1],
       u[3],
       u[2],
       u[4],
       col = 'gray99',
       border = 'transparent')
  abline(h = c(1:nrow(mat)),
         col = 'gray70',
         lwd = .1)
  abline(v = c(1:ncol(mat)),
         col = 'gray70',
         lwd = .1)

  ## plot points as semitransparent and scaled by responses
  ## then add subtle gray points on top.
  if(count) { mat <- nmat }
  for(i in 1:ncol(mat)) {
    cols <- cols_pn$cols[match(mat_pn[, i], cols_pn$values)]
    points(rep(i, nrow(mat_cex)),
           1:nrow(mat_cex),
           pch = 16,
           col = scales::alpha(as.character(cols), .5),
           cex = mat_cex[, i])
    points(rep(i, nrow(mat_cex)),
           1:nrow(mat_cex),
           pch = 16,
           col = 'gray40',
           cex = ((mat_cex[, i]>0) + 0) * 0.3)
  }

  #########################
  ## 6: fungal names bottom center
  #########################
  if(count) {
    v <- x_ave_resp[colnames(mat), 'count_resp']
  } else if(!is.null(x_ave_resp$response_x_bars)) {
    v <- x_ave_resp[colnames(mat), 'response_x_bars']
  } else {
    v <- x_ave_resp[colnames(mat), 'ave_resp_abs']
  }
  v_max <- (max(v) * 2)
  blank_plot(x.lim = c(1, ncol(mat)),
             y.lim = c(0-x_space, 1),
             x.axs = 'r')
  cols <- cols_pn$cols[match(x_ave_resp$x_pn, cols_pn$values)] %>% as.character
  segments((1:ncol(mat)),
           rep(1, ncol(mat)),
           (1:ncol(mat)),
           1 - (v / v_max),
           col = cols,
           lwd = 4,
           lend = 'square')
  ## white grid lines and axis.
  ax1 <- pretty(v, 3)
  lax2 <- ((length(ax1)*5)-4)
  ax2 <- seq(ax1[1],
             ax1[length(ax1)],
             length = lax2)
  abline(h = (1-(ax1/v_max)),
         col = 'white',
         lwd = .6)
  text((1:ncol(mat)) - 0.6,
       0.97 - v / v_max,
       sapply(x_tree$tip.label,
              FUN='simple_cap') %>%
         gsub('_', ' ', .),
       srt = 270,
       pos = 4,
       cex = .9,
       col = cols)
  par(xpd = TRUE,
      col.axis = 'gray50',
      fg = 'gray50')
  ## axis for barplot.
  axis(4, at = (1-(ax2/v_max)),
       labels = FALSE,
       line = -1.5 + x_bar_axis_offset,
       col = 'gray50',
       col.ticks = 'gray50',
       tck = -.01)
  axis(4, at = (1-(ax1/v_max)),
       labels = FALSE,
       line = -1.5 + x_bar_axis_offset,
       col = 'gray50',
       col.ticks = 'gray50',
       tck = -.03,
       las = 1)

  # Kill leading zero that doubles other axis zero
  labax1 <- ax1
  if(ax1[1] == 0) { labax1[1] <- '' }

  axis(4, at = (1-(ax1/v_max)),
       labels = labax1,
       line = -1.8 + x_bar_axis_offset,
       col = 'gray50',
       col.ticks = 'gray50',
       tck = 0,
       lwd = 0,
       las = 1)

  ## 7: blank top right
  blank_plot()


  ## 8: plant names right middle
  # print(cbind(row.names(y_ave_resp), row.names(mat)))
  if(count) {
    v <- y_ave_resp[row.names(mat), 'count_resp']
  } else if(!is.null(y_ave_resp$response_y_bars)) {
    v <- y_ave_resp[row.names(mat), 'bars_resp_abs']
  } else {
    v <- y_ave_resp[row.names(mat), 'ave_resp_abs']
  }
  v_max <- max(y_ave_resp$ave_resp_abs) * 1.5
  par(mar = c(0, 0, 0, 1))
  blank_plot(x.lim = c(0, 1+y_space),
             y.lim = c(1, nrow(mat)),
             y.axs = 'r')
  cols <- cols_pn$cols[match(y_ave_resp$y_pn, cols_pn$values)] %>% as.character
  segments(rep(0, nrow(mat)),
           (1:nrow(mat)),
           v / v_max,
           (1:nrow(mat)),
           col = cols,
           lwd = 2,
           lend = 'square')

  ## white grid lines.
  ax1 <- pretty(v, 3)
  lax2 <- ((length(ax1)*5)-4)
  ax2 <- seq(ax1[1], ax1[length(ax1)], length = lax2)
  # print(v)
  # print(range(v))
  # print(ax1)
  abline(v = (ax1/v_max),
         col = 'white',
         lwd = 1.4)
  abline(v = (ax2/v_max),
         col = 'white',
         lwd = .6)
  text((v / v_max),
       (1:nrow(mat)),
       sapply(y_ave_resp$y_lab,
              FUN='simple_cap')  %>%
         gsub('_', ' ', .),
       pos = 4,
       cex = .9,
       col = cols)
  # print(y_ave_resp$y_lab)
  # print(v / v_max)

  ## axis for barplot.
  par(xpd = TRUE,
      col.axis = 'gray50',
      fg = 'gray50')
  axis(1, at = (ax2/v_max),
       labels = FALSE,
       line = -2 + y_bar_axis_offset,
       col = 'gray50',
       col.ticks = 'gray50',
       tck = -.01)
  axis(1, at = (ax1/v_max),
       labels = FALSE,
       line = -2 + y_bar_axis_offset,
       col = 'gray50',
       col.ticks = 'gray50',
       tck = -.03,
       las = 1)
  axis(1, at = (ax1/v_max),
       labels = ax1,
       line = -2.3 + y_bar_axis_offset,
       col = 'gray50',
       col.ticks = 'gray50',
       tck = 0,
       lwd = 0,
       las = 1)

  # 9 blank with legend
  par(mar = c(0, 0, 0, 0))
  cols <- cols_pn$cols[c(1,2)] %>% as.character
  blank_plot()
  if(count) {
    text(.4, .85,
         'Number of Interactions',
         cex = 1.2,
         font = 2)
    ## Maybe come back to this and add a bubble size legend.
    # par(lend = 'square')
    # legend(.22, .8,
    #                  legend = c('Positive', 'Negative'),
    #                  lwd = 4,
    #                  col = cols,
    #                  bty = 'n',
    #                  y.intersp = .85,
    #                  text.col = 'gray40',
    #                  bg = 'gray97')
  } else {

    text(leg_text_pos, .85,
         'Average Effect Size',
         cex = 1.2,
         font = 2)
    par(lend = 'square')
    legend(.22, .8,
           legend = c('Positive', 'Negative'),
           lwd = 4,
           col = cols,
           bty = 'n',
           y.intersp = .85,
           text.col = 'gray40',
           bg = 'gray97')
  }
  if(dev) dev.off()
}




# ##############################################################
# ## vector for x tree edge colors.
# x_tree_cols <- rep('gray40', length(test$x_tree$edge.length))
# x_tree_cols[1:65] <- '#b154a0'
# x_tree_cols[66:94] <- '#51ad4f'
# ##############################################################
#
#
#
# ##############################################################
# plot_trees(trees_list = test,
#            pn_cols = c('#409ab1', '#dd1d18'),
#            x_tree_col = x_tree_cols,
#            x_type_cols = c('#51ad4f', '#b154a0'),
#            y_type_cols = c('#51ad4f', '#b154a0', '#5a1b1a'),
#            pdf_filename = 'test.pdf')





