
blank_plot <- function(x.lim = c(0, 1),
                       y.lim = c(0, 1),
                       x.axs = 'i',
                       y.axs = 'i') {
  graphics::plot(1, 1,
                 ann = FALSE,
                 xaxt = 'n',
                 yaxt = 'n',
                 type = 'n',
                 bty = 'n',
                 xlim = x.lim,
                 ylim = y.lim,
                 xaxs = x.axs,
                 yaxs = y.axs)
}
