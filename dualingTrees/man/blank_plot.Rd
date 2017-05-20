\name{blank_plot}
\alias{blank_plot}
\title{blank_plot}
\usage{
blank_plot(x.lim = c(0, 1),
           y.lim = c(0, 1),
           x.axs = "i",
           y.axs = "i")
}

\arguments{
\item{x.lim}{passed to \code{xlim}}
\item{y.lim}{passed to \code{ylim}}
\item{x.axs}{passed to \code{xaxs}}
\item{y.axs}{passed to \code{yaxs}}
}

\description{
Internal function that gets called by the \code{plot_trees()} function. Not intended for used outside of \code{plot_trees()} function.
}
\examples{
blank_plot()
}
