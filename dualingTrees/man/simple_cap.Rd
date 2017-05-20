\name{simple_cap}
\alias{simple_cap}
\title{simple_cap}
\usage{
simple_cap(x)
}

\arguments{

  \item{x}{ a string to be capitalized. 'xx' becomes 'Xx', and 'xx_xx' becomes 'Xx_xx'. This is intended to capitalize words for genus+species format.  }
}

\description{
Internal function called by the dualingTrees package. Not intended to be called directly.

}
\examples{
simple_cap('test')
}
