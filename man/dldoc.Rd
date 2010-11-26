\name{dldoc}
\alias{dldoc}
\title{Make directlabels documentation}
\description{Positioning Functions for direct labeling are supposed to work
with only certain plot types. Each Positioning Function is defined
in R/file.R and plot examples that it can be used in are found in
tests/doc/file/*.R so that we can automatically assemble a
database of example plots from the code.}
\usage{
dldoc(pkgdir = "..")
}
\arguments{
  \item{pkgdir}{Package directory root.}
}

\value{Matrix of lists describing example plots and matching builtin
Positioning Functions.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





