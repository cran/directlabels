\name{label.endpoints}
\alias{label.endpoints}
\title{label endpoints}
\description{Make a Positioning Method that labels a certain x value.}
\usage{label.endpoints(FUN, 
    HJUST)}
\arguments{
  \item{FUN}{FUN(d$x) should return an index of which point to label. for
example you can use which.min or which.max.}
  \item{HJUST}{hjust of the labels.}
}

\value{A Positioning Method like \code{\link{first.points}} or \code{\link{last.points}}.}

\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre] (ORCID: <https://orcid.org/0000-0002-3146-0865>)}





