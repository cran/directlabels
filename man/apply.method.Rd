\name{apply.method}
\alias{apply.method}
\title{Apply a Positioning Method}
\description{Run a Positioning Method list on a given data set. This function
contains all the logic for parsing a Positioning Method and
sequentially applying its elements to the input data to obtain the
label positions.}
\usage{apply.method(method, d, columns.to.check = c("x", "y", "groups"), 
    ..., debug = FALSE)}
\arguments{
  \item{method}{Direct labeling Positioning Method, which is a list comprised of
any of the following: (1) a Positioning Function, (2) a character
string, (3) named values, or (4) a Positioning Method
list. Starting from the data frame of points to plot for the
panel, the elements of the Positioning Method list are applied in
sequence, and then each row of the resulting data frame is used to
draw a direct label. The interpretation of Positioning Method list
is described below. (1) a Positioning Function is any
function(d,...) which takes a data.frame d with columns x,y,groups
and returns another data.frame representing the positions of the
desired direct labels. (2) a un-named list item which is a
character string is treated as the name of an R object, so
specifying "last.points" means to look up the variable called
last.points and use that. (3) Named values are used to add data
columns, e.g. cex=1.5 means set the cex column of the direct label
data.frame to 1.5. (4) the element of a Positioning Method list
can be another Positioning Method list, in which case the elements
of the inner list are applied.}
  \item{d}{Data frame to which we apply the Positioning Method.}
  \item{columns.to.check}{After applying each Positioning Function, we check for the
presence of these columns, and if not found we stop with an error.}
  \item{\dots}{Named arguments, passed to Positioning Functions.}
  \item{debug}{
}
}

\value{The final data frame returned after applying all of the items in
the Positioning Method list.}

\author{Toby Dylan Hocking}





