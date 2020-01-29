\name{prostate}
\alias{prostate}
\docType{data}
%## Example page 47
\title{ Prostate Cancer Data }
\description{
  Data to examine the correlation between the level of prostate-specific
  antigen and a number of clinical measures in men who were about to 
  receive a radical prostatectomy.
}
\usage{data(prostate)}
\format{
  A data frame with 97 observations on the following 10 variables.
  \describe{
    \item{lcavol}{log cancer volume}
    \item{lweight}{log prostate weight}
    \item{age}{in years}
    \item{lbph}{log of the amount of benign prostatic hyperplasia}
    \item{svi}{seminal vesicle invasion}
    \item{lcp}{log of capsular penetration}
    \item{gleason}{a numeric vector}
    \item{pgg45}{percent of Gleason score 4 or 5}
    \item{lpsa}{response}
    \item{train}{a logical vector}
  }
}
\details{
  The last column indicates which 67 observations were used as the 
  "training set" and which 30 as the test set.
}
\source{ Stamey, T., Kabalin, J., McNeal, J., Johnstone, I.,
    Freiha, F., Redwine, E. and Yang, N (1989) Prostate specific antigen
    in the diagnosis and treatment of adenocarcinoma of the prostate
    II. Radical prostatectomy treted patients, Journall of Urology 16: 1076--1083.
    This version was adopted from the archived package \pkg{ElemStatLearn},
    as used in the book
    \dQuote{"The Elements of Statistical Learning, Data Mining, Inference,
	    and Prediction} by 
    Trevor Hastie, Robert Tibshirani and Jerome Friedman.
}
\keyword{datasets}



