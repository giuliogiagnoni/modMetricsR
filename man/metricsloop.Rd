% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metricsloop.R
\name{metricsloop}
\alias{metricsloop}
\title{metricsloop}
\usage{
metricsloop(d, var, MT, m, PF, s)
}
\arguments{
\item{d}{a data frame.}

\item{var}{a vector of varibales in the data set.}

\item{MT}{a model type, such as "lm" or "lmer".}

\item{m}{a model such as "x + y", or "x + y + (1|random)", or "x + y, family = Gamma". Varibales have to be present in the dataset d.}

\item{PF}{function for calculate the prediction of the model such as "predict.glm", if empty it will use "predict".}

\item{s}{significant digits
empty:  return all values.
number 1 to 6: will return the desired significant digits.
"std1": will return number as character in a predefined format.}
}
\value{
a data frame with different parameters to evaluate the model
}
\description{
metricsloop
}
