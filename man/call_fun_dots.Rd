% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{call_fun_dots}
\alias{call_fun_dots}
\title{Call a function with a character vector for the \code{...} argument}
\usage{
call_fun_dots(fun, str_args)
}
\arguments{
\item{fun}{(\code{character}) Name of a function where the \code{...} argument shall be replaced by values from \code{str_args}.}

\item{str_args}{(\code{character}) A character vector that the function shall be executed with}
}
\value{
Value of call to \code{fun} with arguments specified in \code{str_args}.
}
\description{
Call a function with a character vector for the \code{...} argument
}
\keyword{internal}
