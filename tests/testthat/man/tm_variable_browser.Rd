% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_variable_browser.R
\name{tm_variable_browser}
\alias{tm_variable_browser}
\title{\code{teal} module: Variable browser}
\usage{
tm_variable_browser(
  label = "Variable Browser",
  datasets_selected = deprecated(),
  datanames = if (missing(datasets_selected)) "all" else datasets_selected,
  parent_dataname = "ADSL",
  pre_output = NULL,
  post_output = NULL,
  ggplot2_args = teal.widgets::ggplot2_args(),
  transformators = list()
)
}
\arguments{
\item{label}{(\code{character(1)}) Label shown in the navigation item for the module or module group.
For \code{modules()} defaults to \code{"root"}. See \code{Details}.}

\item{datasets_selected}{(\code{character}) \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} vector of datasets to show, please
use the \code{datanames} argument.}

\item{datanames}{(\code{character}) Names of the datasets relevant to the item.
There are 2 reserved values that have specific behaviors:
\itemize{
\item The keyword \code{"all"} includes all datasets available in the data passed to the teal application.
\item \code{NULL} hides the sidebar panel completely.
\item If \code{transformators} are specified, their \code{datanames} are automatically added to this \code{datanames}
argument.
}}

\item{parent_dataname}{(\code{character(1)}) string specifying a parent dataset.
If it exists in \code{datanames} then an extra checkbox will be shown to
allow users to not show variables in other datasets which exist in this \code{dataname}.
This is typically used to remove \code{ADSL} columns in \code{CDISC} data.
In non \code{CDISC} data this can be ignored. Defaults to \code{"ADSL"}.}

\item{pre_output}{(\code{shiny.tag}) optional, text or UI element to be displayed before the module's output,
providing context or a title.
with text placed before the output to put the output into context. For example a title.}

\item{post_output}{(\code{shiny.tag}) optional, text or UI element to be displayed after the module's output,
adding context or further instructions. Elements like \code{shiny::helpText()} are useful.}

\item{ggplot2_args}{(\code{ggplot2_args}) object created by \code{\link[teal.widgets:ggplot2_args]{teal.widgets::ggplot2_args()}}
with settings for the module plot.
The argument is merged with options variable \code{teal.ggplot2_args} and default module setup.

For more details see the vignette: \code{vignette("custom-ggplot2-arguments", package = "teal.widgets")}}

\item{transformators}{(\code{list} of \code{teal_transform_module}) that will be applied to transform module's data input.
To learn more check \code{vignette("transform-input-data", package = "teal")}.}
}
\value{
Object of class \code{teal_module} to be used in \code{teal} applications.
}
\description{
Module provides provides a detailed summary and visualization of variable distributions
for \code{data.frame} objects, with interactive features to customize analysis.
}
\details{
Numeric columns with fewer than 30 distinct values can be treated as either discrete
or continuous with a checkbox allowing users to switch how they are treated(if < 6 unique values
then the default is discrete, otherwise it is continuous).
}
\examples{
# general data example
data <- teal_data()
data <- within(data, {
  iris <- iris
  mtcars <- mtcars
  women <- women
  faithful <- faithful
  CO2 <- CO2
})

app <- init(
  data = data,
  modules = modules(
    tm_variable_browser(
      label = "Variable browser"
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# CDISC example data
library(sparkline)
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADTTE <- teal.data::rADTTE
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_variable_browser(
      label = "Variable browser"
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}

}
\section{Examples in Shinylive}{
\describe{
  \item{example-1}{
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6urSMtKJxVTWiFbowpAQsdVmt7YxNEJXZRPD9WYPDzSZQeflmflmT07PNAMIA8gBM9WvrSgC+ihBKaKj1KnnszSm6ALz+GbjNfEIidbdPwmIX-ZW6pDCJWhYtCg9BEiSYRGyohkXx+P2oILgfluCjAADUgSCRLoIVCZKjmpUDkSlAdaCZdOwVORmJYdDZbOVvqJChBWABBdDsY4AEkEtFKvOhjB0jAOuyUYF2AF0gA}{Open in Shinylive}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6urSMtKJxVTWiFbowpAQsdVmt7YxNEJXZRPD9WYPDzSZQeflmflmT07PNAMIA8gBM9WvrSgC+ihBKaKj1KnnszSm6ALz+GbjNfEIidbdPwmIX-ZW6pDCJWhYtCg9BEiSYRGyohkXx+P2oILgfluCjAADUgSCRLoIVCZKjmpUDkSlAdaCZdOwVORmJYdDZbOVvqJChBWABBdDsY4AEkEtFKvOhjB0jAOuyUYF2AF0gA" style="height: 800px; width: 100vw; max-width: 1400px; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: absolute; left: 50\%; margin-top: 30px; transform: translateX(-50\%); z-index: 1"></iframe>}}
    \if{html}{\out{<a style='height: 800px; display: block;'></a>}}
  }
  \item{example-2}{
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dOAAPWFQRXX4oUiglOiYWDlFUFgBrOgg4RQgYuINjLmoAfXyoGyVywt0Ad1pSAAsVdnLcXRAlXV0AQR8AgBka4oxyxERGPsGu3p87O2dhuG5R2KhxybmFpQBfXIArIhUS9LhWUVa12yNouBMoYVISgn5aUQISw+PT8+BoeAu5TkAF03NB0DUVA12DNqgBeaJrXAzPhCESiXQI1HCMQwiDdbqkGAlLQsWhQegiEpMIi1UQyPEEgnUClwaiY3QKMAANTJFKiNLpMi5M26uTFSlytBMunYKnIzEsOhstk6+N0omaEFYPXQ7DQqAAJIJaO0DYb6YwdIxcjslGAdsCgA}{Open in Shinylive}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dOAAPWFQRXX4oUiglOiYWDlFUFgBrOgg4RQgYuINjLmoAfXyoGyVywt0Ad1pSAAsVdnLcXRAlXV0AQR8AgBka4oxyxERGPsGu3p87O2dhuG5R2KhxybmFpQBfXIArIhUS9LhWUVa12yNouBMoYVISgn5aUQISw+PT8+BoeAu5TkAF03NB0DUVA12DNqgBeaJrXAzPhCESiXQI1HCMQwiDdbqkGAlLQsWhQegiEpMIi1UQyPEEgnUClwaiY3QKMAANTJFKiNLpMi5M26uTFSlytBMunYKnIzEsOhstk6+N0omaEFYPXQ7DQqAAJIJaO0DYb6YwdIxcjslGAdsCgA" style="height: 800px; width: 100vw; max-width: 1400px; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: absolute; left: 50\%; margin-top: 30px; transform: translateX(-50\%); z-index: 1"></iframe>}}
    \if{html}{\out{<a style='height: 800px; display: block;'></a>}}
  }
}
}

