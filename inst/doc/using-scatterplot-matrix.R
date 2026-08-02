## ----library, echo=TRUE, message=FALSE, warning=FALSE, results="hide"---------
library(teal.modules.general) # used to create the app
library(dplyr) # used to modify data sets

## ----data, echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL %>%
    mutate(TRTDUR = round(as.numeric(TRTEDTM - TRTSDTM), 1))
  ADRS <- teal.data::rADRS
  ADTTE <- teal.data::rADTTE
  ADLB <- teal.data::rADLB %>%
    mutate(CHGC = as.factor(case_when(
      CHG < 1 ~ "N",
      CHG > 1 ~ "P",
      TRUE ~ "-"
    )))
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

## ----app, echo=TRUE, message=FALSE, warning=FALSE, results="hide"-------------
# configuration for the single wide dataset
mod1 <- tm_g_scatterplotmatrix(
  label = "Single wide dataset",
  variables = picks(
    datasets("ADSL", "ADSL"),
    variables(
      selected = c("AGE", "RACE", "SEX", "BMRKR1", "BMRKR2"),
      multiple = TRUE
    )
  )
)

# configuration for the one long datasets
mod2 <- tm_g_scatterplotmatrix(
  "One long dataset",
  variables = picks(
    datasets("ADTTE", "ADTTE"),
    variables(
      c("AVAL", "BMRKR1", "BMRKR2"),
      c("AVAL", "BMRKR1", "BMRKR2"),
      multiple = TRUE
    )
  )
)

# configuration for the two long datasets
mod3 <- tm_g_scatterplotmatrix(
  label = "Two long datasets",
  variables = list(
    picks(
      datasets("ADRS", "ADRS"),
      variables(
        selected = c("AVAL", "AVALC"),
        multiple = TRUE
      )
    ),
    picks(
      datasets("ADTTE", "ADTTE"),
      variables(
        selected = c("AVAL", "CNSR"),
        multiple = TRUE
      )
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    modules(
      label = "Scatterplot matrix",
      mod1,
      mod2,
      mod3
    )
  )
)

## ----shinyapp, eval=FALSE-----------------------------------------------------
# shinyApp(app$ui, app$server, options = list(height = 1024, width = 1024))

## ----shinylive_url, echo = FALSE, results = 'asis', eval = requireNamespace("roxy.shinylive", quietly = TRUE)----
code <- paste0(c(
  knitr::knit_code$get("library"),
  knitr::knit_code$get("data"),
  knitr::knit_code$get("app"),
  knitr::knit_code$get("shinyapp")
), collapse = "\n")

url <- roxy.shinylive::create_shinylive_url(code)
cat(sprintf("[Open in Shinylive](%s)\n\n", url))

## ----shinylive_iframe, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# knitr::include_url(url, height = "800px")

