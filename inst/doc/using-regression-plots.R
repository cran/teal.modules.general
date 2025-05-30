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
mod1 <- tm_a_regression(
  label = "Single wide dataset",
  response = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  regressor = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "RACE")),
      selected = "AGE",
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for the two wide datasets
mod2 <- tm_a_regression(
  label = "Two wide datasets",
  default_plot_type = 2,
  response = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  regressor = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "RACE")),
      selected = c("AGE", "RACE"),
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for the same long datasets (same subset)
mod3 <- tm_a_regression(
  label = "Same long datasets (same subset)",
  default_plot_type = 2,
  response = data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADTTE"]], c("AVAL", "CNSR")),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE
    ),
    filter = filter_spec(
      label = "Select parameter:",
      vars = "PARAMCD",
      choices = value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
      selected = "PFS",
      multiple = FALSE
    )
  ),
  regressor = data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADTTE"]], c("AGE", "CNSR", "SEX")),
      selected = c("AGE", "CNSR", "SEX"),
      multiple = TRUE
    ),
    filter = filter_spec(
      label = "Select parameter:",
      vars = "PARAMCD",
      choices = value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
      selected = "PFS",
      multiple = FALSE
    )
  )
)

# configuration for the wide and long datasets
mod4 <- tm_a_regression(
  label = "Wide and long datasets",
  response = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADLB"]]$PARAMCD)[2],
        multiple = TRUE,
        label = "Select measurement:"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data[["ADLB"]]$AVISIT),
        selected = levels(data[["ADLB"]]$AVISIT)[2],
        multiple = TRUE,
        label = "Select visit:"
      )
    ),
    select = select_spec(
      label = "Select variable:",
      choices = "AVAL",
      selected = "AVAL",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  regressor = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "BMRKR2", "AGE")),
      selected = "AGE",
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for the same long datasets (different subsets)
mod5 <- tm_a_regression(
  label = "Same long datasets (different subsets)",
  default_plot_type = 2,
  response = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADLB"]]$PARAMCD)[1],
        multiple = TRUE,
        label = "Select lab:"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data[["ADLB"]]$AVISIT),
        selected = levels(data[["ADLB"]]$AVISIT)[1],
        multiple = TRUE,
        label = "Select visit:"
      )
    ),
    select = select_spec(
      choices = "AVAL",
      selected = "AVAL",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  regressor = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADLB"]]$PARAMCD)[1],
        multiple = FALSE,
        label = "Select labs:"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data[["ADLB"]]$AVISIT),
        selected = levels(data[["ADLB"]]$AVISIT)[1],
        multiple = FALSE,
        label = "Select visit:"
      )
    ),
    select = select_spec(
      choices = variable_choices(data[["ADLB"]], c("AVAL", "AGE", "BMRKR1", "BMRKR2", "SEX", "ARM")),
      selected = c("AVAL", "BMRKR1"),
      multiple = TRUE
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    modules(
      label = "Regression plots",
      mod1,
      mod2,
      mod3,
      mod4,
      mod5
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

