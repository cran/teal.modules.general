## ----echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------------
library(teal.modules.general) # used to create the app

## ----echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------------
data <- teal_data()
data <- within(data, {
  ADSL <- teal.modules.general::rADSL
  ADTTE <- teal.modules.general::rADTTE
  ADLB <- teal.modules.general::rADLB
})
datanames <- c("ADSL", "ADTTE", "ADLB")
datanames(data) <- datanames
join_keys(data) <- default_cdisc_join_keys[datanames]

## ----echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------------
# configuration for the two-datasets example
mod1 <- tm_data_table(
  label = "Two datasets",
  variables_selected = list(
    ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
    ADTTE = c(
      "STUDYID", "USUBJID", "SUBJID", "SITEID",
      "PARAM", "PARAMCD", "ARM", "ARMCD", "AVAL", "CNSR"
    )
  )
)

# configuration for the subsetting or changing order of datasets
mod2 <- tm_data_table(
  label = "Datasets order",
  variables_selected = list(
    ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
    ADLB = c(
      "STUDYID", "USUBJID", "SUBJID", "SITEID",
      "PARAM", "PARAMCD", "AVISIT", "AVISITN", "AVAL", "CHG"
    )
  ),
  datasets_selected = c("ADTTE", "ADLB", "ADSL")
)

# configuration for the advanced usage of DT options and extensions
mod3 <- tm_data_table(
  label = "Advanced DT usage",
  dt_args = list(extensions = c("Buttons", "ColReorder", "FixedHeader")),
  dt_options = list(
    searching = FALSE,
    pageLength = 30,
    lengthMenu = c(5, 15, 25, 50, 100),
    scrollX = FALSE,
    dom = "lBrtip",
    buttons = c("copy", "csv", "excel", "pdf", "print"),
    colReorder = TRUE,
    fixedHeader = TRUE
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    mod1,
    mod2,
    mod3
  )
)

## ----echo=TRUE, results="hide"------------------------------------------------
shinyApp(app$ui, app$server, options = list(height = 1024, width = 1024))

