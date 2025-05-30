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
# configuration for a single wide dataset
mod1 <- tm_g_association(
  label = "Single wide dataset",
  ref = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]]),
      selected = "AGE",
      fixed = FALSE
    )
  ),
  vars = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]]),
      selected = "BMRKR1",
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for two wide datasets
mod2 <- tm_g_association(
  label = "Two wide datasets",
  ref = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "STRATA1", "RACE")),
      selected = "STRATA1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  vars = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "RACE", "COUNTRY")),
      selected = c("AGE", "COUNTRY", "RACE"),
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for multiple long datasets
mod3 <- tm_g_association(
  label = "Multiple different long datasets",
  ref = data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADTTE"]]),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE
    ),
    filter = filter_spec(
      label = "Select endpoint:",
      vars = "PARAMCD",
      choices = value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
      selected = c("PFS", "EFS"),
      multiple = TRUE
    )
  ),
  vars = data_extract_spec(
    dataname = "ADRS",
    reshape = TRUE,
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADRS"]], c("AVALC", "BMRKR1", "BMRKR2", "ARM")),
      selected = "AVALC",
      multiple = TRUE,
      fixed = FALSE
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
        selected = "BESRSPI",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = levels(data[["ADRS"]]$AVISIT),
        selected = "SCREENING",
        multiple = TRUE
      )
    )
  )
)

# configuration for wide and long datasets
mod4 <- tm_g_association(
  label = "Wide and long datasets",
  ref = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVAL", "AVALC")),
      selected = "AVALC",
      multiple = FALSE,
      fixed = FALSE,
      label = "Selected variable:"
    ),
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADRS"]]$PARAMCD),
        multiple = TRUE,
        label = "Select response"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data[["ADRS"]]$AVISIT),
        selected = levels(data[["ADRS"]]$AVISIT),
        multiple = TRUE,
        label = "Select visit:"
      )
    )
  ),
  vars = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("SEX", "AGE", "RACE", "COUNTRY", "BMRKR1", "STRATA1", "ARM")),
      selected = "AGE",
      multiple = TRUE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# configuration for the same long dataset (same subsets)
mod5 <- tm_g_association(
  label = "Same long datasets (same subsets)",
  ref = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]]),
      selected = "AVALC",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  vars = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]]),
      selected = "PARAMCD",
      multiple = TRUE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# configuration for the same long dataset (different subsets)
mod6 <- tm_g_association(
  label = "Same long datasets (different subsets)",
  ref = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADLB"]]$PARAMCD)[1],
        multiple = FALSE,
        label = "Select lab:"
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
      choices = variable_choices(data[["ADLB"]], c("AVAL", "CHG2", "PCHG2")),
      selected = "AVAL",
      multiple = FALSE
    )
  ),
  vars = data_extract_spec(
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
      choices = variable_choices(data[["ADLB"]]),
      selected = "STRATA1",
      multiple = TRUE
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    # tm_g_association ----
    modules(
      label = "Association plot",
      mod1,
      mod2,
      mod3,
      mod4,
      mod5,
      mod6
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

