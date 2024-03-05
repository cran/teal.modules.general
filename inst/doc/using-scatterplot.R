## ----echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------------
library(teal.modules.general) # used to create the app
library(dplyr) # used to modify data sets

## ----echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------------
data <- teal_data()
data <- within(data, {
  ADSL <- teal.modules.general::rADSL %>%
    mutate(TRTDUR = round(as.numeric(TRTEDTM - TRTSDTM), 1))
  ADRS <- teal.modules.general::rADRS
  ADTTE <- teal.modules.general::rADTTE
  ADLB <- teal.modules.general::rADLB %>%
    mutate(CHGC = as.factor(case_when(
      CHG < 1 ~ "N",
      CHG > 1 ~ "P",
      TRUE ~ "-"
    )))
})
datanames <- c("ADSL", "ADRS", "ADTTE", "ADLB")
datanames(data) <- datanames
join_keys(data) <- default_cdisc_join_keys[datanames]

## ----ggExtra, include = FALSE-------------------------------------------------
ggextra_available <- requireNamespace("ggExtra", quietly = TRUE)

## ----include = !ggextra_available---------------------------------------------
# NOTE: The code will not be run as package ggExtra is not installed.

## ----eval = ggextra_available, echo=TRUE, message=FALSE, warning=FALSE, results="hide"----
# configuration for the single wide datasets
mod1 <- tm_g_scatterplot(
  label = "Single wide dataset",
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1", "BMRKR2")),
      selected = "AGE",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  color_by = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("RACE", "SEX")),
      selected = NULL,
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for the two wide datasets
mod2 <- tm_g_scatterplot(
  label = "Two wide datasets",
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX")),
      selected = "AGE",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  color_by = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("COUNTRY", "AGE", "RACE")),
      selected = "COUNTRY",
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the different long datasets
mod3 <- tm_g_scatterplot(
  label = "Different long datasets",
  x = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADRS"]]),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE
    ),
    filter = filter_spec(
      label = "Select endpoint:",
      vars = c("PARAMCD", "AVISIT"),
      choices = value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
      selected = "OVRINV - SCREENING",
      multiple = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADTTE"]]),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE
    ),
    filter = filter_spec(
      label = "Select parameters:",
      vars = c("PARAMCD"),
      choices = value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
      selected = "OS",
      multiple = TRUE
    )
  ),
  color_by = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX")),
      selected = "AGE",
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the wide and long datasets
mod4 <- tm_g_scatterplot(
  label = "Wide and long datasets",
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("SEX", "AGE", "BMRKR1", "COUNTRY")),
      selected = "AGE",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADLB"]]$PARAMCD)[1],
        multiple = FALSE,
        label = "Select measurement:"
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
      label = "Selected variable:",
      choices = "AVAL",
      selected = "AVAL",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  color_by = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("SEX", "AGE", "RACE", "COUNTRY")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the same long datasets (same subsets)
mod5 <- tm_g_scatterplot(
  label = "Same long datasets (same subsets)",
  x = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVAL", "BMRKR1", "BMRKR2")),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  y = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVAL", "BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  color_by = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AGE", "SEX", "RACE")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# configuration for the same long datasets (different subsets)
mod6 <- tm_g_scatterplot(
  label = "Same long datasets (different subsets)",
  x = data_extract_spec(
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
      choices = "AVAL",
      selected = "AVAL",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  y = data_extract_spec(
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
      choices = "AVAL",
      selected = "AVAL",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  color_by = data_extract_spec(
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
      choices = variable_choices(data[["ADLB"]], c("RACE", "SEX")),
      selected = "SEX",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    modules(
      label = "Scatterplot",
      mod1,
      mod2,
      mod3,
      mod4,
      mod5,
      mod6
    )
  )
)

## ----echo=TRUE, results="hide"------------------------------------------------
shinyApp(app$ui, app$server, options = list(height = 1024, width = 1024))

