## ----echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------------
library(teal.modules.general) # used to create the app
library(dplyr) # used to modify data sets

## ----echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------------
data <- teal_data()
data <- within(data, {
  ADSL <- teal.modules.general::rADSL
  ADRS <- teal.modules.general::rADRS
  ADLB <- teal.modules.general::rADLB
})
datanames <- c("ADSL", "ADRS", "ADLB")
datanames(data) <- datanames
join_keys(data) <- default_cdisc_join_keys[datanames]

## ----echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------------
# configuration for the single wide dataset
mod1 <- tm_outliers(
  label = "Single wide dataset",
  outlier_var = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
      selected = "AGE",
      fixed = FALSE
    )
  ),
  categorical_var = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(
        data[["ADSL"]],
        subset = names(Filter(isTRUE, sapply(data[["ADSL"]], is.factor)))
      ),
      selected = "RACE",
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the wide and long datasets
mod2 <- tm_outliers(
  label = "Wide and long datasets",
  outlier_var = list(
    data_extract_spec(
      dataname = "ADSL",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
        selected = "AGE",
        fixed = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADLB",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADLB"]], c("AVAL", "CHG2")),
        selected = "AVAL",
        multiple = FALSE,
        fixed = FALSE
      )
    )
  ),
  categorical_var =
    data_extract_spec(
      dataname = "ADSL",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(
          data[["ADSL"]],
          subset = names(Filter(isTRUE, sapply(data[["ADSL"]], is.factor)))
        ),
        selected = "RACE",
        multiple = FALSE,
        fixed = FALSE
      )
    )
)

# configuration for the multiple long datasets
mod3 <- tm_outliers(
  label = "Multiple long datasets",
  outlier_var = list(
    data_extract_spec(
      dataname = "ADRS",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADRS"]], c("ADY", "EOSDY")),
        selected = "ADY",
        fixed = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADLB",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADLB"]], c("AVAL", "CHG2")),
        selected = "AVAL",
        multiple = FALSE,
        fixed = FALSE
      )
    )
  ),
  categorical_var = list(
    data_extract_spec(
      dataname = "ADRS",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(data[["ADRS"]], c("ARM", "ACTARM")),
        selected = "ARM",
        multiple = FALSE,
        fixed = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADLB",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(
          data[["ADLB"]],
          subset = names(Filter(isTRUE, sapply(data[["ADLB"]], is.factor)))
        ),
        selected = "RACE",
        multiple = FALSE,
        fixed = FALSE
      )
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    # tm_outliers ----
    modules(
      label = "Outliers module",
      mod1,
      mod2,
      mod3
    )
  )
)

## ----echo=TRUE, results="hide"------------------------------------------------
shinyApp(app$ui, app$server, options = list(height = 1024, width = 1024))

