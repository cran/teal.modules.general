# Launcher for the demographic-table teal app (uses the working-tree package).
suppressMessages({
  devtools::load_all("/Users/wangb133/Work/teal.modules.general", quiet = TRUE)
  library(teal.picks)
  library(shiny)
})

# --- data prep (from the qmd) ---
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADVS <- teal.data::rADVS
  weightbl <- ADVS[ADVS$PARAMCD == "WEIGHT" & ADVS$ABLFL == "Y", c("USUBJID", "AVAL")]
  names(weightbl)[names(weightbl) == "AVAL"] <- "weightbl"
  ADSL <- merge(ADSL, weightbl, by = "USUBJID", all.x = TRUE)
  ADSL$AGEGR1 <- factor(ifelse(ADSL$AGE < 65, "<65", ">=65"), levels = c("<65", ">=65"))
  sentence <- function(x) {
    x <- tolower(as.character(x))
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  ADSL$RACE <- as.character(ADSL$RACE)
  ADSL$RACE[ADSL$RACE == "MULTIPLE"] <- "MULTIRACIAL (>1 RACE)"
  ADSL$RACE <- sentence(ADSL$RACE)
  ADSL$ETHNIC <- sentence(ADSL$ETHNIC)
  rm(ADVS, weightbl, sentence)
})
join_keys(data) <- default_cdisc_join_keys["ADSL"]

vars <- c("AGE", "AGEGR1", "SEX", "ETHNIC", "RACE", "weightbl")

demographic_decorator <- teal_transform_module(
  label = "Demographic table styling",
  server = function(id, data) {
    reactive({
      within(data(), {
        table <- table |>
          crane::add_overall(last = TRUE) |>
          gtsummary::modify_caption("Table 1. Demographic Characteristics (Safety Population)") |>
          gtsummary::bold_labels() |>
          gtsummary::modify_source_note("Percentages are based on the number of non-missing values.")
      })
    })
  }
)

app <- init(
  data = data,
  modules = modules(
    tm_tbl_roche_summary(
      label = "Demographic Table",
      by = picks(
        datasets("ADSL", "ADSL"),
        variables(choices = c("ARM", "TRT01P", "ACTARM"), selected = "ARM")
      ),
      include = picks(
        datasets("ADSL", "ADSL"),
        variables(choices = vars, selected = vars, multiple = TRUE)
      ),
      nonmissing = "always",
      col_label = list(
        AGE      = "Age (yr)",
        AGEGR1   = "Age Group (yr)",
        SEX      = "Sex",
        ETHNIC   = "Ethnicity",
        RACE     = "Race",
        weightbl = "Weight (kg) at baseline"
      ),
      decorators = list(table = demographic_decorator)
    )
  )
)

cat(">>> teal app starting on http://127.0.0.1:8080\n")
shiny::runApp(
  shinyApp(app$ui, app$server),
  host = "127.0.0.1", port = 8080, launch.browser = FALSE
)
