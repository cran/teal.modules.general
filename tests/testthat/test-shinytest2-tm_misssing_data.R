app_driver_tm_missing_data <- function() {
  data <- within(simple_teal_data(), {
    set.seed(123)

    add_nas <- function(x) {
      x[sample(seq_along(x), floor(length(x) * runif(1, .05, .17)))] <- NA
      x
    }

    iris[] <- lapply(iris, add_nas)
    mtcars[] <- lapply(mtcars, add_nas)
    mtcars[["cyl"]] <- as.factor(mtcars[["cyl"]])
    mtcars[["gear"]] <- as.factor(mtcars[["gear"]])
  })

  init_teal_app_driver(
    data = data,
    modules = tm_missing_data(
      label = "Missing data",
      plot_height = c(600, 400, 5000),
      plot_width = NULL,
      datanames = "all",
      ggtheme = "gray",
      ggplot2_args = list(
        "Combinations Hist" = teal.widgets::ggplot2_args(
          labs = list(subtitle = "Plot produced by Missing Data Module", caption = NULL)
        ),
        "Combinations Main" = teal.widgets::ggplot2_args(labs = list(title = NULL))
      ),
      pre_output = NULL,
      post_output = NULL
    ),
    timeout = 3000,
    seed = 1
  )
}

test_that("e2e - tm_missing_data: Initializes without errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_missing_data()

  app_driver$expect_no_shiny_error()

  testthat::expect_equal(
    app_driver$get_text("#teal-teal_modules-active_tab > li.active > a"),
    "Missing data"
  )

  encoding_dataset <- app_driver$get_text(
    app_driver$active_module_element("dataset_encodings .help-block")
  )

  testthat::expect_match(encoding_dataset, "Datasets.*iris.*mtcars", all = FALSE)


  app_driver$stop()
})

test_that("e2e - tm_missing_data: Default settings and visibility of the summary graph", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_missing_data()
  # default summary tab
  testthat::expect_equal(
    app_driver$get_active_module_input("iris-summary_type"),
    "Summary"
  )

  testthat::expect_setequal(
    app_driver$get_active_module_input("iris-variables_select"),
    c("Petal.Length", "Sepal.Length", "Petal.Width", "Species", "Sepal.Width")
  )

  app_driver$click(selector = app_driver$active_module_element("iris-filter_na"))
  app_driver$expect_no_validation_error()

  app_driver$click(selector = app_driver$active_module_element("iris-any_na"))
  app_driver$expect_no_validation_error()

  testthat::expect_true(
    app_driver$is_visible(
      app_driver$active_module_element("iris-summary_plot-plot_out_main")
    )
  )

  app_driver$stop()
})

test_that("e2e - tm_missing_data: Check default settings and visibility of the combinations graph and encodings", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_missing_data()

  app_driver$expect_no_shiny_error()

  # combination graph

  app_driver$set_active_module_input("iris-summary_type", "Combinations")
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(
      app_driver$active_module_element("iris-combination_plot-plot_out_main")
    )
  )

  # combination encoding

  testthat::expect_true(
    app_driver$is_visible(
      app_driver$active_module_element("iris-cutoff")
    )
  )

  testthat::expect_equal(app_driver$get_active_module_input("iris-combination_cutoff"), 2L)
  app_driver$set_active_module_input("iris-combination_cutoff", 10L)
  app_driver$expect_no_validation_error()

  app_driver$stop()
})

test_that("e2e - tm_missing_data: Validate functionality and UI response for 'By Variable Levels'", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_missing_data()
  # By variable levels
  app_driver$set_active_module_input("iris-summary_type", "By Variable Levels")
  app_driver$expect_no_validation_error()


  testthat::expect_equal(
    app_driver$get_active_module_input("iris-group_by_var"),
    "Species"
  )
  testthat::expect_setequal(
    app_driver$get_active_module_input("iris-group_by_vals"),
    c("NA", "setosa", "versicolor", "virginica")
  )

  app_driver$set_active_module_input("iris-group_by_vals", c("versicolor", "virginica"))
  app_driver$expect_no_validation_error()

  testthat::expect_equal(
    app_driver$get_active_module_input("iris-count_type"),
    "counts"
  )
  app_driver$set_active_module_input("iris-count_type", "proportions")
  testthat::expect_true(app_driver$is_visible(app_driver$active_module_element("iris-levels_table")))

  app_driver$stop()
})

test_that("e2e - tm_missing_data: Validate 'By Variable Levels' table values", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_missing_data()

  app_driver$set_active_module_input("iris-summary_type", "By Variable Levels")
  levels_table <- app_driver$active_module_element("iris-levels_table") %>%
    app_driver$get_html_rvest() %>%
    rvest::html_table(fill = TRUE) %>%
    .[[1]]

  testthat::expect_setequal(
    levels_table$Variable,
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  app_driver$stop()
})
