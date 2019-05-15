library(testwhat)
library(glue)
library(praise)

#' Adapter for `testwhat` to interface with `learnr`
#'
#' @param label Label for exercise chunk.
#' @param user_code R code submitted by the user.
#' @param solution_code Code provided within the “-solution” chunk for the exercise.
#' @param check_code Code provided within the “-check” chunk for the exercise.
#' @param envir_result The R environment after the execution of the chunk.
#' @param evaluate_result The return value from the evaluate::evaluate function.
#' @param envir_prep
#' @param ... Unused (include for compatibility with parameters to be added in the future)
#'
#' @return The checker function should return an R list which contains several
#'   fields indicating the result of the check. Available fields and their
#'   values: \describe{
#'     \item{message}{Feedback message. Can be a plain character vector or can
#'     HTML produced via the htmltools package.}
#'
#'     \item{correct}{Boolean value indicating whether the submitted answer was
#'     correct.}
#'
#'     \item{type}{Feedback type (visual presentation style). Can be “auto”,
#'     “success”, “info”, “warning”, “error”, or “custom”. Note that “custom”
#'     implies that the “message” field is custom HTML rather than a
#'     character vector.}
#'
#'     \item{location}{Location for feedback (“append”, “prepend”, or “replace”).}
#'   }
testwhat_adapter <- function(label, user_code, solution_code, check_code,
                             envir_result, evaluate_result, envir_prep, ...) {
  # prepares the ex() function
  rng_seed <- getOption("testwhat_adapter_seed", NULL)
  setup_state(
    stu_code = glue("set.rseed({rng_seed})\n", user_code),
    sol_code = glue("set.rseed({rng_seed})\n", solution_code)
  )

  # parse and run the solution checking code
  checker <- parse(text = check_code)
  result <- tryCatch(
    eval(checker),
    sct_failure = function(e) e,
    error = function(e) e
  )

  # fail gracefully for bad SCT
  if ("simpleError" %in% class(result)) {
    correct <- FALSE
    message <- glue(
      "Exercise setup error: {result$message} in {result$call}",
      "Please notify instructor or course developer. Thank you!",
      .sep = "\n"
    )
  }

  #
  if ("sct_failure" %in% class(result)) {
    correct <- FALSE
    message <- result$message
  } else {
    correct <- TRUE
    message <- praise("${Adjective}! Well done!")
  }

  list(message = message, correct = correct, location = "append")
}

#' Sets the random number generation seed for the adapter
#'
#' @param seed a single value, interpreted as an integer, or NULL
set_adapter_seed <- function(seed) {
  options(testwhat_adapter_seed = seed)
}
