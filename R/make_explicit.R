#' Replace implicit function calls with explicit ones
#'
#' @param input_path A file with R code
#' @param output_path If different from `input_path`
#' @param comments Should code be replaced in comments? (Default: FALSE)
#'
#' @return the output_path
#'
#' @importFrom rlang .data
#'
#' @export
make_explicit <- function(
    input_path,
    output_path = input_path,
    comments = FALSE) {
  code <- readr::read_lines(input_path)

  df <- funspotr::spot_funs(input_path) %>%
    dplyr::filter(
      .data[["pkgs"]] != "base",
      .data[["pkgs"]] != "(unknown)"
    )

  packages <- unique(df$pkgs)

  functions <- purrr::map(
    packages,
    function(package) {
      df %>%
        dplyr::filter(packages == package) %>%
        dplyr::pull(.data[["funs"]])
    }
  ) %>%
    purrr::set_names(packages)

  replaced_code <- code

  for (package in packages) {
    replaced_code <- stringr::str_replace_all(
      string = replaced_code,
      stringr::regex(
        pattern = paste0(
          "(\\b(?<![:\\.]))((?:",
          paste(functions[[package]], collapse = "|"),
          ")\\()"
        ),
        comments = comments
      ),
      replacement = paste0("\\1", package, "::\\2")
    )
  }

  readr::write_lines(replaced_code, output_path)

  return(output_path)
}
