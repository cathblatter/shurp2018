#' Render individual report as PDF - this works only with Rmd-files
#'
#' @param language a character - either "D" or "F"
#' @param center a number - choose center number e.g. 999
#' @param file the file to render, default = "index.Rmd"
#'
#' @return creates a PDF output named according to language and center
#' @export
#'
#' @examples
write_individual_report <- function(file = "index.Rmd", language, center) {
  rmarkdown::render(file, params = list(
      language = language,
      center = center
    ), envir = new.env(),
    output_file = paste0("Individual_report-", language, "-", center, ".pdf")
  )
}
