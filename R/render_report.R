#' Render individual report as PDF
#'
#' @param language a character - either "D" or "F"
#' @param center a number - choose center number e.g. 999
#'
#' @return creates a PDF output named according to language and center
#' @export
#'
#' @examples render_report(language = "D", center = 148)
render_report <- function(language, center) {
  rmarkdown::render("index.Rmd", params = list(
      language = language,
      center = center
    ), envir = parent.frame(),
    output_file = paste0("Individual_report-", language, "-", center, ".pdf")
  )
}
