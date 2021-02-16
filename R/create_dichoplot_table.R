#' Create a dichotomized table of a selection of variables including the national mean
#'
#' @param data a C or E SHURP-dataframe with dichotomized items
#' @param vars a vector passing variable names
#' @param restrict defaults to TRUE, passed to shurp2018::prop.yes()
#' @param restrict_level defaults to TRUE, passed to shurp2018::prop.yes()
#'
#' @importFrom rlang .data
#'
#' @return
#' @export
#'
#' @examples \dontrun{create_dichoplot_table(shurp2018_C_NA, vars = "C1dich")}
create_dichoplot_table <- function(data,
                                   vars,
                                   restrict = TRUE,
                                   restrict_level = 10L){

  each_center <-
    data %>%
    group_by(center) %>%
    summarise_at(.data, .vars = tidyselect::all_of({{vars}}),
                 .funs = ~shurp2018::prop.yes(., round = FALSE, restrict = restrict,
                                              restrict_level = restrict_level)) %>%
    ungroup() %>%
    mutate(center = as.character(center),
           center_label = as.character(center))


  national <-
    data %>%
    summarise_at(.data, tidyselect::all_of({{vars}}),
                 .funs = ~shurp2018::prop.yes(., round = FALSE, restrict = restrict, restrict_level = restrict_level)) %>%
    ungroup() %>%
    mutate(center = as.character(999),
           center_label = "SHURP 2018")




  # combine and
  # create the filling and the color_link for the target center
  output_data <-
    bind_rows(each_center, national) %>%
    mutate(filling_helper = case_when(center == "999" ~ as.character(999),
                                      TRUE ~ "other")) %>%
    mutate(fill_values_column = case_when(filling_helper  %in%  "other" ~ "other",
                                          filling_helper  %in% "999" ~ "national_mean"))

  return(output_data)

}
