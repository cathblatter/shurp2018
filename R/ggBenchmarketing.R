#' Create the so-called "benchmarketing chart" for overview of percentage agreement
#'
#' @param df a dataframe created by create_dichoplot_table()
#' @param col a variable holding pct-agreement
#' @param title either a string or a column from label$data
#' @param text_caption TRUE/FALSE, default TRUE, if caption holding the corresponding
# item text (to col) should be displayed
#' @param labs TRUE/FALSE, default TRUE, if Title/subtitle should be displayed
#' @param language "german" or "french" - default "german", select subtitle and
# caption
#'
#' @return
#' @export
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
ggBenchmarketing <- function(df,
                             col,
                             title = NULL,
                             text_caption = TRUE,
                             labs = TRUE,
                             language = "german"){


  df <- create_dichoplot_table(data = df,
                               vars = {{col}},
                               restrict = TRUE,
                               restrict_level = 10L)

  subtitle <-
    shurp2018::label_data %>%
    dplyr::filter(var_name == "label_subtitle_benchmarketing") %>%
    dplyr::pull({{language}})

  # drop nas
  df <- df %>% tidyr::drop_na({{col}})

  # defining the colours
  fill_values <- c("other" = "#b2abd2",
                   "national_mean" = "#5e3c99")

  color_values <- c("national_mean" = "#5e3c99")

  # actual plotting
  base_plot <-
    ggplot2::ggplot(df,
                    ggplot2::aes(x = stats::reorder(center, {{col}}),
                                 y = {{col}}))+
    ggplot2::geom_col(ggplot2::aes(fill = fill_values_column)) +
    ggplot2::geom_hline(aes(yintercept = {{col}}),
                        data = dplyr::filter(df, center == "999"),
                        lty = "dashed",
                        color = "#5e3c99")



  # #  create the center - labelling (ie. only the national mean)
    base_plot <-
      base_plot +
      ggrepel::geom_label_repel(ggplot2::aes(colour = fill_values_column,
                                    label = paste0(.data$center_label, ": ", scales::percent({{col}}, .1, 1))),
                                data = filter(df, center %in% c(888, 999)),
                                nudge_x = 0,
                                nudge_y = 1,
                                alpha = 1,
                                label.size = .5,
                                direction = "both",
                                show.legend = F,
                                size = 3.8)
  # if(pp_diff == TRUE){
  #
  #   subtitle_bb_diff <- if_else(df$ownership_di == "public", label_data$label_public, label_data$label_private)
  #
  #   base_plot <-
  #     base_plot +
  #     ggrepel::geom_label_repel(aes(colour = fill_values_column,
  #                                   label = scales::percent({{col}}, .1, 1)),
  #                               data = filter(df, center %in% c(888, 999)),
  #                               nudge_x = 0,
  #                               nudge_y = 1,
  #                               alpha = 1,
  #                               label.size = .5,
  #                               direction = "both",
  #                               show.legend = F,
  #                               size = 3.8)}


  #add the scales
  base_plot <-
    base_plot +
    ggplot2::scale_x_discrete(breaks = NULL, name = NULL) +
    ggplot2::scale_y_continuous(name = NULL,
                                labels = scales::percent_format(accuracy = .1, scale = 1),
                                breaks = seq(0, 100, 10),
                                limits = c(0, 100)) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::scale_fill_manual(values = fill_values,
                               name = "% Zustimmende Antworten \nim Vergleich",
                               labels = c("national_mean" = "Nationaler Durchschnitt",
                                          "target_center" = "Ihr Pflegeheim",
                                          "other" = "alle anderen Pflegeheime"))


  # adding legends
  if(labs == TRUE){

    col2 <- stringr::str_extract(deparse(substitute(col)), "\\w{1}\\d{1,2}")

    base_plot <-
      base_plot +
      ggplot2::labs(title = paste0(title, ": Item ", col2),
                    subtitle = {{subtitle}}) }

  # suppress the legends
  base_plot <-
    base_plot +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   legend.justification = c(.5, 0),
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                   plot.caption = element_text(size = 10))

  ## add the caption text



  if(text_caption == TRUE){

    aa <- names(dplyr::select(df, {{col}}))

    text_data <-
      if(stringr::str_detect(aa , "C\\d")){shurp2018::QC_text} else {shurp2018::QE_text}

    if(language == "german") {text_data <- filter(text_data, Version == "german")} else {

      text_data <- filter(text_data, Version == "french")}

    caption_text <- text_data %>% rename_with(., .fn = ~paste0(., "dich")) %>%  pull({{col}})

    base_plot <-
      base_plot +
      labs(caption = caption_text)

  }


  return(base_plot)

}
