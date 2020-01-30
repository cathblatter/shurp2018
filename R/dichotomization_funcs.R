#' Automatically dichotomoize the C dataframes of SHURP2018
#'
#' @param df a dataframe
#'
#' @section How to?
#'  Simply assign your dataframe like this:
#'  dichotomized_data <- \code{dichotomize.C(your_data)}
#'
#'
#' @return
#' @export
#' @import dplyr
#'
#' @examples
dichotomize.C <- function(df){

  workenv_c <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9")
  collab_c <- c("C10A", "C10B","C10C", "C10D", "C10E", "C10F", "C10G", "C10H",
                "C10I")
  arbeitszuf_c <- c("C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18")
  workfam_c <- c("C19", "C20", "C21", "C22", "C23")
  inttoleave_c <- c("C24", "C25", "C26", "C27", "C28")
  overtime_c <- c("C29")
  freetime_c <- c("C30")
  saq_c <- c("C31", "C32", "C33", "C34", "C35", "C36", "C37", "C38", "C39", "C40",
             "C41", "C42", "C43")
  bernca_c <- c("C44", "C45", "C46", "C47", "C48", "C49", "C50", "C51", "C52", "C53", "C54",
                "C55", "C56", "C57", "C58", "C59", "C60",
                "C61", "C62", "C63", "C64")
  qoc_c <- c("C65", "C67")
  qoc.trend_c <- c("C66")
  pcc_c <- c("C68", "C69", "C70", "C71", "C72",
             "C73", "C74", "C75", "C76", "C77",
             "C78", "C79", "C80")
  gewaltarbeits_c <- c("C82A", "C82B", "C83A", "C83B", "C84A", "C84B")
  abspres_c <- c("C85", "C86")
  beschwerd_c <- c("C87", "C88", "C89", "C90")
  beschwerd.zus_c <- c("C91")
  mbi_c <- c("C92", "C93")
  qualimpr_c <- c("C94", "C95", "C96", "C97", "C98", "C99", "C100", "C101", "C102",
                  "C103")
  medqi.general_c <- c("C104A", "C104B", "C104C", "C104D", "C104E")
  medqi.schmerz_c <- c("C105", "C106", "C107",
                       "C108", "C109", "C110", "C111",
                       "C112", "C113")
  medqi.polymed_c <- c("C114", "C115", "C116", "C117")
  medqi.maln_c <- c("C118", "C119", "C120", "C121", "C122")
  medqi.bem_c <- c("C123", "C124", "C125", "C126", "C127", "C128")
  admin_1_c <- c("C129_1", "C130A1", "C130B1", "C130C1", "C130D1", "C130E1",
                 "C130F1", "C131")
  admin_2_c <- c("C129_2", "C130A2", "C130B2", "C130C2", "C130D2", "C130E2",
                 "C130F2")
  edoku_c <- c("C132A", "C132B", "C132C", "C132D", "C132E", "C133")

  df %>%
    mutate_at(.vars = tidyselect::all_of(workenv_c),
              .funs = list(~case_when(. %in% c(3, 4) ~ 1,
                                      . %in% c(1, 2) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(collab_c),
              .funs = list(~case_when(. %in% c(3, 4) ~ 1,
                                      . %in% c(1, 2) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(arbeitszuf_c),
              .funs = list(~case_when(. %in% c(3, 4) ~ 1,
                                      . %in% c(1, 2) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(workfam_c),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(inttoleave_c),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(overtime_c),
              .funs = list(~case_when(. %in% c(1) ~ 1,
                                      . %in% c(2, 3) ~ 2,
                                      . %in% c(4) ~ 3))) %>%
    mutate_at(.vars = tidyselect::all_of(freetime_c),
              .funs = list(~case_when(. %in% c(1, 2) ~ 1,
                                      . %in% c(3) ~ 2,
                                      . %in% c(4, 5) ~ 3))) %>%
    mutate_at(.vars = tidyselect::all_of(saq_c),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(bernca_c),
              .funs = list(~case_when(. %in% c(2, 3) ~ 1,
                                      . %in% c(77, 0, 1) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(qoc_c),
              .funs = list(~case_when(. %in% c(3, 4) ~ 1,
                                      . %in% c(1, 2) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(qoc.trend_c),
              .funs = list(~case_when(. %in% c(2, 3) ~ 1,
                                      . %in% c(1) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(pcc_c),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(gewaltarbeits_c),
              .funs = list(~case_when(. %in% c(5, 6) ~ 1,
                                      . %in% c(0, 1, 2, 3, 4) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(abspres_c),
              .funs = list(~case_when(. %in% c(0) ~ 1,
                                      . %in% c(1) ~ 2,
                                      . %in% c(2, 3, 4) ~ 3))) %>%
    mutate_at(.vars = tidyselect::all_of(beschwerd_c),
              .funs = list(~case_when(. %in% c(2, 3) ~ 1,
                                      . %in% c(1) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(beschwerd.zus_c),
              .funs = list(~case_when(. %in% c(2, 3, 4, 5) ~ 1,
                                      . %in% c(0, 1) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(mbi_c),
              .funs = list(~case_when(. %in% c(4, 5, 6) ~ 1,
                                      . %in% c(0, 1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(qualimpr_c),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(medqi.general_c),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(medqi.schmerz_c),
              .funs = list(~case_when(. %in% c(3, 4) ~ 1,
                                      . %in% c(1, 2) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(medqi.polymed_c),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(medqi.maln_c),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(medqi.bem_c),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(admin_1_c),
              .funs = list(~case_when(. %in% c(1, 2) ~ 1,
                                      . %in% c(3, 4) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(admin_2_c),
              .funs = list(~case_when(. %in% c(1) ~ 1,
                                      . %in% c(2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(edoku_c),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) -> df2

  # Manual conditional recoding for Variables
  # recoding of C129:130F
  df2$C129_1[df2$C129 %in% 1] <- NA
  df2$C129_2[df2$C129 %in% 1] <- NA
  df2$C130A1[df2$C129 %in% 1] <- NA
  df2$C130A2[df2$C129 %in% 1] <- NA
  df2$C130B1[df2$C129 %in% 1] <- NA
  df2$C130B2[df2$C129 %in% 1] <- NA
  df2$C130C1[df2$C129 %in% 1] <- NA
  df2$C130C2[df2$C129 %in% 1] <- NA
  df2$C130D1[df2$C129 %in% 1] <- NA
  df2$C130D2[df2$C129 %in% 1] <- NA
  df2$C130E1[df2$C129 %in% 1] <- NA
  df2$C130E2[df2$C129 %in% 1] <- NA
  df2$C130F1[df2$C129 %in% 1] <- NA
  df2$C130F2[df2$C129 %in% 1] <- NA

  df2$C132A[df2$c132 %in% 1] <- NA
  df2$C132B[df2$c132 %in% 1] <- NA
  df2$C132C[df2$c132 %in% 1] <- NA
  df2$C132D[df2$c132 %in% 1] <- NA
  df2$C132E[df2$c132 %in% 1] <- NA
  df2$C133[df2$c132 %in% 1] <- NA

  # # check if recoding worked
  #shurp2018_c_dicho %>% select(c132:C133) %>% View()

  # length(shurp2018_c_dicho$C91[shurp2018_c_dicho$C87 %in% 0 &
  #                                shurp2018_c_dicho$C88 %in% 0 &
  #                                shurp2018_c_dicho$C89 %in% 0 &
  #                                shurp2018_c_dicho$C90 %in% 0 ])

  df2$C91[df2$C87 %in% 0 &
            df2$C88 %in% 0 &
            df2$C89 %in% 0 &
            df2$C90 %in% 0 ] <- NA

  df2$C91_nothing <- 0
  df2$C91_nothing[is.na(df2$C87 & df2$C88 & df2$C89 & df2$C90)] <- NA
  df2$C91_nothing[df2$C87 %in% 0 & df2$C88 %in% 0 & df2$C89 %in% 0 & df2$C90 %in% 0 ] <- 1
  df2$C91_nothing <- factor(df2$C91_nothing, levels = c(0, 1))


  return(df2)

}



#' Automatically dichotomoize the E dataframes of SHURP2018
#'
#' @param df a dataframe
#'
#'  @section How to?
#'  Simply assign your dataframe like this:
#'  dichotomized_data <- \code{dichotomize.E(your_data)}
#'
#' @return
#' @export
#' @import dplyr
#'
#' @examples
dichotomize.E <- function(df){

  # set elements for recoding
  workenv_e <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9")
  collab_e <- c("E10A", "E10B","E10C", "E10D", "E10E", "E10F", "E10G", "E10H",
                "E10I", "E10J")
  arbeitszuf_e <- c("E11", "E12", "E13", "E14", "E15", "E16", "E17")
  workfam_e <- c("E18", "E19", "E20", "E21", "E22")
  inttoleave_e <- c("E23", "E24", "E25", "E26", "E27")
  overtime_e <- c("E28")
  saq_e <- c("E29", "E30", "E31", "E32", "E33", "E34", "E35", "E36", "E37", "E38", "E39", "E40",
             "E41", "E42", "E43", "E44", "E45")
  gewaltarbeits_e <- c("E46A", "E46B", "E47A", "E47B", "E48A", "E48B")
  abspres_e <- c("E49", "E50")
  beschwerd_e <- c("E51", "E52", "E53", "E54")
  beschwerd.zus_e <- c("E55")
  mbi_e <- c("E56", "E57")

  df %>%
    mutate_at(.vars = tidyselect::all_of(workenv_e),
              .funs = list(~case_when(. %in% c(3, 4) ~ 1,
                                      . %in% c(1, 2) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(collab_e),
              .funs = list(~case_when(. %in% c(3, 4) ~ 1,
                                      . %in% c(1, 2) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(arbeitszuf_e),
              .funs = list(~case_when(. %in% c(3, 4) ~ 1,
                                      . %in% c(1, 2) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(workfam_e),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(inttoleave_e),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(overtime_e),
              .funs = list(~case_when(. %in% c(1) ~ 1,
                                      . %in% c(2, 3) ~ 2,
                                      . %in% c(4) ~ 3))) %>%
    mutate_at(.vars = tidyselect::all_of(saq_e),
              .funs = list(~case_when(. %in% c(4, 5) ~ 1,
                                      . %in% c(1, 2, 3) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(gewaltarbeits_e),
              .funs = list(~case_when(. %in% c(5, 6) ~ 1,
                                      . %in% c(0, 1, 2, 3, 4) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(abspres_e),
              .funs = list(~case_when(. %in% c(0) ~ 1,
                                      . %in% c(1) ~ 2,
                                      . %in% c(2, 3, 4) ~ 3))) %>%
    mutate_at(.vars = tidyselect::all_of(beschwerd_e),
              .funs = list(~case_when(. %in% c(2, 3) ~ 1,
                                      . %in% c(1) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(beschwerd.zus_e),
              .funs = list(~case_when(. %in% c(2, 3, 4, 5) ~ 1,
                                      . %in% c(0, 1) ~ 0))) %>%
    mutate_at(.vars = tidyselect::all_of(mbi_e),
              .funs = list(~case_when(. %in% c(4, 5, 6) ~ 1,
                                      . %in% c(0, 1, 2, 3) ~ 0)))  -> df2


  # # check number of NAs in E55 prior to recode
  # table(shurp2018_e_dicho$E55, useNA = "a")
  # # check number of people withouth health problems
  # length(shurp2018_e_dicho$E55[shurp2018_e_dicho$E51 %in% 0 &
  #                             shurp2018_e_dicho$E52 %in% 0 &
  #                             shurp2018_e_dicho$E53 %in% 0 &
  #                             shurp2018_e_dicho$E54 %in% 0 ] %in% T)


  # additional recoding
  # If E51-E54 were 0, make E55 NA (nothing)
  df2$E55[df2$E51 %in% 0 &
            df2$E52 %in% 0 &
            df2$E53 %in% 0 &
            df2$E54 %in% 0 ] <- NA

  # # check if difference is as expected - here yes
  # table(df2$E55, useNA = "a")

  # add a variable to see how many had actually no troubles with their health
  df2$E55_nothing <- 0
  df2$E55_nothing[is.na(df2$E51 & df2$E52 &
                          df2$E53 & df2$E54)] <- NA
  df2$E55_nothing[df2$E51 %in% 0 &
                    df2$E52 %in% 0 &
                    df2$E53 %in% 0 &
                    df2$E54 %in% 0 ] <- 1

  df2$E55_nothing <- factor(df2$E55_nothing, levels = c(0, 1))

  return(df2)

}
