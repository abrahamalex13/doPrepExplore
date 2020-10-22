#' ggplot a bar chart, with by-group flexibility and optional print-to-pdf.
#'
#' @param df Data frame with plotting data.
#' @param varname_x Variable name (as character string) whose distribution will be plotted.
#' @param ... Other named arguments -- character string input values -- passing to ggplot2's aesthetics.
#' @param order_x Variable values are ordered along axis by frequency.
#' @param units_share Vertical axis units are in terms of within-x value share.
#' @param path_filename_print Optional pdf print file
#'
#' @return ggplot plot object.


ggplot_bar_x <- function(df, varname_x, ..., order_x = TRUE, units_share = FALSE, path_filename_print = NULL) {

  title <- paste("Distribution: ", varname_x, sep = "")
  varname_x.sym <- rlang::sym(varname_x)
  args_aes <- list(...)

  if ("group" %in% names(args_aes))
    varnames_grp_tab.syms <- rlang::syms(c(varname_x, args_aes[["group"]]))
  else varnames_grp_tab.syms <- rlang::syms(varname_x)



  #summary data prep -----

  df_plot <- df %>%
    group_by(!!!varnames_grp_tab.syms) %>%
    summarize(n = n()) %>%
    ungroup()

  df_plot <- df_plot %>%
    group_by(!!varname_x.sym) %>%
    mutate(n_x = sum(n)) %>%
    ungroup()

  if (!units_share) {

    df_plot <- df_plot %>%
      mutate(value = n)

  } else if (units_share) {

    if ("group" %in% names(args_aes)) {

      df_plot <- df_plot %>%
        mutate(denom = n_x)

    } else df_plot[["denom"]] <- sum(df_plot[["n"]])

    df_plot <- df_plot %>%
      mutate(value = n / denom)

  }

  #end summary data prep ---



  #plot construction ----

  #assuming character string inputs for general aes args
  if (order_x) {

    p <- ggplot(data = df_plot) +
      geom_col(mapping = aes_string(x = expr(reorder(!!varname_x.sym, -n_x)), y = "value", ...)) +
      labs(title = title, x = varname_x)

  } else {

    p <- ggplot() +
      geom_col(data = df_plot, mapping = aes_string(x = varname_x, y = "value", ...)) +
      labs(title = title, x = varname_x)

  }

  #end plot constr ------



  if (!is.null(path_filename_print)) {

    pdf(path_filename_print, width = 13, height = 8)
    print(p)
    dev.off()

  }

  return(p)

}
