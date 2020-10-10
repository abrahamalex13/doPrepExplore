#' ggplot a bar chart, with by-group flexibility and optional print-to-pdf.
#'
#' @param df Data frame with plotting data.
#' @param varname_x Variable name whose distribution will be plotted.
#' @param varname_grp Variable name used for grouping.
#' @param order_x Variable values ordered along axis by frequency.
#' @param units_share Vertical axis units are in terms of within-x value share.
#' @param path_filename_print Optional pdf print file
#'
#' @return ggplot plot object.


ggplot_bar_x <- function(df, varname_x, varname_grp = NULL, order_x = T, units_share = F, path_filename_print = NULL) {

  title <- paste("Distribution: ", varname_x, sep = "")
  varname_x.sym <- rlang::sym(varname_x)


  if (is.null(varname_grp)) {

    #summary data prep
    df_plot <- df %>%
      group_by(!!varname_x.sym) %>%
      summarize(n = n())

    if (!units_share) {

      df_plot <- df_plot %>%
        mutate(value = n)

    } else if (units_share) {

      df_plot[["denom"]] <- sum(df_plot[["n"]])
      df_plot <- df_plot %>%
        mutate(value = n / denom)

    }


    #plot construction
    if (order_x) {

      p <- ggplot(df_plot) +
        geom_col(aes(x = reorder(!!varname_x.sym, -value), y = value)) +
        labs(title = title, x = varname_x)

    } else if (!order_x) {

      p <- ggplot(df_plot) +
        geom_col(aes(x = !!varname_x.sym, y = value)) +
        labs(title = title, x = varname_x)

    }





  } else if (!is.null(varname_grp)) {

    varname_grp.sym <- rlang::sym(varname_grp)

    #summary data prep
    df_plot <- df %>%
      group_by(!!varname_x.sym, !!varname_grp.sym) %>%
      summarize(n = n())

    if (!units_share) {

      df_plot <- df_plot %>%
        mutate(value = n)

    } else if (units_share) {

      df_plot <- df_plot %>%
        group_by(!!varname_x.sym) %>%
        mutate(denom = sum(n)) %>%
        ungroup()

      df_plot <- df_plot %>%
        mutate(value = n / denom)

    }


    #plot construction
    if (order_x) {

      p <- ggplot(df_plot) +
        geom_col(aes(x = reorder(!!varname_x.sym, -n), y = value,
                     group = !!varname_grp.sym, fill = !!varname_grp.sym)) +
        labs(title = title, x = varname_x)

    } else if (!order_x) {

      p <- ggplot(df_plot) +
        geom_col(aes(x = !!varname_x.sym, y = value,
                     group = !!varname_grp.sym, fill = !!varname_grp.sym)) +
        labs(title = title, x = varname_x)

    }

  }



  if (!is.null(path_filename_print)) {

    pdf(path_filename_print, width = 13, height = 8)
    print(p)
    dev.off()

  }

  return(p)

}
