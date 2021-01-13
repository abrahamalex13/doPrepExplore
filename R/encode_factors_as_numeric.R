#' Encode factor data as a single numeric feature, via Bayesian posterior summary value.
#'
#' Objective: given a factor level, derive an implied y-value.
#' Utilize a 'y-aware' derivation method.
#' Given a factor level, what does its Y vector look like?
#' Then consider an instance of factor level _j_ in rows 1:k.
#' To derive an implied y-value here, don't use those instances in training.
#' Use other available instances, in cross-validated fashion.
#'
#' Given a factor level (and if 'train'-type execution, data split ID),
#' join implied-y value stored in reference operator.
#'
#' Execute connected sequence for encoding factor levels
#' using implied y values.
#'
#' @name encode_factors_as_numeric
#' @seealso _Feature Engineering and Selection: A Practical Approach for Predictive Models_; Max Kuhn, Kjell Johnson.
NULL
#> NULL



#' @rdname encode_factors_as_numeric
#' @export
#'
derive_posteriors_p_summaries_supervised_factor <-
  function(df, n_splits = 3, varname_factor, par_prior_beta, Y, directory_operators) {

    #all elements in factor vector receive operation
    feature_complete <- df[[varname_factor]]

    #consider one data split as operation _recipient_.
    #under cross-validated approach, utilize other splits for _training an operator_.
    id_split <- sample(rep(1:n_splits, length.out = length(feature_complete)), replace = FALSE)

    #cross-validation approach: for each split, (a) derive then (b) apply operator.
    #one operator set -- over a collection of factor levels -- matches to one _recipient set_ (set of row indexes).
    par_posterior_op.l <-
      lapply(1:n_splits, id_split = id_split, feature_complete = feature_complete, Y = Y,
             function(x, id_split, feature_complete, Y) {

               #isolate the split for which, each level _receives_ operation
               idx_rec_op <- which(id_split == x)
               feature_rec_op <- feature_complete[idx_rec_op]

               #isolate splits used to train proportion estimate
               feature_train_op <- feature_complete[-idx_rec_op]
               Y_train_op <- Y[-idx_rec_op]

               #one posterior update corresponds to one factor level.
               par_posterior_op.l <-
                 lapply(levels(feature_rec_op), feature_train_op = feature_train_op, Y_train_op = Y_train_op,
                        function(x, feature_train_op, Y_train_op) {

                          #0-1 Y offers the evidence for updating \theta
                          D <- Y_train_op[feature_train_op == x]
                          if (length(D) > 0) plugin_posterior_beta_binomial(par_prior_beta, D) else par_prior_beta

                        })
               par_posterior_op <- bind_rows(par_posterior_op.l)
               par_posterior_op[[varname_factor]] <- levels(feature_rec_op)

               #posterior needs summarized
               par_posterior_op <- par_posterior_op %>%
                 mutate(p_summary = a / (a + b),
                        l_odds_p_summary = log(p_summary / (1 - p_summary)))

               list("idx_op" = idx_rec_op, "par_posterior_op" = par_posterior_op)

             })

    saveRDS(par_posterior_op.l,
            file = paste(directory_operators, varname_factor, "_posteriors_p.rds", sep = ""))

    par_posterior_op.l

  }



#' @rdname encode_factors_as_numeric
#' @export
#'
join_posteriors_p_summaries_supervised_factor <- function(df, varname_factor, run_type, directory_operators) {

  #operator list element's position conveys data split ID, a by-var for join to df.
  op.l <- readRDS(paste(directory_operators, varname_factor, "_posteriors_p.rds", sep = ""))
  op <- bind_rows(
    lapply(1:length(op.l), function(x) op.l[[x]]$par_posterior_op %>% mutate(id_split_op = x))
  )

  varname_factor_ren <- paste("l_odds_p_summ_", varname_factor, sep = "")

  if (run_type == "train") {

    #data split ID corresponds to saved row indexes
    for (i in 1:length(op.l)) df[op.l[[i]]$idx_op, "id_split_op"] <- i

    df <- left_join(df, op[, c("id_split_op", varname_factor, "l_odds_p_summary")]) %>%
      dplyr::select(-c(id_split_op)) %>%
      rename_at(vars("l_odds_p_summary"), ~ varname_factor_ren)

  } else if (run_type == "test") {

    #data splits not relevant for 'test' run type.
    #test obs were not available at the time of proportion estimation.
    #so average over partition IDs.
    op <- op %>%
      group_by(!!rlang::sym(varname_factor)) %>%
      summarize(p_summary = mean(p_summary)) %>%
      ungroup() %>%
      mutate(l_odds_p_summary = log(p_summary / (1 - p_summary)))

    df <- left_join(df, op[, c(varname_factor, "l_odds_p_summary")]) %>%
      rename_at(vars("l_odds_p_summary"), ~ varname_factor_ren)

  }

  df

}



#' @rdname encode_factors_as_numeric
#' @export
#'
transform_posteriors_p_summaries_supervised_factor <-
  function(df, varname_factor, run_type, par_prior_beta, Y, directory_operators) {

    if (run_type == "train") {

      invisible(derive_posteriors_p_summaries_supervised_factor(
        df = df, varname_factor = varname_factor, par_prior_beta = par_prior_beta, Y = Y,
        directory_operators = directory_operators
      ))

    }

    df <- join_posteriors_p_summaries_supervised_factor(
      df = df, varname_factor = varname_factor, run_type = run_type,
      directory_operators = directory_operators
    )

    df

  }
