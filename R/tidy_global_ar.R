

### quality checking:
# check format of columns (id, variable, value)
# all ids have the same variables
# within a id, the same length is required for each variable,
#  it can be forced to the interesection, but default it is a stop
#  no missing days inbetween the date range
#  no NAs

#sanity check, all end dates are the same
#sanity check if mutistep, check early if it is thet same as horizon, or just ignore horizon


# SERPARAR LAS FUNCIONES DE EMBEDDING PARA DAR FUNCIONALIDAD APARTE,
# POR EJEMLO PARA EXPERIMENTOS DE INSAMPLE LOSS
# MATCH NAMES PARE TO AVOID ERRORS WITH ARRANGING OF IDS AND VARIABLES AND DATES

#checl params_tbl is valid


library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)


embed_lag = function(x, lag, step_ahead) {
  stopifnot(length(x) > lag + step_ahead)
  stats::embed(x, lag + step_ahead)

}

calc_tgt_cols_in_multivar_embed = function(num_variables, lag, step_ahead) {
  ind_mat = sapply(1:num_variables, function(i) { #this ugly code to get the tgt cols in the multivariate matrix 1,2,3 - 21,22,23 - 41,42,43
    1:step_ahead + (i-1)*(lag+step_ahead)
  })
  as.vector(ind_mat)
}

#dset has the format id, variable, value
get_mvar_emb = function(dset, lag, step_ahead) {

  #get the ids and last date
  id_var_date = dset %>%
    arrange(id) %>%
    group_by(id,variable) %>%
    summarise(date=max(date), .groups="drop") %>%
    ungroup()
  #get the variable names for constructing the output
  var_names = id_var_date %>% select(variable) %>% unique() %>% arrange()
  num_variables = length(var_names$variable)

  if (0) { ##deprecated code, other branch is the optimized one

  #multivar AND multisep embedding from a tibble
  emb_list = dset %>% arrange(id) %>% group_by(id) %>%
    arrange(date, variable) %>%
    group_map( function(.x, .y) {
      Xvar = .x %>% group_by(variable) %>%
        group_map( function (.xvar, .yvar) { #embed each variable for each id and column join
          tseries = c(.xvar$value, rep(NA, step_ahead-1)) #pad with NA for the multi-step, so shorter horizon dont miss observations
          Xvar = embed_lag(tseries, lag, step_ahead)

        })
      do.call(cbind, Xvar)
    })

  X = do.call(rbind, emb_list)

  ###!!!! TO DO: do the fast embedding, not the rbind, but precreating X and Y matrices and filling them


  last_rows = sapply(emb_list, nrow) #get the position of the last observation in each series, it is the one used for forecasting
  #get the columns that will be used for forecasting, removing the last lag because the first is not the target
  last_cols = as.vector( sapply(1:num_variables, function(i) {(step_ahead - 1 + 1:lag) + (i-1)*(lag+step_ahead)}) )
  X_last = X[cumsum(last_rows), last_cols, drop=FALSE] #save the last observation, useful for forecasting

  #!!!!!clear memory just in case it gets rough
  #rm(emb_list);gc()
  y_cols = calc_tgt_cols_in_multivar_embed(num_variables, lag, step_ahead)
  Y = X[, y_cols, drop=FALSE]
  X = X[, -y_cols, drop=FALSE]
  } else {
    emb_list = dset %>% arrange(id) %>% group_by(id) %>%
      arrange(date, variable) %>%
      group_map( function(.X, .y) {
        .X %>% group_by(variable) %>%
          group_map( function (.xvar, .yvar) { #embed each variable for each id and column join
            c(.xvar$value, rep(NA, step_ahead-1)) #pad with NA for the multi-step, so shorter horizon dont miss observations
          })
      })
    lengths <- sapply(emb_list, function (x) length(x[[1]]))
    lengths <- lengths - lag

    #!!!!maybe precalc the size of the matrix and use hard drive for very large matrices

    X = matrix(0, nrow=sum(lengths), ncol=(lag) * num_variables)
    Y = matrix(0, nrow=sum(lengths), ncol=step_ahead*num_variables)

    X_last = matrix(0, nrow=length(emb_list), ncol=lag*num_variables)

    row_count = 1
    for (i in 1:length(emb_list)) {

      for (j in 1:length(emb_list[[i]])) {
        emb <- embed(emb_list[[i]][[j]], lag + step_ahead)
        X[row_count:(row_count + nrow(emb)-1), (j-1)*lag + 1:lag] <- emb[,-(1:step_ahead), drop=FALSE]
        Y[row_count:(row_count + nrow(emb)-1) , (j-1)*step_ahead + 1:step_ahead] <- emb[,1:step_ahead, drop=FALSE]
        X_last[i, (j-1)*lag + 1:lag] = emb[nrow(emb), (step_ahead - 1) + 1:lag, drop=FALSE]
      }
      row_count <- row_count + nrow(emb)

    }
  }

  colnames(Y) <- paste(rep(var_names$variable, each=step_ahead), "_hor_", step_ahead:1, sep="")
  colnames(X) <- colnames(X_last) <- paste(rep(var_names$variable, each=lag), "_lag_", 1:lag, sep="")

  list(X=X, Y=Y, id_var_date=id_var_date, X_last = X_last, lag=lag, step_ahead=step_ahead,
       num_variables = num_variables,
       var_names = var_names)
}


fit_fastlm = function(X, Y, lag, step_ahead, num_variables ) {

  COEF = do.call(cbind, lapply(1:(step_ahead*num_variables), function(i) {
    valid_rows = !is.na(Y[,i]) #for a given step ahead, remove the special masked row, no to lose info for shorter horizons
    RcppArmadillo::fastLmPure(X[valid_rows, , drop=FALSE],
                              Y[valid_rows,i])$coefficients
  }))

  COEF <- as.matrix(COEF)
  rownames(COEF) <- colnames(X)
  colnames(COEF) <- colnames(Y)
  COEF
}



#get predictions
forec_mvar <- function(COEF, X_last, horizon, lag, step_ahead, num_variables) {
  if (step_ahead == 1) { #recursive forec
    preds = X_last %*% COEF

    if (horizon > 1) {
      PRED = matrix(0, nrow(preds), horizon*ncol(preds))
      pos_in_PRED = seq(horizon, horizon*num_variables, horizon)
      PRED[, pos_in_PRED] = preds

      newpred = preds

      newpred_indic = seq(1, lag*num_variables, lag)
      X_tmp = X_last
      for (i in 2:horizon) {
        X_tmp[ , 2:ncol(X_tmp)] = X_tmp[ , 1:(ncol(X_tmp)-1), drop=FALSE ]
        X_tmp[, newpred_indic] = newpred
        newpred = X_tmp %*% COEF
        PRED[, pos_in_PRED - i + 1] = newpred
      }
      preds = PRED
    }
  } else { #multi step ahead
    if (step_ahead != horizon) {
      stop(paste("ERROR: Multi-step and Horizon dont match! Cannot do multi-step:", step_ahead, "with this model for horizon:", horizon))
    }

    preds = X_last %*% COEF
  }

  preds
}

preds_to_tibble <- function(preds, horizon, id_var_date, var_names, num_variables) {

  id_last_date = id_var_date %>% select(id, date) %>% distinct()
  #turn them into a tibble
  pred_dset = lapply(1:length(id_last_date$id),
                     function (i) tibble(id=id_last_date$id[i],
                                         variable=rep(var_names$variable, each=horizon),
                                         date = id_last_date$date[i] + rep(horizon:1, num_variables), #!dates are reversed because embed reverses
                                         value=preds[i,]))
  pred_dset = bind_rows(pred_dset) %>% arrange(id, date, variable)

}


predict_multivar_tbl <- function(dset, step_ahead, lag, horizon) {

  mvr_emb = get_mvar_emb(dset, lag, step_ahead)

  COEF <- fit_fastlm(mvr_emb$X, mvr_emb$Y, mvr_emb$lag, mvr_emb$step_ahead, mvr_emb$num_variables)

  preds <- forec_mvar(COEF, mvr_emb$X_last, horizon = horizon,
                      lag = mvr_emb$lag,
                      step_ahead = mvr_emb$step_ahead, num_variables = mvr_emb$num_variables)

  pred_dset = preds_to_tibble(preds, horizon=horizon,
                              id_var_date = mvr_emb$id_var_date,
                              var_names = mvr_emb$var_names,
                              num_variables =mvr_emb$num_variables)

  pred_dset
}

predict_univar_tbl <- function(dset, step_ahead, lag, horizon) {
  dset %>% group_by(variable) %>% group_modify(
    function (.x, .y)
      predict_multivar_tbl(mutate(.x, variable=.y$variable), step_ahead=step_ahead,
                       lag=lag, horizon=horizon) %>% select(-variable) ) %>%
    select(id, variable, date, value) %>% ungroup()
}




maselike_normtable <- function(dset) {
  dset %>% group_by(id, variable) %>%
    summarize(scale_type="mase",
              scale=mean(abs(diff(value))),
              .groups="drop")
}

max_normtable <- function(dset) {
  dset %>% group_by(id, variable) %>%
    summarize(scale_type="max",
              scale=max(value), .groups="drop")
}

diffmax_normtable <- function(dset) {
  dset %>% group_by(id, variable) %>%
    summarize(scale_type="diffmax",
              scale=max(abs(diff(value))), .groups="drop")
}

norm_by_table <- function(dset, table) {
  dset %>% left_join(table, by=c("id", "variable")) %>%
    group_by(id,variable) %>% mutate(value = value / scale) %>%
    ungroup() %>% select(-scale, -scale_type)
}

denorm_by_table <- function(dset, table) {
  dset %>% left_join(table, by=c("id", "variable")) %>%
    group_by(id,variable) %>% mutate(value = value * scale) %>%
    ungroup() %>% select(-scale, -scale_type)
}

flatbase_transform <- function(dset, base) {
  dset$value = dset$value + base
  dset
}

diff_transform <- function(dset) {
  dset %>% group_by(id, variable) %>%
    mutate(value = diff(c(0, value))) %>%
    ungroup()
}
dediff_transform <- function(dset) {
  dset %>% group_by(id, variable) %>%
    mutate(value = cumsum(value)) %>%
    ungroup()
}





rollorig_predict = function(dset, forec_dates, params_tbl) {
  max_normtable; #these are needed for parallelization, so they know the functions can be used
  maselike_normtable;
  diffmax_normtable;
  results_rollorig = NULL
  for (i in 1:nrow(params_tbl)) {
    param_set = params_tbl[i,]
    multi_var = param_set$multi_var
    lag = param_set$lag
    horizon = param_set$horizon
    norm_f = get(param_set$norm_type)
    multi_step = param_set$multi_step
    id_set = unlist(param_set$id_set)

    if (multi_step) {
      step_ahead = horizon
    } else {
      step_ahead = 1
    }

    for (forec_date in forec_dates) {
      forec_date = as_date(forec_date)
      train_set = dset %>% filter(date <= forec_date, id %in% id_set)
      norm_table = train_set %>% norm_f()
      forec_dset = train_set %>%
        norm_by_table(norm_table) %>%
        ifelse(multi_var, predict_multivar_tbl, predict_univar_tbl)(step_ahead=step_ahead,
                                                                    lag=lag,
                                                                    horizon=horizon)

      forec_dset = forec_dset %>% denorm_by_table(norm_table)
      results_rollorig = bind_rows(results_rollorig, tibble(forec_date = forec_date,
                                                    param_set,
                                                    forec_dset))
    }
  }
  results_rollorig
}

future_rollorig_predict = function(dset, forec_dates, params_tbl) {
  furrr::future_map(1:nrow(params_tbl), function(indi) { #we use indices to simplify: the id_set col are lists
    param_set = tibble(params_tbl[indi,])
    rollorig_predict(dset, forec_dates, param_set)},
    .options = furrr::furrr_options(seed = 123)) %>% bind_rows()
}



#!!!! TO DO !!!!
#spain: add italy covariates
#byrow normalization
#general model classes, external features
#efficient bigdata, wide format, chunking, keras
#unfiying frequency


add_forec_errors <- function(dset, ground_truth) {
  dset %>% inner_join(ground_truth, by=c("id", "variable", "date"),
                      suffix=c("", "_true")) %>%
    mutate(MAE = (abs(value - value_true)),
              MAPE = (abs(value - value_true) / (abs(value_true)+1)) )

}

#weekly aggregation for the covid forecast hub and others
#sum, week starting in sundays, remove incomplete weeks
epiweek_aggreg = function(dset) {
  dset %>%
    group_by(id, variable, date = floor_date(date, "week", week_start = 7)+6) %>%
    filter(n() ==7) %>%
    summarize(value = sum(value), .groups="drop") %>%
    ungroup()
}

pad_zeros = function(dset, start_date) {

  dset %>% group_by(id,variable) %>%
    group_modify( function (.x, .y) {
      min_date = min(.x$date)
      if (min_date > start_date) {
        date_range = seq(as_date(start_date), min_date-1, by="days")
        .x = tibble(date=as_date(date_range),
                    value=0) %>% bind_rows( .x)
      }
      .x
    }) %>% ungroup()
}





covid_clean_anomalies = function(dset) {

  clean_x = function(x) {
    #set negatives to zero
    x[x < 0] = 0

    #remove one day peaks
    for (i in 8:(length(x)-8)) {
      is_peak = x[i] > sum(x[i-  1:7]) && x[i] > sum(x[i +1:7])
      if (is_peak) {
        weights = c(1:8, 7:1)
        weights = weights / sum(weights)
        x[i] = sum(x[i + (-7):7] * weights)
      }
    }

    #remove big swing days
    for (i in 1:(length(x)-1)) {
      is_peak = x[i +1] > 5*x[i]
      if (is_peak) {
        x[i] = x[i] + x[i+1]*0.25
        x[i+1] = 0.75*x[i+1]
      } else {
        is_low = x[i +1] < 0.2*x[i]
        if (is_low) {
          x[i+1] = x[i+1] + x[i]*0.25
          x[i] = 0.75*x[i]
        }
      }
    }
    x
  }

  dset %>%
    group_by(id, variable) %>%
    mutate(value = clean_x(value)) %>%
    ungroup()

}



#function that does forecasting on the given set of dates
#for the hyperparameter combination given (lag order, normalization type, dataset (external series))
#then the errors are calculated against a ground truth
#and a forecast combination of the best univariate and multivariate are calculated
#the output are the forecast at the dates given
#the summary of the best hyperparam for each variable and their error (according to MAE)
#and the forecast for only the best for each variable, at the later date given as input

future_val_pred = function(train_dset, forec_dates, ground_truth_dset, params_tbl) {

  #(quick/dirty) code to randomly access the params table, to better share the load among parallel workers
  #http://www.cookbook-r.com/Numbers/Saving_the_state_of_the_random_number_generator/
  if (exists(".Random.seed", .GlobalEnv)) {
    oldseed <- .GlobalEnv$.Random.seed
  } else {
    oldseed <- NULL
  }
  set.seed(1234)
  shuff_params_tbl = params_tbl[sample(nrow(params_tbl)),]

  if (!is.null(oldseed)) {
    .GlobalEnv$.Random.seed <- oldseed
  } else {
    rm(".Random.seed", envir = .GlobalEnv)
  }


  ro_forec_dset = future_rollorig_predict(train_dset, forec_dates, shuff_params_tbl)


  errs = ro_forec_dset %>%
    add_forec_errors(ground_truth_dset) %>%
    filter(forec_date %in% forec_dates)

  #best per variable and multi/uni variate
  best_unimulti = errs %>% group_by(lag, multi_var, multi_step,
                                    horizon, norm_type, id_set, variable) %>%
    summarize(avg_MAE = mean(MAE), .groups="drop") %>%
    group_by(variable, multi_var) %>% top_n(1, -avg_MAE) %>% ungroup()

  #forecast combination of the best uni-multi, per variable
  best_combi_forec = ro_forec_dset %>%
    inner_join(best_unimulti, by=c("lag", "multi_var", "multi_step",
                                   "horizon", "norm_type",
                                   "id_set", "variable")) %>%
    select(-avg_MAE) %>%
    group_by(forec_date, id, variable,  date) %>%
    summarize(value = mean(value), .groups="drop") %>%
    mutate(combi=TRUE) %>% ungroup()


  #error of the best uni-multi combination
  best_combi_err = best_combi_forec %>%
    add_forec_errors(ground_truth_dset) %>%
    group_by(variable) %>% summarize(avg_MAE = mean(MAE))

  best_final = best_unimulti %>%
    mutate(combi=FALSE) %>%
    bind_rows( best_combi_err %>% mutate(combi=TRUE) ) %>%
    group_by(variable) %>%
    top_n(1, -avg_MAE) %>%
    ungroup()

  all_ro_forec_dset = ro_forec_dset %>% mutate(combi=FALSE) %>%
    bind_rows(best_combi_forec)

  best_forec = all_ro_forec_dset %>%
    inner_join(best_final, by=c("lag", "multi_var",
                                "multi_step", "horizon",
                                "norm_type", "id_set",
                                "variable", "combi")) %>%
    select(-avg_MAE)

  best_forec = best_forec %>% group_by(id) %>%
    filter(forec_date == max(forec_date)) %>% ungroup()

  #return the table of best models
  #the best forecasts for the last date
  #all forecasts including the older

  list(rollor_forec = all_ro_forec_dset,
       best_table = best_final,
       best_forec = best_forec)
}
