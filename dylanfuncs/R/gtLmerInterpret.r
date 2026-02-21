#' Function to print a pretty version of your model output
#'
#' This function prints the output of linear models in a pretty fashion using gt
#' @param model This is a model of class lm or lmerMod
#' @param ci If True, function will print confidence intervals around the estimates. Defaults to True. Can increase runtime semi-substantially
#' @param var Specify a variable name or vector of variable names for which to calculate confidence intervals. This can make things more efficient if you only need the CI of your main variable. Can be useful to run summary(model) first to see exactly what the variable name looks like in the coefficient table. For example, sometimes it will be "variableNameLevelName" i.e., variable name followed by level name if its a categorical variable.
#' @param or.only If true and the input model is of the class glmerMod, then only the odds ratios will be reported (i.e., not the associated coefficient estimate aka log odds)
#' @param r2 If true, model r-squared will be returned
#' @param alternate.if If true, confidence interval will be calculated using the formula: estimate ± 1.96 * standard error
#' @param row.names Can set this to a list of custom row names for prettier gt tables
#' @param rowname.dict Can provide a dictionary (in list form) that has names as the 'ugly'/messy names that appear in the gt output and the values as prettier versions.
#' @param return.table if True, then return an object that stores the table version (e.g., for csvs)


gtLmerInterpret <- function(model,
                            ci = T,
                            var = NULL,
                            or.only = F,
                            r2 = F,
                            alternate.ci = F,
                            row.names = NULL,
                            rowname.dict = NULL,
                            return.table = F) {

  while (!require(MuMIn)) {
    install.packages("MuMIn")
  }

  while (!require(Metrics)) {
    install.packages("Metrics")
  }

  while (!require(performance)) {
    install.packages("performance")
  }

  read_p = function(value, digits = 3) {
    if (value == 0) {
      newVal = paste0("<", 2e-16)
    } else if (value < 0.001) {
      newVal = format(value, scientific = T, digits = digits)
    } else {
      newVal = round(value, digits)
    }
    return(newVal)
  }

  confint_alternative = function(summary, class) {
    confint = matrix(
      ncol = 2,
      nrow = nrow(summary)
    )

    if (class == "lmerModLmerTest") {
      lowFun = function(x,y) {x - 1.96 * y}
      highFun = function(x,y) {x + 1.96 * y}
    } else if (class == "glmerMod") {
      lowFun = function(x,y) {exp(x - 1.96 * y)}
      highFun = function(x,y) {exp(x + 1.96 * y)}
    }

    confint[,1] = mapply(lowFun, x = summary[,1], y = summary[,2])

    confint[,2] = mapply(highFun, x = summary[,1], y = summary[,2])

    return(confint)
  }

  pullOutcome <- function(model) {
    if (class(model) == 'lmerModLmerTest') {
      call = as.character(model@call)
    } else if (class(model) == 'lm') {
      call = as.character(model$call)
    }

    split = strsplit(call[2], split = '')[[1]]
    tilde = which(split == '~')
    outcome = substr(call[2], 1, tilde-2)

    return(outcome)
  }

  # -- Stop the function if var is specified and alternate.ci is True
  if (!is.null(var) & alternate.ci == T) {
    stop("\n\nThis function does not support the calculation of specific vars using the alternate.ci function. However, I assure you that the alternate.ci method is fast enough such that there is no need to specify the specific variable for which to compute confidence interval")
  }

  summ <- summary(model)$coefficients

  if (class(model) == 'lm') {
    pcol = 4

  } else if (class(model) == 'lmerModLmerTest' | class(model) == 'lmerMod') {
    pcol = 5

  } else if (class(model) == "glmerMod") {
    pcol = 4
  }

  new <- data.frame(
    'Estimate' = round(summ[,1],3),
    'Std.Error' = round(summ[,2], 4),
    'P' = sapply(summ[,pcol], function(x) {read_p(x, digits = 3)})
  )
  new$`95CI` = rep("--", nrow(new))
  rownames(new) = rownames(summ)
  #new$P = ifelse(summ[,pcol] < 0.05, paste0(new$P, '*'), new$P)

  if (class(model) == "glmerMod") {
    new$OR = round(exp(summ[,1]), 3)
  }

  if (ci == T) {
    if (is.null(var)) {

        if (class(model) == "glmerMod") {

          if (alternate.ci == F) { ## If alternate.ci argument is/isnt used.
            ci <- exp(confint(model, parm = rownames(new)))
          } else if (alternate.ci == T) {
            ci <- confint_alternative(summary = summ, class = class(model))
          }

        } else { # if the model is not glmerMod

          if (alternate.ci == F) {
            ci <- confint(model, parm = rownames(new))
          } else if (alternate.ci == T) {
            ci <- confint_alternative(summary = summ, class = class(model))
          }
        }

      low95CI = round(as.numeric(ci[,1]), 3)
      high95CI = round(as.numeric(ci[,2]), 3)

      new$`95CI` = paste0("[", low95CI, ", ", high95CI, "]")


    } else { # if var is specified
      where = which(grepl(var, rownames(new)))

      exactVar = rownames(new)[where]

      if (class(model) == "glmerMod") {
        ci <- exp(confint(model, parm = exactVar))
      } else {
        ci <- confint(model, parm = exactVar)
      }

      low95CI = round(ci[,1], 3)
      high95CI =round(ci[,2], 3)

      new$`95CI`[where] = paste0("[", low95CI, ", ", high95CI, "]")
    }
  }

  if (class(model) == "glmerMod") {
    if (or.only == T) {
      new <- new[, c("OR", "P", "95CI")]
      colnames(new) = c("OR","P","95% CI")
    } else if (or.only == F) {
      new <- new[, c("Estimate", "Std.Error", "OR", "P", "95CI")]
      colnames(new) = c("Estimate","Std.Error","OR","P","95% CI")
    }
  } else {
    colnames(new) = c("Estimate","Std.Error","P","95% CI")
  }

  # if rowname.dict is not null create rownames
  if (!is.null(rowname.dict)) {

    if (class(rowname.dict) != "list") {
      stop("You've provided a rowname.dict in the incorrect format. It should be a list e.g., 'NAME' = 'VALUE'")
    }

    row.names=NULL

    check_dict = function(x) {
      if (x %in% names(rowname.dict)) {
        return(rowname.dict[[x]])
      } else {
        warning(x, " not found in rowname.dict")
        return(x)
      }
    }

    rownames(new) = sapply(rownames(new),
                           function(x) {
                             if (grepl(":", x, fixed = T)) {
                               split = strsplit(x, ":", fixed = T)[[1]]

                               vars = rep(NA, length(split))

                               for (i in 1:length(vars)) {
                                 vars[i] = check_dict(split[i])
                               }
                               val = paste(vars, collapse = " x ")

                               return(val)
                             } else {
                               check_dict(x)
                             }
                           })
  }

  if (!is.null(row.names)) {

    if (length(row.names) != nrow(new)) {

      if (length(row.names) == nrow(new)-1 & !any(grepl("(Intercept)", row.names))) {
        row.names = c("(Intercept)", row.names)
        message("Looks like you forgot the intercept in your row.names vector. Adding it to the beginning for you..")
        rownames(new) = row.names
      } else {

        stop("You have provided a new set of row.names, however, the vector you provided is not of the same length of the table. Please double check.")
      }

    } else {
      rownames(new) = row.names
    }
  }

  table <- gt(new, rownames_to_stub = T) %>%
    tab_style(style = cell_text(weight = 'bold'), locations = cells_column_labels()) %>%
    tab_style(style = cell_text(weight = 'bold'), locations = cells_stub()) %>%
    tab_header(title = paste0('Outcome = ', pullOutcome(model)))

  if (r2 == T) {

    if (class(model) == "glmerMod") {
      # calculate AUC
      dat = model@frame[,1]

      predicted = predict(model)

      auc = Metrics::auc(actual = dat, predicted = predicted)

      # calculate r2
      margrsquare = r.squaredGLMM(model)[1,1]
      condrsquare = r.squaredGLMM(model)[1,2]

      if (margrsquare == condrsquare) {
        condrsquare = NA
      }

      table <- table %>%
        tab_footnote(md(paste0("Marginal R<sup>2</sup> = ",
                        round(margrsquare, 3),
                        " / Conditional R<sup>2</sup> = ",
                        round(condrsquare, 3)))) %>%
        tab_footnote(paste0("AUC = ",
                      round(auc, 3)))

    } else if (class(model) == 'lmerModLmerTest' | class(model) == 'lmerMod') {
      margrsquare = r.squaredGLMM(model)[[1]]
      condrsquare = r.squaredGLMM(model)[[2]]

      table <- table %>%
        tab_footnote(md(paste0("Marginal R<sup>2</sup> = ",
                        round(margrsquare, 3),
                        " / Conditional R<sup>2</sup> = ",
                        round(condrsquare, 3))))
    } else if (class(model) == "lm") {
      rsquare = summary(model)$adj.r.squared

      table <- table %>%
        tab_footnote(md(paste0("Adjusted R<sup>2</sup> = ",
                        round(rsquare, 3))))
    }
  }

  ## Return table as table
  if (return.table == T) {
    print(table)
    return_list = list("gt_obj" = table,
                       "table_obj" = new
                      )
  } else {
    return(table)
  }



}
