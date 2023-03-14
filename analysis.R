library(mice)
library(dummies)
library(MatchIt)
library(tableone)
library(survival)


classifyVar <- function (df) {
  numeric_vars <- c()
  categorical_vars <- c()
  other_vars <- c()
  for (var in names(df)) {
    if (class(df[[var]]) %in% c("numeric", "integer", "logical")) {
      if (length(na.omit(unique(df[[var]]))) < 10) {
        categorical_vars <- c(categorical_vars, var)
      }
      else {
        numeric_vars <- c(numeric_vars, var)
      }
    }
    else {
      other_vars <- c(other_vars, var)
    }
  }
  var_class <- list(numeric=numeric_vars, categorical=categorical_vars, other=other_vars)
  return (var_class)
}

checkNorm <- function (df, vars) {
  norm_vars <- c()
  for (var in vars) {
    if (shapiro.test(na.omit(df[[var]]))$p.value >= 0.05) {
      norm_vars <- c(norm_vars, var)
    }
  }
  return (norm_vars)
}

diagnosis <- function (df, var_class, normal_vars) {
  missing <- c()
  data_type <- c()
  options <- c()
  range <- c()
  is_normal <- c()
  for (var in names(df)) {
    n_miss <- dim(df)[1] - sum(! is.na(df[[var]]))
    miss_percent <- round(n_miss / dim(df)[1] * 100)
    missing <- c(missing, paste0(n_miss, " (", miss_percent, "%)"))
    if (var %in% var_class$numeric) {
      data_type <- c(data_type, "Numeric")
      range <- c(range, paste0(min(df[[var]], na.rm=TRUE), " - ",
                               median(df[[var]], na.rm=TRUE), " - ",
                               max(df[[var]], na.rm=TRUE)))
      options <- c(options, "")
      if (var %in% normal_vars) {
        is_normal <- c(is_normal, "Yes")
      } else {
        is_normal <- c(is_normal, "No")
      }
    } 
    else {
      if (var %in% var_class$categorical) {
        data_type <- c(data_type, "Categorical")
      }
      else {
        data_type <- c(data_type, "Other")
      }
      unique_opts <- sort(unique(na.omit(df[[var]])))
      if (length(unique_opts) > 5) {
        options <- c(options, paste(paste(unique_opts[1:5], collapse=","), "..."))
      }
      else {
        options <- c(options, paste(unique_opts, collapse=","))
      }
      range <- c(range, "")
      is_normal <- c(is_normal, "")
    }
  }
  result <- data.frame(names(df), missing, data_type, options, range, is_normal)
  names(result) <- c("Variable", "Missing", "Data type", "Options", "Range (min-median-max)", "Normal distribution")
  return (result)
}

simpleImpute <- function(df, var_class, norm_vars) {
  for (var in names(df)) {
    if (var %in% var_class$numeric) {
      if (var %in% norm_vars) {
        df[[var]][is.na(df[[var]])] <- mean(df[[var]], na.rm=TRUE)
      }
      else {
        df[[var]][is.na(df[[var]])] <- median(df[[var]], na.rm=TRUE)
      }
    }
    else if (var %in% var_class$categorical) {
      df[[var]][is.na(df[[var]])] <- names(sort(-table(na.omit(df[[var]]))))[1]
    }
  }
  return (df)
}

multipleImpute <- function(df, vars) {
  imp <- mice(df[vars], m=3, seed=66)
  new_df <- cbind(complete(imp), df[-which(colnames(df) %in% vars)])[names(df)]
  return (new_df)
}

splitNumeric <- function(df, var, breaks) {
  breaks <- as.numeric(strsplit(breaks, ",")[[1]])
  labels <- c(paste("<=", breaks[1]))
  if (length(breaks) > 1) {
    for (i in 1:(length(breaks)-1)) {
      labels <- c(labels, paste(breaks[i], "-", breaks[i + 1]))
    }
  }
  labels <- c(labels, paste(">", breaks[length(breaks)]))
  breaks <- c(-Inf, breaks, Inf)
  new_var <- paste0(var, "_cut")
  df[new_var] <- cut(df[[var]], breaks, labels)
  output <- list(new_var=new_var, df=df)
  return (output)
}

mapOption <- function(df, var) {
  options <- sort(unique(na.omit(df[[var]])))
  new_var <- paste0(var, "_std")
  df[new_var] <- factor(df[[var]], levels=options, ordered=TRUE)
  df[new_var] <- as.integer(df[[new_var]]) - 1
  output <- list(new_var=new_var, df=df)
  return (output)
}

setDummy <- function(df, categorical_vars) {
  refers <- list()
  dum_vars <- list()
  for (var in categorical_vars) {
    df[[var]][is.na(df[[var]])] <- "NA"
    dmy <- dummy(var, df, sep="_")
    df <- cbind(df, dmy[, 2:ncol(dmy)])
    refers[[var]] <- colnames(dmy)[1]
    dum_vars[[var]] <- colnames(dmy)[2: ncol(dmy)]
  }
  output <- list(df=df, refers=refers, dum_vars=dum_vars)
  return (output)
}

baseline <- function(df, group, vars, categorical_vars, nonnormal_vars) {
  table1 <- CreateTableOne(data=df, vars=vars, factorVars=categorical_vars, strata=group, addOverall=TRUE)
  table1_preview <- print(table1, showAllLevels=TRUE, nonnormal=nonnormal_vars)
  return (table1_preview)
}

findInbalance <- function(table1, vars) {
  alpha <- 0.05
  inbalance_vars <- c()
  for (var in vars) {
    if (grepl("<", table1[var, "p"])) {
      inbalance_vars <- c(inbalance_vars, var)
    }
    else if (!is.na(as.numeric(table1[var, "p"]))) {
      if (as.numeric(table1[var, "p"]) < alpha) {
        inbalance_vars <- c(inbalance_vars, var)
      }
    }
  }
  return (inbalance_vars)
}

psm <- function(df, table1, group, vars) {
  inbalance_vars <- findInbalance(table1, vars)
  subdf <- na.omit(df[c(group, inbalance_vars)])
  if (length(inbalance_vars) > 0) {
    f <- as.formula(paste(group, "~", paste(inbalance_vars, collapse="+")))
    psm <- matchit(f, data=subdf, method="nearest", ratio=1)
    match_data <- match.data(psm)
    output <- merge(df[setdiff(names(df), inbalance_vars)], match_data[inbalance_vars], by="row.names", all=FALSE)[names(df)]
    return (output)
  }
  else {
    return (df)
  }
}

associationAnalysis <- function(df, group, outcome, confounder=c(), model) {
  valid_vars <- c(group, outcome, confounder)
  subdata <- na.omit(df[valid_vars])
  if (model %in% c("linear", "possion")) {
    group_mean <- aggregate(subdata[[outcome]], by=list(group=subdata[[group]]), mean)
    group_std <- aggregate(subdata[[outcome]], by=list(group=subdata[[group]]), sd)
    group_median <- aggregate(subdata[[outcome]], by=list(group=subdata[[group]]), median)
    group_iqr <- aggregate(subdata[[outcome]], by=list(group=subdata[[group]]), FUN=quantile, probs=c(0.25, 0.75))
    group_mean_std <- paste(round(group_mean[["x"]], digit=2), round(group_mean[["x"]], digit=2), sep="±")
    group_median_iqr <- paste0(round(group_median[["x"]], digit=2), " [", round(group_iqr[["x"]][,1], digit=2), ", ",  round(group_iqr[["x"]][,2], digit=2), "]")
    crosstab_preview <- data.frame(mean_std=group_mean_std, median_iqr=group_median_iqr)
    rownames(crosstab_preview) <- paste(paste0(group, ":"), group_mean[["group"]])
  }
  else if (model %in% c("logistic")) {
    crosstab <- as.data.frame.array(table(subdata[[group]], subdata[[outcome]]))
    rownames(crosstab) <- paste(paste0(group, ":"), rownames(crosstab))
    colnames(crosstab) <- paste(paste0(outcome, ":"), colnames(crosstab))
    n_group <- rowSums(crosstab)
    crosstab_preview <- data.frame(matrix(nrow=nrow(crosstab), ncol=ncol(crosstab)))
    rownames(crosstab_preview) <- rownames(crosstab)
    colnames(crosstab_preview) <- colnames(crosstab)
    for (var in rownames(crosstab)) {
      percent <- round(crosstab[var,]/n_group[[var]]*100, digit=1)
      crosstab_preview[var,] <- paste0(paste0(crosstab[var,], "/", n_group[[var]]), " (", percent, ")")
    }
  }
  f <- as.formula(paste(outcome, "~", group))
  if (model == "linear") {
    fit <- lm(f, data=subdata)
    or <- round(coef(fit)[[group]], digit=2)
    ci95 <- paste(round(confint(fit, group), digit=2), collapse="~")
    pvalue <- round(summary(fit)$coefficients[group, 4], digit=2)
  }
  else if (model == "logistic") {
    fit <- glm(f, data=subdata, family=binomial(link="logit"))
    or <- round(exp(coef(fit)[[group]]), digit=2)
    ci95 <- paste(round(exp(confint(fit, group)), digit=2), collapse="~")
    pvalue <- round(summary(fit)$coefficients[group, 4], digit=2)
  }
  else if (model == "possion") {
    fit <- glm(f, data=subdata, family=poisson(link="log"))
    or <- round(exp(coef(fit)[[group]]), digit=2)
    ci95 <- paste(round(exp(confint(fit, group)), digit=2), collapse="~")
    pvalue <- round(summary(fit)$coefficients[group, 4], digit=2)
  }
  if (is.null(confounder)) {
    result <- data.frame(matrix(nrow=nrow(crosstab_preview), ncol=3))
    colnames(result) <- c("OR", "95% CI", "p value")
    result[nrow(result): nrow(result),] <- c(or, ci95, pvalue)
  }
  else {
    f <- as.formula(paste(outcome, "~", paste(c(group, confounder), collapse="+")))
    if (model == "linear") {
      fit <- lm(f, data=subdata)
      adjust_or <- round(coef(fit)[[group]], digit=2)
      adjust_ci95 <- paste(round(confint(fit, group), digit=2), collapse="~")
      adjust_pvalue <- round(summary(fit)$coefficients[group, 4], digit=2)
    }
    else if (model == "logistic") {
      fit <- glm(f, data=subdata, family=binomial(link="logit"))
      adjust_or <- round(exp(coef(fit)[[group]]), digit=2)
      adjust_ci95 <- paste(round(exp(confint(fit, group)), digit=2), collapse="~")
      adjust_pvalue <- round(summary(fit)$coefficients[group, 4], digit=2)
    }
    else if (model == "possion") {
      fit <- glm(f, data=subdata, family=poisson(link="log"))
      adjust_or <- round(exp(coef(fit)[[group]]), digit=2)
      adjust_ci95 <- paste(round(exp(confint(fit, group)), digit=2), collapse="~")
      adjust_pvalue <- round(summary(fit)$coefficients[group, 4], digit=2)
    }
    result <- data.frame(matrix(nrow=nrow(crosstab_preview), ncol=6))
    colnames(result) <- c("Crude OR", "Crude 95% CI", "Crude p value", "Adjusted OR", "Adjusted 95% CI", "Adjusted p value")
    result[nrow(result): nrow(result),] <- c(or, ci95, pvalue, adjust_or, adjust_ci95, adjust_pvalue)
  }
  result <- cbind(crosstab_preview, result)
  return (result)
}

dummyAssociationAnalysis <- function(df, group_refer, group_dummies, outcome, confounder=c(), model) {
  valid_vars <- c(names(group_dummies), group_dummies[[1]], outcome, confounder)
  subdata <- na.omit(df[valid_vars])
  if (model %in% c("linear", "possion")) {
    group_mean <- aggregate(subdata[[outcome]], by=list(group=subdata[[names(group_dummies)]]), mean)
    group_std <- aggregate(subdata[[outcome]], by=list(group=subdata[[names(group_dummies)]]), sd)
    group_median <- aggregate(subdata[[outcome]], by=list(group=subdata[[names(group_dummies)]]), median)
    group_iqr <- aggregate(subdata[[outcome]], by=list(group=subdata[[names(group_dummies)]]), FUN=quantile, probs=c(0.25, 0.75))
    group_mean_std <- paste(round(group_mean[["x"]], digit=2), round(group_mean[["x"]], digit=2), sep="±")
    group_median_iqr <- paste0(round(group_median[["x"]], digit=2), " [", round(group_iqr[["x"]][,1], digit=2), ", ",  round(group_iqr[["x"]][,2], digit=2), "]")
    crosstab_preview <- data.frame(mean_std=group_mean_std, median_iqr=group_median_iqr)
    rownames(crosstab_preview) <- paste0(paste0(names(group_dummies), "_"), group_mean[["group"]])
  }
  else if (model %in% c("logistic")) {
    crosstab <- as.data.frame.array(table(subdata[[names(group_dummies)]], subdata[[outcome]]))
    rownames(crosstab) <- paste0(paste0(names(group_dummies), "_"), rownames(crosstab))
    colnames(crosstab) <- paste(paste0(outcome, ":"), colnames(crosstab))
    n_group <- rowSums(crosstab)
    crosstab_preview <- data.frame(matrix(nrow=nrow(crosstab), ncol=ncol(crosstab)))
    rownames(crosstab_preview) <- rownames(crosstab)
    colnames(crosstab_preview) <- colnames(crosstab)
    for (var in rownames(crosstab)) {
      percent <- round(crosstab[var,]/n_group[[var]]*100, digit=1)
      crosstab_preview[var,] <- paste0(paste0(crosstab[var,], "/", n_group[[var]]), " (", percent, ")")
    }
  }
  f <- as.formula(paste(outcome, "~", paste(group_dummies[[1]], collapse="+")))
  stat_res <- list()
  if (model == "linear") {
    fit <- lm(f, data=subdata)
    for (var in setdiff(group_dummies[[1]], group_refer)) {
      if (var %in% rownames(summary(fit)$coefficients) & !is.na(coef(fit)[[var]])) {
        or <- round(coef(fit)[[var]], digit=2)
        ci95 <- paste(round(confint(fit, var), digit=2), collapse="~")
        pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
        stat_res[[var]] <- c(or, ci95, pvalue)
      }
    }
  }
  else if (model == "logistic") {
    fit <- glm(f, data=subdata, family=binomial(link="logit"))
    for (var in setdiff(group_dummies[[1]], group_refer)) {
      if (var %in% rownames(summary(fit)$coefficients) & !is.na(coef(fit)[[var]])) {
        or <- round(exp(coef(fit)[[var]]), digit=2)
        ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
        pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
        stat_res[[var]] <- c(or, ci95, pvalue)
      }
    }
  }
  else if (model == "possion") {
    fit <- glm(f, data=subdata, family=poisson(link="log"))
    for (var in setdiff(group_dummies[[1]], group_refer)) {
      if (var %in% rownames(summary(fit)$coefficients) & !is.na(coef(fit)[[var]])) {
        or <- round(exp(coef(fit)[[var]]), digit=2)
        ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
        pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
        stat_res[[var]] <- c(or, ci95, pvalue)
      }
    }
  }
  if (is.null(confounder)) {
    result <- data.frame(matrix(nrow=nrow(crosstab_preview), ncol=3))
    colnames(result) <- c("OR", "95% CI", "p value")
    rownames(result) <- rownames(crosstab_preview)
    for (var in setdiff(names(stat_res), group_refer)) {
      result[var,] <- stat_res[[var]]
    }
  }
  else {
    f <- as.formula(paste(outcome, "~", paste(c(group_dummies[[1]], confounder), collapse="+")))
    adjust_stat_res <- list()
    if (model == "linear") {
      fit <- lm(f, data=subdata)
      for (var in setdiff(group_dummies[[1]], group_refer)) {
        if (var %in% rownames(summary(fit)$coefficients) & !is.na(coef(fit)[[var]])) {
          or <- round(coef(fit)[[var]], digit=2)
          ci95 <- paste(round(confint(fit, var), digit=2), collapse="~")
          pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
          adjust_stat_res[[var]] <- c(or, ci95, pvalue)
        }
      }
    }
    else if (model == "logistic") {
      fit <- glm(f, data=subdata, family=binomial(link="logit"))
      for (var in setdiff(group_dummies[[1]], group_refer)) {
        if (var %in% rownames(summary(fit)$coefficients) & !is.na(coef(fit)[[var]])) {
          or <- round(exp(coef(fit)[[var]]), digit=2)
          ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
          pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
          adjust_stat_res[[var]] <- c(or, ci95, pvalue)
        }
      }
    }
    else if (model == "possion") {
      fit <- glm(f, data=subdata, family=poisson(link="log"))
      for (var in setdiff(group_dummies[[1]], group_refer)) {
        if (var %in% rownames(summary(fit)$coefficients) & !is.na(coef(fit)[[var]])) {
          or <- round(exp(coef(fit)[[var]]), digit=2)
          ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
          pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
          adjust_stat_res[[var]] <- c(or, ci95, pvalue)
        }
      }
    }
    result <- data.frame(matrix(nrow=nrow(crosstab_preview), ncol=6))
    colnames(result) <- c("Crude OR", "Crude 95% CI", "Crude p value", "Adjusted OR", "Adjusted 95% CI", "Adjusted p value")
    rownames(result) <- rownames(crosstab_preview)
    for (var in setdiff(names(stat_res), group_refer)) {
      result[var,] <- c(stat_res[[var]], adjust_stat_res[[var]])
    }
  }
  result <- cbind(crosstab_preview, result)
  return (result)
}

survivalAnalysis <- function(df, group, survtime, survevent, confounder=c(), model) {
  valid_vars <- c(group, survtime, survevent, confounder)
  subdata <- na.omit(df[valid_vars])
  if (model %in% c("cox")) {
    crosstab <- as.data.frame.array(table(subdata[[group]], subdata[[survevent]]))
    rownames(crosstab) <- paste(paste0(group, ":"), rownames(crosstab))
    colnames(crosstab) <- paste(paste0(survevent, ":"), colnames(crosstab))
    n_group <- rowSums(crosstab)
    crosstab_preview <- data.frame(matrix(nrow=nrow(crosstab), ncol=ncol(crosstab)))
    rownames(crosstab_preview) <- rownames(crosstab)
    colnames(crosstab_preview) <- colnames(crosstab)
    for (var in rownames(crosstab)) {
      percent <- round(crosstab[var,]/n_group[[var]]*100, digit=1)
      crosstab_preview[var,] <- paste0(paste0(crosstab[var,], "/", n_group[[var]]), " (", percent, ")")
    }
  }
  if (model == "cox") {
    f <- as.formula(paste("Surv(", survtime, ",", survevent, ") ~ ", group))
    fit <- coxph(f, data=subdata)
    hr <- round(exp(coef(fit)[[group]]), digit=2)
    ci95 <- paste(round(exp(confint(fit, group)), digit=2), collapse="~")
    pvalue <- round(summary(fit)$coefficients[group, 4], digit=2)
  }
  if (is.null(confounder)) {
    result <- data.frame(matrix(nrow=nrow(crosstab_preview), ncol=3))
    colnames(result) <- c("HR", "95% CI", "p value")
    result[nrow(result): nrow(result),] <- c(hr, ci95, pvalue)
  }
  else {
    if (model == "cox") {
      f <- as.formula(paste("Surv(", survtime, ",", survevent, ") ~ ", paste(c(group, confounder), collapse="+")))
      fit <- coxph(f, data=subdata)
      adjust_hr <- round(exp(coef(fit)[[group]]), digit=2)
      adjust_ci95 <- paste(round(exp(confint(fit, group)), digit=2), collapse="~")
      adjust_pvalue <- round(summary(fit)$coefficients[group, 4], digit=2)
    }
    result <- data.frame(matrix(nrow=nrow(crosstab_preview), ncol=6))
    colnames(result) <- c("Crude HR", "Crude 95% CI", "Crude p value", "Adjusted HR", "Adjusted 95% CI", "Adjusted p value")
    result[nrow(result): nrow(result),] <- c(hr, ci95, pvalue, adjust_hr, adjust_ci95, adjust_pvalue)
  }
  result <- cbind(crosstab_preview, result)
  return (result)
}

dummySurvivalAnalysis <- function(df, group_refer, group_dummies, survtime, survevent, confounder=c(), model) {
  valid_vars <- c(names(group_dummies), group_dummies[[1]], survtime, survevent, confounder)
  subdata <- na.omit(df[valid_vars])
  if (model %in% c("cox")) {
    crosstab <- as.data.frame.array(table(subdata[[names(group_dummies)]], subdata[[survevent]]))
    rownames(crosstab) <- paste0(paste0(names(group_dummies), "_"), rownames(crosstab))
    colnames(crosstab) <- paste(paste0(survevent, ":"), colnames(crosstab))
    n_group <- rowSums(crosstab)
    crosstab_preview <- data.frame(matrix(nrow=nrow(crosstab), ncol=ncol(crosstab)))
    rownames(crosstab_preview) <- rownames(crosstab)
    colnames(crosstab_preview) <- colnames(crosstab)
    for (var in rownames(crosstab)) {
      percent <- round(crosstab[var,]/n_group[[var]]*100, digit=1)
      crosstab_preview[var,] <- paste0(paste0(crosstab[var,], "/", n_group[[var]]), " (", percent, ")")
    }
  }
  stat_res <- list()
  if (model == "cox") {
    f <- as.formula(paste("Surv(", survtime, ",", survevent, ") ~ ", paste(group_dummies[[1]], collapse="+")))
    fit <- coxph(f, data=subdata)
    for (var in setdiff(group_dummies[[1]], group_refer)) {
      if (var %in% rownames(summary(fit)$coefficients) & !is.na(coef(fit)[[var]])) {
        hr <- round(exp(coef(fit)[[var]]), digit=2)
        ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
        pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
        stat_res[[var]] <- c(hr, ci95, pvalue)
      }
    }
  }
  if (is.null(confounder)) {
    result <- data.frame(matrix(nrow=nrow(crosstab_preview), ncol=3))
    colnames(result) <- c("HR", "95% CI", "p value")
    rownames(result) <- rownames(crosstab_preview)
    for (var in setdiff(names(stat_res), group_refer)) {
      result[var,] <- stat_res[[var]]
    }
  }
  else {
    adjust_stat_res <- list()
    if (model == "COX") {
      f <- as.formula(paste("Surv(", survtime, ",", survevent, ") ~ ", paste(c(group_dummies[[1]], confounder), collapse="+")))
      fit <- coxph(f, data=subdata)
      for (var in setdiff(group_dummies[[1]], group_refer)) {
        if (var %in% rownames(summary(fit)$coefficients) & !is.na(coef(fit)[[var]])) {
          hr <- round(exp(coef(fit)[[var]]), digit=2)
          ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
          pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
          adjust_stat_res[[var]] <- c(hr, ci95, pvalue)
        }
      }
    }
    result <- data.frame(matrix(nrow=nrow(crosstab_preview), ncol=6))
    colnames(result) <- c("Crude HR", "Crude 95% CI", "Crude p value", "Adjusted HR", "Adjusted 95% CI", "Adjusted p value")
    rownames(result) <- rownames(crosstab_preview)
    for (var in setdiff(names(stat_res), group_refer)) {
      result[var,] <- c(stat_res[[var]], adjust_stat_res[[var]])
    }
  }
  result <- cbind(crosstab_preview, result)
  return (result)
}

associationStratification <- function(df, group, outcome, confounder=c(), stratas, model) {
  output <- data.frame()
  for (strata in stratas) {
    options <- sort(unique(na.omit(df[[strata]])))
    for (option in options) {
      subdata <- df[which(df[[strata]]==option),]
      if (nrow(na.omit(subdata[c(group, outcome, confounder)])) > 0) {
        res <- associationAnalysis(subdata, group, outcome, confounder, model)
        source_df <- data.frame(matrix(nrow=nrow(res), ncol=2))
        source_df[1,1] <- paste0(strata, ": ", option)
        source_df[,2] <- rownames(res)
        colnames(source_df) <- c("Subgroup", "Group")
        res <- cbind(source_df, res)
        rownames(res) <- NULL
        output <- rbind(output, res)
      }
    }
  }
  return (output)
}

dummyAssociationStratification <- function(df, group_refer, group_dummies, outcome, confounder=c(), stratas, model) {
  output <- data.frame()
  for (strata in stratas) {
    options <- sort(unique(na.omit(df[[strata]])))
    for (option in options) {
      subdata <- df[which(df[[strata]]==option),]
      if (nrow(na.omit(subdata[c(names(group_dummies), group_dummies[[1]], outcome, confounder)])) > 0) {
        res <- dummyAssociationAnalysis(subdata, group_refer, group_dummies, outcome, confounder, model)
        source_df <- data.frame(matrix(nrow=nrow(res), ncol=2))
        source_df[1,1] <- paste0(strata, ": ", option)
        source_df[,2] <- rownames(res)
        colnames(source_df) <- c("Subgroup", "Group")
        res <- cbind(source_df, res)
        rownames(res) <- NULL
        output <- rbind(output, res)
      }
    }
  }
  return (output)
}

survivalStratification <- function(df, group, survtime, survevent, confounder=c(), stratas, model) {
  output <- data.frame()
  for (strata in stratas) {
    options <- sort(unique(na.omit(df[[strata]])))
    for (option in options) {
      subdata <- df[which(df[[strata]]==option),]
      if (nrow(na.omit(subdata[c(group, survevent, confounder)])) > 0) {
        res <- survivalAnalysis(subdata, group, survtime, survevent, confounder, model)
        source_df <- data.frame(matrix(nrow=nrow(res), ncol=2))
        source_df[1,1] <- paste0(strata, ": ", option)
        source_df[,2] <- rownames(res)
        colnames(source_df) <- c("Subgroup", "Group")
        res <- cbind(source_df, res)
        rownames(res) <- NULL
        output <- rbind(output, res)
      }
    }
  }
  return (output)
}

dummySurvivalStratification <- function(df, group_refer, group_dummies, survtime, survevent, confounder=c(), stratas, model) {
  output <- data.frame()
  for (strata in stratas) {
    options <- sort(unique(na.omit(df[[strata]])))
    for (option in options) {
      subdata <- df[which(df[[strata]]==option),]
      if (nrow(na.omit(subdata[c(names(group_dummies), group_dummies[[1]], survevent, confounder)])) > 0) {
        res <- dummySurvivalAnalysis(subdata, group_refer, group_dummies, survtime, survevent, confounder, model)
        source_df <- data.frame(matrix(nrow=nrow(res), ncol=2))
        source_df[1,1] <- paste0(strata, ": ", option)
        source_df[,2] <- rownames(res)
        colnames(source_df) <- c("Subgroup", "Group")
        res <- cbind(source_df, res)
        rownames(res) <- NULL
        output <- rbind(output, res)
      }
    }
  }
  return (output)
}

univariateRegression <- function(df, outcome, vars, confounder=c(), model) {
  output <- data.frame()
  for (var in vars) {
    valid_vars <- c(outcome, var, confounder)
    subdata <- na.omit(df[valid_vars])
    f <- as.formula(paste(outcome, "~", var))
    if (model == "linear") {
      fit <- lm(f, data=subdata)
      or <- round(coef(fit)[[var]], digit=2)
      ci95 <- paste(round(confint(fit, var), digit=2), collapse="~")
      lower95 <- round(confint(fit, var)[, "2.5 %"], digit=2)
      upper95 <- round(confint(fit, var)[, "97.5 %"], digit=2)
      pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
    }
    else if (model == "logistic") {
      fit <- glm(f, data=subdata, family=binomial(link="logit"))
      or <- round(exp(coef(fit)[[var]]), digit=2)
      ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
      lower95 <- round(exp(confint(fit, var)[["2.5 %"]]), digit=2)
      upper95 <- round(exp(confint(fit, var)[["97.5 %"]]), digit=2)
      pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
    }
    else if (model == "possion") {
      fit <- glm(f, data=subdata, family=poisson(link="log"))
      or <- round(exp(coef(fit)[[var]]), digit=2)
      ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
      lower95 <- round(exp(confint(fit, var)[["2.5 %"]]), digit=2)
      upper95 <- round(exp(confint(fit, var)[["97.5 %"]]), digit=2)
      pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
    }
    if (is.null(confounder)) {
      result <- data.frame(matrix(nrow=1, ncol=5))
      colnames(result) <- c("OR", "95% CI", "lower95", "upper95", "p value")
      rownames(result) <- c(var)
      result[1,] <- c(or, ci95, lower95, upper95, pvalue)
    }
    else {
      f <- as.formula(paste(outcome, "~", paste(c(var, confounder), collapse="+")))
      if (model == "linear") {
        fit <- lm(f, data=subdata)
        adjust_or <- round(coef(fit)[[var]], digit=2)
        adjust_ci95 <- paste(round(confint(fit, var), digit=2), collapse="~")
        adjust_lower95 <- round(confint(fit, var)[, "2.5 %"], digit=2)
        adjust_upper95 <- round(confint(fit, var)[, "97.5 %"], digit=2)
        adjust_pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
      }
      else if (model == "logistic") {
        fit <- glm(f, data=subdata, family=binomial(link="logit"))
        adjust_or <- round(exp(coef(fit)[[var]]), digit=2)
        adjust_ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
        adjust_lower95 <- round(exp(confint(fit, var)[["2.5 %"]]), digit=2)
        adjust_upper95 <- round(exp(confint(fit, var)[["97.5 %"]]), digit=2)
        adjust_pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
      }
      else if (model == "possion") {
        fit <- glm(f, data=subdata, family=poisson(link="log"))
        adjust_or <- round(exp(coef(fit)[[var]]), digit=2)
        adjust_ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
        adjust_lower95 <- round(exp(confint(fit, var)[["2.5 %"]]), digit=2)
        adjust_upper95 <- round(exp(confint(fit, var)[["97.5 %"]]), digit=2)
        adjust_pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
      }
      result <- data.frame(matrix(nrow=1, ncol=10))
      colnames(result) <- c("Crude OR", "Crude 95% CI", "Crude lower95", "Crude upper95", "Crude p value", "Adjusted OR", "Adjusted 95% CI", "Adjusted lower95", "Adjusted upper95", "Adjusted p value")
      rownames(result) <- c(var)
      result[1,] <- c(or, ci95, lower95, upper95, pvalue, adjust_or, adjust_ci95, adjust_lower95, adjust_upper95, adjust_pvalue)
    }
    output <- rbind(output, result)
  }
  return (output)
}

multivariateRegression <- function(df, outcome, vars, model) {
  valid_vars <- c(outcome, vars)
  subdata <- na.omit(df[valid_vars])
  f <- as.formula(paste(outcome, "~", paste(vars, collapse="+")))
  if (model == "linear") {
    fit <- lm(f, data=subdata)
    vars <- intersect(vars, rownames(summary(fit)$coefficients))
    or <- round(coef(fit)[vars], digit=2)
    ci95 <- round(confint(fit, vars), digit=2)
    ci95 <- paste(ci95[,1], ci95[,2], sep="~")
    lower95 <- round(confint(fit, vars)[,"2.5 %"], digit=2)
    upper95 <- round(confint(fit, vars)[,"97.5 %"], digit=2)
    pvalue <- round(summary(fit)$coefficients[vars, 4], digit=2)
  }
  else if (model == "logistic") {
    fit <- glm(f, data=subdata, family=binomial(link="logit"))
    vars <- intersect(vars, rownames(summary(fit)$coefficients))
    or <- round(exp(coef(fit)[vars]), digit=2)
    ci95 <- round(exp(confint(fit, vars)), digit=2)
    ci95 <- paste(ci95[,1], ci95[,2], sep="~")
    lower95 <- round(exp(confint(fit, vars)[,"2.5 %"]), digit=2)
    upper95 <- round(exp(confint(fit, vars)[,"97.5 %"]), digit=2)
    pvalue <- round(summary(fit)$coefficients[vars, 4], digit=2)
  }
  else if (model == "possion") {
    fit <- glm(f, data=subdata, family=poisson(link="log"))
    vars <- intersect(vars, rownames(summary(fit)$coefficients))
    or <- round(exp(coef(fit)[vars]), digit=2)
    ci95 <- round(exp(confint(fit, vars)), digit=2)
    ci95 <- paste(ci95[,1], ci95[,2], sep="~")
    lower95 <- round(exp(confint(fit, vars)[,"2.5 %"]), digit=2)
    upper95 <- round(exp(confint(fit, vars)[,"97.5 %"]), digit=2)
    pvalue <- round(summary(fit)$coefficients[vars, 4], digit=2)
  }
  output <- as.data.frame(cbind(or, ci95, lower95, upper95, pvalue))
  colnames(output) <- c("OR", "95% CI", "lower95", "upper95", "p value")
  return (output)
}

univariateSurvival <- function(df, survtime, survevent, vars, confounder=c(), model) {
  output <- data.frame()
  for (var in vars) {
    valid_vars <- c(survtime, survevent, var, confounder)
    subdata <- na.omit(df[valid_vars])
    if (model == "cox") {
      f <- as.formula(paste("Surv(", survtime, ",", survevent, ") ~ ", paste(var, collapse="+")))
      fit <- coxph(f, data=subdata)
      hr <- round(exp(coef(fit)[[var]]), digit=2)
      ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
      lower95 <- round(exp(confint(fit, var)[, "2.5 %"]), digit=2)
      upper95 <- round(exp(confint(fit, var)[, "97.5 %"]), digit=2)
      pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
    }
    if (is.null(confounder)) {
      result <- data.frame(matrix(nrow=1, ncol=5))
      colnames(result) <- c("HR", "95% CI", "lower95", "upper95", "p value")
      rownames(result) <- c(var)
      result[1,] <- c(hr, ci95, lower95, upper95, pvalue)
    }
    else {
      if (model == "cox") {
        f <- as.formula(paste("Surv(", survtime, ",", survevent, ") ~ ", paste(c(var, confounder), collapse="+")))
        fit <- coxph(f, data=subdata)
        adjust_hr <- round(exp(coef(fit)[[var]]), digit=2)
        adjust_ci95 <- paste(round(exp(confint(fit, var)), digit=2), collapse="~")
        adjust_lower95 <- round(exp(confint(fit, var)[, "2.5 %"]), digit=2)
        adjust_upper95 <- round(exp(confint(fit, var)[, "97.5 %"]), digit=2)
        adjust_pvalue <- round(summary(fit)$coefficients[var, 4], digit=2)
      }
      result <- data.frame(matrix(nrow=1, ncol=10))
      colnames(result) <- c("Crude HR", "Crude 95% CI", "Crude lower95", "Crude upper95", "Crude p value", "Adjusted HR", "Adjusted 95% CI", "Adjusted lower95", "Adjusted upper95", "Adjusted p value")
      rownames(result) <- c(var)
      result[1,] <- c(hr, ci95, lower95, upper95, pvalue, adjust_hr, adjust_ci95, adjust_lower95, adjust_upper95, adjust_pvalue)
    }
    output <- rbind(output, result)
  }
  return (output)
}

multivariateSurvival <- function(df, survtime, survevent, vars, model) {
  valid_vars <- c(survtime, survevent, vars)
  subdata <- na.omit(df[valid_vars])
  if (model == "cox") {
    f <- as.formula(paste("Surv(", survtime, ",", survevent, ") ~ ", paste(vars, collapse="+")))
    fit <- coxph(f, data=subdata)
    vars <- intersect(vars, rownames(summary(fit)$coefficients))
    hr <- round(exp(coef(fit)[vars]), digit=2)
    ci95 <- round(exp(confint(fit, vars)), digit=2)
    ci95 <- paste(ci95[,1], ci95[,2], sep="~")
    lower95 <- round(exp(confint(fit, vars)[,"2.5 %"]), digit=2)
    upper95 <- round(exp(confint(fit, vars)[,"97.5 %"]), digit=2)
    pvalue <- round(summary(fit)$coefficients[vars, 4], digit=2)
  }
  output <- as.data.frame(cbind(hr, ci95, lower95, upper95, pvalue))
  colnames(output) <- c("HR", "95% CI", "lower95", "upper95", "p value")
  return (output)
}
