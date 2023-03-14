library(DT)
library(survminer)
library(forestplot)
library(showtext)
showtext_auto(enable=T)

source("./analysis.R")


shinyServer(function(input, output, session) {

  source_data <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath, header=T, encoding="UTF-8", stringsAsFactors=FALSE)
  })
  
  values <- reactiveValues()
  
  output$select_vars <- renderUI({
    if ("df" %in% names(values)) {
      choices <- colnames(values$df)
      selected <- colnames(values$df)
    }
    else {
      choices <- colnames(source_data())
      selected <- c()
    }
    pickerInput("variables", "", choices=choices,
                options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"),
                selected=selected, multiple=TRUE)
  })

  output$select_exposure <- renderUI({
    selectInput("exposure", "Select exposure variable", choices=input$variables)
  })

  output$select_outcome <- renderUI({
    selectInput("outcome", "Select outcome variable", choices=setdiff(input$variables, input$exposure))
  })
  
  observeEvent(input$outcome, {
    outcome_options <- sort(unique(na.omit(source_data()[[input$outcome]])))
    if (length(outcome_options) == 2 & ! all(outcome_options == c(0,1))) {
      showNotification(paste("Values in the outcome should be 0 or 1."), type="error")
    }
  })

  output$select_survtime <- renderUI({
    selectInput("survtime", "Select survival time variable", choices=setdiff(input$variables, input$exposure))
  })

  output$select_survevent <- renderUI({
    selectInput("survevent", "Select survival event variable", choices=setdiff(input$variables, c(input$exposure, input$survtime)))
  })
  
  observeEvent(input$data_submit, {
    outcome_options <- sort(unique(na.omit(source_data()[[input$outcome]])))
    if (length(outcome_options) == 2 & ! all(outcome_options == c(0,1))) {
      showNotification(paste("Values in the outcome should be 0 or 1."), type="error")
    }
    else {
      if ("df" %in% names(values)) {
        values$df <- values$df[input$variables]
      }
      else {
        values$df <- source_data()[input$variables]
        values$df[values$df == ""] = NA
      }
      values$exposure <- input$exposure
      values$outcome <- input$outcome
      values$survtime <- input$survtime
      values$survevent <- input$survevent
      if (! "var_class" %in% names(values)) {
        values$var_class <- classifyVar(values$df)
      }
      if (! "norm_vars" %in% names(values)) {
        values$norm_vars <- checkNorm(values$df, values$var_class$numeric)
      }
      if (! "cut_vars" %in% names(values)) {
        values$cut_vars <- c()
      }
      if (! "dummy" %in% names(values)) {
        values$dummy <- c()
        for (var in values$var_class$categorical) {
          if (length(unique((na.omit(values$df[[var]])))) > 2) {
            values$dummy <- c(values$dummy, var)
          }
        }
        values$dummy_refers <- c()
      }
      updateTabItems(session, "sidebar", selected="data_diagnosis_tab")
    }
  })
  
  # Display data diagnosis and frame -------------------------------------------
  
  output$data_summary <- DT::renderDataTable({
    diagnosis(values$df, values$var_class, values$norm_vars)
  }, options=list(paging=FALSE, scrollX=TRUE, dom="t"), rownames=FALSE)
  
  output$data_preview <- DT::renderDataTable({
    values$df
  }, options=list(pageLength=10, scrollX=TRUE), rownames=TRUE)
  
  # Select variables -----------------------------------------------------------
  
  output$variable_filter <- renderUI({
    choices <- setdiff(names(values$df), c(values$exposure, values$survtime, values$survevent, values$outcome))
    pickerInput("filter_vars", "", choices=choices,
                options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                multiple=TRUE, selected=choices)
  })
  
  observeEvent(input$update_data1, {
    valid_vars <- intersect(names(values$df), c(input$filter_vars, values$exposure, values$survtime, values$survevent, values$outcome))
    values$df <- values$df[valid_vars]
    values$var_class$numeric <- intersect(valid_vars, values$var_class$numeric)
    values$var_class$categorical <- intersect(valid_vars, values$var_class$categorical)
    updateTabItems(session, "sidebar", selected="data_diagnosis_tab")
  })
  
  # Data completeness ----------------------------------------------------------
  
  observeEvent(input$dropna, {
    values$df <- na.omit(values$df)
    updateTabItems(session, "sidebar", selected="data_diagnosis_tab")
  })
  
  observeEvent(input$simple_impute, {
    values$df <- simpleImpute(values$df, values$var_class, values$norm_vars)
    updateTabItems(session, "sidebar", selected="data_diagnosis_tab")
  })
  
  observeEvent(input$multiple_impute, {
    valid_vars <- setdiff(colnames(values$df), c(values$var_class$other))
    if (length(valid_vars) > 1) {
      values$df <- multipleImpute(values$df, valid_vars)
    }
    updateTabItems(session, "sidebar", selected="data_diagnosis_tab")
  })
  
  # Classify variables ---------------------------------------------------------
  
  output$select_numeric <- renderUI({
    selectInput("numeric", label="numeric", choices=values$var_class$numeric, multiple=TRUE, selectize=FALSE)
  })
  
  output$select_categorical <- renderUI({
    selectInput("categorical", label="categorical", choices=values$var_class$categorical, multiple=TRUE, selectize=FALSE)
  })
  
  output$select_other <- renderUI({
    selectInput("other", label="other", choices=values$var_class$other, multiple=TRUE, selectize=FALSE)
  })
  
  observeEvent(input$num2cat, {
    values$var_class$categorical <- c(values$var_class$categorical, input$numeric)
    values$var_class$numeric <- setdiff(values$var_class$numeric, input$numeric)
  })
  
  observeEvent(input$cat2num, {
    values$var_class$numeric <- c(values$var_class$numeric, input$categorical)
    values$var_class$categorical <- setdiff(values$var_class$categorical, input$categorical)
  })
  
  observeEvent(input$cat2other, {
    values$var_class$other <- c(values$var_class$other, input$categorical)
    values$var_class$categorical <- setdiff(values$var_class$categorical, input$categorical)
  })
  
  observeEvent(input$other2cat, {
    values$var_class$categorical <- c(values$var_class$categorical, input$other)
    values$var_class$other <- setdiff(values$var_class$other, input$other)
  })
  
  observeEvent(input$update_data2, {
    updateTabItems(session, "sidebar", selected="data_diagnosis_tab")
  })
  
  # Split variable -------------------------------------------------------------
  
  output$split_num <- renderUI({
    selectInput("split_vars", label="Select a numeric variable", choices=values$var_class$numeric, multiple=FALSE, selectize=FALSE)
  })
  
  observeEvent(input$update_data3, {
    split_output <- splitNumeric(values$df, input$split_vars, input$cutpoint)
    values$df <- split_output$df
    if (! split_output$new_var %in% values$var_class$other) {
      values$var_class$other <- c(values$var_class$other, split_output$new_var)
    }
    values$cut_vars <- c(values$cut_vars, input$split_vars)
    
    map_output <- mapOption(values$df, split_output$new_var)
    values$df <- map_output$df
    if (! map_output$new_var %in% values$var_class$categorical) {
      values$var_class$categorical <- c(values$var_class$categorical, map_output$new_var)
    }
    updateTabItems(session, "sidebar", selected="data_diagnosis_tab")
  })
  
  # Map option -----------------------------------------------------------------
  
  output$map_option <- renderUI({
    choices <- c(values$var_class$categorical, values$var_class$other)
    selectInput("option_var", label="Select a categorical variable", choices=choices, multiple=FALSE, selectize=FALSE)
  })
  
  observeEvent(input$update_data5, {
    map_output <- mapOption(values$df, input$option_var)
    values$df <- map_output$df
    if (! map_output$new_var %in% values$var_class$categorical) {
      values$var_class$categorical <- c(values$var_class$categorical, map_output$new_var)
    }
    updateTabItems(session, "sidebar", selected="data_diagnosis_tab")
  })
  
  # Dummy variables ------------------------------------------------------------
  
  output$select_nondummy <- renderUI({
    choices <- setdiff(c(values$var_class$categorical, values$var_class$other), c(values$dummy, unname(unlist(values$dum_vars))))
    selectInput("nondummy_vars", label="source vars", choices=choices, multiple=TRUE, selectize=FALSE)
  })
  
  output$select_dummy <- renderUI({
    selectInput("dummy_vars", label="dummy vars", choices=values$dummy, multiple=TRUE, selectize=FALSE)
  })
  
  observeEvent(input$nondum2dum, {
    values$dummy <- c(values$dummy, input$nondummy)
  })
  
  observeEvent(input$dum2nondum, {
    values$dummy <- setdiff(values$dummy, input$dummy)
  })
  
  observeEvent(input$update_data4, {
    recover_vars <- setdiff(names(values$dummy_refers), values$dummy)
    recover_dum_vars <- c()
    for (var in recover_vars) {
      recover_dum_vars <- c(recover_dum_vars, values$dum_vars[[var]])
      values$dummy_refers[var] <- NULL
      values$dum_vars[var] <- NULL
    }
    if (length(recover_vars) > 0) {
      values$df <- values$df[setdiff(names(values$df), recover_dum_vars)]
      values$var_class$categorical <- setdiff(values$var_class$categorical, recover_dum_vars)
    }
    dummy_output <- setDummy(values$df, setdiff(values$dummy, names(values$dummy_refers)))
    values$df <- dummy_output$df
    values$dummy_refers <- c(values$dummy_refers, dummy_output$refers)
    values$dum_vars <- c(values$dum_vars, dummy_output$dum_vars)
    for (dum_var in dummy_output$dum_vars) {
      values$var_class$categorical <- c(values$var_class$categorical, dum_var)
    }
    updateTabItems(session, "sidebar", selected="data_diagnosis_tab")
  })
  
  # Baseline -------------------------------------------------------------------
  
  output$select_base_vars <- renderUI({
    choices <- setdiff(names(values$df), c(values$exposure, values$survtime, values$survevent, values$outcome, names(values$dummy_refers)))
    pickerInput("base_vars", "Select variables", choices=choices,
                options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                multiple=TRUE, selected=choices)
  })
  
  output$baseline_title <- renderUI({
    h4(paste("Grouped by", values$exposure), align="center")
  })
  
  output$table1 <- DT::renderDataTable({
    table1_cat_vars <- intersect(input$base_vars, values$var_class$categorical)
    table1_nonnorm_vars <- intersect(input$base_vars, setdiff(values$var_class$numeric, values$norm_vars))
    values$table1 <- baseline(values$df, values$exposure, input$base_vars, table1_cat_vars, table1_nonnorm_vars)
  }, options=list(paging=FALSE, dom="t"), rownames=TRUE)
  
  # Propensity score matching --------------------------------------------------
  
  output$select_psm_vars <- renderUI({
    choices <- setdiff(names(values$df), c(values$exposure, values$survtime, values$survevent, values$outcome))
    pickerInput("psm_vars", "Select skewed variables for PSM", choices=choices,
                options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                multiple=TRUE, selected=choices)
  })
  
  observeEvent(input$psm, {
    exposure_options <- sort(unique(na.omit(values$df[[values$exposure]])))
    if (length(exposure_options) != 2 | ! all(exposure_options == c(0,1))) {
      showNotification(paste("The group should be a binary variable in 0 and 1 before this step."), type="error")
    }
    else {
      values$df <- psm(values$df, as.data.frame(values$table1), values$exposure, input$psm_vars)
      updateTabItems(session, "sidebar", selected="data_diagnosis_tab")
    }
  })
  
  # Risk factor analysis -------------------------------------------------------
  
  output$select_univariate_vars <- renderUI({
    choices <- setdiff(c(values$var_class$numeric, values$var_class$categorical), c(values$survtime, values$survevent, values$outcome, names(values$dummy_refers)))
    pickerInput("univariate_vars", "Select factor variables", choices=choices,
                options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                multiple=TRUE, selected=choices)
  })
  
  output$select_univariate_confounders <- renderUI({
    choices <- setdiff(c(values$var_class$numeric, values$var_class$categorical), c(values$survtime, values$survevent, values$outcome, names(values$dummy_refers), input$univariate_vars))
    pickerInput("univariate_confounders", "Select confounder variables", choices=choices,
                options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"),
                multiple=TRUE )
  })
  
  output$select_univariate_model <- renderUI({
    if (input$analysis_type == "Association analysis") {
      if (values$outcome %in% values$var_class$categorical) {
        pickerInput("univariate_model", "Select model", choices=c("logistic"),
                    options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                    multiple=FALSE, selected="logistic")
      }
      else {
        pickerInput("univariate_model", "Select model", choices=c("linear", "possion"),
                    options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                    multiple=FALSE, selected="linear")
      }
    }
    else {
      pickerInput("univariate_model", "Select model", choices=c("cox"),
                  options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                  multiple=FALSE, selected="cox")
    }
  })
  
  observeEvent(input$univariate_analyze, {
    dum_vars <- c()
    other_vars <- c()
    for (var in input$univariate_vars) {
      if (var %in% names(values$dummy_refers)) {
        dum_vars <- c(dum_vars, values$dum_vars$var)
      }
      else {
        other_vars <- c(other_vars, var)
      }
    }
    if (input$analysis_type == "Association analysis") {
      values$univariate_regression <- univariateRegression(values$df, values$outcome, c(dum_vars, other_vars), input$univariate_confounders, input$univariate_model)
    }
    else {
      values$univariate_regression <- univariateSurvival(values$df, values$survtime, values$survevent, c(dum_vars, other_vars), input$univariate_confounders, input$univariate_model)
    }
    
    if (length(input$univariate_confounders) == 0) {
      values$univariate_regression[["p value"]][values$univariate_regression[["p value"]]<0.01] <- "<0.01"
      tabletext <- cbind(c("variable", rownames(values$univariate_regression)),
                         c("p", as.vector(unlist(values$univariate_regression[["p value"]]))))
      if (input$analysis_type == "Association analysis") {
        coef <- "OR"
      }
      else {
        coef <- "HR"
      }
      values$univariate_forest <- forestplot(
        labeltext=tabletext,
        mean=c(NA, as.numeric(unlist(values$univariate_regression[[coef]]))),
        lower=c(NA, as.numeric(unlist(values$univariate_regression$lower95))),
        upper=c(NA, as.numeric(unlist(values$univariate_regression$upper95))),
        col=fpColors(box="black", lines="black", zero="gray50"),
        zero=1, ci.vertices=TRUE, ci.vertices.height=0.3, lineheight="auto", boxsize=0.3)
    }
    else {
      values$univariate_regression[["Crude p value"]][values$univariate_regression[["Crude p value"]]<0.01] <- "<0.01"
      tabletext <- cbind(c("variable", rownames(values$univariate_regression)),
                         c("Crude p", as.vector(unlist(values$univariate_regression[["Crude p value"]]))))
      if (input$analysis_type == "Association analysis") {
        coef <- "Crude OR"
      }
      else {
        coef <- "Crude HR"
      }
      values$univariate_forest <- forestplot(
        labeltext=tabletext,
        mean=c(NA, as.numeric(unlist(values$univariate_regression[[coef]]))),
        lower=c(NA, as.numeric(unlist(values$univariate_regression[["Crude lower95"]]))),
        upper=c(NA, as.numeric(unlist(values$univariate_regression[["Crude upper95"]]))),
        col=fpColors(box="black", lines="black", zero="gray50"),
        zero=1, ci.vertices=TRUE, ci.vertices.height=0.3, lineheight="auto", boxsize=0.3)
      
      values$univariate_regression[["Adjusted p value"]][values$univariate_regression[["Adjusted p value"]]<0.01] <- "<0.01"
      tabletext <- cbind(c("variable", rownames(values$univariate_regression)),
                         c("Adjusted p", as.vector(unlist(values$univariate_regression[["Adjusted p value"]]))))
      if (input$analysis_type == "Association analysis") {
        coef <- "Adjusted OR"
      }
      else {
        coef <- "Adjusted HR"
      }
      values$adjust_univariate_forest <- forestplot(
        labeltext=tabletext,
        mean=c(NA, as.numeric(unlist(values$univariate_regression[[coef]]))),
        lower=c(NA, as.numeric(unlist(values$univariate_regression[["Adjusted lower95"]]))),
        upper=c(NA, as.numeric(unlist(values$univariate_regression[["Adjusted upper95"]]))),
        col=fpColors(box="black", lines="black", zero="gray50"),
        zero=1, ci.vertices=TRUE, ci.vertices.height=0.3, lineheight="auto", boxsize=0.3)
    }
  })
  
  output$univariate_regression_table <- DT::renderDataTable({
    if ("univariate_regression" %in% names(values)) {
      values$univariate_regression[! grepl("lower|upper", names(values$univariate_regression))]
    }
  }, options=list(paging=FALSE, scrollX=TRUE, dom="t"), rownames=TRUE)
  
  output$univariate_forest_fig <- renderPlot({
    if ("univariate_regression" %in% names(values)) {
      values$univariate_forest
    }
  }, width=900)
  
  output$adjust_univariate_forest_fig <- renderPlot({
    if (("univariate_regression" %in% names(values)) & (length(input$univariate_confounders) > 0)) {
      values$adjust_univariate_forest
    }
  }, width=900)
  
  output$select_multivariate_vars <- renderUI({
    choices <- setdiff(c(values$var_class$numeric, values$var_class$categorical), c(values$survtime, values$survevent, values$outcome, names(values$dummy_refers)))
    pickerInput("multivariate_vars", "Select variables", choices=choices,
                options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                multiple=TRUE, selected=choices )
  })
  
  output$select_multivariate_model <- renderUI({
    if (input$analysis_type == "Association analysis") {
      if (values$outcome %in% values$var_class$categorical) {
        pickerInput("multiregression_model", "Select model", choices=c("logistic"),
                    options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                    multiple=FALSE, selected="logistic")
      }
      else {
        pickerInput("multiregression_model", "Select model", choices=c("linear", "possion"),
                    options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                    multiple=FALSE, selected="linear")
      }
    }
    else {
      pickerInput("multiregression_model", "Select model", choices=c("cox"),
                  options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                  multiple=FALSE, selected="cox")
    }
  })
  
  observeEvent(input$multivariate_analyze, {
    dum_vars <- c()
    other_vars <- c()
    for (var in input$multivariate_vars) {
      if (var %in% names(values$dummy_refers)) {
        dum_vars <- c(dum_vars, values$dum_vars$var)
      }
      else {
        other_vars <- c(other_vars, var)
      }
    }
    if (input$analysis_type == "Association analysis") {
      values$multivariate_regression <- multivariateRegression(values$df, values$outcome, c(dum_vars, other_vars), input$multiregression_model)
    }
    else {
      values$multivariate_regression <- multivariateSurvival(values$df, values$survtime, values$survevent, c(dum_vars, other_vars), input$multiregression_model)
    }
    values$multivariate_regression[["p value"]][values$multivariate_regression[["p value"]]<0.01] <- "<0.01"
    tabletext <- cbind(c("variable", rownames(values$multivariate_regression)),
                       c("p", as.vector(unlist(values$multivariate_regression[["p value"]]))))
    if (input$analysis_type == "Association analysis") {
      coef <- "OR"
    }
    else {
      coef <- "HR"
    }
    values$multivariate_forest <- forestplot(
      labeltext=tabletext,
      mean=c(NA, as.numeric(unlist(values$multivariate_regression[[coef]]))),
      lower=c(NA, as.numeric(unlist(values$multivariate_regression$lower95))),
      upper=c(NA, as.numeric(unlist(values$multivariate_regression$upper95))),
      col=fpColors(box="black", lines="black", zero="gray50"),
      zero=1, ci.vertices=TRUE, ci.vertices.height=0.3, lineheight="auto", boxsize=0.3)
  })
  
  output$multivariate_regression_table <- DT::renderDataTable({
    if ("multivariate_regression" %in% names(values)) {
      values$multivariate_regression[! grepl("lower|upper", names(values$multivariate_regression))]
    }
  }, options=list(paging=FALSE, scrollX=TRUE, dom="t"), rownames=TRUE)
  
  output$multivariate_forest_fig <- renderPlot({
    if ("multivariate_regression" %in% names(values)) {
      values$multivariate_forest
    }
  })
  
  # Effect estimation ----------------------------------------------------------
  
  output$select_effect_confounders <- renderUI({
    choices <- setdiff(c(values$var_class$numeric, values$var_class$categorical), c(values$exposure, values$survtime, values$survevent, values$outcome, names(values$dummy_refers)))
    pickerInput("effect_confounders", "Select variables to adjust", choices=choices,
                options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                multiple=TRUE )
  })
  
  output$select_effect_model <- renderUI({
    if (input$analysis_type == "Association analysis") {
      if (values$outcome %in% values$var_class$categorical) {
        pickerInput("effect_model", "Select model", choices=c("logistic"),
                    options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                    multiple=FALSE, selected="logistic")
      }
      else {
        pickerInput("effect_model", "Select model", choices=c("linear", "possion"),
                    options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                    multiple=FALSE, selected="linear")
      }
    }
    else {
      pickerInput("effect_model", "Select model", choices=c("cox"),
                  options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                  multiple=FALSE, selected="cox")
    }
  })
  
  output$effect_estimation_table <- DT::renderDataTable({
    if (input$analysis_type == "Association analysis") {
      if (values$exposure %in% names(values$dummy_refers)) {
        dummyAssociationAnalysis(values$df, values$dummy_refers[[values$exposure]], values$dum_vars[values$exposure], values$outcome, input$effect_confounders, input$effect_model)
      }
      else {
        # If the group variable has more than 2 options, it will be considered as rank type.
        associationAnalysis(values$df, values$exposure, values$outcome, input$effect_confounders, input$effect_model)
      }
    }
    else {
      if (values$exposure %in% names(values$dummy_refers)) {
        dummySurvivalAnalysis(values$df, values$dummy_refers[[values$exposure]], values$dum_vars[values$exposure], values$survtime, values$survevent, input$effect_confounders, input$effect_model)
      }
      else {
        # If the group variable has more than 2 options, it will be considered as rank type.
        survivalAnalysis(values$df, values$exposure, values$survtime, values$survevent, input$effect_confounders, input$effect_model)
      }
    }
  }, options=list(paging=FALSE, scrollX=TRUE, dom="t"), rownames=TRUE)
  
  output$survival_plot <- renderPlot({
    subdata <- na.omit(values$df[c(values$exposure, values$survtime, values$survevent)])
    f <- as.formula(paste("Surv(", values$survtime, ",", values$survevent, ") ~ ", values$exposure))
    fit <- do.call(survfit, args=list(formula=f, data=subdata))
    ggsurvplot(fit, 
               legend.title=values$exposure,
               pval=TRUE, 
               risk.table=TRUE, 
               tables.height=0.3,
               tables.theme=theme_bw(),
               break.time.by=365)
  }, width=600)
  
  # Subgroup analysis ----------------------------------------------------------
  
  output$select_stratify_vars <- renderUI({
    choices <- setdiff(values$var_class$categorical, c(values$exposure, values$survtime, values$survevent, values$outcome, unname(unlist(values$dum_vars))))
    pickerInput("stratify_vars", "Select variables to stratify", choices=choices,
                options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                multiple=TRUE, selected=choices )
  })
  
  output$select_stratify_confounders <- renderUI({
    choices <- setdiff(c(values$var_class$numeric, values$var_class$categorical), c(values$exposure, values$survtime, values$survevent, values$outcome, input$stratify_vars, names(values$dummy_refers)))
    pickerInput("stratify_confounders", "Select variables to adjust", choices=choices,
                options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                multiple=TRUE )
  })
  
  output$select_stratify_model <- renderUI({
    if (input$analysis_type == "Association analysis") {
      if (values$outcome %in% values$var_class$categorical) {
        pickerInput("stratify_model", "Select model", choices=c("logistic"),
                    options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                    multiple=FALSE, selected="logistic")
      }
      else {
        pickerInput("stratify_model", "Select model", choices=c("linear", "possion"),
                    options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                    multiple=FALSE, selected="linear")
      }
    }
    else {
      pickerInput("stratify_model", "Select model", choices=c("cox"),
                  options=list("actions-box"=TRUE, "live-search"=TRUE, "selected-text-format"="values"), 
                  multiple=FALSE, selected="cox")
    }
  })
  
  output$stratify_table <- DT::renderDataTable({
    if (input$analysis_type == "Association analysis") {
      if (values$exposure %in% names(values$dummy_refers)) {
        dummyAssociationStratification(values$df, values$dummy_refers[[values$exposure]], values$dum_vars[values$exposure], values$outcome, input$stratify_confounders, input$stratify_vars, input$stratify_model)
      }
      else {
        associationStratification(values$df, values$exposure, values$outcome, input$stratify_confounders, input$stratify_vars, input$stratify_model)
      }
    }
    else {
      if (values$exposure %in% names(values$dummy_refers)) {
        dummySurvivalStratification(values$df, values$dummy_refers[[values$exposure]], values$dum_vars[values$exposure], values$survtime, values$survevent, input$stratify_confounders, input$stratify_vars, input$stratify_model)
      }
      else {
        # If the group variable has more than 2 options, it will be considered as rank type.
        survivalStratification(values$df, values$exposure, values$survtime, values$survevent, input$stratify_confounders, input$stratify_vars, input$stratify_model)
      }
    }
  }, options=list(paging=FALSE, scrollX=TRUE, dom="t"), rownames=FALSE)
})
