library(shiny)
library(shinybusy)
library(shinyWidgets)
library(shinydashboard)

header <- dashboardHeader(title="Observational Study Understanding", titleWidth=350)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id="sidebar",
    menuItem("Study definition", tabName="study_definition_tab"),
    menuItem("Data Diagnosis", tabName="data_diagnosis_tab"),
    menuItem("Data preprocessing", tabName="data_preprocess_tab"),
    menuItem("Baseline", tabName="baseline_tab"),
    menuItem("Risk factor analysis", tabName="risk_factor_analysis_tab"),
    menuItem("Effect estimation", tabName="effect_estimation_tab")
  )
)

# Study definition -------------------------------------------------------------

study_def_tab <- tabItem(
  tabName="study_definition_tab",
  fluidRow(
    box(
      title="Upload data",
      status="primary",
      solidHeader=TRUE,
      width=10,
      height=200,
      helpText("A table with columns containing values of variables and rows representing patients."),
      fileInput("data_file", "Upload CSV File",
                accept=c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"))
    ),
    box(
      title="Analysis type",
      status="primary",
      solidHeader=TRUE,
      width=10,
      selectInput("analysis_type", "", choices=c("Association analysis", "Survival analysis"))
    ),
    box(
      title="Variable selection",
      status="primary",
      solidHeader=TRUE,
      width=10,
      uiOutput("select_vars")
    ),
    box(
      title="Exposure",
      status="primary",
      solidHeader=TRUE,
      width=10,
      uiOutput("select_exposure")
    ),
    conditionalPanel(
      "input.analysis_type == 'Association analysis'", 
      box(
        title="Outcome",
        status="primary",
        solidHeader=TRUE,
        width=10,
        helpText("The output should be a continuous variable or a binary variable (0/1)."),
        uiOutput("select_outcome")
      )
    ),
    conditionalPanel(
      "input.analysis_type == 'Survival analysis'", 
      box(
        title="survival time",
        status="primary",
        solidHeader=TRUE,
        width=10,
        uiOutput("select_survtime")
      ),
      box(
        title="survival event",
        status="primary",
        solidHeader=TRUE,
        width=10,
        uiOutput("select_survevent")
      )
    ),
    actionButton("data_submit", "Submit", icon("rocket"), width=100,
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  )
)

# Data diagnosis ---------------------------------------------------------------

data_diagnosis_tab <- tabItem(
  tabName="data_diagnosis_tab",
  tabsetPanel(
    id="data_diagnosis_panel",
    tabPanel(
      title="Data diagnosis",
      fluidRow(
        box(
          status="primary",
          solidHeader=TRUE,
          width=10,
          DT::dataTableOutput("data_summary")
        )
      )
    ),
    tabPanel(
      title="Data preview",
      fluidRow(
        box(
          status="primary", 
          solidHeader=TRUE,
          width=10,
          DT::dataTableOutput("data_preview")
        )
      )
    )
  )
)

# Data pre-processing -----------------------------------------------------------

data_preprocess_tab <- tabItem(
  tabName="data_preprocess_tab",
  fluidRow(
    add_busy_bar(color="wheat"),
    box(
      title="Variable filtering",
      status="primary",
      solidHeader=TRUE,
      width=10,
      uiOutput("variable_filter"),
      actionButton("update_data1", "Update Data", icon("sync"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    box(
      title="Data completeness",
      status="primary",
      solidHeader=TRUE,
      width=10,
      column(1, offset=1, actionButton("dropna", "Drop empty value", icon("trash"),
                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
      column(1, offset=2, actionButton("simple_impute", "Simple imputation", icon("wrench"),
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
      column(1, offset=2, actionButton("multiple_impute", "Multiple imputation", icon("toolbox"),
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
    ),
    box(
      title="Variable classification",
      status="primary",
      solidHeader=TRUE,
      width=10,
      height=230,
      column(3, uiOutput("select_numeric")),
      column(1, br(), actionButton("num2cat", ">>"), br(), br(), actionButton("cat2num", "<<")),
      column(3, uiOutput("select_categorical")),
      column(1, br(), actionButton("cat2other", ">>"), br(), br(), actionButton("other2cat", "<<")),
      column(3, uiOutput("select_other")),
      actionButton("update_data2", "Update Data", icon("sync"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    box(
      title="Data discretization",
      status="primary",
      solidHeader=TRUE,
      width=10,
      helpText("Cut point format: 'number,number,number'"),
      column(6, uiOutput("split_num")),
      column(6, textInput("cutpoint", label="Cut points")),
      actionButton("update_data3", "update data", icon("sync"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    box(
      title="Option mapping",
      status="primary",
      solidHeader=TRUE,
      width=10,
      helpText("Map string options to numbers starting from 0."),
      uiOutput("map_option"),
      actionButton("update_data5", "update data", icon("sync"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    box(
      title="Dummy variable",
      status="primary",
      solidHeader=TRUE,
      width=10,
      height=275,
      helpText("Missing data for the target variable should be solved before this step, otherwise, missing data will be assigned with a special string (NA)."),
      column(5, uiOutput("select_nondummy")),
      column(1, br(), actionButton("nondum2dum", ">>"), br(), br(), actionButton("dum2nondum", "<<")),
      column(5, uiOutput("select_dummy")),
      actionButton("update_data4", "Update Data", icon("sync"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  )
)

# Baseline ---------------------------------------------------------------------

baseline_tab <- tabItem(
  tabName="baseline_tab",
  fluidRow(
    box(
      title="Baseline",
      status="primary",
      solidHeader=TRUE,
      width=10,
      uiOutput("select_base_vars"),
      uiOutput("baseline_title"),
      DT::dataTableOutput("table1"),
      tags$ul(
        tags$li("For categorical variables, apply the chi-square test to test the difference between groups."),
        tags$li("For continuous variable,"),
        tags$ul(
          tags$li("  if the group level is 2, apply t-test to variable in normal distribution, 
                      and Wilcoxon rank sum test to variable in non-normal distribution."),
          tags$li("  if the group level is over 2, apply ANOVA to variable in normal distribution, 
                      and Kruskal Wallis test to variable in non-normal distribution.")
        )
      )
    ),
    box(
      title="Propensity Score Matching",
      status="primary",
      solidHeader=TRUE,
      width=10,
      helpText("The group should be a binary variable in 0 and 1 before this step."),
      uiOutput("select_psm_vars"),
      actionButton("psm", "PSM Data", icon("balance-scale"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  )
)

# Risk factor analysis ---------------------------------------------------------

risk_factor_tab <- tabItem(
  tabName="risk_factor_analysis_tab",
  tabsetPanel(
    id="univariate_regression_panel",
    tabPanel(
      title="Univariate analysis",
      fluidRow(
        box(
          status="primary",
          solidHeader=TRUE,
          width=10,
          uiOutput("select_univariate_vars"),
          uiOutput("select_univariate_confounders"),
          uiOutput("select_univariate_model"),
          actionButton("univariate_analyze", "Analyze", icon("calculator"),
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          DT::dataTableOutput("univariate_regression_table"),
          plotOutput("univariate_forest_fig"),
          plotOutput("adjust_univariate_forest_fig")
        )
      )
    ),
    tabPanel(
      title="Multivariate analysis",
      fluidRow(
        box(
          status="primary",
          solidHeader=TRUE,
          width=10,
          uiOutput("select_multivariate_vars"),
          uiOutput("select_multivariate_model"),
          actionButton("multivariate_analyze", "Analyze", icon("calculator"),
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          DT::dataTableOutput("multivariate_regression_table"),
          plotOutput("multivariate_forest_fig")
        )
      )
    )
  )
)

# Effect estimation ------------------------------------------------------------

effect_estimation_tab <- tabItem(
  tabName="effect_estimation_tab",
  tabsetPanel(
    id="effect_estimation_panel",
    tabPanel(
      title="Effect estimation",
      fluidRow(
        box(
          status="primary",
          solidHeader=TRUE,
          width=10,
          uiOutput("select_effect_confounders"),
          uiOutput("select_effect_model"),
          DT::dataTableOutput("effect_estimation_table"),
          conditionalPanel(
            "input.analysis_type == 'Survival analysis'",
            plotOutput("survival_plot")
          )
        )
      )
    ),
    tabPanel(
      title="Subgroup analysis",
      fluidRow(
        box(
          status="primary",
          solidHeader=TRUE,
          width=10,
          uiOutput("select_stratify_vars"),
          uiOutput("select_stratify_confounders"),
          uiOutput("select_stratify_model"),
          DT::dataTableOutput("stratify_table")
        )
      )
    )
  )
)

body <- dashboardBody(
  tabItems(study_def_tab, data_diagnosis_tab, data_preprocess_tab, baseline_tab, risk_factor_tab, effect_estimation_tab)
)

ui <- dashboardPage(header, sidebar, body, title="Observational Study Understanding", skin="blue")
