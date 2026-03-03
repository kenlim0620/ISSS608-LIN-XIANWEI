#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# ==========================================
# FinRetain Shiny Application (FINAL VERSION)
# Includes EDA, CDA, K-Means, Survival, RFM, Sankey & Risk Simulator
# ==========================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(survival)   
library(survminer)  
library(networkD3)
library(ggcorrplot)  # NEW: For correlation matrix
library(ggridges)    # NEW: For ridgeline plots
library(ggstatsplot) # NEW: For statistical testing plots

# --- 1. DATA LOADING & PREPARATION ---
df <- readRDS("app_data.rds")

# FIX 1: Create RFM Segments dynamically if they are missing
if (!"RFM_Segment" %in% names(df)) {
  df <- df %>%
    mutate(
      R_Score = ntile(customer_tenure, 3),
      F_Score = ntile(tx_count, 3),
      M_Score = ntile(total_tx_volume, 3),
      RFM_Segment = case_when(
        R_Score == 3 & F_Score == 3 & M_Score == 3 ~ "Champions",
        R_Score <= 2 & F_Score == 3 & M_Score == 3 ~ "Loyal Customers",
        R_Score == 3 & F_Score <= 2 ~ "Recent Users",
        R_Score == 1 & F_Score == 1 & M_Score == 1 ~ "Lost/Churned Risks",
        TRUE ~ "Average Users"
      ),
      RFM_Segment = as.factor(RFM_Segment)
    )
}

# FIX 2: Create Churn Event dynamically
if (!"churn_event" %in% names(df)) {
  df$churn_event <- ifelse(df$churn_probability > 0.5, 1, 0)
}
# ... (rest of the code continues normally)

# FIX: If 'churn_event' doesn't exist, create it dynamically from 'churn_probability'
# (Assuming anyone with > 50% probability is categorized as "Churned" = 1)
if (!"churn_event" %in% names(df)) {
  df$churn_event <- ifelse(df$churn_probability > 0.5, 1, 0)
}

# Ensure required factors for plotting
df$churn_factor <- factor(df$churn_event, levels = c(0, 1), labels = c("Active", "Churned"))

# Run K-Means Clustering on Load (for the Segmentation Tab)
set.seed(123)
cluster_data <- scale(df %>% select(tx_count, total_tx_volume) %>% replace(is.na(.), 0))
kmeans_res <- kmeans(cluster_data, centers = 3, nstart = 25)
df$Behavior_Cluster <- as.factor(kmeans_res$cluster)

# Train predictive models for the Simulator
churn_model <- lm(churn_probability ~ age + tx_count + support_tickets_count + satisfaction_score, data = df)
clv_model <- lm(customer_lifetime_value ~ age + tx_count + support_tickets_count + satisfaction_score, data = df)

# --- 2. USER INTERFACE (UI) ---
ui <- page_navbar(
  title = "FinRetain: Customer Visual Analytics",
  theme = bs_theme(version = 5, bootswatch = "flatly"), 
  
  # MODULE 1: EDA (Demographics & Distributions)
  nav_panel("Exploratory Data (EDA)",
            page_sidebar(
              sidebar = sidebar(
                title = "Demographic Filters",
                selectInput("gender_filter", "Select Gender:", choices = c("All", unique(as.character(df$gender))), selected = "All"),
                selectInput("segment_filter", "Customer Segment:", choices = c("All", unique(as.character(df$customer_segment))), selected = "All")
              ),
              layout_columns(
                card(card_header("Income Distribution"), plotlyOutput("income_bar_plot")),
                card(card_header("Age by Education"), plotlyOutput("age_box_plot"))
              ),
              card(
                card_header("Customer Tenure Density by RFM Segment (Ridgeline)"), 
                plotOutput("ridgeline_plot", height = "300px")
              )
            )
  ),
  
  # MODULE 2: CDA (Correlation & Stats)
  nav_panel("Confirmatory Data (CDA)",
            layout_columns(
              card(
                card_header("Macro Correlation Matrix"), 
                plotOutput("correlation_heatmap", height = "500px"),
                p("Identifies strong positive/negative relationships across all numerical features.", class = "text-muted")
              ),
              card(
                card_header("Statistical Hypothesis Testing (Spearman)"),
                selectInput("cda_facet", "Facet Analysis By:", choices = c("Income Bracket" = "income_bracket", "Education Level" = "education_level")),
                plotOutput("cda_stats_plot", height = "450px"),
                p("Proves the statistical significance between Transaction Count and Satisfaction.", class = "text-muted")
              )
            )
  ),
  
  # MODULE 3: Segmentation (RFM & K-Means)
  nav_panel("Customer Segmentation",
            layout_columns(
              card(
                card_header("Algorithmic Personas (K-Means Clustering)"),
                plotlyOutput("kmeans_scatter", height = "500px"),
                p("Machine learning automatically groups users by volume and frequency behaviors.", class = "text-muted mt-2")
              ),
              card(
                card_header("Value Stratification (RFM Treemap)"),
                plotlyOutput("rfm_treemap", height = "500px"),
                p("Size represents user count; Color represents Average CLV.", class = "text-muted mt-2")
              )
            )
  ),
  
  # MODULE 4: Survival Analysis
  nav_panel("Survival Timeline",
            page_sidebar(
              sidebar = sidebar(
                title = "Survival Parameters",
                selectInput("surv_group", "Group Customers By:", choices = c("Acquisition Channel" = "acquisition_channel", "Income Bracket" = "income_bracket", "Customer Segment" = "customer_segment"))
              ),
              card(
                card_header("Kaplan-Meier Churn Probability Curve"),
                plotOutput("survival_plot", height = "600px"),
                p("Displays the exact tenure month where drop-off risk is highest.", class = "text-muted mt-2")
              )
            )
  ), 
  
  # MODULE 5: Macro Cash Flow (Sankey) 
  nav_panel("Ecosystem Cash Flow",
            card(
              card_header("Macro Liquidity Flow ($)"),
              sankeyNetworkOutput("sankey_plot", height = "600px"),
              p(class = "text-muted mt-2", "Traces capital entering the platform (Deposits) vs. leaving the platform (Withdrawals).")
            )
  ),
  
  # MODULE 6: Risk Simulator
  nav_panel("Predictive Risk Simulator",
            page_sidebar(
              sidebar = sidebar(
                title = "Adjust User Behavior",
                sliderInput("sim_age", "Age:", min = 18, max = 80, value = 35),
                sliderInput("sim_tx", "Total Transactions:", min = 0, max = 500, value = 50),
                sliderInput("sim_tickets", "Support Tickets Logged:", min = 0, max = 20, value = 2),
                sliderInput("sim_sat", "Satisfaction Score (1-5):", min = 1, max = 5, value = 3, step = 1),
                actionButton("run_sim", "Simulate Future Value", class = "btn-primary w-100 mt-3")
              ),
              layout_columns(
                card(card_header("Predicted Churn Risk", class = "bg-danger text-white"), div(class = "text-center mt-4", h1(textOutput("pred_churn")), p("Probability of leaving in the next 30 days."))),
                card(card_header("Predicted CLV", class = "bg-success text-white"), div(class = "text-center mt-4", h1(textOutput("pred_clv")), p("Estimated lifetime revenue generated.")))
              ),
              card(card_header("Identify Similar At-Risk Customers"), DTOutput("similar_customers_table"))
            )
  )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Filter Data for EDA Tab
  filtered_demo_data <- reactive({
    data <- df
    if (input$gender_filter != "All") data <- data %>% filter(gender == input$gender_filter)
    if (input$segment_filter != "All") data <- data %>% filter(customer_segment == input$segment_filter)
    data
  })
  
  # MODULE 1: EDA Plots
  output$income_bar_plot <- renderPlotly({
    p <- ggplot(filtered_demo_data(), aes(x = income_bracket, fill = income_bracket)) + geom_bar() + theme_minimal() + scale_fill_brewer(palette = "Set2") + theme(legend.position = "none") + labs(x="Income Bracket", y="Count")
    ggplotly(p)
  })
  
  output$age_box_plot <- renderPlotly({
    p <- ggplot(filtered_demo_data(), aes(x = education_level, y = age, fill = education_level)) + geom_boxplot(alpha = 0.7) + theme_minimal() + theme(legend.position = "none") + labs(x="Education", y="Age")
    ggplotly(p)
  })
  
  output$ridgeline_plot <- renderPlot({
    ggplot(df, aes(x = customer_tenure, y = RFM_Segment, fill = RFM_Segment)) +
      geom_density_ridges(alpha = 0.7, scale = 1.5) + scale_fill_viridis_d(option = "mako") + theme_minimal() + theme(legend.position = "none") + labs(x = "Tenure (Months)", y = "")
  })
  
  # MODULE 2: CDA Plots
  output$correlation_heatmap <- renderPlot({
    corr_data <- df %>% select(age, tx_count, total_tx_volume, satisfaction_score, support_tickets_count, customer_tenure, churn_probability) %>% filter(complete.cases(.))
    ggcorrplot(cor(corr_data), hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 4, colors = c("#e46764", "white", "#6495ed"), title = "Numerical Feature Correlation") + theme_minimal()
  })
  
  output$cda_stats_plot <- renderPlot({
    ggscatterstats(data = df, x = tx_count, y = satisfaction_score, type = "nonparametric", messages = FALSE) + facet_wrap(vars(!!sym(input$cda_facet)))
  })
  
  # MODULE 3: Segmentation Plots
  output$kmeans_scatter <- renderPlotly({
    plot_ly(df, x = ~tx_count, y = ~total_tx_volume, color = ~Behavior_Cluster, colors = c("#1f77b4", "#ff7f0e", "#2ca02c"), type = "scatter", mode = "markers", hoverinfo = "text", text = ~paste("Risk:", round(churn_probability*100, 1), "%")) %>%
      layout(xaxis = list(title = "Transaction Count"), yaxis = list(title = "Total Volume ($)"))
  })
  
  output$rfm_treemap <- renderPlotly({
    # Summarize data and create a blank Parent column (required by Plotly JS)
    rfm_summary <- df %>% 
      group_by(RFM_Segment) %>% 
      summarise(Count = n(), Avg_CLV = mean(customer_lifetime_value, na.rm = TRUE)) %>%
      mutate(Parent = "") 
    
    plot_ly(
      data = rfm_summary, 
      type = "treemap", 
      labels = ~RFM_Segment, 
      parents = ~Parent,  # Fixed: explicit parent mapping
      values = ~Count, 
      marker = list(colors = ~Avg_CLV, colorscale = "Viridis", showscale = TRUE), 
      textinfo = "label+value"
    ) %>% layout(margin = list(l=0, r=0, b=0, t=0))
  })
  
  # MODULE 4: Survival Analysis
  output$survival_plot <- renderPlot({
    form <- as.formula(paste("Surv(customer_tenure, churn_event) ~", input$surv_group))
    surv_obj <- surv_fit(form, data = df) 
    ggsurvplot(surv_obj, data = df, pval = TRUE, conf.int = TRUE, risk.table = TRUE, ggtheme = theme_minimal(), title = paste("Survival by", input$surv_group))
  })
  
  # MODULE 5: Sankey Cash Flow
  output$sankey_plot <- renderSankeyNetwork({
    actual_in <- sum(df$total_amount_Deposit, na.rm = TRUE)
    actual_out <- sum(df$total_amount_Payment, na.rm = TRUE) + sum(df$total_amount_Transfer, na.rm = TRUE) + sum(df$total_amount_Withdrawal, na.rm = TRUE)
    fmt <- function(x) paste0("$", formatC(x, format="f", digits=0, big.mark=","))
    
    nodes <- data.frame(name = c(paste0("Incoming: \n", fmt(actual_in*0.4)), paste0("Deposits: \n", fmt(actual_in*0.6)), "FinRetain App Wallet", paste0("Payments: \n", fmt(sum(df$total_amount_Payment, na.rm = TRUE))), paste0("Outgoing: \n", fmt(sum(df$total_amount_Transfer, na.rm = TRUE))), paste0("Withdrawals: \n", fmt(sum(df$total_amount_Withdrawal, na.rm = TRUE)))))
    links <- data.frame(source = c(0, 1, 2, 2, 2), target = c(2, 2, 3, 4, 5), value = c(40, 60, 100*(sum(df$total_amount_Payment, na.rm = TRUE)/actual_out), 100*(sum(df$total_amount_Transfer, na.rm = TRUE)/actual_out), 100*(sum(df$total_amount_Withdrawal, na.rm = TRUE)/actual_out)))
    
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value", NodeID = "name", fontSize = 14, nodeWidth = 30, margin = list(left = 100, right = 100))
  })
  
  # MODULE 6: Risk Simulator
  observeEvent(input$run_sim, {
    sim_data <- data.frame(age = input$sim_age, tx_count = input$sim_tx, support_tickets_count = input$sim_tickets, satisfaction_score = input$sim_sat)
    
    output$pred_churn <- renderText({ paste0(round(max(0, min(1, predict(churn_model, newdata = sim_data))) * 100, 1), "%") })
    output$pred_clv <- renderText({ paste0("$", format(round(predict(clv_model, newdata = sim_data), 2), big.mark = ",")) })
    
    similar_users <- df %>% mutate(age_diff = abs(age - input$sim_age), tx_diff = abs(tx_count - input$sim_tx)) %>% arrange(age_diff, tx_diff) %>% head(5) %>% select(customer_id, age, tx_count, support_tickets_count, satisfaction_score, churn_probability)
    output$similar_customers_table <- renderDT({ datatable(similar_users, options = list(dom = 't')) })
  }, ignoreNULL = FALSE) 
}

shinyApp(ui = ui, server = server)
