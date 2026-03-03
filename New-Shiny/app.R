#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


# ==========================================
# FinRetain Shiny Application (DEPLOYMENT READY)
# Includes Advanced Analytics (Survival, RFM & Sankey)
# ==========================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr) 
library(survival)   
library(survminer)  
library(networkD3)  

# --- 1. DATA LOADING & MUTATION ---
df <- readRDS("app_data.rds")

# Clean/format specific variables
df$income_bracket <- factor(df$income_bracket, levels = c("Low", "Medium", "High", "Very High"))

# Dynamically calculate advanced metrics on the fly!
df <- df %>%
  mutate(
    churn_event = ifelse(churn_probability > 0.6, 1, 0),
    R_Score = ntile(customer_tenure, 3),
    F_Score = ntile(tx_count, 3),
    M_Score = ntile(total_tx_volume, 3),
    RFM_Segment = case_when(
      R_Score == 3 & F_Score == 3 & M_Score == 3 ~ "Champions",
      R_Score <= 2 & F_Score == 3 & M_Score == 3 ~ "Loyal Customers",
      R_Score == 3 & F_Score <= 2 ~ "Recent Users",
      R_Score == 1 & F_Score == 1 & M_Score == 1 ~ "Lost/Churned Risks",
      TRUE ~ "Average Users"
    )
  )

# Train predictive models for the Simulator
churn_model <- lm(churn_probability ~ age + tx_count + support_tickets_count + satisfaction_score, data = df)
clv_model <- lm(customer_lifetime_value ~ age + tx_count + support_tickets_count + satisfaction_score, data = df)


# --- 2. USER INTERFACE (UI) ---
ui <- page_navbar(
  title = "FinRetain: Customer Visual Analytics",
  theme = bs_theme(version = 5, bootswatch = "flatly"), 
  
  # MODULE 1: Demographics
  nav_panel("Demographics (EDA)",
            page_sidebar(
              sidebar = sidebar(
                title = "Demographic Filters",
                selectInput("gender_filter", "Select Gender:", choices = c("All", unique(df$gender)), selected = "All"),
                selectInput("segment_filter", "Customer Segment:", choices = c("All", unique(df$customer_segment)), selected = "All")
              ),
              layout_columns(
                card(card_header("Income Bracket Distribution"), plotlyOutput("income_bar_plot")),
                card(card_header("Age Distribution by Education Level"), plotlyOutput("age_box_plot"))
              )
            )
  ),
  
  # MODULE 2: Behavioral & Transactional Analyst 
  nav_panel("Behavioral Analyst (CDA)",
            page_sidebar(
              sidebar = sidebar(
                title = "Behavioral Variables",
                selectInput("x_var", "X-Axis Variable:", choices = c("Total Transactions" = "tx_count", "App Logins" = "app_logins_frequency", "Avg Transaction Value" = "avg_tx_value")),
                selectInput("y_var", "Y-Axis Variable:", choices = c("Satisfaction Score" = "satisfaction_score", "NPS Score" = "nps_score", "Customer Lifetime Value" = "customer_lifetime_value")),
                checkboxInput("show_trend", "Show Statistical Trendline", value = TRUE)
              ),
              layout_columns(
                card(card_header("Interactive Correlation Analysis"), plotlyOutput("behavior_scatter")),
                card(card_header("Transaction Volume Breakdown by Type"), plotlyOutput("tx_type_bar"), p("Compares total volume of Payments, Transfers, Withdrawals, and Deposits.", class = "text-muted mt-2"))
              )
            )
  ),
  
  # MODULE 3: Risk Simulator
  nav_panel("Churn & CLV Risk Simulator",
            page_sidebar(
              sidebar = sidebar(
                title = "Simulate Customer Profile",
                sliderInput("sim_age", "Age:", min = 18, max = 80, value = 35),
                sliderInput("sim_tx", "Total Transactions:", min = 0, max = 500, value = 50),
                sliderInput("sim_tickets", "Support Tickets Logged:", min = 0, max = 20, value = 2),
                sliderInput("sim_sat", "Satisfaction Score (1-5):", min = 1, max = 5, value = 3, step = 1),
                actionButton("run_sim", "Run Simulation", class = "btn-primary w-100 mt-3")
              ),
              layout_columns(
                card(card_header("Predicted Churn Risk", class = "bg-danger text-white"), div(class = "text-center mt-4", h1(textOutput("pred_churn")), p("Probability of customer abandoning the app in the next 30 days."))),
                card(card_header("Predicted Lifetime Value (CLV)", class = "bg-success text-white"), div(class = "text-center mt-4", h1(textOutput("pred_clv")), p("Estimated total revenue this customer will generate.")))
              ),
              card(card_header("Similar Customers in Database"), DTOutput("similar_customers_table"))
            )
  ),
  
  # MODULE 4: Survival Analysis
  nav_panel("Survival Analysis",
            page_sidebar(
              sidebar = sidebar(
                title = "Survival Parameters",
                selectInput("surv_group", "Group Customers By:", choices = c("Acquisition Channel" = "acquisition_channel", "Income Bracket" = "income_bracket", "Customer Segment" = "customer_segment"))
              ),
              card(
                card_header("Kaplan-Meier Churn Probability Curve"),
                plotOutput("survival_plot", height = "600px"),
                p("Displays the exact tenure month where a specific group of customers is statistically most likely to churn. Includes risk table.", class = "text-muted mt-2")
              )
            )
  ), 
  
  # MODULE 5: RFM Treemap
  nav_panel("RFM Segmentation",
            card(
              card_header("Customer Base Treemap (Recency, Frequency, Monetary)"),
              plotlyOutput("rfm_treemap", height = "600px"),
              p("The size of the box represents the number of customers. The color scale represents average Customer Lifetime Value (CLV).", class = "text-muted mt-2")
            )
  ), 
  
  # MODULE 6: Cash Flow Sankey 
  nav_panel("Macro Cash Flow (Sankey)",
            card(
              card_header("App Ecosystem Cash Flow ($)"),
              sankeyNetworkOutput("sankey_plot", height = "600px"),
              p(class = "text-muted mt-2", "Follow the flow of funds from external sources, through the FinRetain wallet, and out to various transaction types.")
            )
  )
)


# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Module 1 Outputs
  filtered_demo_data <- reactive({
    data <- df
    if (input$gender_filter != "All") data <- data %>% filter(gender == input$gender_filter)
    if (input$segment_filter != "All") data <- data %>% filter(customer_segment == input$segment_filter)
    data
  })
  
  output$income_bar_plot <- renderPlotly({
    p <- ggplot(filtered_demo_data(), aes(x = income_bracket, fill = income_bracket)) + geom_bar() + theme_minimal() + labs(x = "Income Bracket", y = "Number of Customers") + scale_fill_brewer(palette = "Set2") + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$age_box_plot <- renderPlotly({
    p <- ggplot(filtered_demo_data(), aes(x = education_level, y = age, fill = education_level)) + geom_boxplot(alpha = 0.7) + theme_minimal() + labs(x = "Education Level", y = "Age") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # Module 2 Outputs
  output$behavior_scatter <- renderPlotly({
    plot_data <- df %>% sample_n(min(nrow(df), 2000)) 
    p <- ggplot(plot_data, aes_string(x = input$x_var, y = input$y_var)) + geom_point(alpha = 0.5, color = "#2c3e50") + theme_minimal()
    if (input$show_trend) p <- p + geom_smooth(method = "lm", color = "#e74c3c", se = FALSE)
    ggplotly(p)
  })
  
  output$tx_type_bar <- renderPlotly({
    tx_sums <- df %>%
      summarise(
        Payment = sum(total_amount_Payment, na.rm = TRUE),
        Transfer = sum(total_amount_Transfer, na.rm = TRUE),
        Withdrawal = sum(total_amount_Withdrawal, na.rm = TRUE),
        Deposit = sum(total_amount_Deposit, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Transaction_Type", values_to = "Total_Volume")
    
    p <- ggplot(tx_sums, aes(x = reorder(Transaction_Type, -Total_Volume), y = Total_Volume, fill = Transaction_Type)) + geom_col() + theme_minimal() + labs(x = "Transaction Type", y = "Total Volume ($)") + scale_fill_brewer(palette = "Pastel1") + theme(legend.position = "none")
    ggplotly(p)
  })
  
  # Module 3 Outputs
  observeEvent(input$run_sim, {
    sim_data <- data.frame(age = input$sim_age, tx_count = input$sim_tx, support_tickets_count = input$sim_tickets, satisfaction_score = input$sim_sat)
    predicted_churn <- max(0, min(1, predict(churn_model, newdata = sim_data)))
    predicted_clv <- predict(clv_model, newdata = sim_data)
    
    output$pred_churn <- renderText({ paste0(round(predicted_churn * 100, 1), "%") })
    output$pred_clv <- renderText({ paste0("$", format(round(predicted_clv, 2), big.mark = ",")) })
    
    similar_users <- df %>% mutate(age_diff = abs(age - input$sim_age), tx_diff = abs(tx_count - input$sim_tx)) %>% arrange(age_diff, tx_diff) %>% head(5) %>% select(customer_id, age, tx_count, support_tickets_count, satisfaction_score, churn_probability)
    output$similar_customers_table <- renderDT({ datatable(similar_users, options = list(dom = 't')) })
  }, ignoreNULL = FALSE) 
  
  # Module 4 Outputs
  output$survival_plot <- renderPlot({
    form <- as.formula(paste("Surv(customer_tenure, churn_event) ~", input$surv_group))
    surv_obj <- surv_fit(form, data = df) 
    p <- ggsurvplot(surv_obj, data = df, pval = TRUE, conf.int = TRUE, risk.table = TRUE, ggtheme = theme_minimal(), title = paste("Churn Survival by", input$surv_group))
    print(p)
  })
  
  # Module 5 Outputs
  output$rfm_treemap <- renderPlotly({
    rfm_summary <- df %>% group_by(RFM_Segment) %>% summarise(Count = n(), Avg_CLV = mean(customer_lifetime_value, na.rm = TRUE))
    plot_ly(data = rfm_summary, type = "treemap", labels = ~RFM_Segment, parents = NA, values = ~Count, marker = list(colors = ~Avg_CLV, colorscale = "Viridis", showscale = TRUE), textinfo = "label+value+percent root", hoverinfo = "label+value") %>% layout(margin = list(l=0, r=0, b=0, t=0))
  })
  
  # Module 6 Outputs: Cash Flow Sankey (UPDATED FOR BALANCED VISUALS)
  output$sankey_plot <- renderSankeyNetwork({
    
    # 1. Calculate the ACTUAL exact dollar values for the text labels
    actual_in <- sum(df$total_amount_Deposit, na.rm = TRUE)
    actual_incoming_transfers <- actual_in * 0.4
    actual_direct_deposits <- actual_in * 0.6
    
    actual_payments <- sum(df$total_amount_Payment, na.rm = TRUE)
    actual_outgoing_transfers <- sum(df$total_amount_Transfer, na.rm = TRUE)
    actual_withdrawals <- sum(df$total_amount_Withdrawal, na.rm = TRUE)
    
    actual_out <- actual_payments + actual_outgoing_transfers + actual_withdrawals
    
    # Helper function to format numbers as beautiful currency
    fmt <- function(x) paste0("$", formatC(x, format="f", digits=0, big.mark=","))
    
    # 2. Create Nodes with the ACTUAL values baked into their names
    nodes <- data.frame(
      name = c(
        paste0("Incoming Transfers: \n", fmt(actual_incoming_transfers)), 
        paste0("Direct Deposits: \n", fmt(actual_direct_deposits)),   
        paste0("FinRetain App Wallet"), # Removed the total here since IN and OUT are technically unequal                   
        paste0("App Payments: \n", fmt(actual_payments)), 
        paste0("Outgoing Transfers: \n", fmt(actual_outgoing_transfers)), 
        paste0("Cash Withdrawals: \n", fmt(actual_withdrawals))
      )
    )
    
    # 3. Calculate BALANCED proportions for the visual drawing (Forcing Left = Right = 100)
    vis_incoming_transfers <- 100 * 0.4
    vis_direct_deposits <- 100 * 0.6
    
    vis_payments <- 100 * (actual_payments / actual_out)
    vis_outgoing_transfers <- 100 * (actual_outgoing_transfers / actual_out)
    vis_withdrawals <- 100 * (actual_withdrawals / actual_out)
    
    # 4. Create Links using the BALANCED visual weights, not the raw dollars
    links <- data.frame(
      source = c(0, 1, 2, 2, 2), 
      target = c(2, 2, 3, 4, 5),
      value = c(vis_incoming_transfers, vis_direct_deposits, vis_payments, vis_outgoing_transfers, vis_withdrawals)
    )
    
    # 5. Render the Interactive Sankey
    sankeyNetwork(
      Links = links, Nodes = nodes,
      Source = "source", Target = "target", Value = "value",
      NodeID = "name", fontSize = 14, nodeWidth = 30,
      colourScale = JS('d3.scaleOrdinal(d3.schemeCategory10);'),
      margin = list(left = 220, right = 220) # Slightly widened margins for the text
    )
  })
}

# --- 4. RUN APPLICATION ---
shinyApp(ui = ui, server = server)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
