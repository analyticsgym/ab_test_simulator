library(shiny)
library(tidyverse)
library(broom)
library(effsize)
library(pwr)
library(shinybusy)

# Define UI for application that draws a histogram
ui <- fluidPage(
    add_busy_bar(color = "dodgerblue", height = "8px"),
    titlePanel("A/B Test Simulator"),
    
    sidebarLayout(
        sidebarPanel(
            p("This app sets up a fictitious A/B test where users can select 
            test inputs and simulate results. The purpose of this app is to help
            a user build intuition for how test results vary based on 
            size of effect, within group variation, and sample size."),
            p("Author: Brian Moore (@analyticsanalyst)"),
            HTML("<p><a href='https://github.com/analyticsanalyst/ab_test_simulator'>Code on Github</a></p>"),
            numericInput("baseline", "Control group mean (sales)",
                         value=150, min=1),
            sliderInput("diff", "% change control vs variant",
                        min=-100, value=10, max=100),
            sliderInput("control_sd", "Control group standard deviation (sales)",
                        value=30, min=1, max=200),
            sliderInput("variant_sd", "Variant group standard deviation (sales)",
                        value=30, min=1, max=200),
            numericInput("n", "Test group sample size",
                         value=500, min=50),
            selectInput("bw", "Histogram binwidth", 
                        choices = c(1, 10, 20, 30, 40, 50), selected = 30),
            actionButton("simulate", "Simulate"),
            actionButton("reset", "Reset inputs")
        ),
        mainPanel(
            verbatimTextOutput("power_text"),
            tableOutput("power_result"),
            verbatimTextOutput("ttest_text"),
            tableOutput("ttest_result"),  
            plotOutput("histchart"),
            plotOutput("boxplotchart"),
            plotOutput("mean_and_ci"),
            textOutput("reminder")
        )
    )
)

server <- function(input, output) {
    # simulate test data based on inputs
    df <- eventReactive(input$simulate, {
        control_and_variant_sample_size <- input$n
        control_vs_variant_diff <- input$diff/100
        
        control_mean <- input$baseline
        variant_mean <- control_mean * (1+control_vs_variant_diff)
        
        control_sd <- as.numeric(input$control_sd)
        variant_sd <- as.numeric(input$variant_sd)
        
        tibble(control = abs(rnorm(n=control_and_variant_sample_size,
                                   mean=control_mean,
                                   sd=control_sd)),
               variant = abs(rnorm(n=control_and_variant_sample_size,
                                   mean=variant_mean,
                                   sd=variant_sd)))
    })
    
    # df in tidy format for plotting
    tidy_df <- eventReactive(input$simulate, {
        df() %>% 
            pivot_longer(
                everything(),
                names_to = "experiment_group", 
                values_to = "sales"
            ) %>%
            group_by(experiment_group) %>%
            mutate(group_average = mean(sales)) %>%
            ungroup()
    })
    
    output$power_result <- renderTable({
        d <- cohen.d(df()$control, df()$variant)
        
        pwr_result <- pwr.t.test(n = length(df()$control),
                                 d = d$estimate,
                                 sig.level = 0.05,
                                 type = c("two.sample"))
        
        tibble(cohens_d_effect_size = round(pwr_result$d,3),
               sig_level = pwr_result$sig.level,
               power = round(pwr_result$power,2))
    })
    
    output$ttest_result <- renderTable({
        broom::tidy(t.test(df()$control, df()$variant)) %>%
            rename(control_mean = estimate1,
                   variant_mean = estimate2,
                   p_value = p.value,
                   confidence_interval_lower_bound = conf.low,
                   confidence_interval_upper_bound = conf.high) %>%
            select(control_mean, 
                   variant_mean,
                   p_value,
                   confidence_interval_lower_bound,
                   confidence_interval_upper_bound)
    })
    
    output$histchart <- renderPlot({
        tidy_df() %>%
            ggplot(aes(x=sales,
                       fill=experiment_group)) +
            geom_histogram(alpha=0.6, position = "identity", 
                           binwidth = as.numeric(input$bw)) +
            geom_vline(aes(xintercept=
                               ifelse(
                                   experiment_group=="control",
                                   group_average, NA
                               ), 
                           color="control average"), 
                       linetype="dashed", 
                       size=1.5) +
            geom_vline(aes(xintercept=
                               ifelse(
                                   experiment_group=="variant",
                                   group_average,
                                   NA
                               ), 
                           color="variant average"),
                       linetype="dashed",
                       size=1.5) +
            theme(legend.position = "top",
                  legend.direction = "vertical") +
            guides(fill=guide_legend(nrow=2,byrow=TRUE),
                   color=guide_legend(nrow=2,byrow=TRUE)) +
            labs(title= "\n\nHistograms of Control vs Variant group sales",
                 x="Sales",
                 y="Observation Count",
                 color="Experiment Group Average",
                 fill="Experiment Group Observations")
    }, res = 96)
    
    output$boxplotchart <- renderPlot({
        tidy_df() %>%
            ggplot(aes(x=sales,
                       y=reorder(experiment_group, desc(experiment_group)),
                       fill=experiment_group)) +
            geom_boxplot() +
            theme(legend.position = "top") +
            guides(fill=guide_legend(nrow=2,byrow=TRUE)) + 
            labs(title= "\n\nBoxplots of Control vs Variant group sales",
                 x="Sales",
                 y="",
                 fill="Experiment Group")
    }, res = 96)
    
    ### plot means and CIs of mean
    output$mean_and_ci <- renderPlot({
        tidy_df() %>%
            group_by(experiment_group) %>%
            summarize(group_average = mean(sales),
                      sd = sd(sales),
                      se = sd/sqrt(n()),
                      lb = group_average - (1.96 * se),
                      ub = group_average + (1.96 * se)) %>%
            ungroup() %>%
            ggplot(aes(x=group_average,
                       y=reorder(experiment_group, desc(experiment_group)),
                       group=1)) +
            geom_errorbar(aes(xmin=lb, xmax=ub), width=0.5, colour="red") +
            geom_text(aes(label=round(group_average)), angle=90, hjust=-0.35) +
            geom_text(aes(x=lb, label=round(lb)), angle=90, vjust=-0.4) +
            geom_text(aes(x=ub, label=round(ub)), angle=90, vjust=1.4) +
            geom_point(size=4) +
            scale_x_continuous(expand = expansion(0.5,0.5)) +
            labs(title= "\n\nMean of Control vs Variant with 95% Confidence Interval of Mean",
                 subtitle = "These confidence internals are not the same as the 
confidence internal comparing group means.",
                 x="Sales",
                 y="",
                 fill="Experiment Group")
    }, res = 96)
    
    # when reset action button click update inputs back to default
    observeEvent(input$reset, {
        updateNumericInput(inputId = "baseline", value=150)
        updateSliderInput(inputId = "diff", value=10)
        updateSliderInput(inputId = "control_sd", value=30)
        updateSliderInput(inputId = "variant_sd", value=30)
        updateNumericInput(inputId = "n", value=500)
        updateSliderInput(inputId = "bw", value=30)
    })
    
    # text above power analysis results
    # text shows after first simulation button click
    output$power_text <- renderText({
        req(input$simulate)
        "Scenario Power
        \U2022 power: probability of rejecting the null hypothesis if it is false
        \U2022 power calculation based on test simulated test inputs
        \U2022 0.8 tends to be industry default
        \U2022 when power is less than 0.8, adjust inputs to build intuition for factors that increase power"
    })
    
    # text above ttest results
    # text shows after first simulation button click
    output$ttest_text <- renderText({
        req(input$simulate)
        "Ttest Results
      \U2022 two sided ttest comparing mean of the control vs mean of the variant
      \U2022 null hypothesis: group difference is zero
      \U2022 alternative hypothesis: group difference is not zero
      \U2022 when pvalue is less than 0.05 we reject the null hypothesis in favor of the alternative hypothesis"
    })
}

shinyApp(ui = ui, server = server)


