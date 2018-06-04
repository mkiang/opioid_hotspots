## Imports ----
library(shiny)
library(geofacet)
library(tidyverse)
library(digest)
library(DT)
library(fs)
library(statebins)
library(cowplot)
source(normalizePath("./../code/mk_nytimes.R"))
source(normalizePath("./../code/utils.R"))

## Load data ----
jp_results <- readRDS(
  normalizePath("./../data/joinpoint_results_dupe_rows_2016.RDS")
)

## Remove total population from levels
jp_results$race_cat <- factor(jp_results$race_cat)

## Make cached folders ----
dir.create(normalizePath("./cached_images"), showWarnings = FALSE)

## Make a state name:abbrev dictionary ----
st_name_abbrev <- as.list(c(state.abb, "DC"))
names(st_name_abbrev) <- c(state.name, "District of Columbia")
st_name_abbrev <- as.list(sort(unlist(st_name_abbrev)))

# Define UI ----
# for application that draws a histogram
ui <- navbarPage(
  "Results Explorer", 
  
  ## NATIONAL PANEL ----
  tabPanel(
    title = "National Overview",
    
    ## Top row ====
    fluidRow(
      ## Paper citation ####
      column(
        width = 4, 
        wellPanel(
          tags$blockquote(
            em("Geographic variation in opioid mortality by", 
               "race/ethnicity in the United States, 1999-2016:", 
               "Identifying epidemic hotspots")
          ), 
          tags$ul(
            tags$li(a(href="https://mathewkiang.com", 
                      "Mathew V Kiang"), 
                    "(", 
                    a(href="https://twitter.com/mathewkiang", 
                      "@mathewkiang"),
                    ")"
            ), 
            tags$li(a(href="http://monicaalexander.com", 
                      "Monica J Alexander"),
                    "(", 
                    a(href="https://twitter.com/monjalexander", 
                      "@monjalexander"),
                    ")"
            ),
            tags$li(a(href="https://sites.google.com/view/zhezhang/home", 
                      "Zhe Zhang")
            ), 
            tags$li(a(href=paste0("http://www.dfhcc.harvard.edu/",
                                  "insider/member-detail/member/",
                                  "jarvis-t-chen-scd/"),
                      "Jarvis T Chen")
            )
          )
        )
      ), 
      
      column(
        width = 8, 
        h4("Explore national results"), 
        p("We performed joinpoint regressions by state, 
          race/ethnicity, and opioid type. Below, each line is the 
          result of a single joinpoint regression. We arrange the results
          approximately in their geographical location (except AK and HI) so
          the geograhpical patterning is clearer. Individual states can
          be explored in the tab above. Solid lines indicate a 
          statistically significant slope (i.e., increase or decrease). 
          Solid points on the line represent a statistically 
          significant change in the slope before and after that 
          point. Raw data (and 95% confidence intervals) 
          are presented in the back. We defined statistical significance 
          as P<0.01; however, you can adjust this value below."),
        
        h4("More information"), 
        p("This is an interactive companion to our paper, which", 
          a(href=paste0("https://paa.confex.com/paa/2018/", 
                        "webprogrampreliminary/Paper22264.html"), 
            "was presented at PAA 2018"), "and", 
          a(href=paste0("https://eaps.confex.com/eaps/2018/meetingapp.cgi/Paper/2208"), 
            "EPC 2018."),
          "To learn more about the paper and code (or report bugs), see our", 
          a(href="https://github.com/mkiang/opioid_hotspots", 
            "Github repository."), 
          "Data from 1999 to 2015 come from the", "restricted-access",  
          a("multiple cause of death", 
            href = "https://www.cdc.gov/nchs/nvss/dvs_data_release.htm"), 
          "files provided by the", 
          a("National Center for Health Statistics.", 
            href = "https://www.cdc.gov/nchs/index.htm"), 
          "When possible, we supplement this data with 2016 data from",
          a("CDC WONDER.", href = "https://wonder.cdc.gov/mcd-icd10.html"),
          "A table of the average annual percent change (AAPC) is shown at the
          bottom. The AAPC can be interpreted as the estimated average change
          from the beginning of the period to the end of the period."
        )
        )
      ),  
    
    hr(), 
    
    ## National plot ####
    fluidRow(
      column(
        width = 12,
        align = "center", 
        h3("Joinpoint Results Plot"), 
        imageOutput("state_map", height = "auto", width = "auto")
      )
    ), 
    
    hr(), 
    
    ## Settings row ====
    ## subsetting data ####
    fluidRow(
      column(
        width = 4,
        h4("Data / Significance"), 
        selectInput(
          "outcome", 
          label = "Select outcome",
          choices = list("All opioids" = "opioid", 
                         "Natural/Semi-synethtic" = "natural_opioid",
                         "Heroin" = "heroin",
                         "Synthetic" = "synth_opioid"),
          selected = "All opioids"),
        checkboxGroupInput(
          "groups",
          label = "Race/Ethnicity:",
          choices = list("Non-Hispanic white" = "nhw",
                         "Non-Hispanic black" = "nhb"), 
          selected = c("nhw", "nhb")), 
        selectInput(
          "sigpvalue", 
          label = "Significance level:",
          choices = list("P < 0.100" = 0.100, 
                         "P < 0.050" = 0.050,
                         "P < 0.010" = 0.010,
                         "P < 0.005" = 0.005, 
                         "P < 0.001" = 0.001),
          selected = 0.010)
      ), 
      
      ## Applying geom_*s ####
      column(
        width = 4, 
        h4("Select plotting geometries"), 
        checkboxInput(
          "show_raw", 
          "Plot raw data",
          value = TRUE),
        conditionalPanel(
          condition = "input.show_raw == true",
          checkboxInput(
            "raw_ci", 
            "Plot raw data 95% CI", 
            value = FALSE)
        ), 
        checkboxInput(
          "model_fit", 
          "Plot model line",
          value = TRUE),
        conditionalPanel(
          condition = "input.model_fit == true",
          checkboxInput(
            "linetype_sig", 
            "Show significant segments", 
            value = TRUE)
        ),
        checkboxInput(
          "joinpoint", 
          "Plot joinpoint locations",
          value = TRUE), 
        conditionalPanel(
          condition = "input.joinpoints == true",
          checkboxInput(
            "joinpoint_sig", 
            "Show significant slope changes", 
            value = TRUE)
        )
      ), 
      
      ## Plot parameters ####
      column(
        width = 4, 
        h4("Select plot parameters"), 
        sliderInput("ymax", "Maximum y-axis value:",
                    min = 5, max = 50,
                    value = 30, step = 1),
        checkboxInput(
          "sig_aapc_only", 
          "Show significant AAPCs only", 
          value = FALSE), 
        checkboxInput(
          "legends_on", 
          "Show legends", 
          value = FALSE), 
        sliderInput("scale", "Scale plot elements:",
                    min = 1, max = 3,
                    value = 1.75, step = .05),
        submitButton(
          text = "Submit", 
          icon = NULL, 
          width = NULL)
      ) 
    ),
    
    hr(), 
    
    ## Bottom row ====
    ## data table ####
    fluidRow(
      column(
        width = 12, 
        align = "center", 
        h3("AAPC Summary Table"), 
        DT::dataTableOutput("aapc_table")
      )
    ),
    
    hr(),
    
    ## Footer ====
    fluidRow(
      column(
        width = 12, 
        align = 'center', 
        HTML("Created in <a href='https://shiny.rstudio.com/'>Shiny</a> by 
             <a href='https://mathewkiang.com'>Mathew Kiang</a>.
             Generously hosted by the 
             <a href='http://www.demog.berkeley.edu/'>Department of 
             Demography at University of California, Berkeley</a>.")
        )
        )
        ),
  
  ## STATE SECTION ----
  tabPanel(
    title = "State Results", 
    ## Top row ====
    ## Top row ====
    fluidRow(
      ## Paper citation ####
      column(
        width = 4, 
        wellPanel(
          tags$blockquote(
            em("Geographic variation in opioid mortality by", 
               "race/ethnicity in the United States, 1999-2016:", 
               "Identifying epidemic hotspots")
          ), 
          tags$ul(
            tags$li(a(href="https://mathewkiang.com", 
                      "Mathew V Kiang"), 
                    "(", 
                    a(href="https://twitter.com/mathewkiang", 
                      "@mathewkiang"),
                    ")"
            ), 
            tags$li(a(href="http://monicaalexander.com", 
                      "Monica J Alexander"),
                    "(", 
                    a(href="https://twitter.com/monjalexander", 
                      "@monjalexander"),
                    ")"
            ),
            tags$li(a(href="https://sites.google.com/view/zhezhang/home", 
                      "Zhe Zhang")
            ), 
            tags$li(a(href=paste0("http://www.dfhcc.harvard.edu/",
                                  "insider/member-detail/member/",
                                  "jarvis-t-chen-scd/"),
                      "Jarvis T Chen")
            )
          )
        )
      ), 
      
      ## More information ####
      column(
        width = 8, 
        h4("Explore state results"), 
        p("We performed joinpoint regressions by state, 
          race/ethnicity, and opioid type. Below, each line is the 
          result of a single joinpoint regression. Solid lines indicate a 
          statistically significant slope (i.e., increase or decrease). 
          Solid points on the line represent a statistically 
          significant change in the slope before and after that 
          point. Raw data (and 95% confidence intervals) 
          are presented in the back. We defined statistical significance 
          as P<0.01; however, you can adjust this value below. There is
          a clear geographical patterning for some opioids --- see the
          national results tab for more."),
        
        h4("More information"), 
        p("This is an interactive companion to our paper, which", 
          a(href=paste0("https://paa.confex.com/paa/2018/", 
                        "webprogrampreliminary/Paper22264.html"), 
            "was presented at PAA 2018"), "and", 
          a(href=paste0("https://eaps.confex.com/eaps/2018/meetingapp.cgi/Paper/2208"), 
            "EPC 2018."),
          "To learn more about the paper and code (or report bugs), see our", 
          a(href="https://github.com/mkiang/opioid_hotspots", 
            "Github repository."), 
          "Data from 1999 to 2015 come from the", "restricted-access",  
          a("multiple cause of death", 
            href = "https://www.cdc.gov/nchs/nvss/dvs_data_release.htm"), 
          "files provided by the", 
          a("National Center for Health Statistics.", 
            href = "https://www.cdc.gov/nchs/index.htm"), 
          "When possible, we supplement this data with 2016 data from",
          a("CDC WONDER.", href = "https://wonder.cdc.gov/mcd-icd10.html"),
          "A table of the average annual percent change (AAPC) is shown at the
          bottom. The AAPC can be interpreted as the estimated average change
          from the beginning of the period to the end of the period."
        )
        )
        ),  
    
    hr(), 
    
    ## Plotting row ====
    fluidRow(
      column(
        width = 12,
        align = "center", 
        h3("State-specific Joinpoint Results"),
        imageOutput("state_specific", height = "auto", width = "auto")
      )
    ), 
    
    hr(), 
    
    ## Plotting parameters ====
    fluidRow(
      ## Subset data ####
      column(
        width = 4,
        h4("Data / Significance"), 
        selectInput(
          "s_state", 
          label = "Select state",
          choices = st_name_abbrev,
          selected = "Alaska"),
        checkboxGroupInput(
          "s_groups",
          label = "Race/Ethnicity:",
          choices = list("Non-Hispanic white" = "nhw",
                         "Non-Hispanic black" = "nhb"), 
          selected = c("nhw", "nhb")), 
        selectInput(
          "s_sigpvalue", 
          label = "Significance level:",
          choices = list("P < 0.100" = 0.100, 
                         "P < 0.050" = 0.050,
                         "P < 0.010" = 0.010,
                         "P < 0.005" = 0.005, 
                         "P < 0.001" = 0.001),
          selected = 0.010)
      ), 
      
      ## Subset data ####
      column(
        width = 4, 
        h4("Select plotting geometries"), 
        checkboxInput(
          "s_show_raw", 
          "Plot raw data",
          value = TRUE),
        conditionalPanel(
          condition = "input.show_raw == true",
          checkboxInput(
            "s_raw_ci", 
            "Plot raw data 95% CI", 
            value = FALSE)
        ), 
        checkboxInput(
          "s_model_fit", 
          "Plot model line",
          value = TRUE),
        conditionalPanel(
          condition = "input.model_fit == true",
          checkboxInput(
            "s_linetype_sig", 
            "Show significant segments", 
            value = TRUE)
        ),
        checkboxInput(
          "s_joinpoint", 
          "Plot joinpoint locations",
          value = TRUE), 
        conditionalPanel(
          condition = "input.joinpoints == true",
          checkboxInput(
            "s_joinpoint_sig", 
            "Show significant slope changes", 
            value = TRUE)
        )
      ), 
      
      column(
        width = 4, 
        h4("Select plot parameters"), 
        sliderInput("s_ymax", "Maximum y-axis value:",
                    min = 5, max = 50,
                    value = 30, step = 1),
        checkboxInput(
          "s_legends_on", 
          "Show legends", 
          value = FALSE), 
        sliderInput("s_scale", "Scale plot elements:",
                    min = 1, max = 3,
                    value = 1.75, step = .05),
        submitButton(
          text = "Submit", 
          icon = NULL, 
          width = NULL)
      ) 
    ),
    
    hr(), br(), br(), 
    
    ## Table row ====
    fluidRow(
      column(
        width = 12, 
        align = "center", 
        h3("Model parameter estimates"), 
        DT::dataTableOutput("state_table_estimates")
      )
    ),
    br(), br(), 
    fluidRow(
      column(
        width = 7, 
        align = "center", 
        h3("Predicted and observed rates"), 
        DT::dataTableOutput("state_table_predictions")
      ), 
      column(
        width = 4,
        offset = .75, 
        align = "center", 
        h3("Model fit summary"), 
        DT::dataTableOutput("state_table_fit")
      )
    ),
    
    hr(),
    ## Footer ====
    fluidRow(
      column(
        width = 12, 
        align = 'center', 
        HTML("Created in <a href='https://shiny.rstudio.com/'>Shiny</a> by 
             <a href='https://mathewkiang.com'>Mathew Kiang</a>.
             Generously hosted by the 
             <a href='http://www.demog.berkeley.edu/'>Department of 
             Demography at University of California, Berkeley</a>.")
        )
      )
    ), 
  
  ## EPIDEMIC HOTSPOTS SECTION ----
  tabPanel(
    title = "Epidemic Hotspots",
    
    ## Top row ====
    fluidRow(
      ## Paper citation ####
      column(
        width = 4, 
        wellPanel(
          tags$blockquote(
            em("Geographic variation in opioid mortality by", 
               "race/ethnicity in the United States, 1999-2016:", 
               "Identifying epidemic hotspots")
          ), 
          tags$ul(
            tags$li(a(href="https://mathewkiang.com", 
                      "Mathew V Kiang"), 
                    "(", 
                    a(href="https://twitter.com/mathewkiang", 
                      "@mathewkiang"),
                    ")"
            ), 
            tags$li(a(href="http://monicaalexander.com", 
                      "Monica J Alexander"),
                    "(", 
                    a(href="https://twitter.com/monjalexander", 
                      "@monjalexander"),
                    ")"
            ),
            tags$li(a(href="https://sites.google.com/view/zhezhang/home", 
                      "Zhe Zhang")
            ), 
            tags$li(a(href=paste0("http://www.dfhcc.harvard.edu/",
                                  "insider/member-detail/member/",
                                  "jarvis-t-chen-scd/"),
                      "Jarvis T Chen")
            )
          )
        )
      ), 
      
      column(
        width = 8, 
        h4("Identify Epidemic Hotspots"), 
        p("We define epidemic hotspots as areas with both high rates of
          mortality as well as rapid increases in their mortality rates. 
          Below, you can change the threshold for what is considered a 'high'
          mortality rate and a 'rapid' increase by adjusting the boundaries of
          the middle category. The text on the below (on the right) will 
          indicate what percentile your chosen boundaries represent in the 
          data. The plot only shows statistically significant increases, 
          which we define as P < 0.01; however, you can adjust the level
          of statistical significance below."),
        
        h4("More information"), 
        p("This is an interactive companion to our paper, which", 
          a(href=paste0("https://paa.confex.com/paa/2018/", 
                        "webprogrampreliminary/Paper22264.html"), 
            "was presented at PAA 2018"), "and", 
          a(href=paste0("https://eaps.confex.com/eaps/2018/meetingapp.cgi/Paper/2208"), 
            "EPC 2018."),
          "To learn more about the paper and code (or report bugs), see our", 
          a(href="https://github.com/mkiang/opioid_hotspots", 
            "Github repository."), 
          "Data from 1999 to 2015 come from the", "restricted-access",  
          a("multiple cause of death", 
            href = "https://www.cdc.gov/nchs/nvss/dvs_data_release.htm"), 
          "files provided by the", 
          a("National Center for Health Statistics.", 
            href = "https://www.cdc.gov/nchs/index.htm"), 
          "When possible, we supplement this data with 2016 data from",
          a("CDC WONDER.", href = "https://wonder.cdc.gov/mcd-icd10.html")
          )
        )
      ),  
    
    hr(), 
    
    ## Epidemic Hotspots Plot ####
    fluidRow(
      column(
        width = 3,
        align = "right", 
        h2(""),
        br(), 
        style = "vertical-align: bottom; margin-top: 25px;",
        imageOutput("hotspots_legend", height = "auto", width = "auto")
      ),
      column(
        width = 9,
        align = "left", 
        h3("Epidemic Hotspots"), 
        imageOutput("hotspots_map", height = "auto", width = "auto")
      )
    ), 
    
    hr(), 
    
    ## Settings row ====
    ## subsetting data ####
    fluidRow(
      column(
        width = 4,
        h4("Select data"), 
        selectInput(
          "h_outcome", 
          label = "Select outcome",
          choices = list("All opioids" = "opioid", 
                         "Natural/Semi-synethtic" = "natural_opioid",
                         "Heroin" = "heroin",
                         "Synthetic" = "synth_opioid"),
          selected = "All opioids"),
        selectInput(
          "h_sigpvalue", 
          label = "Significance level:",
          choices = list("P < 0.100" = 0.100, 
                         "P < 0.050" = 0.050,
                         "P < 0.010" = 0.010,
                         "P < 0.005" = 0.005, 
                         "P < 0.001" = 0.001),
          selected = 0.010), 
        submitButton(
          text = "Submit", 
          icon = NULL, 
          width = NULL)
      ), 
      
      ## Mortality bins
      column(
        width = 4, 
        h4("Define bins and significance:"), 
        sliderInput("mort_mid_bin", 
                    label = "Range of the 'Medium' Mortality bin:", 
                    min = 2.5, 
                    max = 30, 
                    step = 2.5, 
                    value = c(10, 20)
                    ), 
        ## APC bins
        sliderInput("apc_mid_bin", label = "Range of the 'Moderate' APC bin:", 
                    min = 5, 
                    max = 100,
                    step = 5,
                    value = c(10, 30)
                    ), 
        sliderInput("h_scale", "Scale plot elements:",
                    min = 1, max = 3,
                    value = 1.75, step = .05)
    ),
      
      ## Plot parameters ####
      column(
        width = 4, 
        h4("Percentiles"), 
        htmlOutput("percentile_hotspots")
      ) 
    ),
    
    hr(), 
    
    ## Footer ====
    fluidRow(
      column(
        width = 12, 
        align = 'center', 
        HTML("Created in <a href='https://shiny.rstudio.com/'>Shiny</a> by 
             <a href='https://mathewkiang.com'>Mathew Kiang</a>.
             Generously hosted by the 
             <a href='http://www.demog.berkeley.edu/'>Department of 
             Demography at University of California, Berkeley</a>.")
        )
      )
    )
  )

## Define server ----
server <- function(input, output, session) {
  
  ## State facet plot ====
  output$state_map <- renderImage({
    
    clean_cached_folder()
    
    retina_x <- session$clientData$pixelratio
    
    # We need a unique file name based on the input parameters
    outfile <- normalizePath(
      sprintf("./cached_images/%s.jpg",
              digest(list(
                input$outcome,
                input$groups,
                input$show_raw,
                input$raw_ci,
                input$model_fit,
                input$linetype_sig,
                input$joinpoint,
                input$joinpoint_sig,
                input$ymax,
                input$legends_on, 
                input$sig_aapc_only, 
                input$sigpvalue, 
                input$scale, 
                retina_x))
      )
    )
    
    sigpvalue  <- as.numeric(input$sigpvalue)
    fig_height <- 1
    
    # Return the cached object if the file exists, otherwise, run this:
    if (!file.exists(outfile)) {
      
      ## Filter to race and outcome
      temp_df <- jp_results %>% 
        filter(opioid_type == input$outcome, 
               race %in% input$groups)
      
      ## Make significance categories
      temp_df <- temp_df %>% 
        mutate(
          sig_aapc     = ifelse(aapc_pval  < sigpvalue, 1, 0), 
          sig_slope    = ifelse(slope_pval < sigpvalue, 1, 0), 
          sig_slopechg = ifelse(slopechange_pval < sigpvalue, 1, 0)
        ) %>% 
        mutate(
          slope_cat = factor(sig_slope, 
                             levels = 0:1, 
                             labels = c("Non-Sig.", "Sig."), 
                             ordered = TRUE), 
          slopechg_cat = factor(sig_slopechg, 
                                levels = 0:1, 
                                labels = c("Non-Sig.", "Sig."), 
                                ordered = TRUE)
        )
      
      ## Sig AAPCs?
      if (input$sig_aapc_only){
        temp_df <- temp_df %>% 
          filter(sig_aapc == 1)
      }
      
      ## Start a plot
      p <- ggplot(data = temp_df)
      
      ## Add all the necessities
      p <- p +
        facet_geo(~ abbrev) +
        coord_cartesian(ylim = c(0, input$ymax)) +
        scale_x_continuous(NULL,
                           breaks = seq(2000, 2015, 5),
                           labels = c("'00", "'05", "'10", "'15"),
                           expand = c(0, .25)) +
        scale_y_continuous(sprintf("Mortality Rate (per 100,000), %s",
                                   temp_df$opioid_cat[1]),
                           expand = c(0, .5)) + 
        scale_color_brewer("Race/Ethnicity", palette = "Set1", 
                           drop = FALSE)
      
      ## Should we plot the raw?
      if (input$show_raw) {
        if (input$model_fit) {
          alpha_point <- .2
          alpha_line  <- .2
        } else {
          alpha_point <- .8
          alpha_line  <- .5
        }
        
        ## Should we plot the raw CI?
        if (input$raw_ci) {
          p <- p +
            geom_errorbar(aes(x = year, 
                              ymax = rate + 1.96 * rate_se,
                              ymin = rate - 1.96 * rate_se,
                              color = race_cat),
                          width = 0, alpha = alpha_line)
        }
        
        p <- p + 
          geom_point(aes(x = year, y = rate, color = race_cat), 
                     alpha = alpha_point, shape = 5) 
      }
      
      ## Draw model fit?
      if (input$model_fit) {
        if (input$linetype_sig) {
          p <- p + 
            geom_line(aes(x = year, y = pred_rate, color = race_cat,
                          group = interaction(race_cat, line_seg, abbrev),
                          linetype = slope_cat)) 
          p <- p +
            scale_linetype_manual(
              name = sprintf("Slope (P < %0.3f)",sigpvalue),
              limits = c("Non-Sig.", "Sig."), 
              values = c("dashed", "solid"),
              drop = FALSE)
        } else {
          p <- p + 
            geom_line(aes(x = year, y = pred_rate, color = race_cat,
                          group = interaction(race_cat, line_seg)))
        }
      }
      
      ## Draw joinpoints?
      if (input$joinpoint) {
        if (input$joinpoint_sig) {
          p <- p +
            geom_point(aes(x = j_year, y = pred_rate, 
                           color = race_cat,
                           shape = slopechg_cat))
          p <- p +
            scale_shape_manual(
              name = sprintf("Change in slope (P < %0.3f)", sigpvalue), 
              values = c(0, 19), 
              limits = c("Non-Sig.", "Sig."), 
              drop = FALSE)
        } else {
          p <- p +
            geom_point(aes(x = j_year, y = pred_rate, 
                           color = race_cat))
        }
      }
      
      ## Show legends?
      if (input$legends_on) {
        p <- p + 
          mk_nytimes(panel.border = 
                       element_rect(linetype = "solid",
                                    fill = NA,
                                    color = "grey75"), 
                     legend.position = "bottom", 
                     legend.direction = "vertical") 
        fig_height <- fig_height + .25
      } else {
        p <- p + 
          mk_nytimes(panel.border = 
                       element_rect(linetype = "solid",
                                    fill = NA,
                                    color = "grey75"), 
                     legend.position = "none")
      }
      
      ggsave(outfile, p, 
             width = 10 / retina_x, 
             height = 7 * fig_height / retina_x, 
             dpi = 150 * retina_x, 
             scale = input$scale)
    }
    
    ## Return a list containing the filename, type, dimensions, etc.
    list(
      src = outfile, 
      contentType = 'image/jpg', 
      width = 1000, 
      height = 700 * fig_height
    )
  }, deleteFile = FALSE)
  
  ## AAPC table for national tab ====
  output$aapc_table = DT::renderDataTable({
    
    sigpvalue <- as.numeric(input$sigpvalue)
    
    ## Filter to race and outcome
    temp_df <- jp_results %>% 
      filter(opioid_type == input$outcome, 
             race %in% input$groups)
    
    ## Make significance categories
    temp_df <- temp_df %>% 
      mutate(
        sig_aapc     = ifelse(aapc_pval  < sigpvalue, 1, 0))
    
    ## Sig AAPCs?
    if (input$sig_aapc_only){
      temp_df <- temp_df %>% 
        filter(sig_aapc == 1)
    }
    
    aapc_df <- temp_df %>%
      group_by(abbrev, race_cat) %>%
      summarize(
        N = n_distinct(year), 
        period  = sprintf("%i - %i", min(year), max(year)), 
        aapc    = sprintf("%0.2f%%", first(aapc)),
        aapc_ci = sprintf("%0.2f, %0.2f", 
                          mean(aapc_lb, na.rm = TRUE),
                          mean(aapc_ub, na.rm = TRUE)
        ),
        aapc_pv = ifelse(mean(aapc_pval, na.rm = TRUE) < .001, 
                         "<.001", 
                         sprintf("%0.3f", mean(aapc_pval, na.rm = TRUE))
        )
        ) %>% 
      arrange(abbrev, race_cat) %>%
      ungroup() %>%
      DT::datatable(data = ., 
                    colnames = 
                      c("State", "Race/Ethnicity", 
                        "Years (N)", "Obs. Range", "AAPC", "AAPC 95% CI", 
                        "AAPC P-Value"), 
                    rownames = FALSE, 
                    filter = list(position = 'top', 
                                  clear = TRUE, 
                                  plain = FALSE), 
                    caption = "Average annual percent change (AAPC) for all states, 
                    by race/ethnicity. The average annual percent change is the 
                    weighted average of the annual percent change estimates 
                    (i.e., the model slopes expressed as percent change), 
                    where the weights are the number of years in each 
                    segment.")
    
    aapc_df
  })
  
  ## State-specific plot ====
  output$state_specific <- renderImage({
    
    clean_cached_folder()
    
    retina_x <- session$clientData$pixelratio
    fig_height <- 1
    
    outfile <- normalizePath(
      sprintf("./cached_images/%s.jpg",
              digest(list(
                input$s_state, 
                input$s_groups,
                input$s_show_raw,
                input$s_raw_ci,
                input$s_model_fit,
                input$s_linetype_sig,
                input$s_joinpoint,
                input$s_joinpoint_sig,
                input$s_ymax,
                input$s_legends_on, 
                input$s_sigpvalue, 
                input$s_scale,
                retina_x))
      )
    )
    
    sigpvalue <- as.numeric(input$s_sigpvalue)
    
    # Return the cached object if the file exists, otherwise, run this:
    if (!file.exists(outfile)) {
      temp_df <- jp_results %>% 
        filter(race %in% input$s_groups, 
               abbrev == input$s_state)
      
      ## Make significance categories
      temp_df <- temp_df %>% 
        mutate(
          sig_aapc     = ifelse(aapc_pval  < sigpvalue, 1, 0), 
          sig_slope    = ifelse(slope_pval < sigpvalue, 1, 0), 
          sig_slopechg = ifelse(slopechange_pval < sigpvalue, 1, 0)
        ) %>% 
        mutate(
          slope_cat = factor(sig_slope, 
                             levels = 0:1, 
                             labels = c("Non-Sig.", "Sig."), 
                             ordered = TRUE), 
          slopechg_cat = factor(sig_slopechg, 
                                levels = 0:1, 
                                labels = c("Non-Sig.", "Sig."), 
                                ordered = TRUE)
        )
      
      ## Start a plot
      p <- ggplot(data = temp_df)
      
      ## Add all the necessities
      p <- p +
        facet_grid(~ opioid_cat) + 
        coord_cartesian(ylim = c(0, input$s_ymax)) +
        scale_x_continuous(NULL,
                           breaks = seq(2000, 2015, 5),
                           labels = c("'00", "'05", "'10", "'15"),
                           expand = c(0, .25)) +
        scale_y_continuous(sprintf("Mortality Rate (per 100,000), %s",
                                   input$s_state),
                           expand = c(0, .5)) + 
        scale_color_brewer("Race/Ethnicity", palette = "Set1", 
                           drop = FALSE)
      
      
      ## Should we plot the raw?
      if (input$s_show_raw) {
        if (input$s_model_fit) {
          alpha_point <- .2
          alpha_line  <- .2
        } else {
          alpha_point <- .8
          alpha_line  <- .5
        }
        
        ## Should we plot the raw CI?
        if (input$s_raw_ci) {
          p <- p +
            geom_errorbar(aes(x = year, ymax = rate + 1.96 * rate_se,
                              ymin = rate - 1.96 * rate_se,
                              color = race_cat),
                          width = 0, alpha = alpha_line)
        }
        
        p <- p + 
          geom_point(aes(x = year, y = rate, color = race_cat), 
                     alpha = alpha_point, shape = 5) 
      }
      
      ## Draw model fit?
      if (input$s_model_fit) {
        if (input$s_linetype_sig) {
          p <- p + 
            geom_line(aes(x = year, y = pred_rate, color = race_cat,
                          group = interaction(race_cat, line_seg),
                          linetype = slope_cat)) + 
            scale_linetype_manual(
              name = sprintf("Slope (P < %0.3f)", sigpvalue),
              values = c("dashed", "solid"),
              labels = c("Non-significant","Significant"),
              drop = FALSE)
        } else {
          p <- p + 
            geom_line(aes(x = year, y = pred_rate, color = race_cat,
                          group = interaction(race_cat, line_seg)))
        }
      }
      
      ## Draw joinpoints?
      if (input$s_joinpoint) {
        if (input$s_joinpoint_sig) {
          p <- p +
            geom_point(aes(x = j_year, y = pred_rate, 
                           color = race_cat,
                           shape = slopechg_cat)) +
            scale_shape_manual(
              name = sprintf("Change in slope\n(P < %0.3f)", sigpvalue), 
              values = c(0, 19), 
              labels = c("Non-significant", "Significant"), 
              drop = FALSE)
        } else {
          p <- p +
            geom_point(aes(x = j_year, y = pred_rate, 
                           color = race_cat))
        }
      }
      
      ## Show legends?
      if (input$s_legends_on) {
        fig_height <- 1.4
        p <- p + 
          mk_nytimes(panel.border = 
                       element_rect(linetype = "solid",
                                    fill = NA,
                                    color = "grey75")) +
          theme(legend.position = "top", 
                legend.direction = "vertical")
      } else {
        p <- p + 
          mk_nytimes(panel.border = 
                       element_rect(linetype = "solid",
                                    fill = NA,
                                    color = "grey75"), 
                     legend.position = "none")
      }
      
      ## If we drew a subplot, add it to the figure
      if (input$s_joinpoint) {
        ## If we draw joinpoints, also draw a subplot of CIs
        p_sub <- ggplot(data = temp_df, 
                        aes(x = j_year, xmin = year_lb, 
                            xmax = year_ub, y = race_cat, 
                            color = race_cat)) + 
          geom_errorbarh(height = .5, alpha = .9) + 
          geom_point(color = "white", size = 2.5) + 
          geom_point(size = 1.5) + 
          facet_grid(~ opioid_cat, scales = "free_y") + 
          scale_linetype_manual(values = c("dashed", "solid"), 
                                drop = FALSE) + 
          scale_shape_manual(values = c(0, 19), 
                             drop = FALSE) + 
          scale_x_continuous("Joinpoint (95% CI)", 
                             breaks = seq(2000, 2015, 5), 
                             labels = c("'00", "'05", "'10", "'15"), 
                             expand = c(0, .25), 
                             limits = c(1999, 2016)) + 
          scale_y_discrete(NULL, drop = FALSE, expand = c(0, 1.5)) + 
          scale_color_brewer(palette = "Set1", 
                             drop = FALSE) + 
          mk_nytimes(panel.border = element_rect(linetype = "solid", 
                                                 fill = NA, 
                                                 color = "grey75"), 
                     strip.text = element_blank(), 
                     strip.background = element_blank(), 
                     axis.text.y = element_blank(), 
                     legend.position = "none", 
                     panel.grid.major.y = element_blank(), 
                     plot.margin = unit(c(0, 0, 0, 0), "cm"))
        
        p1 <- plot_grid(p, p_sub, nrow = 2, 
                       axis = "tblr", align = "v", 
                       rel_heights = c(4, 1))
        fig_height <- fig_height + .2
      }
      
      ggsave(outfile, p1, 
             width = 10 / retina_x, 
             height = 3 * fig_height / retina_x, 
             dpi = 150 * retina_x, scale = input$s_scale)
    }
    ## Return a list containing the filename, type, dimensions, etc.
    list(
      src = outfile, 
      contentType = 'image/jpg', 
      width = 1000, 
      height = 300 * fig_height
    )
  }, deleteFile = FALSE)
    
    ## State table fit ====
    output$state_table_fit = DT::renderDataTable({
      state_df <- jp_results %>% 
        filter(race %in% input$s_groups, 
               abbrev == input$s_state)
      
      
      state_df <- state_df %>% 
        filter(dupe_row == 0) %>% 
        group_by(opioid_cat, race_cat) %>% 
        filter(year == min(year)) %>% 
        select(joinpoints, df, sse, mse) %>% 
        mutate(sse = sprintf("%0.3f", sse), 
               mse = sprintf("%0.3f", mse)) %>% 
        ungroup() %>%
        DT::datatable(data = ., 
                      colnames = c("Opioid type", "Race/Ethnicity", 
                                   "Joins (N)", "DF", "SSE", "MSE"), 
                      rownames = FALSE, 
                      caption = "Model statistics by race/ethnicity and
                      opioid type. As required by NCHS, if a model includes
                      rates with based on observations with fewer than 10
                      deaths, we suppressed the mean squared error (MSE) and
                      sum of squared errors (SSE).", 
                      options = list(paging = FALSE, searching = FALSE))
      
      state_df
    })
    
    ## State table estimates ====
    output$state_table_estimates = DT::renderDataTable({
      state_df <- jp_results %>% 
        filter(race %in% input$s_groups, 
               abbrev == input$s_state)
      
      
      state_df <- state_df %>% 
        filter(dupe_row == 0) %>% 
        group_by(opioid_type, race, segment) %>% 
        slice(1) %>% 
        group_by(opioid_cat, race_cat) %>% 
        select(opioid_cat, race_cat, segment, int_est, int_se, int_pval, 
               slope_est, slope_se, slope_pval, slopechange_pval) %>% 
        mutate(segment = sprintf("%i of %i", segment + 1, max(segment) + 1), 
               int_est = sprintf("%0.2f", int_est),
               int_se = sprintf("%0.2f", int_se), 
               int_pval = ifelse(int_pval < .001, "<.001", 
                                 sprintf("%0.3f", int_pval)), 
               slope_est = sprintf("%0.2f", slope_est),
               slope_se = sprintf("%0.2f", slope_se), 
               slope_pval = ifelse(slope_pval < .001, "<.001", 
                                   sprintf("%0.3f", slope_pval)), 
               slopechange_pval =  ifelse(slopechange_pval < .001, "<.001", 
                                          sprintf("%0.3f", slopechange_pval))
        ) %>% 
        arrange(opioid_cat, race_cat) %>% 
        ungroup() %>%
        DT::datatable(data = ., 
                      colnames = c("Opioid type", "Race/Ethnicity", "Segment", 
                                   "Int. Est.", "Int. SE", "Int. P-Value", 
                                   "Slope Est.", "Slope SE", "Slope P-Value", 
                                   "Slope Change P-Value"), 
                      rownames = FALSE, 
                      caption = "Model statistics by race/ethnicity and
                      opioid type. As required by NCHS, if a model includes
                      rates with based on observations with fewer than 10
                      deaths, we suppressed the mean squared error (MSE) and
                      sum of squared errors (SSE).", 
                      list(paging = FALSE))
      
      state_df
    })
    
    ## State table predictions ====
    output$state_table_predictions = DT::renderDataTable({
      state_df <- jp_results %>% 
        filter(race %in% input$s_groups, 
               abbrev == input$s_state)
      
      
      state_df <- state_df %>% 
        filter(dupe_row == 0) %>% 
        group_by(opioid_cat, race_cat) %>% 
        select(year, pred_rate, rate, rate_se) %>% 
        mutate(pred_rate = sprintf("%0.2f", pred_rate), 
               rate = sprintf("%0.2f", rate), 
               rate_se = sprintf("%0.2f", rate_se)
               ) %>% 
        arrange(opioid_cat, race_cat) %>% 
        ungroup() %>%
        DT::datatable(data = ., 
                      colnames = c("Opioid type", "Race/Ethnicity", "Year",
                                   "Modeled Rate", "Observed Rate", 
                                   "Observed Rate SE"), 
                      filter = list(position = 'top', 
                                    clear = TRUE, 
                                    plain = FALSE), 
                      rownames = FALSE, 
                      caption = "Predicted and observed rates for each year,
                      by race/ethnicity and opioid type. Observed rates based
                      on observations with fewer than 10 deaths are suppressed
                      per NCHS restrictions.")
      
      state_df
    })
  
  ## Hotspots legend ----
    ## Create the color palette
    c_legend <- 
      expand.grid(list(mort_lev = c("low_mort", "med_mort", "high_mort"), 
                       apc_lev  = c("low_apc", "med_apc", "high_apc")), 
                  stringsAsFactors = TRUE) %>% 
      as_tibble() %>% 
      mutate(
        color_hex = c("#e8e6f2", "#b5d3e7", "#4fadd0", 
                      "#e5b4d9", "#b8b3d8", "#3983bb", 
                      "#d34fa6", "#b03598", "#2a1a8a"),
        color_hex_a = c("#e8e8e8", "#e4acac", "#c85a5a", 
                        "#b0d5df", "#ad9ea5", "#985356", 
                        "#64acbe", "#627f8c", "#574249")
      )
    
    output$hotspots_legend <- renderImage({
      
      retina_x <- session$clientData$pixelratio
      
      # We need a unique file name based on the input parameters
      outfile <- normalizePath(
        sprintf("./cached_images/%s.jpg",
                digest(list(
                  "hotspot_legend", 
                  input$h_outcome, 
                  input$mort_mid_bin, 
                  input$apc_mid_bin, 
                  input$h_scale, 
                  retina_x))
        )
      )
      
      # Return the cached object if the file exists, otherwise, run this:
      if (!file.exists(outfile)) {
        
        sigpvalue  <- as.numeric(input$h_sigpvalue)
        
        ## Create the plot
        p_legend <- 
          ggplot(c_legend, 
                 aes(x = mort_lev, y = apc_lev, fill = color_hex)) + 
          geom_tile(color = "white") + 
          scale_fill_identity(na.value = "grey50") + 
          scale_x_discrete("Mortality rate\n(per 100,000)", expand = c(0.05, 0), 
                           labels = c(
                             sprintf("Low: 0-%0.1f", 
                                     input$mort_mid_bin[1]), 
                             sprintf("Medium: %0.1f-%0.1f", 
                                     input$mort_mid_bin[1], 
                                     input$mort_mid_bin[2]), 
                             sprintf("High: >%0.1f", 
                                     input$mort_mid_bin[2]))
          ) + 
          scale_y_discrete("APC (%)", expand = c(0.05, 0), 
                           labels = c(
                             sprintf("Slow: 0-%i", 
                                     input$apc_mid_bin[1]), 
                             sprintf("Moderate: %i-%i", 
                                     input$apc_mid_bin[1], 
                                     input$apc_mid_bin[2]),
                             sprintf("Rapid: >%i", 
                                     input$apc_mid_bin[2]))
          ) + 
          mk_nytimes(panel.grid.major = element_blank(), 
                     axis.text = element_text(size = 11), 
                     axis.text.x = element_text(angle = 45, 
                                                hjust = 1, 
                                                vjust = 1), 
                     axis.line = element_line(
                       arrow = arrow(
                         length = unit(.15, "inches"), 
                         type = "open", angle = 20)
                     ), 
                     plot.subtitle = element_text(size = 13, hjust=0.5)
          ) + 
          labs(subtitle = "Legend") + 
          coord_equal()
        
        
        ggsave(outfile, p_legend, 
               width = 3 / retina_x, 
               height = 3 / retina_x, 
               dpi = 150 * retina_x, 
               scale = input$h_scale)
      }
      
      ## Return a list containing the filename, type, dimensions, etc.
      list(
        src = outfile, 
        contentType = 'image/jpg', 
        width = 300, 
        height = 300
      )
    }, deleteFile = FALSE)
    
    output$hotspots_map <- renderImage({
      
      retina_x <- session$clientData$pixelratio
      
      # We need a unique file name based on the input parameters
      outfile <- normalizePath(
        sprintf("./cached_images/%s.jpg",
                digest(list(
                  "hotspot_map", 
                  input$h_sigpvalue,
                  input$h_outcome, 
                  input$mort_mid_bin, 
                  input$apc_mid_bin, 
                  input$h_scale,
                  retina_x))
        )
      )
      
      # Return the cached object if the file exists, otherwise, run this:
      if (!file.exists(outfile)) {
        
        sigpvalue  <- as.numeric(input$h_sigpvalue)
        
        ## Subset to current segment and make apc/mortality categories
        hotspots <- jp_results %>% 
          group_by(abbrev, race, opioid_type) %>% 
          filter(year == max(year), 
                 opioid_type == input$h_outcome, 
                 slope_pval < sigpvalue) %>% 
          transmute(mort_lev = 
                      case_when(
                        between(rate, 0, input$mort_mid_bin[1]) ~ "low_mort", 
                        between(rate, input$mort_mid_bin[1], 
                                input$mort_mid_bin[2]) ~ "med_mort", 
                        rate > input$mort_mid_bin[2] ~ "high_mort", 
                        TRUE ~ NA_character_
                      ), 
                    apc_lev = 
                      case_when(
                        between(apc, 0, input$apc_mid_bin[1]) ~ "low_apc", 
                        between(apc, input$apc_mid_bin[1], 
                                input$apc_mid_bin[2]) ~ "med_apc", 
                        apc > input$apc_mid_bin[2] ~ "high_apc", 
                        TRUE ~ NA_character_
                      )
          ) 
        
        ## Join with color scale and join back with original data
        hotspots <- hotspots %>% 
          left_join(c_legend) %>% 
          right_join(jp_results %>% 
                       group_by(abbrev, race, opioid_type) %>% 
                       filter(year == max(year), 
                              opioid_type == input$h_outcome)
          ) 
        
        ## Convert them to factors so they retain order
        hotspots <- hotspots %>% 
          mutate(
            color_hex_na = ifelse(is.na(color_hex), "grey97", color_hex),  
            mort_lev = factor(mort_lev, 
                              levels = c("low_mort", "med_mort", "high_mort"), 
                              ordered = TRUE), 
            apc_lev = factor(apc_lev,
                             levels = c("low_apc", "med_apc", "high_apc"), 
                             ordered = TRUE)
          )
        
        p1 <- ggplot(hotspots, 
                     aes(state = abbrev, fill = color_hex_na)) +
          scale_fill_identity(NULL, na.value = "black") + 
          geom_statebins(border_col = "grey90", border_size = .5) +
          coord_equal() +
          scale_x_continuous(expand = c(0, 0)) + 
          scale_y_continuous(expand = c(0, 0)) + 
          facet_grid( ~ race_cat) +
          mk_nytimes(legend.position = "none", 
                     panel.grid.major = element_blank(), 
                     axis.text = element_blank(), 
                     panel.border = element_rect(linetype = "solid", 
                                                 fill = NA, 
                                                 color = "grey75"))

        ggsave(outfile, p1, 
               width = 9 / retina_x, 
               height = 4 / retina_x, 
               dpi = 150 * retina_x, 
               scale = input$h_scale)
      }
      
      ## Return a list containing the filename, type, dimensions, etc.
      list(
        src = outfile, 
        contentType = 'image/jpg', 
        width = 900, 
        height = 400
      )
    }, deleteFile = FALSE)
    
  ## Hotspots Percentile things ====
  output$percentile_hotspots = renderUI({
    sigpvalue  <- as.numeric(input$h_sigpvalue)
    
    ## Subset data to just this outcome and most recent segment
    hotspot_df <- jp_results %>% 
      filter(opioid_type == input$h_outcome, 
             dupe_row == 0) %>% 
      group_by(race, abbrev) %>% 
      filter(year == max(year))
    
    mort_lo <- mean(hotspot_df$rate < input$mort_mid_bin[1], na.rm = TRUE)
    mort_hi <- mean(hotspot_df$rate < input$mort_mid_bin[2], na.rm = TRUE)
    
    apc_lo_all <- mean(hotspot_df$apc < input$apc_mid_bin[1], na.rm = TRUE)
    apc_hi_all <- mean(hotspot_df$apc < input$apc_mid_bin[2], na.rm = TRUE)
    
    apc_lo_sig <- mean(hotspot_df %>% 
                         filter(slope_pval < sigpvalue) %>% 
                         pull(apc) < input$apc_mid_bin[1], na.rm = TRUE)
    apc_hi_sig <- mean(hotspot_df %>% 
                         filter(slope_pval < sigpvalue) %>% 
                         pull(apc) < input$apc_mid_bin[2], na.rm = TRUE)
    
    HTML(
      sprintf("Mortality rates of %0.1f and %0.1f per 100,000 represent 
              the %i%%ile and %i%%ile of observed mortality rates, 
              respectively. <br> <br> 
              
              Annual percent changes (APCs) of %0.0f%% and %0.0f%% 
              represent the %i%%ile and %i%%ile of all APCs and the 
              %i%%ile and %i%%ile of statistically significant APCs 
              (at the P<%0.3f level), respectively.", 
            input$mort_mid_bin[1], input$mort_mid_bin[2], 
            round(mort_lo * 100, 0), round(mort_hi * 100, 0), 
            input$apc_mid_bin[1], input$apc_mid_bin[2], 
            round(apc_lo_all * 100, 0), round(apc_hi_all * 100, 0), 
            round(apc_lo_sig * 100, 0), round(apc_hi_sig * 100, 0), 
            sigpvalue
            )
      )
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
