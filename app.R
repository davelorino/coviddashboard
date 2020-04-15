
library(shiny)
library(rintrojs)
library(tidyverse)
library(plotly)
library(lubridate)
library(shinythemes)
library(magrittr)
library(bsplus)

pdf(NULL)

# COVID-19 Insights 

#   Key Dates

#   The Saatchi & Saatchi COVID-19 dashboard examines the timeline of the COVID-19 outbreak from the first identified case of the virus, to now. 

#*	Dec 31: First coronavirus case in Wuhan, China
#*	Jan 13: First case outside of China (Thailand)
#*	Jan 25: First “imported” case reported in Australia 
#*	Mar 1-2: First community cases reported in AU (29 total cases) 
#*	Mar 21: AU reaches 1,000 cases
#*	Mar 19-22: AU govt closes borders to non-residents and non-citizens, and announced mandatory closures of non-essential businesses

#### By Saatchi & Saatchi
    
# GLOBAL OPTS & FUNCTIONS -------------------------------------------------------------------------------------------------    
    options(shiny.maxRequestSize=100*1024^2)


     
    stripHTML <- function(htmlString) {
      return(gsub("<.*?>", "", htmlString))
    }
    
    onlyASCII <- function(stringVector){
      return(gsub('[^\x20-\x7E]', '', stringVector))
    }
    
    meltwater_week <- function(arg){
      arg %>%
        mutate(Date_Fix = as.Date(`Date`, "%d/%m/%Y")) %>%
        mutate(`Week Beginning` = floor_date(Date_Fix, "weeks")) %>%
        filter(!is.na(`Week Beginning`))
    }
    
    summarise_meltwater_by_week <- function(arg){
      arg %>%
        group_by(`Week Beginning`) %>%
        summarise(All = sum(`All`)) %>%
        arrange(`Week Beginning`)
    }
    
    summarise_meltwater_sentiment_by_week <- function(arg){
      arg %>%
        group_by(`Week Beginning`) %>%
        summarise(Negative = sum(`Negative`), Positive = sum(Positive), Neutral = sum(Neutral)) %>%
        mutate(Percentage_Positive = round(Positive / (Positive + Negative + Neutral) * 100, 2)) %>%
        mutate(Percentage_Negative = round(Negative / (Positive + Negative + Neutral) * 100, 2)) %>%
        mutate(Percentage_Neutral = round(Neutral / (Positive + Negative + Neutral) * 100, 2)) %>%
        arrange(`Week Beginning`)
    }

    
# DATA---------------------------------------------------------------------------------------------------------------------

    volumeovertime <- read_csv("coronaconvo2.csv")
    coronasent <- read_csv("coronasent.csv")
   # coronaverbatims <- read_csv("coronaverbatims_l7d_wed8thmar.csv")
   # afinn <- readRDS("afinn.rds")
    
    coronavirus <- readRDS("coronavirus2.rds")

    # coronavirus <- coronavirus %>%
    #   group_by(Country.Region, date, type) %>%
    #   mutate(`Worldwide` = sum(cases)) %>%
    #   ungroup() %>%
    #   filter(`Country.Region` %in% c("Australia", "China")) %>%
    #   group_by(Country.Region, date, type) %>%
    #   summarise(Total = sum(cases)) %>%
    #   pivot_wider(names_from = c(type, Country.Region), values_from = Total) %>%
    #   mutate(`Week Beginning` = floor_date(date, unit = "weeks")) %>%
    #   group_by(`Week Beginning`) %>%
    #   summarise(confirmed_Australia = sum(confirmed_Australia),
    #             death_Australia = sum(death_Australia),
    #             recovered_Australia = sum(recovered_Australia),
    #             confirmed_China = sum(confirmed_China),
    #             death_China = sum(death_China),
    #             recovered_China = sum(recovered_China)) %>%
    #   mutate(cumulative_confirmed_Australia = cumsum(confirmed_Australia)) %>%
    #   mutate(cumulative_death_Australia = cumsum(death_Australia)) %>%
    #   mutate(cumulative_confirmed_China = cumsum(confirmed_China)) %>%
    #   mutate(cumulative_death_China = cumsum(death_China))

    
    #coronavirus::update_datasets()
   
    corona_sentiment <- coronasent %>%
      meltwater_week() %>%
      summarise_meltwater_sentiment_by_week()
    
    
    # write_rds(corona_sentiment, "~/NetBaseApi/coviddashboard/corona_sentiment.rds")
    
    corona_weeks <- volumeovertime %>%
        meltwater_week() %>%
        summarise_meltwater_by_week() %>%
        left_join(coronavirus, by = "Week Beginning")
    
    # write_rds(corona_weeks, "~/NetBaseApi/coviddashboard/corona_sentiment.rds")
    
    
    # base64 encoded string of each image
    
    # uris <- purrr::map_chr(
    #      corona_weeks$`Week Beginning`, ~ base64enc::dataURI(file = sprintf("~/NetBaseApi/coviddashboard/%s.jpeg", .x))
    # )
    # 
    #uri_df <- data.frame(uri = uris)
    
    #write_rds(uri_df, "~/NetBaseApi/coviddashboard/uri_df.rds")
    
    uris <- read_rds("uri_df.rds")
    
    urisclick <- purrr::map_chr(
      corona_weeks$`Week Beginning`, ~ base64enc::dataURI(file = sprintf("%s-2.jpeg", .x))
    )
    
    
    
    total_mentions_colour <- "#FFFFFF"
    twitter_colour <- "#1da1f2"
    legend_features <- list(
        font = list(
            size = 12,
            color = "#FFFFFF"),
        bgcolor = "#212121",
        bordercolor = "#FFFFFF",
        borderwidth = 1,
        x = 0.1, 
        y = 0.9)
    
    contribution_plot <- read_rds("plotly_object.rds")
    bing_sentiment_plot <- read_rds("covid_bingsent_plot.rds")

# UI-------------------------------------------------------------------------------------------------------------
    
    ?p
    
    ui = navbarPage(title = "Saatchi COVID-19 Dashboard", theme = shinytheme("darkly"),
                   tabPanel(title =  
                   introBox("Insights", 
                            data.step = 1,
                            data.position = "bottom",
                            data.intro = 
                              "The Insights tab covers a range of timeseries and sentiment based insights around the 
                            spread and containment of COVID-19 from late 2019 until the present."),
                       sidebarPanel(img(src="unnamed.png", width="80%", height="80%"),
                                    br(), br(),
                                    actionButton("helpMe", "Tour"),
                                    introjsUI(),
                                    width = 2,
                                    tags$head(
                                      tags$style(HTML("
                      .introjs-tooltiptext {
                        color: #212121;
                      }
                
                    "))
                                    )),
                       mainPanel(column(width = 12, align = "left",
                         h4("Volume of Mentions", align = "center"),
                        wellPanel(introBox(plotlyOutput("lineplot"), 
                                 data.step = 2, 
                                 data.intro = "Here we analyze <b>volume</b> of conversation over time among twitter, 
                                 blogs and forums.<br/>
                                 <b>Hover</b> over points to see what drove conversation.",
                                 data.position = "bottom-left_aligned"),
                                 bs_collapse(
                                   id = "volume_collapse", 
                                   content = tags$div(class = "well", 
                                column(width = 12, p("This chart displays the volume of mentions of COVID-19 on a weekly basis from Australians only. 
                                       Each point represents a 7 day period, beginning at the labelled date.", )),
                                                      column(width = 6, h5(tags$u("Dec 22 - Jan 26th"), align = "left"),
                                "Mentions only really began to kick off in late Feb when cases in Australia had begun to ramp up. 
                                By mid-March mentions had skyrocketed as draconian containment measures were enacted by the Australian 
                                government.", br(), br(),
                                h5(tags$u("Jan 26th - Feb 16th")),
                                "Topics that drove conversation in late Jan to mid Feb were the first 6 cases of the virus detected 
                                in Australia, the sad passing of the doctor who broke the news of the virus, and the World Health 
                                Organisation officially named the disease COVID-19.", 
                                br(), br(),
                                h5(tags$u("Feb 16th - Mar 1st")),
                                "In mid February Australians were talking about the harsh containment measures being implemented in China, 
                                the PM announced that we are facing a global emergency, and the Australian people were talking about the 
                                capacity for our healthcare systems to cope with the number of expected patients.", br(), br()), 
                                column(width = 6, h5(tags$u("Dec 22 - Jan 26th"), align = "left"),
                                       "Mentions only really began to kick off in late Feb when cases in Australia had begun to ramp up. 
                                By mid-March mentions had skyrocketed as draconian containment measures were enacted by the Australian 
                                government.", br(), br(),
                                       h5(tags$u("Jan 26th - Feb 16th")),
                                       "Topics that drove conversation in late Jan to mid Feb were the first 6 cases of the virus detected 
                                in Australia, the sad passing of the doctor who broke the news of the virus, and the World Health 
                                Organisation officially named the disease COVID-19.", 
                                       br(), br(),
                                       h5(tags$u("Feb 16th - Mar 1st")),
                                       "In mid February Australians were talking about the harsh containment measures being implemented in China, 
                                the PM announced that we are facing a global emergency, and the Australian people were talking about the 
                                capacity for our healthcare systems to cope with the number of expected patients.", br(), br())))
                                , 
                                 bs_button("Analysis", button_type = "default") %>%
                                   bs_attach_collapse("volume_collapse")), br(), br(),
                        h4("Proportion of Sentiment Over Time", align = "center"),
                        wellPanel(introBox(plotlyOutput("sentiment_plot"),
                                 data.step = 3,
                                 data.intro = "Here we analyze <b>sentiment</b> of conversation over time among twitter, blogs and forums."),
                                bs_collapse(id = "sentiment_collapse",
                                            content = tags$div(class = "well",
                                  "Here are some observations about sentiment. 
                                  At the start there are so few mentions that the sentiment scorer is thrown by all sorts of outliers. 
                                  As the data starts to become more abundant, the sentiment of the conversation is a lot better understood.")
                                ),
                                bs_button("Analysis", button_type = "default") %>%
                                  bs_attach_collapse("sentiment_collapse")),
                        br(), br(),
                        column(width = 6, wellPanel(introBox(data.step = 4, 
                                                             data.intro = "This chart analyzes the top 25 words contributing to positive 
                                                             or negative sentiment.", 
                                                             plotlyOutput("contribution_plot"),
                                                             bs_collapse(id = "contribution_collapse", content = tags$div(class = "well",
                                                              "Here are the top 25 contributing words to sentiment as determined by the AFINN
                                                              sentiment analysis lexicon. AFINN gives every word in the dictionary a score
                                                              between -5 and 5 to determine its level of sentiment. The chart above shows the result
                                                              of summing all of the points earned by each word in the mentions to determine which
                                                              words had the most frequent and significant impact on overall sentiment.")),
                                                             bs_button("Analysis", button_type = "default") %>%
                                                               bs_attach_collapse("contribution_collapse")))),
                        column(width = 6, wellPanel(introBox(data.step = 5, 
                                                             data.intro = "This chart analyzes the top 25 words contributing to both
                                                             positive and negative sentiment.", plotlyOutput("bing_sentiment_plot"),
                                                             bs_collapse(id = "bing_sent", content = tags$div(class = "well", "This chart displays the top 25 most common contributing words 
                                                                         to positive or negative sentiment on the Bing Liu sentiment scale.
                                                                         The Bing Liu sentiment lexicon apportions the label 'positive' or
                                                                         'negative' to each word in the dictionary. The above chart shows us the frequency/
                                                                         count of occurences for positive and negative words.")),
                                                             bs_button("Analysis", button_type = "default") %>%
                                                               bs_attach_collapse("bing_sent")
                                                             )))
                      #  wellPanel(introBox(column(width = 4, plotlyOutput("udpipe_plot")))),
                            )
                          )
                        )
                       )
    
                  
                   
# SERVER-----------------------------------------------------------------------------------------------------------      
         server = function(input, output, session){
           
           observeEvent(input$helpMe , {
             introjs(session)  
           })
    
                        
            output$sentiment_plot <- plotly::renderPlotly(
              plotly::plot_ly(data = corona_sentiment,
                              source = "hoverplotsource",
                              mode = "none",
                              stackgroup = "one",
                              hoveron = 'points+fills'
                                 ) %>%
                   plotly::config(displayModeBar = FALSE) %>%
                   plotly::add_trace(
                     x = ~`Week Beginning`,
                     y = ~`Percentage_Neutral`,
                     name = "Neutral",
                     fillcolor = total_mentions_colour)
                    %>%
                   plotly::config(displayModeBar = FALSE) %>%
                   plotly::add_trace(
                     x = ~`Week Beginning`,
                     y = ~`Percentage_Negative`,
                     name = "Negative",
                     fillcolor = "red"
                   ) %>%
                   plotly::config(displayModeBar = FALSE) %>%
                   plotly::add_trace(
                     x = ~`Week Beginning`,
                     y = ~`Percentage_Positive`,
                     name = "Positive",
                     fillcolor = "lightgreen"
                   ) %>%
                   plotly::layout(
                     title = "",
                     yaxis = list(title = "Sentiment Over Time", color = "#ffffff"),
                     xaxis = list(title = "Week Beginning", color = "#ffffff"),
                     legend = legend_features,
                     paper_bgcolor='#212121',
                     plot_bgcolor='#212121',
                     hovermode = "compare"
                   )
              ) 
                   
              output$lineplot <- plotly::renderPlotly(
                 plotly::plot_ly(data = corona_weeks,
                                 source = "hoverplotsource",
                             customdata = ~map2(uris$uri, urisclick, ~list(.x, .y))) %>%
                   plotly::config(displayModeBar = FALSE) %>%
                 plotly::add_trace(
                     x = ~`Week Beginning`,
                     # y = ~active_cum,
                     y = ~`All`,
                     type = "scatter",
                     mode = "lines+markers",
                     # name = "Active",
                     name = "Total Mentions",
                     line = list(color = total_mentions_colour),
                     marker = list(color = total_mentions_colour)
                 ) %>%
                   plotly::add_trace(
                     x = ~`Week Beginning`,
                     # y = ~active_cum,
                     y = ~`cumulative_confirmed_Australia`,
                     type = "scatter",
                     mode = "lines+markers",
                     # name = "Active",
                     name = "Confirmed Cases Australia",
                     line = list(color = twitter_colour),
                     marker = list(color = twitter_colour)
                   ) %>%
                   plotly::add_trace(
                     x = ~`Week Beginning`,
                     # y = ~active_cum,
                     y = ~`cumulative_confirmed_China`,
                     type = "scatter",
                     mode = "lines+markers",
                     # name = "Active",
                     name = "Confirmed Cases China",
                     line = list(color = "red"),
                     marker = list(color = "red")
                   ) %>%
                 htmlwidgets::onRender(readLines("tooltip-image.js")) %>%
                   htmlwidgets::onRender(readLines("tooltip-imageclick.js")) %>%
                 plotly::add_annotations(
                     x = as.Date("2019-12-29"),
                     y = 1,
                     text = paste("First case"),
                     xref = "x",
                     yref = "y",
                     arrowhead = 5,
                     arrowhead = 3,
                     arrowsize = 1,
                     showarrow = TRUE,
                     font = list(color = '#FFFFFF'),
                     ax = -90,
                     ay = -90
                 ) %>%
                 plotly::add_annotations(
                     x = as.Date("2020-01-12"),
                     y = 2,
                     text = paste("First Case Outside of China (Thailand)"),
                     xref = "x",
                     yref = "y",
                     arrowhead = 10,
                     arrowhead = 3,
                     arrowsize = 1,
                     showarrow = TRUE,
                     font = list(color = '#FFFFFF'),
                     ax = -120,
                     ay = -120
                 ) %>%
                 plotly::add_annotations(
                     x = as.Date("2020-01-26"),
                     y = 3,
                     text = paste("First 'Imported' Case in Australia"),
                     xref = "x",
                     yref = "y",
                     arrowhead = 5,
                     arrowhead = 3,
                     arrowsize = 1,
                     showarrow = TRUE,
                     font = list(color = '#FFFFFF'),
                     ax = -150,
                     ay = -180
                 ) %>%
                 plotly::add_annotations(
                     x = as.Date("2020-03-15"),
                     y = 3,
                     text = paste(
                         "New containment measures"
                     ),
                     xref = "x",
                     yref = "y",
                     arrowhead = 10,
                     arrowhead = 3,
                     arrowsize = 1,
                     showarrow = TRUE,
                     font = list(color = '#FFFFFF'),
                     ax = -10,
                     ay = -90
                 ) %>%
                 plotly::layout(
                     title = "",
                     yaxis = list(title = "Conversation Over Time", color = "#ffffff"),
                     xaxis = list(title = "Week Beginning", color = "#ffffff"),
                     legend = legend_features,
                     paper_bgcolor='#212121',
                     plot_bgcolor='#212121'
                 )) 
        
              output$contribution_plot <- renderPlotly({
                contribution_plot
              })
              
              output$bing_sentiment_plot <- renderPlotly({
                bing_sentiment_plot
              })

      
            
         }
    

shinyApp(ui = ui, server = server)
            

         
             
             #Mar 1-2: First community cases reported in AU (29 total cases) 
             #Mar 21: AU reaches 1,000 cases
             #Mar 19-22: AU govt closes borders to non-residents and non-citizens, and announced mandatory closures of non-essential businesses
             
             
             
             ### Sentiment + Drivers	
             
             
             
             
            
             ###	Sentiment Weekly Snapshot
             
             
             ### Trending Topics + Hashtags
             
             
             ### Desktop Search Phrases
             
             
             
             
             #-	Top 25 (or 50) trending Desktop coronavirus-related key phrases based on volume/yearly trend/leader (site receiving most traffic from keyword) >> identify trending related concerns/topics
             
             ### Mobile Search Phrases
             
             #-	Top 25 (or 50) trending Mobile Web coronavirus-related key phrases based on volume/yearly trend/leader (site receiving most traffic from keyword) >> identify trending related concerns/topics/ key differences with Desktop search behaviours
             
             ### Search Volume, Visits + Phrases
             #
             #-	Identify/analyse search volume, search visits
             
             ### Traffic Distribution
             
             #- Traffic distribution and organic traffic breakdown (by domains and keywords) incl. new/fast emerging domains and keywords
             
      



