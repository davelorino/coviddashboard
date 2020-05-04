
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
    
    meltwater_sentiment_week <- function(arg){
      arg %>%
        mutate(Date_Fix = as.Date(`Date`, "%d/%m/%y")) %>%
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

    sevendaydonut <- read_rds("sevendaydonut.rds")
    #volumeovertime <- read_csv("coronaconvo2.csv")
    #coronasent <- read_csv("coronasent.csv")
    hashtags_7days <- read_rds("hashtags_plot7day.rds")
   # coronaverbatims <- read_csv("coronaverbatims_l7d_wed8thmar.csv")
   # afinn <- readRDS("afinn.rds")
    
    uris <- readRDS("uris.rds")
    urisclick <- readRDS("urisclick.rds")
    #coronavirus <- readRDS("coronavirus2.rds")
    corona7daycases <- readRDS("corona7daycases.rds")
    sw_desktop_plot <- readRDS("sw_desktop_plot.rds")
    sw_mobile_plot <- readRDS("sw_mobile_plot.rds")
    sw_mobile_phrases_donut <- readRDS("mobile_phrases_donut.rds")
    sw_desktop_phrases_donut <- readRDS("desktop_phrases_donut.rds")
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
   
    # corona_sentiment <- coronasent %>%
    #   meltwater_sentiment_week() %>%
    #   summarise_meltwater_sentiment_by_week() %>%
    #   filter(`Week Beginning` > as.Date("22/12/2019", "%d/%m/%Y"))
    # 

    # write_rds(corona_sentiment, "~/NetBaseApi/coviddashboard/corona_sentiment.rds")
    
    # corona_weeks <- volumeovertime %>%
    #     meltwater_week() %>%
    #     summarise_meltwater_by_week() %>%
    #     left_join(coronavirus, by = "Week Beginning") %>%
    #     filter(`Week Beginning` > as.Date("22/12/2019", "%d/%m/%Y"))
    
    corona_7day <- read_rds("corona7day.rds")
    
    
    # write_rds(corona_weeks, "~/NetBaseApi/coviddashboard/corona_sentiment.rds")
    
    
    # base64 encoded string of each image
    
     # uris <- purrr::map_chr(
     #        corona_weeks$`Week Beginning`, ~ base64enc::dataURI(file = sprintf("~/NetBaseApi/coviddashboard/%s.jpeg", .x))
     #   )
     #  
     #uri_df <- data.frame(uri = uris)
    
    #write_rds(uri_df, "~/NetBaseApi/coviddashboard/uri_df.rds")
    
  #  uris <- read_rds("uri_df.rds")
    
    uri_links <- c( "https://twitter.com/MackayIM/status/1211957651849920513",
                   "https://twitter.com/GreenEpidemic/status/1215253360057544704",
                   "https://twitter.com/GreyHaired07/status/1218742383795302405",
                   "https://twitter.com/the_LoungeFly/status/1221054861506039813",
                   "https://twitter.com/message_planet/status/1221416982009876481",
                   "https://twitter.com/nycjim/status/1225517571589451782?s=20",
                   "https://twitter.com/WHO/status/1227248333871173632?s=20",
                   "https://twitter.com/paulmozur/status/1228751784111271936?s=20",
                   "https://twitter.com/TheNewDailyAu/status/1232899467310821376",
                   "https://twitter.com/JamesMelville/status/1234409838358224897?s=20",
                   "https://twitter.com/SBSNews/status/1237296031403892736?s=20",
                   "https://twitter.com/Darleneillyana/status/1241298043556646912?s=20",
                   "https://twitter.com/billbowtell/status/1242544469875949569?s=20",
                   "https://twitter.com/theprojecttv/status/1244900134925860865?s=20",
                   "https://twitter.com/broomstick33/status/1247767301253783557?s=20"
   )
    
    # urisclick <- purrr::map_chr(
    #   corona_weeks$`Week Beginning`, ~ base64enc::dataURI(file = sprintf("%s-2.jpeg", .x))
    # )
    
    total_mentions_colour <- "#FFFFFF"
    twitter_colour <- "#1da1f2"
    legend_features <- list(
        font = list(
            size = 12,
            color = "#FFFFFF"),
        bgcolor = "#212121",
        bordercolor = "#FFFFFF",
        borderwidth = 1,
        x = 0.4, 
        y = 0.9)
    
    legend_features3 <- list(
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
    
    ui = navbarPage(
                    
                   title = "Saatchi & Saatchi COVID-19 Pulse", theme = shinytheme("darkly"),
                   tabPanel(title = "Social Pulse",
                       sidebarPanel(img(src="unnamed2.png", width="80%", height="80%"),
                                    br(), br(),
                                    actionButton("helpMe", "Tour"),
                                    introjsUI(),
                                    width = 2,
                                    tags$head(
                                      tags$style(HTML("
                      .introjs-tooltiptext {
                        color: #212121;
                      }
                
                    "))), 
                                    ),
                       mainPanel(column(width = 12, align = "left",
                 
                         h4("Volume of Covid-19 conversation over time, Australia (VoC only)", align = "center"),
                      wellPanel(introBox(plotlyOutput("lineplot"), 
                                 data.step = 1, 
                                 data.intro = "Here we analyze the <b>volume</b> of conversation over time among twitter, 
                                 blogs and forums.<br/>
                                 <b>Hover</b> over the points to see what drove the conversation.",
                                 data.position = "bottom-left_aligned"),
                                 bs_collapse(
                                   id = "volume_collapse", 
                                   content = tags$div(class = "well", 
                                column(width = 12, tags$em(p("This chart displays the volume of mentions of COVID-19 on a weekly basis from Australians only. 
                                       Each point represents a 7 day period, beginning at the labelled date. For example, 
                                       the point labelled 'Dec 29, 2019' represents
                                         the ",  
                                          tags$b("total weekly mentions "),  
                                          "between the 29th of December and the 4th of January inclusive. 
                                          Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets Dec 29, 2019 - April 12th, 2020. 
                                          Images on hover are selected from the top 10 of the week retweeted by people with < 1000 reach.", 
                                                     tags$style(type = "text/css", "p { font-size: 12px; }")))),
                                br(), br(),
                                column(12, align = "left", h4("Drivers of Conversation")), br(), br(),
                                                      column(width = 6, h5(tags$u("Dec 29th - Jan 26th"), 
                                                                           align = "left"),
                                tags$li("First Australian case confirmed Jan 25th."), 
                                tags$li("Mentions in Australia surge from under 100 to 34,000 per week."), 
                                tags$li("Cases in China still quickly on the rise at 1406 cases."), 
                                br(),
                                h5(tags$u("Jan 26th - Feb 16th")),
                                tags$li("Australia reaches 6 cases, China at 61,000."), 
                                tags$li("The sad passing of the doctor who broke the news of the virus."), 
                                tags$li("The WHO officially names the disease COVID-19."), 
                                br(),
                                h5(tags$u("Feb 16th - Mar 1st")), 
                                tags$li("Harsh containment measures being implemented in China."), 
                                tags$li("Scott Morrison announced that we are facing a global emergency."), 
                                tags$li("Healthcare systems anticipate they will not meet demand."), 
                                br()), 
                                column(width = 6, 
                                       h5(tags$u("Mar 1st - Mar 22nd"), align = "left"),
                                       tags$li("First detected community spread in Australia."),
                                       tags$li("Cases in Australia reach 100."),
                                       tags$li("Ausralia enters mandatory lockdown for non-essentials."),
                                       br(),
                                       h5(tags$u("Mar 22nd - Apr 12th")),
                                       tags$li("Medical experts call for a nationwide lockdown."),
                                       tags$li("New restrictions and penalties including jail time."),
                                       tags$li("Large portion of Australian workforce adjusts to working from home."),
                                       br(),
                                       h5(tags$u("Apr 12th - Present")),
                                       tags$li("Criminal investigations into the Ruby Princess."),
                                       tags$li("Uncertain financial security for many Australians."),
                                       tags$li("Australia begins to flatten the curve."), 
                                       br()
                                       ))), 
                                 bs_button(button_size = "small", "Analysis", button_type = "default") %>%
                                   bs_attach_collapse("volume_collapse")),
                                         # wellPanel(plotlyOutput("lineplot_7days"),
                                         # bs_collapse("weeklytimelinecollapsed", content = tags$div(class = "well", column(width = 12, 
                                         #         p("This chart shows a 7 day window between the 9th - 15th of April (inclusive).")))),
                                         # bs_button("Analysis", button_type = "default") %>%
                                         #   bs_attach_collapse("weeklytimelinecollapsed")
                                         #   ),
                                 br(), br(),
                        h4("Sentiment of Covid-19 Conversation Over Time, Australia (Voc only)", align = "center"),
                        wellPanel(introBox(plotlyOutput("sentiment_plot"),
                                  data.step = 2,
                                  data.intro = "Here we analyze the <b>sentiment</b> of conversation over time among twitter, blogs and forums."),
                                  bs_collapse(id = "sentiment_collapse",
                                              content = tags$div(class = "well",
                                tags$em(  tags$p("This chart displays the percentage of mentions of COVID-19 from Australians only that are positive, negative and neutral. 
                                         Each date on the graph represents a 7 day period, beginning at the labelled date. For example, the first point labelled 'Dec 29, 2019' represents
                                         the average weekly sentiment between the 29th of December and the 4th of January inclusive. Data: Meltwater Explore; Sources: Blogs, Forums, Comments 
                                                 and Tweets Dec 29, 2019 - April 12th, 2020.")), 
                                  "Not surprisingly, sentiment has been largely negative in relation to COVID-19. This difference is even more pronounced when neutral mentions are removed 
                                  (click on ‘neutral’ in the legend box). 
                                  Key drivers are further elaborated on below.", tags$style(type = "text/css", "p { font-size: 12px; }"))
                                ),
                                bs_button("Analysis", button_type = "default") %>%
                                  bs_attach_collapse("sentiment_collapse")),
                                  column(width = 6, h5("VoC Sentiment in the last 7 days"), 
                                         wellPanel(introBox(data.step = 3,
                                                            data.intro = "This charts shows a snapshot of the consumer sentiment over the last week.", 
                                                            plotlyOutput("sevendaydonut"),
                                                            bs_collapse(id = "weekly_sentiment_collapse",
                                                            content = tags$div(class = "well",
                                                                             tags$em( tags$p("Sentiment in the 7 day period between the 9th and 15th of April inclusive. 
                                                                                      Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets.")),
                                                                   
                                                                               "Sentiment was on par with the monthly average.
                                                                               Drivers of negative sentiment in the last week have primarily been around:", 
                                                                               br(), br(),
                                                                               tags$li("Financial implications of the disease."),
                                                                                tags$li("The Ruby Princess investigation."),
                                                                                tags$li("Donald Trump accused the WHO of failure of duty."))),
                                                            bs_button("Analysis", button_type = "default") %>%
                                                                bs_attach_collapse("weekly_sentiment_collapse")))),
                                   column(width = 6, h5("VoC Sentiment in the last 7 days"), 
                                          wellPanel(introBox(data.step = 4, 
                                                             
                                                             data.intro = "This chart analyzes the top trending hashtags in the last week.
                                                             ", 
                                                             plotlyOutput("hashtags_7days"),
                                                     bs_collapse(id = "weekly_hashtags_collapse",
                                                                 content = tags$div(class = "well",
                                                                                  tags$em(  tags$p("Trending hashtags between the 9th and 15h of April inclusive.
                                                                                           Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets.")),
                                                                                    "Trending hashtags were grouped around three main concerns:",
                                                                                    br(), br(),
                                                                                    tags$li("The Ruby Princess cruise ship (this was the #1 hashtag in mentions of coronavirus for the week)."),
                                                                                    tags$li("Lockdown measures and containment (#stayathome, #stayathomesavelives, #stayhome) collectively 
                                                                                            had more mentions than the Ruby Princess."),
                                                                                    tags$li("International politics (#Trump, #Iran) was a matter of particular interest to Australians last week too."))),
                                                     bs_button("Analysis", button_type = "default") %>%
                                                         bs_attach_collapse("weekly_hashtags_collapse")))),
                                          br(), br(),
                                  column(width = 6,  h5("Text Sentiment Score - Weekly Snapshot"), 
                                         wellPanel(introBox(data.step = 5, 
                                                             data.intro = paste("This chart analyzes the top 25 words contributing to positive 
                                                             or negative sentiment in the last week using the ", 
                                                                                tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"), " sentiment lexicon"),
                                                             plotlyOutput("contribution_plot"),
                                                             bs_collapse(id = "contribution_collapse", content = tags$div(class = "well",
                                                             tags$em(tags$p("Top 25 contributing words to sentiment as determined by the", 
                                                                     tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"),
                                                              " sentiment analysis lexicon. 7 day period 9th - 15th Apr inclusive. 
                                                                     Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets.")), 
                                                              "Not surprisingly sentiment around coronavirus is weighted to the negative end of the spectrum,
                                                              with the largest contribution to negative sentiment being the notion of
                                                               a crisis, and the large numbers of death worldwide. Positive sentiment exhibits no clear trend, 
                                                              not surprising as it accounts for only a small proportion of all mentions (~10%).")),
                                                     bs_button("Analysis", button_type = "default") %>%
                                                        bs_attach_collapse("contribution_collapse")))),
                                  column(width = 6, h5("Text Sentiment Frequency - Weekly Snapshot"), 
                                          wellPanel(introBox(data.step = 6, 
                                                             data.intro = paste("This chart analyzes the top 25 words contributing to positive 
                                                             or negative sentiment in the last week using the ", 
                                                                                tags$a(href = "https://www.cs.uic.edu/~liub/FBS/opinion-mining-final-WSDM.pdf", "Bing Liu"), " sentiment lexicon"),
                                                             plotlyOutput("bing_sentiment_plot"),
                                                             bs_collapse(id = "bing_sent", content = tags$div(class = "well", 
                                                           tags$em( tags$p("Most frequent positive and negative words. Classifications
                                                                     determined by the", tags$a(href = "https://www.cs.uic.edu/~liub/FBS/opinion-mining-final-WSDM.pdf", 
                                                                                                "Bing Liu"), " sentiment lexicon. 7 day period 9th - 15th Apr inclusive. 
                                                                    Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets.")),
                                                               
                                                               "Echoing observations from previous charts we can see that negative sentiment 
                                                             is primarily about the high rate of death and infection, though positive sentiment
                                                              is more about support and unity.")),
                                                      bs_button("Analysis", button_type = "default") %>%
                                                        bs_attach_collapse("bing_sent")
                                          
                                                                         )))
                      #  wellPanel(introBox(column(width = 4, plotlyOutput("udpipe_plot")))),
                            )
                          )
                        ),
                 tabPanel(title = introBox( data.step = 7, data.intro = "Let's move over to the Search page - click 'Search'. <br/><br/> Then, click 'Next' to continue the tour.", "Search"),
                            sidebarPanel(img(src="unnamed2.png", width="80%", height="80%"),
                                         br(), br(),
                                         br(), br(),
                                         width = 2,
                                         tags$head(
                                           tags$style(HTML("
                      .introjs-tooltiptext {
                        color: #212121;
                      }
                
                    ")))
                            ),
                            mainPanel(tabsetPanel(tabPanel("Desktop", column(width = 12, align = "left",
                                             h4("Digital Search Phrase Match", align = "center"),
                                      wellPanel( introBox(data.step = 8, data.intro = "This plot shows us the phrases most commonly associated with 'coronavirus'
                                                           from all search engine traffic.", plotlyOutput("sw_desktop_plot")),
                                                bs_collapse(id = "sw_desktop_collapse", content = tags$div(class = "well", 
                                               tags$em(p("Associated phrases in searches for 'coronavirus' 
                                                  on desktop devices ranked by volume of searches; Period: 
                                                  Jan 1st - Mar 31st 2020; Data: SimilarWeb.") ), tags$li("The number one destination for searches of 'coronavirus' on desktop
                                                devices is www.health.gov.au."), tags$li("If we double-click on health.gov.au in the legend box to the right of the chart
                                                 we can see all of the matched phrases that led to this destination."), tags$li("When isolating other domains we can see that people 
                                                 searching for updates on international 
                                                news arrive at Al-Jazeera."), tags$li("Visitors to the Telegraph.co.uk are asking 'how did coronavirus start?'."), 
                                               tags$li("People asking about a 'vaccine' and a 'cure' arrive at theguardian.com.au."))),
                                                bs_button("Analysis", button_type = "default") %>%
                                                  bs_attach_collapse("sw_desktop_collapse"))
                                              , column(width = 12, align = "left", 
                                                       h4("Associated Phrases by Domain", align = "center"),
                                                       wellPanel(
                                                         introBox(data.step = 9, data.intro = "This chart shows the proportion of traffic that went to each domain from 
                                                                  search engine searches for 'coronavirus' and associated phrases.",
                                                         plotlyOutput("sw_desktop_phrases_donut")),
                                                                 bs_collapse(id = "sw_desktop_donut_collapse", 
                                                                 content = tags$div(class = "well", 
                                                                 tags$em(p("Associated phrases in searches for 'coronavirus' 
                                                                                on desktop devices ranked by destination; Period: 
                                                                                Jan 1st - Mar 31st 2020; Data: SimilarWeb.")), 
                                                                tags$li("Health.gov.au accounts for 56% of searches for 'coronavirus' from Australians."), 
                                                                tags$li("Nytimes.com and abc.com.au take the next two spots for most popular web destinations for searches of coronavirus."),
                                                                tags$li("Most domains are generating significantly smaller proportions of traffic share (< 5%)."))),
                                                             bs_button("Analysis", button_type = "default") %>%
                                                                   bs_attach_collapse("sw_desktop_donut_collapse")
                                                                 )))),
                                      tabPanel("Mobile", column(width = 12, align = "left",
                                                                h4("Digital Search Phrase Match", align = "center"),
                                                                wellPanel(
                                                                   plotlyOutput("sw_mobile_plot"),
                                                                  bs_collapse(id = "sw_mobile_collapse", content = tags$div(class = "well", tags$em(p("Associated phrases in searches for 'coronavirus' 
                                                                                                                          on mobile devices ranked by volume of searches; Period: 
                                                                                                                          Jan 1st - Mar 31st 2020; Data: SimilarWeb.")), 
                                                                                                                            tags$li("On mobile devices news.com.au is the most popular destination 
                                                                                                                            for Australians searching 'coronavirus'."), 
                                                                                                                            tags$li("Health.gov.au comes in second, 
                                                                                                                            followed by the world health organisation."), 
                                                                                                                            tags$li("Searchers are predominantly 
                                                                                                                            interested in the symptoms and updates on the spread of the disease."))),
                                                                  bs_button("Analysis", button_type = "default") %>%
                                                                    bs_attach_collapse("sw_mobile_collapse")
                                                                  ),
                                                                column(width = 12, align = "left",
                                                                       h4("Associated Phrases by Domain", align = "center"),
                                                                       wellPanel(
                                                                         plotlyOutput("sw_mobile_phrases_donut"),
                                                                         bs_collapse(id = "sw_mobile_donut_collapse", content = tags$div(class = "well", 
                                                                                                                      tags$em(  p("Associated phrases in searches for 'coronavirus' 
                                                                                                                          on mobile devices ranked by destination; Period: 
                                                                                                                          Jan 1st - Mar 31st 2020; Data: SimilarWeb.")), 
                                                                                                                      tags$li("News.com.au, health.gov.au and abc.com.au receive
                                                                                                                         the most traffic for searches of 'coronavirus' on mobile devices."),  
                                                                                                                        tags$li("Mobile traffic generates a significant number of non-Australian destinations, 
                                                                                                                        reflecting the global nature of the disease."),
                                                                                                                        tags$li("Most domains are receiving less than 2% of traffic share, 
                                                                                                                                indicating that desinations for searches of coronavirus are quite broad."))),
                                                                         bs_button("Analysis", button_type = "default") %>%
                                                                           bs_attach_collapse("sw_mobile_donut_collapse")
                                                                         )))))
                   )
           )
    )
    
                  
                   
# SERVER-----------------------------------------------------------------------------------------------------------      
         server = function(input, output, session){
           
           observeEvent(input$helpMe , {
             introjs(session)  
           })
           
           
           output$sw_mobile_plot <- renderPlotly({
             sw_mobile_plot
           })

           output$sw_mobile_phrases_donut <- renderPlotly({
             sw_mobile_phrases_donut
           })
                     
           output$sw_desktop_plot <- renderPlotly({
             sw_desktop_plot
           })
           
           output$sw_desktop_phrases_donut <- renderPlotly({
             sw_desktop_phrases_donut
           })
           
           output$sevendaydonut <- renderPlotly({
             sevendaydonut
           })             
           
           output$hashtags_7days <- renderPlotly({
             hashtags_7days
           })

           sentiment_timeline_plot  <- read_rds("sentiment_timeline_plot.rds")
           
           output$sentiment_plot <- plotly::renderPlotly({
             sentiment_timeline_plot
           })           
           
            # output$sentiment_plot <- plotly::renderPlotly(
            #   plotly::plot_ly(data = corona_sentiment,
            #                   source = "hoverplotsource",
            #                   mode = "none",
            #                   stackgroup = "one",
            #                   hoveron = 'points+fills'
            #                      ) %>%
            #        plotly::config(displayModeBar = FALSE) %>%
            #        plotly::add_trace(
            #          x = ~`Week Beginning`,
            #          y = ~`Percentage_Neutral`,
            #          name = "Neutral",
            #          fillcolor = total_mentions_colour)
            #         %>%
            #        plotly::config(displayModeBar = FALSE) %>%
            #        plotly::add_trace(
            #          x = ~`Week Beginning`,
            #          y = ~`Percentage_Negative`,
            #          name = "Negative",
            #          fillcolor = "red"
            #        ) %>%
            #        plotly::config(displayModeBar = FALSE) %>%
            #        plotly::add_trace(
            #          x = ~`Week Beginning`,
            #          y = ~`Percentage_Positive`,
            #          name = "Positive",
            #          fillcolor = "lightgreen"
            #        ) %>%
            #        plotly::layout(
            #          title = "",
            #          xaxis = list(title = "", color = "#ffffff"),
            #          yaxis = list(title = "Sentiment Over Time", color = "#ffffff"),
            #          legend = legend_features3,
            #          paper_bgcolor='#212121',
            #          plot_bgcolor='#212121',
            #          hovermode = "compare"
            #        )
            #   ) 
                   
            volume_chart <- readRDS("volumechart.rds")
            
            output$lineplot <- plotly::renderPlotly(
              volume_chart
            )
            
              # output$lineplot <- plotly::renderPlotly(
              #    plotly::plot_ly(data = corona_weeks,
              #                    source = "hoverplotsource",
              #                customdata = ~map2(uris$uri, urisclick, ~list(.x, .y))
              #                ) %>%
              #      plotly::config(displayModeBar = FALSE) %>%
              #    plotly::add_trace(
              #        x = ~`Week Beginning`,
              #        # y = ~active_cum,
              #        y = ~`All`,
              #        type = "scatter",
              #        mode = "lines+markers",
              #        # name = "Active",
              #        name = "Total Mentions",
              #        line = list(color = total_mentions_colour),
              #        marker = list(color = total_mentions_colour)
              #    ) %>%
              #      plotly::add_trace(
              #        x = ~`Week Beginning`,
              #        # y = ~active_cum,
              #        y = ~`cumulative_confirmed_Australia`,
              #        type = "scatter",
              #        mode = "lines+markers",
              #        # name = "Active",
              #        name = "Confirmed Cases Australia",
              #        line = list(color = twitter_colour),
              #        marker = list(color = twitter_colour)
              #      ) %>%
              #      plotly::add_trace(
              #        x = ~`Week Beginning`,
              #        # y = ~active_cum,
              #        y = ~`cumulative_confirmed_China`,
              #        type = "scatter",
              #        mode = "lines+markers",
              #        # name = "Active",
              #        name = "Confirmed Cases China",
              #        line = list(color = "red"),
              #        marker = list(color = "red")
              #      ) %>%
              #    htmlwidgets::onRender(readLines("tooltip-image.js")) %>%
              #      htmlwidgets::onRender(readLines("tooltip-imageclick.js")) %>%
              #    plotly::add_annotations(
              #        x = as.Date("2019-12-29"),
              #        y = 1,
              #        text = paste("First case in Wuhan, China"),
              #        xref = "x",
              #        yref = "y",
              #        arrowhead = 5,
              #        arrowhead = 3,
              #        arrowsize = 1,
              #        showarrow = TRUE,
              #        font = list(color = '#FFFFFF'),
              #        ax = 30,
              #        ay = -90
              #    ) %>%
              #    plotly::add_annotations(
              #        x = as.Date("2020-01-12"),
              #        y = 2,
              #        text = paste("First Case Outside of China (Thailand)"),
              #        xref = "x",
              #        yref = "y",
              #        arrowhead = 5,
              #        arrowhead = 3,
              #        arrowsize = 1,
              #        showarrow = TRUE,
              #        font = list(color = '#FFFFFF'),
              #        ax = 30,
              #        ay = -120
              #    ) %>%
              #    plotly::add_annotations(
              #        x = as.Date("2020-01-26"),
              #        y = 3,
              #        text = paste("First 'Imported' Case in Australia"),
              #        xref = "x",
              #        yref = "y",
              #        arrowhead = 5,
              #        arrowhead = 3,
              #        arrowsize = 1,
              #        showarrow = TRUE,
              #        font = list(color = '#FFFFFF'),
              #        ax =  0,
              #        ay = -180
              #    ) %>%
              #    plotly::add_annotations(
              #        x = as.Date("2020-03-15"),
              #        y = 3,
              #        text = paste(
              #            "New containment measures"
              #        ),
              #        xref = "x",
              #        yref = "y",
              #        arrowhead = 10,
              #        arrowhead = 3,
              #        arrowsize = 1,
              #        showarrow = TRUE,
              #        font = list(color = '#FFFFFF'),
              #        ax = -10,
              #        ay = -90
              #    ) %>%
              #      plotly::add_annotations(
              #        x = as.Date("2020-03-01"),
              #        y = 3,
              #        text = paste(
              #          "First community spread cases in Australia"
              #        ),
              #        xref = "x",
              #        yref = "y",
              #        arrowhead = 10,
              #        arrowhead = 3,
              #        arrowsize = 1,
              #        showarrow = TRUE,
              #        font = list(color = '#FFFFFF'),
              #        ax = -0,
              #        ay = -130
              #      ) %>% plotly::add_annotations(
              #        x = as.Date("2020-03-22"),
              #        y = 3,
              #        text = paste(
              #          "Australia reaches 1000 cases"
              #        ),
              #        xref = "x",
              #        yref = "y",
              #        arrowhead = 10,
              #        arrowhead = 3,
              #        arrowsize = 1,
              #        showarrow = TRUE,
              #        font = list(color = '#FFFFFF'),
              #        ax = -10,
              #        ay = -150
              #      ) %>%
              #    plotly::layout(
              #        title = "",
              #        yaxis = list(title = "Conversation Over Time", color = "#FFFFFF"),
              #        xaxis = list(title = "", color = "#FFFFFF", showticklabels = T, type = "category"
              #                     ),
              #        legend = legend_features,
              #        paper_bgcolor='#212121',
              #        plot_bgcolor='#212121'
              #    ))
              
              
          #    uris_7day$uri 
              
           #   urisclick_7day
              
           
            #    corona7day <- read_rds("~/NetBaseApi/coviddashboard/corona7day.rds")
              #   
              #   legend_features2 <- list(
              #     font = list(
              #       size = 12,
              #       color = "#FFFFFF"),
              #     bgcolor = "#212121",
              #     bordercolor = "#FFFFFF",
              #     borderwidth = 1,
              #     x = 0.1, 
              #     y = 0.2)
              #   
              #   corona7day <- corona7day %>%
              #     ungroup()
              #   
              # output$lineplot_7days <- plotly::renderPlotly({ 
              #   plotly::plot_ly(data = corona7day,
              #                   source = "hoverplotsource"
              #                #   ,customdata = ~map2(uris_7day$uri, urisclick_7day, ~list(.x, .y))
              #                   ) %>%
              #     plotly::config(displayModeBar = FALSE) %>%
              #     plotly::add_trace(
              #       x = ~`date2`,
              #       # y = ~active_cum,
              #       y = ~`Total`,
              #       type = "scatter",
              #       mode = "lines+markers",
              #       # name = "Active",
              #       name = "Total Mentions",
              #       line = list(color = total_mentions_colour),
              #       marker = list(color = total_mentions_colour)
              #     ) %>%
              #     plotly::add_trace(
              #       x = ~`date2`,
              #       # y = ~active_cum,
              #       y = ~`cumulative_confirmed_Australia`,
              #       type = "scatter",
              #       mode = "lines+markers",
              #       # name = "Active",
              #       name = "Confirmed Cases Australia",
              #       line = list(color = twitter_colour),
              #       marker = list(color = twitter_colour)
              #     ) %>%
              #     plotly::add_trace(
              #       x = ~`date2`,
              #       # y = ~active_cum,
              #       y = ~`cumulative_confirmed_China`,
              #       type = "scatter",
              #       mode = "lines+markers",
              #       # name = "Active",
              #       name = "Confirmed Cases China",
              #       line = list(color = "red"),
              #       marker = list(color = "red")
              #     ) %>%
              # #    htmlwidgets::onRender(readLines("tooltip-image.js")) %>%
              # #    htmlwidgets::onRender(readLines("tooltip-imageclick.js")) %>%
              #     # plotly::add_annotations(
              #     #   x = as.Date("2019-12-29"),
              #     #   y = 1,
              #     #   text = paste("First case"),
              #     #   xref = "x",
              #     #   yref = "y",
              #     #   arrowhead = 5,
              #     #   arrowhead = 3,
              #     #   arrowsize = 1,
              #     #   showarrow = TRUE,
              #     #   font = list(color = '#FFFFFF'),
              #     #   ax = -90,
              #     #   ay = -90
              #     # ) %>%
              #     # plotly::add_annotations(
              #     #   x = as.Date("2020-01-12"),
              #     #   y = 2,
              #     #   text = paste("First Case Outside of China (Thailand)"),
              #     #   xref = "x",
              #     #   yref = "y",
              #     #   arrowhead = 10,
              #     #   arrowhead = 3,
              #     #   arrowsize = 1,
              #     #   showarrow = TRUE,
              #     #   font = list(color = '#FFFFFF'),
              #     #   ax = -120,
              #     #   ay = -120
              #     # ) %>%
              #     # plotly::add_annotations(
              #     #   x = as.Date("2020-01-26"),
              #     #   y = 3,
              #     #   text = paste("First 'Imported' Case in Australia"),
              #     #   xref = "x",
              #     #   yref = "y",
              #     #   arrowhead = 5,
              #     #   arrowhead = 3,
              #     #   arrowsize = 1,
              #     #   showarrow = TRUE,
              #     #   font = list(color = '#FFFFFF'),
              #     #   ax = -150,
              #     #   ay = -180
              #     # ) %>%
              #     # plotly::add_annotations(
              #     #   x = as.Date("2020-03-15"),
              #     #   y = 3,
              #     #   text = paste(
              #     #     "New containment measures"
              #     #   ),
              #     #   xref = "x",
              #     #   yref = "y",
              #     #   arrowhead = 10,
              #     #   arrowhead = 3,
              #     #   arrowsize = 1,
              #     #   showarrow = TRUE,
              #     #   font = list(color = '#FFFFFF'),
              #     #   ax = -10,
              #     #   ay = -90
              #     # ) %>%
              #     plotly::layout(
              #       title = "",
              #       yaxis = list(title = "Conversation Over Time", color = "#FFFFFF"),
              #       xaxis = list(title = "", color = "#FFFFFF"),
              #       legend = legend_features2,
              #       paper_bgcolor='#212121',
              #       plot_bgcolor='#212121'
              #     )
              # })
        
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
             
      



