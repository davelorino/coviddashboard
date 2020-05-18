
library(shiny)
library(rintrojs)
library(tidyverse)
library(plotly)
library(lubridate)
library(shinythemes)
library(magrittr)
library(bsplus)
library(wordcloud2)
library(reactable)
library(htmltools)
library(stringr)
library(twitterwidget)
library(DT)
library(tm)

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
    
    bar_chart <- function(label, value, destination, width = "100%", height = "16px", background = NULL) {
      
      if(value == "100 anz.com.au")               
        bar <- div(style = list(background = "#004164", width = width, height = height))
      
      else if(value == "100 commbank.com.au")
        bar <- div(style = list(background = "#f2c40e", width = width, height = height))
      
      else if(value == "100 westpac.com.au")
        bar <- div(style = list(background = "#db002c", width = width, height = height))
      
      else if(value == "100 nab.com.au")
        bar <- div(style = list(background = "#3fc1c9", width = width, height = height))
      
      else if(value == "100 stgeorge.com.au")
        bar <- div(style = list(background = "#78be20", width = width, height = height))
      
      else
        bar <- div(style = list(background = "#FFFFF", width = width, height = height))
      
      chart <- div(style = list(flexGrow = 1
                                , marginLeft = "8px"
                                , background = background), bar)
      div(style = list(display = "flex", alignItems = "center"), paste(round(as.numeric(label)), "%"), chart)
    }

    
# DATA---------------------------------------------------------------------------------------------------------------------
    #whatisthis <- read_rds("uris.rds")
    sevendaydonut <- read_rds("sevendaydonut2.rds")
    #volumeovertime <- read_csv("coronaconvo2.csv")
    #coronasent <- read_csv("coronasent.csv")
    hashtags_7days <- read_rds("hashtags_plot7day.rds")
   # coronaverbatims <- read_csv("coronaverbatims_l7d_wed8thmar.csv")
   # afinn <- readRDS("afinn.rds")
    apple_trending_apps1 <- read_csv("apple_store_apps.csv")
    google_trending_apps1 <- read_csv("playstore_apps.csv")
    #uris <- readRDS("uris.rds")
    #urisclick <- readRDS("urisclick.rds")
    destinations_test_data <- readRDS("destinations_test.rds")
    kw_branded_top25 <- readRDS("kw_branded_top25.rds")
  
    #coronavirus <- readRDS("coronavirus2.rds")
    corona7daycases <- readRDS("corona7daycases.rds")
    sw_keywords_plot <- readRDS("sw_keywords_plot.rds")
    volume_chart <- readRDS("volumechart.rds")
    sentiment_timeline_plot  <- read_rds("sentiment_timeline_plot.rds")
    sw_mobile_plot <- readRDS("sw_mobile_plot.rds")
    sw_mobile_phrases_donut <- readRDS("mobile_phrases_donut.rds")
    sw_desktop_phrases_donut <- readRDS("desktop_phrases_donut.rds")
    rona_cloud <- readRDS("ronacloud.rds")
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
                   "https://twitter.com/broomstick33/status/1247767301253783557?s=20",
                   "https://twitter.com/BillGates/status/1250292126643941376",
                   "https://twitter.com/ScottMorrisonMP/status/1252785723830226944"
                   
                   
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
    contribution_plot_30days <- read_rds("plotly_object30days.rds")
    bing_sentiment_plot <- read_rds("covid_bingsent_plot.rds")

# UI-------------------------------------------------------------------------------------------------------------
    
    ui = navbarPage(
                    
                   title = "Saatchi & Saatchi COVID-19 Pulse", theme = shinytheme("darkly"),
                   tabPanel(title = "Social",
                       sidebarPanel(img(src="Artboard1Logo.png", width="80%", height="80%"),
                                    tags$head(HTML("<meta name=\"google-site-verification\" content=\"[google-site-verification: googlea55a212b590645dc.html]\" />")),
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
                       mainPanel(
                         fluidRow( height = 12,
                         column(width = 12, align = "left",
                              
                         h4("Volume of COVID-19 conversation over time, Australia (VoC only)", align = "center"),
                      wellPanel(introBox(plotlyOutput("lineplot", height = "500px"), 
                                 data.step = 1, 
                                 data.intro = "Here we analyse the <b>volume</b> of conversation over time among Twitter, comments, 
                                 blogs and forums. <b>Hover</b> over the points to see what drove the conversation.",
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
                                          Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets Dec 29, 2019 - May 8th, 2020. 
                                          Images on hover are selected from the top 10 of the week retweeted by people with < 1000 reach.", 
                                                     tags$style(type = "text/css", "p { font-size: 12px; }")))),
                                br(), br(),
                                column(12, align = "left", h4("Drivers of Conversation to Date")), br(), br(),
                               column(width = 12),
                                br(), br(),
                                column(width = 6, align = "left",
                                       h5("Since Australians became aware of COVID-19 in December 2019, 
                                                       there has been fluctuating consumer interest in the virus as 
                                                       seen through the volume of online conversations over time."),
                                tags$li("While the first case was reported in Wuhan on the 29th of December, 
                                        Australian consumer interest about COVID-19 remained low for a number 
                                        of weeks.", tags$b("At this early stage, there appeared to be no immediate threat 
                                        to Australia and with bushfires still raging across the country, 
                                        COVID-19 was visibly not a cause for concern for the vast majority of 
                                        Australians. The resolution of the bushfire crisis was then top of mind for many.")),
                                br(),
                        
                                tags$li("This state of play changed in late January with conversations spiking first around the 
                                        discovery of the the first ‘imported’ case in Australia, and then again with the government 
                                        enactment of new lockdown measures to control the rapid spread of the virus.",  
                                        tags$b("The mood of 
                                        the nation then changed significantly, with once again Australians primarily concerned 
                                        with domestic developments impacting not only their personal health and safety but also 
                                        their day to day life and lifestyle. ")),
                                
                                br(),
                                tags$li("As the virus continued spreading in Australia, the VoC continued rising with its focus shifting 
                                from international to local news events, such as state government policies and the possibility 
                                of a stricter lockdown. As the public became more politically engaged online, comparisons 
                                started emerging between Australia and New Zealand as their government enacted a strict lock-down 
                                to curb the spread of COVID-19."),
                                br(),
                                tags$li("Since the mid-March peak following the introduction of the first containment measures, 
                                        the VoC conversation around the virus  has been on the decline,", tags$b("suggesting increased 
                                        media fatigue and disengagement by the Australian public. The worst of the crisis 
                                        appears to be over as the curve begins to flatten and conversation shifts from outbreak, 
                                        to the subsequent economic fallout.")), br(),
                                tags$li("Last weekend’s announcement of the easing of the restrictions for the first time since 
                                        lockdown failed to spark any significant interest with conversations continuing to decline,", 
                                        tags$b("indicative of a cautious behaviour and an overall fatigue with the ongoing COVID crisis."))
                                ),
                                column(width = 6, h4(tags$b(tags$u("Key Events"))),
                                                    h5(tags$u("Dec 29th - Jan 26th")),
                                tags$li("First Australian case confirmed Jan 25th."), 
                                tags$li("Mentions in Australia surge from under 100 to 34,000 per week."), 
                                tags$li("Cases in China still quickly on the rise at 1406 cases."), 
                                br(),
                                h5(tags$u("Jan 26th - Feb 16th")),
                                tags$li("Australia reaches 6 cases, China at 61,000."), 
                                tags$li("Whistle-blowing doctor who broke news of the virus, dies in China"), 
                                tags$li("The WHO officially names the disease COVID-19."), 
                                br(),
                                h5(tags$u("Feb 16th - Mar 1st")), 
                                tags$li("Harsh containment measures being implemented in China."), 
                                tags$li("Scott Morrison announced that we are facing a global emergency."), 
                                tags$li("Healthcare systems anticipate they will not meet demand."), 
                                br(), 
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
                                       h5(tags$u("Apr 12th - May 3rd")),
                                       tags$li("Donald Trump halts funding to the World Health Organisation."),
                                       tags$li("Australian government releases COVIDSafe app."),
                                       tags$li("Some Australian states begin relaxing lockdown laws as curve flattens."), 
                                       br(),
                                h5(tags$u("May 3rd - May 10th")),
                                      tags$li("With restrictions easing, parents debate whether schools should re-open.")
                                       ))), 
                               
                                 introBox(data.step = 2, data.intro = "Click the Analysis button located on the bottom-left of every chart to see insights.", 
                                          bs_button(button_size = "small", "Analysis", button_type = "default") %>%
                                   bs_attach_collapse("volume_collapse")))
                      ,
                      
                                         # wellPanel(plotlyOutput("lineplot_7days"),
                                         # bs_collapse("weeklytimelinecollapsed", content = tags$div(class = "well", column(width = 12, 
                                         #         p("This chart shows a 7 day window between the 9th - 15th of April (inclusive).")))),
                                         # bs_button("Analysis", button_type = "default") %>%
                                         #   bs_attach_collapse("weeklytimelinecollapsed")
                                         #   ),
                                 br(), br(),
                     
                        h4("Sentiment of COVID-19 conversation over time, Australia (VoC only)", align = "center"),
                        wellPanel(introBox(plotlyOutput("sentiment_plot", height = "450px"),
                                  data.step = 3,
                                  data.intro = "Here we analyse the <b>sentiment</b> of conversation over time among Twitter, comments, blogs and forums."),
                                  bs_collapse(id = "sentiment_collapse",
                                              content = tags$div(class = "well",
                                tags$em(  
                                  tags$p("This chart displays the percentage of mentions of COVID-19 from Australians 
                                          only that are positive, negative and neutral. 
                                         Each date on the graph represents a 7 day period, beginning at the 
                                         labelled date. For example, the first point labelled 'Dec 29, 2019' represents
                                         the average weekly sentiment between the 29th of December and the 4th of January inclusive. 
                                         Data: Meltwater Explore; Sources: Blogs, Forums, Comments 
                                          and Tweets Dec 29, 2019 - May 8th, 2020. Note: Neutral sentiment refers to 
                                          mentions in which either negative or positive keywords could not be identified.")), 
                                  
                               column(width=12, 
                               br(),
                               tags$li("On the 5th of January, negative sentiment quickly spiked in correlation 
                               with the World Health Organisation’s global announcement of a new coronavirus outbreak. At
                               this early stage however, Australian consumer interest was low and largely driven by international 
                               news coverage.", tags$b("Australians were then mostly untroubled by the events unfolding overseas as yet 
                               unaware of the implications the outbreak would soon have on their daily lives.")),
                                br(),
                                tags$li("Negative sentiment has since stayed high, averaging 51% and not dropping below 45%. 
                                Interestingly, while we witnessed a significant spike in mentions mid-March, this was not 
                                mirrored by a dramatic shift in consumer sentiment.", tags$b("This suggests that despite varying levels of 
                                interest since the start of the COVID-19 crisis, Australians have been mostly feeling anxious and 
                                uncertain about the its outcome and the future.")), 
                                br(),
                                tags$li("From early March, we observed a sustained increase in the proportion of positive COVID-19
                                conversations online (from <11% to >14%). This correlates with the introduction of mandatory 
                                government measures to protect essential workers and encourage the public to socially distance. ", 
                                tags$b("While criticism and uncertainty about the future remain, this slight improvement in consumer 
                                sentiment reflects widespread support and empathy for the measures in light of a new ‘preventative’ 
                                national mindset.")),
                                br(),
                                tags$li("Yet despite Australia’s recent success in flattening the curve, since mid-April positive 
                                        sentiment has marginally fallen.", tags$b("With a large portion of the nation still in lockdown and 
                                        media fatigue setting in, Australians appear mostly unaffected by the good news around the 
                                        containment of the virus or easing of the restrictions. Instead they seem more 
                                        preoccupied with controversy surrounding government policy (in particular the launch of 
                                        the government’s new COVIDSafe app and potential re-opening of schools).")), 
                                br(), br()
                                , tags$style(type = "text/css", "p { font-size: 12px; }")))
                                ),
                                bs_button("Analysis", button_type = "default") %>%
                                  bs_attach_collapse("sentiment_collapse")),
                                  column(width = 6, h5("VoC sentiment - weekly snapshot"), 
                                         wellPanel(introBox(data.step = 4,
                                                            plotlyOutput("sevendaydonut"),
                                                            data.intro = "This chart shows a snapshot of the consumer sentiment from Twitter, 
                                                            comments, blogs and forums over the last week.", 
                                                            bs_collapse(id = "weekly_sentiment_collapse",
                                                            content = tags$div(class = "well",
                                                             tags$em( tags$p("Sentiment in the 7 day period between the 2nd and the 8th of May inclusive. 
                                                                      Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets. 
                                                                      Note: Neutral sentiment refers to mentions in which either 
                                                                      negative or positive keywords could not be identified.")), 
                                                             br(),
                                     
                                                "At the end of last week, lockdown restrictions began to ease in many states for the first time since mid-March. 
                                                Despite this there has been absolutely no shift in sentiment, showing that the ", 
                                                tags$b("development has done little 
                                                to bolster the mood of a media-fatigued and disheartened public."), 
                                                br(), br(),
                                                "As the curve continues to flatten and the health crisis begins to stabilize, conversation this week has shifted to
                                                whether children should return to school. While the government believes it’s necessary to allow parents to return to 
                                                work, some are questioning whether kids are truly resistant to the virus. ",  tags$b("With financial hardship a key area of concern 
                                                for most Australians, they still appear to be apprehensive of easing restrictions at the risk of another surge in infections."),
                                                 br(), br())),
                                                            bs_button("Analysis", button_type = "default") %>%
                                                                bs_attach_collapse("weekly_sentiment_collapse")))),
                                   column(width = 6, h5("Trending hashtags - weekly snapshot"), 
                                          wellPanel(introBox(data.step = 5, 
                                                             
                                                             data.intro = "This chart analyses the top trending hashtags from Twitter in the last week.
                                                             ", 
                                                             plotlyOutput("hashtags_7days"),
                                                     bs_collapse(id = "weekly_hashtags_collapse",
                                                                 content = tags$div(class = "well",
                                                                                  tags$em(  tags$p("Trending hashtags between the 2nd and the 8th of May inclusive.
                                                                                           Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets.")),
       
                                                                                    column(width = 12,
                                                                                    tags$li("Politics was at the forefront of online conversation with ", tags$b("#scottyfrommarketing "), 
                                                                                            "emerging as the top hashtag this week, as controversy around the new ", tags$b("#COVIDSafe "), 
                                                                                            "app continues - with the PM requesting the ABC stops reporting negatively on the app."), br(),
                                                                                    
                                                                                    tags$li(tags$b("#RubyPrincess "), "fell from #1 to #6 this week, however as investigations continue it remains 
                                                                                            a key topic of conversation as the source of Australia’s largest outbreak."), br(),
                                                                                    tags$li("Despite a preference for local news stories, international politics remained of 
                                                                                    interest to Australians last week with ", tags$b("#Trump "), "trending in response to breaking news stories."),
                                                                                     
                                                                                            br(), br()
                                                                                      ))),
                                                     bs_button("Analysis", button_type = "default") %>%
                                                         bs_attach_collapse("weekly_hashtags_collapse")))),
                                          br(), br(),
                                 column(width = 6,  h5("Text sentiment score - weekly snapshot"), 
                                         wellPanel(introBox(data.step = 6, 
                                                             data.intro = paste("This chart analyses the top 25 words contributing to positive or negative sentiment from Twitter, comments, 
                                                                                blogs and forums in the last week using the ", 
                                                                                tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"), " sentiment lexicon."),
                                                             plotlyOutput("contribution_plot")
                                                             
                                                     )
                                                   )),
                                  column(width = 6, h5("Text sentiment score - monthly snapshot"), 
                                          wellPanel(introBox(data.step = 7, 
                                                             data.intro = paste("This chart analyses the top 25 words contributing to positive 
                                                             or negative sentiment from Twitter, blogs, comments and forums in the last month using the ", 
                                                                                tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"), " sentiment lexicon."),
                                                             plotlyOutput("contribution_plot_30days")
                                                                         ))),
                      column(width = 12, wellPanel(
                      bs_collapse(id = "contribution_collapse", content = tags$div(class = "well", 
                                                                                   column(width=6,
                                                                                   tags$em(tags$p("Top 25 contributing words to sentiment as determined by the", 
                                                                                                  tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"),
                                                                                                  " sentiment analysis lexicon. 7 day period 2nd and the 8th of May inclusive. 
                                                                     Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets."))),                                                        
                                                                column(width = 6,
                                                                                   tags$em(tags$p("Top 25 contributing words to sentiment as determined by the", 
                                                                                                  tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"),
                                                                                        " sentiment analysis lexicon. 4 week period 2nd and the 8th of May inclusive. 
                                                                     Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets."))), br(), 
                                                            
                                                                    tags$li("The largest contributors to negative sentiment continue to be focused on virus casualties 
                                                                    and spread (e.g. dead, infected, die, death, died). 
                                                                    ", tags$b("There is visibly still a high level of fear within the community, 
                                                                              with many questioning whether the country is ready to start easing restrictions.")), br(),
                                              tags$li("While only accounting for a small portion of mentions, terms that generated 
                                              positive online sentiment reflect the ‘good news stories’ that are emerging from this 
                                              crisis (e.g. help, care, support, thanks, love) ", tags$b("showing that there is a consumer appetite for 
                                              uplifting stories in these challenging times.")), br(),
                                              tags$li("In the last 7 days conspiracy has emerged as a new trending negative term, as anti-lockdown protests are held across 
                                                      the country. Vaccines, the 5G network and tracking apps are at the centre of debate, with a small but loud minority 
                                                      questioning whether “COVID-19 is a pandemic or a plandemic?”."))),
                      bs_button("Analysis", button_type = "default") %>%
                        bs_attach_collapse("contribution_collapse"))), br(), br(),
                      column(width = 12, h5("Most talked about brands in connection with Coronavirus (Australia VoC only) - weekly snapshot", align = "center"), 
                             wellPanel(introBox(data.step = 8, 
                                                data.hint = "Click on a word to see a snippet of the conversation.", 
                             data.intro = "This chart shows the brands and organisations that are being
                                                 talked about most by cosumers in the last week, and the sentiment towards that brand or organisation.", 
                                                wordcloud2Output("rona_cloud")
                             ,                  tags$script(HTML(
                                                  "$(document).on('click', '#canvas', function() {",
                                                  'word = document.getElementById("wcSpan").innerHTML;',
                                                  "Shiny.onInputChange('selected_word', word);",
                                                  "});"
                                                ))
                             ),
                        bs_collapse("business_cloud", 
                                    content = tags$div(class = "well",
                                        column(width = 12,
                                            tags$em(tags$p("Top 7 most talked about brands in connection with Coronavirus 
                                            in the last 7 days, ranked by VoC sentiment and volume of mentions (Australia only). 
                                            Brand mentions may be split out by both negative and positive sentiments where applicable. 
                                            Period May 2nd to May 8th. Data: Meltwater. Brand Detection: OpenNLP."))), br(),
                                          column(width = 12,
                                            h5("Not surprisingly, some of the most impacted industries by the virus outbreak 
                                               (retail, air travel, sporting codes and pharmaceuticals) have been at the centre of many debates in the last week:"),
                                            tags$li("In the past week, the ", tags$b("NRL "), "has generated the largest volume of brand conversations in Australia. A majority 
                                                    of this was negative, largely in relation to reports that a number of players are opposed to mandatory flu 
                                                    vaccinations for all teams (including in the ", tags$b("AFL "), "). Despite this, a number of players have come out supporting 
                                                    the measures, as well as advocating for the government’s new COVIDSafe app - which has generated positive 
                                                    conversation online for the brand."), br(),
                                            tags$li("Australian supermarkets ", tags$b("Coles ", "and ", tags$b("Woolworths "), "continue to dominate the COVID conversation, as controversial 
                                                    measures to protect customers and staff are introduced such as their new checkout policies. From a business 
                                                    perspective, both brands have released figures each citing a multi-billion dollar COVID-19 boom which has 
                                                    garnered positive speculation.")), br(),
                                            tags$li("Home-grown airline ", tags$b("Virgin "), "Australia has also been the centre of conversation online after they entered voluntary 
                                                    administration off the back of travel bans and declining sales. There has however been a number of positive 
                                                    conversations online around the possibility of the airline becoming a public asset owned by the QLD government."), br(),
                                            tags$li("Pharmaceutical company ", tags$b("Pfizer "), "was the only brand to generate a largely positive conversation in the past week, 
                                            with the announcement that they may begin testing a coronavirus vaccine by September. With a public desperate for a 
                                            solution to the current health crisis, this is welcoming news for fearful Australians."), br(),
                                            tags$li("The Wuhan Institute of Virology (", tags$b("WIOV"), ") generated the third highest level of negative sentiment in the last week, 
                                                    recently blacklisted by the NIH over suspicions that they falsified scientific records."), br()
                                            
                                            ),
                                        tags$style(" 
                                               .blockquote.twitter-tweet {
                                                display: inline-block;
                                                font-family: 'Helvetica Neue', Roboto, 'Segoe UI', Calibri, sans-serif;
                                                font-size: 12px;
                                                font-weight: bold;
                                                line-height: 16px;
                                                border-color: #eee #ddd #bbb;
                                                border-radius: 5px;
                                                border-style: solid;
                                                border-width: 1px;
                                                box-shadow: 0 1px 3px rgba(0, 0, 0, 0.15);
                                                margin: 10px 5px;
                                                padding: 0 16px 16px 16px;
                                                max-width: 10px;
                                              } ")
                                        #,
                                        # column(width = 6, align = "center",
                                        #     wellPanel( style = "overflow-y:scroll; max-height: 700px",  
                                        #                h5("Scroll down to see more."),
                                        #    HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">I hope to proved wrong but I have the feeling the push to open up Australia too early is going to be a disaster. Big push by Murdoch, big business, Morrison, NRL. It might end in tears. <a href="https://twitter.com/hashtag/auspol?src=hash&amp;ref_src=twsrc%5Etfw">#auspol</a> <a href="https://twitter.com/hashtag/qanda?src=hash&amp;ref_src=twsrc%5Etfw">#qanda</a></p>&mdash; Eddy Jokovich (@EddyJokovich) <a href="https://twitter.com/EddyJokovich/status/1254757356107268102?ref_src=twsrc%5Etfw">April 27, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        #    HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Coles delivers +13.1% supermarket sales growth<br><br>Alcohol sales jump +7.2%<br><br>But online sales growth drops to 14% given widespread restrictions on home deliveries due to the decision to prioritise elderly and vulnerable customers <br><br>Costs also higher due to extra staff &amp; cleaning <a href="https://t.co/847hTsNnCF">pic.twitter.com/847hTsNnCF</a></p>&mdash; Gemma Felicity Acton (@GemmaActon) <a href="https://twitter.com/GemmaActon/status/1255274677666226178?ref_src=twsrc%5Etfw">April 28, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        #    HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Virgin Australia administrators stop giving credits and refunds for cancelled flights <a href="https://t.co/2O4HDlx4zG">https://t.co/2O4HDlx4zG</a></p>&mdash; The Guardian (@guardian) <a href="https://twitter.com/guardian/status/1258297808379170818?ref_src=twsrc%5Etfw">May 7, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        #    HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Would-be owners of Virgin Australia have two weeks to lodge their first offer, in what the administrator says will be &quot;a very competitive process&quot; involving &quot;high-quality bidders with fantastic credentials and the ability to restructure this business.” <a href="https://t.co/zlW1XPZdtf">https://t.co/zlW1XPZdtf</a></p>&mdash; Executive Traveller (@AusBT) <a href="https://twitter.com/AusBT/status/1255690289236197377?ref_src=twsrc%5Etfw">April 30, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        #    HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">A German company working with US pharmaceutical giant Pfizer has begun human trials of a potential Covid-19 vaccine that could supply millions by the end of the year, according to the two firms. <a href="https://t.co/Ip2DaZDsCH">https://t.co/Ip2DaZDsCH</a></p>&mdash; CNN (@CNN) <a href="https://twitter.com/CNN/status/1255731644587180032?ref_src=twsrc%5Etfw">April 30, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        #    HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr"><a href="https://t.co/UfZg0aVHH0">https://t.co/UfZg0aVHH0</a><br><br>&quot;Wuhan Institute of Virology has been engaged in so-called &quot;gain of function&quot; (GOF) research ... used to turn viruses into human pathogens capable of causing a global pandemic.&quot; <br><br>Thank you, Newsweek, for confirming what I have been saying for two months</p>&mdash; StevenWMosher (@StevenWMosher) <a href="https://twitter.com/StevenWMosher/status/1254992528518111236?ref_src=twsrc%5Etfw">April 28, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
                                        #    
                                        #       ))
                                        )),
                        bs_button("Analysis", button_type = "default") %>%
                          bs_attach_collapse("business_cloud"), br()
                        )), br(), br() 
                      # ,column(width = 6, h4("Test"), 
                      #        wellPanel(twitterwidgetOutput("tweet_output")
                        #verbatimTextOutput("print")
                       # ))
                      #  wellPanel(introBox(column(width = 4, plotlyOutput("udpipe_plot")))),
                            )
                    
                          )
                        )
                      )
                 ,
                 tabPanel(title = introBox( data.step = 9, data.intro = "Let's move over to the Search page - click 'Search'. 
                                            <br/><br/> Then, click 'Next' to continue the tour.", "Search"),
                             sidebarPanel(img(src="Artboard1Logo.png", width="80%", height="80%"),
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
                             mainPanel(column(width = 12, align = "left",
                                              h4("Top organic search keywords inc. ‘coronavirus’ (desktop and mobile web, Australia only)", align = "center"),
                                      wellPanel(introBox(data.step = 10, 
                                      data.intro = "This plot shows us the most popular search term variations including coronavirus from all search engine traffic.", plotlyOutput("sw_keywords_plot")),
                                                bs_collapse(id = "sw_keywords_collapse", content = tags$div(class = "well", 
                                                tags$em(p("Top organic search keywords including ‘coronavirus’ ranked by volume 
                                                of searches and destination (mobile and desktop traffic combined) for the period Feb 1st to Apr 30th; Data: SimilarWeb.")),
                                                br(),
                                                "From Feb-April Australians have been mostly concerned with understanding the virus 
                                                and its symptoms, assessing how it was spread (both locally and abroad) and the 
                                                possibility of a vaccine. ", tags$b("They are visibly as anxious about personal safety with 
                                                detection of the disease front of mind as they are about its eradication."), br(), br(),
                                                
                                                tags$li("Australians have been turning to government websites (health.gov.au, cdc.gov, 
                                                who.int) as their primary source of truth on COVID-19, particularly when it came to 
                                                researching symptoms and policy updates."), br(),
                                                tags$li("Local news outlets (news.com.au, couriermail.com, abc.net.au, theguardian.com, 
                                                watoday.com) have been the preferred go-to destinations for the latest developments on 
                                                the spread of the disease at home and the development of a vaccine, with a particular 
                                                interest in news with a geographic lens (i.e. NSW, QLD, Sydney, Perth and Melbourne)."), br(),
                                                tags$li("Well-established reputable international news outlets  (straitstimes.com, 
                                                aljazeera.com, telegraph.co.uk) while less popular, were the top destinations 
                                                for those searching ‘how did coronavirus start’ and other key overseas 
                                                developments (i.e. total cases and the outbreak in Italy and Singapore)."))
                                                
                                                
                                                ),
                                                 bs_button("Analysis", button_type = "default") %>%
                                                   bs_attach_collapse("sw_keywords_collapse"))), br(), br(),
                                     
                                      column(width = 6, align = "left", h5("Trending up apps, App Store/iPhone (last 28 days)"), introBox(data.step = 11, data.intro = "This chart shows the top 10 free trending apps across all categories in Australia over the last 28 days in the App store (iPhone only).", 
                                                                                                                                         wellPanel(reactableOutput("apple_table")))),
                                      column(width = 6, align = "left", h5("Trending up apps, Play Store (last 28 days)"), introBox(data.step = 12, data.intro = "This chart shows the top 10 free trending apps across all categories in Australia over the last 28 days in Google Play Store.", wellPanel(reactableOutput("google_trending_apps")))),
                                      br(),
                                     column(width = 12, wellPanel(
                                       bs_collapse(id = "apps_collapse", content = tags$div(class = "well", 
                                                                                                   column(width=12,
                                                                                                          tags$em(tags$p("Top Free Apps (across all categories) with the biggest increase over the last 28 days in Store Rank or in Usage Rank, Australia only." 
                                                                                                                         )), 
                                                                                                          
                                                                                                          tags$li("Interestingly, all of the trending up apps for the period pan hard to either end of the 
                                                                                                          entertainment/utility scale. With less to do, but more to worry about than ever before, ", tags$b("it 
                                                                                                          appears Australians are playing a balancing act between staying informed, while staying sane.")), 
                                                                                                          br(),
                                                                                                          tags$li("As such, entertainment (in particular gaming, social, music and dating) apps dominate the charts 
                                                                                                          as ", tags$b("consumers look for novel ways to distract themselves during the current crisis.")), br(),  
                                                                                                          tags$li("On the utility end of the spectrum, the official Coronavirus Australia app took the top 
                                                                                                          spot - providing consumers with live government updates. With a large segment of 
                                                                                                          the workforce now working from home, functional apps such as Google Home, 
                                                                                                          NordVPN and Canon Print are also trending up."), br()
                                                                                                          )
                                                                                            # ,      column(width = 6,
                                                                                            #               tags$em(tags$p("Top 10 upward trending apps on the Google Play store in the last 28 days. Data: SimilarWeb. ")), br(), 
                                                                                            #        )
                                                                                            )),
                                      bs_button("Analysis", button_type = "default") %>%
                                        bs_attach_collapse("apps_collapse"))), br(), br()
                                     # ,
                                     # column(width = 12, align = "center", h5("Search Top  Keywords - Paid & Organic - Banking Category", align = "center"), 
                                     #        wellPanel(reactableOutput("better_keywords_branded")
                                     #                  ) 
                                     #        )
                             ))
         
    )
    
                  
                   
# SERVER-----------------------------------------------------------------------------------------------------------      
         server = function(input, output, session){
           
           observeEvent(input$helpMe , {
             introjs(session)  
           })
           
           output$sw_mobile_phrases_donut <- renderPlotly({
             sw_mobile_phrases_donut
           })
                     
           output$sw_keywords_plot <- renderPlotly({
             sw_keywords_plot
           })
          
           
           output$sevendaydonut <- renderPlotly({
             sevendaydonut
           })             
           
           output$hashtags_7days <- renderPlotly({
             hashtags_7days
           })
          
          output$rona_cloud <- renderWordcloud2({
            rona_cloud
          })
           
           output$sentiment_plot <- plotly::renderPlotly({
             sentiment_timeline_plot
           })           
           
           
            
            output$lineplot <- plotly::renderPlotly(
              volume_chart
            )
            
            theme1<- reactableTheme(color = "hsl(0, 0%, 90%)", backgroundColor = "hsl(0, 0%, 10%)", 
                                    borderColor = "hsl(0, 0%, 18%)", stripedColor = "hsl(0, 0%, 13%)", 
                                    headerStyle = list(`&:hover[aria-sort]` = list(backgroundColor = "hsl(0, 0%, 14%)")), 
                                    tableBodyStyle = list(color = "hsl(0, 0%, 75%)"), rowHighlightStyle = list(color = "hsl(0, 0%, 90%)", 
                                                                                                               backgroundColor = "hsl(0, 0%, 14%)"), selectStyle = list(backgroundColor = "hsl(0, 0%, 20%)"), 
                                    inputStyle = list(backgroundColor = "hsl(0, 0%, 10%)", borderColor = "hsl(0, 0%, 21%)", 
                                                      `&:hover, &:focus` = list(borderColor = "hsl(0, 0%, 30%)")), 
                                    pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"), 
                                    pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)"))
          
              output$contribution_plot <- renderPlotly({
                contribution_plot
              })
              
              
              output$contribution_plot_30days <- renderPlotly({
                contribution_plot_30days
              })
              
              
              output$apple_table <- renderReactable({
                                              reactable(apple_trending_apps1, 
                                                  style = list(fontFamily = "Arial, sans-serif", fontSize = "14px"),
                                                  resizable = TRUE, showPageSizeOptions = TRUE, 
                                                  onClick = "expand", highlight = TRUE, 
                                                  columns = 
                                                    list(
                                                      App = colDef(
                                                        minWidth = 200,
                                                        cell = function(value){
                                                                     image <- img(class = "appletrendingapplogo",
                                                                                   alt= "",
                                                                                   src = sprintf("images/%s.png", value))
                                                                    # image_src <- knitr::image_uri(sprintf("images/%s.png", value))
                                                                    # image <- img(src=image_src, height = "24p", alt = "alt") 
                                                                    tagList(
                                                                       div(style = list(display = "inline-block", width = "24px"), image),
                                                                       value
                                                                     )
                                                                   } )), 
                                                  # details = function(index) {
                                                  #       if (index == 3) {
                                                  #         tabsetPanel(
                                                  #         )
                                                  #       } else if (index == 5) {
                                                  #         paste("Details for row:", index)
                                                  #       }
                                                  #     }, 
                                                  theme = spotify_theme())
              })
              
              output$google_trending_apps <- renderReactable({
                reactable(google_trending_apps1, 
                          style = list(fontFamily = "Arial, sans-serif", fontSize = "14px"),
                          resizable = TRUE, showPageSizeOptions = TRUE, 
                          onClick = "expand", highlight = TRUE, 
                          columns = 
                            list(
                              App = colDef(
                                minWidth = 200,
                                cell = function(value){
                                  image <- img(class = "googletrendingapplogo",
                                               alt= "",
                                               src = sprintf("images/%s.png", value))
                                  # image_src <- knitr::image_uri(sprintf("images/%s.png", value))
                                  # image <- img(src=image_src, height = "24p", alt = "alt") 
                                  tagList(
                                    div(style = list(display = "inline-block", width = "24px"), image),
                                    value
                                  )
                                } )), 
                          # details = function(index) {
                          #       if (index == 3) {
                          #         tabsetPanel(
                          #         )
                          #       } else if (index == 5) {
                          #         paste("Details for row:", index)
                          #       }
                          #     }, 
                          theme = spotify_theme())
              })
          
              # reactive_brand_selection <- reactive({
              #   #cleaned_brand <- str_remove(input$selected_word, ":.*")
              #   cleaned_brand <- input$selected_word
              #   cleaned_brand
              #                 })
              # 
         #      reactive_tweet <- reactive({
         # #       output$tweet_output <- FALSE
         #        if(reactive_brand_selection() == "NRL:437"){
         #          twitterwidget("1252785723830226944")
         #        }
         #        else if(reactive_brand_selection() == "NRL:285"){
         #          twitterwidget("1252785725549842432")
         #        }
         #    
         #      })
              
              spotify_theme <- function() {
                  
                  text_color <- "hsl(0, 0%, 95%)"
                  text_color_light <- "hsl(0, 0%, 70%)"
                  text_color_lighter <- "hsl(0, 0%, 55%)"
                  bg_color <- "hsl(0, 0%, 10%)"
                  reactableTheme(
                    color = text_color,
                    backgroundColor = bg_color,
                    borderColor = "hsl(0, 0%, 16%)",
                    borderWidth = "1px",
                    highlightColor = "rgba(255, 255, 255, 0.1)",
                    cellPadding = "10px 8px",
                    style = list(
                      fontFamily = "Work Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
                      fontSize = "14px",
                      "a" = list(
                        color = text_color,
                        "&:hover, &:focus" = list(
                          textDecoration = "none",
                          borderBottom = "1px solid currentColor"
                        )
                      ),
                      ".number" = list(
                        color = text_color_light,
                        fontFamily = "Source Code Pro, Consolas, Monaco, monospace"
                      ),
                      ".tag" = list(
                        padding = "2px 4px",
                        color = "hsl(0, 0%, 40%)",
                        fontSize = "12px",
                        border = "1px solid hsl(0, 0%, 24%)",
                        borderRadius = "2px"
                      )
                    ),
                    headerStyle = list(
                      color = text_color_light,
                      fontWeight = 400,
                      fontSize = "12px",
                      letterSpacing = "1px",
                      textTransform = "uppercase",
                      "&:hover, &:focus" = list(color = text_color)
                    ),
                    rowHighlightStyle = list(
                      ".tag" = list(color = text_color, borderColor = text_color_lighter)
                    ),
                    # Full-width search bar with search icon
                    searchInputStyle = list(
                      paddingLeft = "30px",
                      paddingTop = "8px",
                      paddingBottom = "8px",
                      width = "100%",
                      border = "none",
                      backgroundColor = bg_color,
                      backgroundSize = "16px",
                      backgroundPosition = "left 8px center",
                      backgroundRepeat = "no-repeat",
                      "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
                      "::placeholder" = list(color = text_color_lighter),
                      "&:hover::placeholder, &:focus::placeholder" = list(color = text_color)
                    ),
                    paginationStyle = list(color = text_color_light),
                    pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
                    pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)")
                  )
                }
              
### DESTINATIONS DATA START --------------------------------------------------------
                            
              # output$destinations_test_table <- renderReactable({
              #   destinations_test_data
              # })
              
### DESTINATIONS DATA END   --------------------------------------------------------
              
              # observeEvent(input$selected_word, {
              #   
              #   if(reactive_brand_selection() == "NRL:437"){
              #     reactive_tweet <- twitterwidget("1252785723830226944")
              #   }
              #   else if(reactive_brand_selection() == "NRL:285"){
              #     reactive_tweet <- twitterwidget("1252785725549842432")
              #   }
              # })
              
              
              # renderKeywordsBankingSectorTable <- function(data){
              #   tracks_table <- function(data) {
              #     reactable(
              #       data,
              #       searchable = TRUE,
              #       highlight = TRUE,
              #       wrap = FALSE,
              #       paginationType = "simple",
              #       minRows = 10,
              #       columns = list(
              #         "Search Terms" = colDef(
              #           minWidth = 300
              #           ),
              #         "Traffic Share" = colDef(
              #           cell = function(value) {
              #             value2 <- removeWords(value, c("anz.com.au", "nab.com.au", "commbank.com.au", "westpac.com.au", "stgeorge.com.au"))
              #             value3 <- as.numeric(value2)
              #             width <- paste0(value3 / max(kw_branded_top25$`_TrafficShare`) * 100, "%")
              #             label <- format(value3, nsmall = 5)
              #             bar_chart(label, value = value, width = width)
              #           }
              #         ),
              #         "_TrafficShare" = colDef(show = FALSE)
              #       ),
              #       language = reactableLang(
              #         searchPlaceholder = "Filter search terms",
              #         noData = "No tracks found",
              #         pageInfo = "{rowStart}\u2013{rowEnd} of {rows} terms",
              #         pagePrevious = "\u276e",
              #         pageNext = "\u276f",
              #       ),
              #       theme = spotify_theme()
              #     )
              #   }
              #   
              #   # Icon to indicate trend: unchanged, up, down, or new
              #   trend_indicator <- function(value = c("unchanged", "up", "down", "new")) {
              #     value <- match.arg(value)
              #     label <- switch(value,
              #                     unchanged = "Unchanged", up = "Trending up",
              #                     down = "Trending down", new = "New")
              #     # Add img role and tooltip/label for accessibility
              #     args <- list(role = "img", title = label)
              #     if (value == "unchanged") {
              #       args <- c(args, list("–", style = "color: #666; font-weight: 700"))
              #     } else if (value == "up") {
              #       args <- c(args, list(shiny::icon("caret-up"), style = "color: #1ed760"))
              #     } else if (value == "down") {
              #       args <- c(args, list(shiny::icon("caret-down"), style = "color: #cd1a2b"))
              #     } else {
              #       args <- c(args, list(shiny::icon("circle"), style = "color: #2e77d0; font-size: 10px"))
              #     }
              #     do.call(span, args)
              #   }
              #   spotify_theme <- function() {
              #   
              #     text_color <- "hsl(0, 0%, 95%)"
              #     text_color_light <- "hsl(0, 0%, 70%)"
              #     text_color_lighter <- "hsl(0, 0%, 55%)"
              #     bg_color <- "hsl(0, 0%, 10%)"
              #     reactableTheme(
              #       color = text_color,
              #       backgroundColor = bg_color,
              #       borderColor = "hsl(0, 0%, 16%)",
              #       borderWidth = "1px",
              #       highlightColor = "rgba(255, 255, 255, 0.1)",
              #       cellPadding = "10px 8px",
              #       style = list(
              #         fontFamily = "Work Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
              #         fontSize = "14px",
              #         "a" = list(
              #           color = text_color,
              #           "&:hover, &:focus" = list(
              #             textDecoration = "none",
              #             borderBottom = "1px solid currentColor"
              #           )
              #         ),
              #         ".number" = list(
              #           color = text_color_light,
              #           fontFamily = "Source Code Pro, Consolas, Monaco, monospace"
              #         ),
              #         ".tag" = list(
              #           padding = "2px 4px",
              #           color = "hsl(0, 0%, 40%)",
              #           fontSize = "12px",
              #           border = "1px solid hsl(0, 0%, 24%)",
              #           borderRadius = "2px"
              #         )
              #       ),
              #       headerStyle = list(
              #         color = text_color_light,
              #         fontWeight = 400,
              #         fontSize = "12px",
              #         letterSpacing = "1px",
              #         textTransform = "uppercase",
              #         "&:hover, &:focus" = list(color = text_color)
              #       ),
              #       rowHighlightStyle = list(
              #         ".tag" = list(color = text_color, borderColor = text_color_lighter)
              #       ),
              #       # Full-width search bar with search icon
              #       searchInputStyle = list(
              #         paddingLeft = "30px",
              #         paddingTop = "8px",
              #         paddingBottom = "8px",
              #         width = "100%",
              #         border = "none",
              #         backgroundColor = bg_color,
              #         backgroundSize = "16px",
              #         backgroundPosition = "left 8px center",
              #         backgroundRepeat = "no-repeat",
              #         "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
              #         "::placeholder" = list(color = text_color_lighter),
              #         "&:hover::placeholder, &:focus::placeholder" = list(color = text_color)
              #       ),
              #       paginationStyle = list(color = text_color_light),
              #       pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
              #       pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)")
              #     )
              #   }
              #    tracks_table(data)
              # }
              # 
              # branded_table <- renderKeywordsBankingSectorTable(kw_branded_top25)
              # 
              # output$better_keywords_branded <- renderReactable({
              #   branded_table
              # })
                
              
               
               nrl_neg_tweet <- twitterwidget("1254757356107268102", width = "100%", height = "10px")
               
               coles_pos_tweet <- twitterwidget("1255274677666226178") 
               
               output$coles_positive_tweet <- renderTwitterwidget({
                 coles_pos_tweet
               })
               
                output$nrl_negative_tweet <- renderTwitterwidget({
                  nrl_neg_tweet
                })
                
              output$tweet_output = renderTwitterwidget({
                reactive_tweet()
                })
          
              
              output$print  = renderPrint(
                reactive_brand_selection()
              )
         }
    
shinyApp(ui = ui, server = server)
            

         
 