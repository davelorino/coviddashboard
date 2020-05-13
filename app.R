
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
    #whatisthis <- read_rds("uris.rds")
    sevendaydonut <- read_rds("sevendaydonut2.rds")
    #volumeovertime <- read_csv("coronaconvo2.csv")
    #coronasent <- read_csv("coronasent.csv")
    hashtags_7days <- read_rds("hashtags_plot7day.rds")
   # coronaverbatims <- read_csv("coronaverbatims_l7d_wed8thmar.csv")
   # afinn <- readRDS("afinn.rds")
    apple_trending_apps1 <- read_csv("apple_store_apps.csv")
    google_trending_apps1 <- readRDS("google_trending_table.rds")
    #uris <- readRDS("uris.rds")
    #urisclick <- readRDS("urisclick.rds")
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
                                          Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets Dec 29, 2019 - May 2nd, 2020. 
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
                                        enactment of new lockdown measures to control the rapid spread of the virus.",  tags$b("The mood of 
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
                                        to the subsequent economic fallout.")),
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
                                       h5(tags$u("Apr 12th - Present")),
                                       tags$li("Criminal investigations into the Ruby Princess."),
                                       tags$li("Uncertain financial security for many Australians."),
                                       tags$li("Australia begins to flatten the curve."), 
                                       br()
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
                                          and Tweets Dec 29, 2019 - May 2nd, 2020. Note: Neutral sentiment refers to 
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
                                sentiment has marginally fallen.", tags$b("With a large portion of the nation still in lockdown and media 
                                fatigue setting in, Australians appear mostly unaffected by the good news around the containment 
                                of the virus and are otherwise preoccupied with controversy surrounding government policy (in 
                                particular the launch of the government’s new COVIDSafe app).")), 
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
                                                             tags$em( tags$p("Sentiment in the 7 day period between the 26th of April and the 2nd of May inclusive. 
                                                                      Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets. 
                                                                      Note: Neutral sentiment refers to mentions in which either 
                                                                      negative or positive keywords could not be identified.")), 
                                                             br(),
                                     
                                                "In the past week, online conversation has largely revolved around Australia’s path back to ‘normal’,
                                                 with controversy around the launch of the COVIDSafe app driving a large portion of negative sentiment. 
                                                 Simultaneously, the relaxation of lockdown laws has been met with mixed reactions by the public - with 
                                                 some questioning whether it will prompt another jump in the infection rate.", tags$b("While the curve has started 
                                                 to flatten, it appears Australians are proceeding with caution, rather than celebration."), 
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
                                                                                  tags$em(  tags$p("Trending hashtags between the 26th of April and the 2nd of May inclusive.
                                                                                           Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets.")),
                                                                                    
                                                                                  
                                                                                  tags$h5("Trending hashtags were grouped around four main concerns:"), br(),
                                                                                    column(width = 12,
                                                                                    tags$li("The Ruby Princess cruise ship (", tags$b("#rubyprincess"), "was the #1 hashtag in mentions 
                                                                                    of coronavirus for the week). As the source of Australia’s largest outbreak, this 
                                                                                    remains a hot news topic and key area of public interest."), br(),
                                                                                    tags$li("The launch of the government’s COVIDSafe App has dominated headlines 
                                                                                    and consumer conversation, with a mixed response from the public. Privacy concerns, 
                                                                                    tech issues and regulation have been the key areas of concern related to use of this 
                                                                                    hashtag (", tags$b("#covidsafe"), ")."), br(),
                                                                                    tags$li("With a large portion of the workforce now working from home or self-isolating, 
                                                                                    lockdown measures have fuelled a significant portion of conversation online both positive 
                                                                                    and negative. While some are critical of the government’s approach, others have rallied 
                                                                                    behind it – as demonstrated by widespread use of these hashtags (", tags$b("#stayhome, #stayhome, 
                                                                                    #socialdistancing"), ")."), br(),
                                                                                    tags$li("Despite a preference for local news stories, international politics was a 
                                                                                    matter of particular interest to Australians last week with ", tags$b("#Trump "), "and ", 
                                                                                            tags$b("#China "),   
                                                                                    "trending in response to breaking news stories."), br(), br()
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
                                                                                                  " sentiment analysis lexicon. 7 day period 26th April - 2nd May inclusive. 
                                                                     Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets."))),                                                        
                                                                column(width = 6,
                                                                                   tags$em(tags$p("Top 25 contributing words to sentiment as determined by the", 
                                                                                                  tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"),
                                                                                        " sentiment analysis lexicon. 4 week period 5th April - 2nd May inclusive. 
                                                                     Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets."))), br(), 
                                                                    tags$li("The largest contributors to negative sentiment continue to be focused on virus casualties 
                                                                    and spread (e.g. dead, infected, die, death, died). 
                                                                    ", tags$b("Although the curve is flattening, 
                                                                    this demonstrates there is still a high level of fear within the community, and 
                                                                              that possibly most Australians aren’t ready to return to normal and let go 
                                                                              of social distancing just yet.")), br(),
                                              tags$li("While only accounting for a small portion of mentions, terms that generated 
                                              positive online sentiment reflect the ‘good news stories’ that are emerging from this 
                                              crisis (e.g. help, care, support, thank) ", tags$b("showing that there is a consumer appetite for 
                                              uplifting stories in these challenging times.")), br(),
                                              tags$li("In the last 7 days, there has been a higher proportion of negative words 
                                                      driving sentiment. These new emerging terms (shit, worse, important) ", tags$b("continue 
                                                      to reflect a climate of uncertainty despite an improvement in the domestic crisis situation.")))),
                      bs_button("Analysis", button_type = "default") %>%
                        bs_attach_collapse("contribution_collapse"))), br(), br(),
                      column(width = 6, h4("Brand sentiment (Australian VoC only)", align = "center"), 
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
                                            tags$em(tags$p("Top 7 brands associated with positive 
                                                           or negative consumer sentiment in the last 7 days."))))),
                        bs_button("Analysis", button_type = "default") %>%
                          bs_attach_collapse("business_cloud"), br()
                        )), br(), br(), 
                      column(width = 6, h4("Test"), 
                             wellPanel(twitterwidgetOutput("tweet_output")
                        #verbatimTextOutput("print")
                        ))
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
                                              h4("Top COVID-19 keywords and destinations (mobile and desktop)", align = "center"),
                                      wellPanel(introBox(data.step = 10, 
                                      data.intro = "This plot shows us the phrases most commonly associated with 'coronavirus'
                                                            from all search engine traffic.", plotlyOutput("sw_keywords_plot")),
                                                 bs_collapse(id = "sw_keywords_collapse", content = tags$div(class = "well", 
                                                tags$em(p("Associated phrases in searches for 'coronavirus' 
                                                   on ranked by volume of searches; Period: 
                                                 Jan 1st - Mar 31st 2020; Data: SimilarWeb.") ))),
                                                 bs_button("Analysis", button_type = "default") %>%
                                                   bs_attach_collapse("sw_keywords_collapse")), br(), br(),
                                      column(width = 6, align = "left", h4("Trending up apps - Apple app store"), wellPanel(reactableOutput("apple_table"))),
                                      column(width = 6, align = "left", h4("Trending up apps - Google Play store") ,wellPanel(reactableOutput("google_trending_apps")))
                             )))
         
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
              
              output$apple_trending_apps <- renderReactable({
                apple_trending_apps1
              })
              
              output$google_trending_apps <- renderReactable({
                google_trending_apps1
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
                                                                     tagList(
                                                                       div(style = list(display = "inline-block", width = "60px"), image),
                                                                       value
                                                                     )
                                                                   } ), 
                                                      "Store Rank" = colDef(name = "Store Rank", 
                                                                            headerStyle = list(fontWeight = 700),
                                                                            defaultSortOrder = "asc")), 
                                                  # details = function(index) {
                                                  #       if (index == 3) {
                                                  #         tabsetPanel(
                                                  #         )
                                                  #       } else if (index == 5) {
                                                  #         paste("Details for row:", index)
                                                  #       }
                                                  #     }, 
                                                  theme = theme1)
              })
          
              reactive_brand_selection <- reactive({
                #cleaned_brand <- str_remove(input$selected_word, ":.*")
                cleaned_brand <- input$selected_word
                cleaned_brand
                              })
              
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
              
              
              observeEvent(input$selected_word, {
                
                if(reactive_brand_selection() == "NRL:437"){
                  reactive_tweet <- twitterwidget("1252785723830226944")
                }
                else if(reactive_brand_selection() == "NRL:285"){
                  reactive_tweet <- twitterwidget("1252785725549842432")
                }
                
              output$tweet_output = renderTwitterwidget({
                reactive_tweet
                })
              })
              
              output$print  = renderPrint(
                reactive_brand_selection()
              )
         }
    
?str_remove
shinyApp(ui = ui, server = server)
            

         
 