
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
      
      if(str_detect(value, "anz.com.au"))               
        bar <- div(style = list(background = "#004164", width = width, height = height))
      
      else if(str_detect(value, "commbank.com.au"))
        bar <- div(style = list(background = "#f2c40e", width = width, height = height))
      
      else if(str_detect(value, "westpac.com.au"))
        bar <- div(style = list(background = "#db002c", width = width, height = height))
      
      else if(str_detect(value, "nab.com.au"))
        bar <- div(style = list(background = "#3fc1c9", width = width, height = height))
      
      else if(str_detect(value, "stgeorge.com.au"))
        bar <- div(style = list(background = "#78be20", width = width, height = height))
      
      else 
        bar <- div(style = list(background = "#FFFFF", width = width, height = height))
      
      chart <- div(style = list(flexGrow = 1
                                , marginLeft = "8px"
                                , background = background), bar)
      if(str_detect(value, "100")){
      div(style = list(display = "flex", alignItems = "center"), paste(round(as.numeric(label)), "%"), chart)
      } else{
        div(style = list(display = "flex", alignItems = "center"), str_pad(paste(round(as.numeric(label)), "%"), width = 5, side = "left", pad = "0"), chart)
      }
    }
    
    renderKeywordsBankingSectorTableUnbranded <- function(data){
      tracks_table <- function(data) {
        reactable(
          data,
          searchable = TRUE,
          highlight = TRUE,
          wrap = FALSE,
          paginationType = "simple",
          minRows = 10,
          columns = list(
            "Search Terms" = colDef(
              minWidth = 300
              ),
            "Traffic Share" = colDef(
              cell = function(value) {
                value2 <- removeWords(value, c("anz.com.au", "nab.com.au", "commbank.com.au", "westpac.com.au", "stgeorge.com.au"))
                value3 <- as.numeric(value2)
                width <- paste0(value3 / max(kw_unbranded_top25$`_TrafficShare`) * 100, "%")
                label <- format(value3, nsmall = 5)
                bar_chart(label, value = value, width = width)
              }
            ),
            "_TrafficShare" = colDef(show = FALSE)
          ),
          language = reactableLang(
            searchPlaceholder = "Filter search terms",
            noData = "No tracks found",
            pageInfo = "{rowStart}\u2013{rowEnd} of {rows} terms",
            pagePrevious = "\u276e",
            pageNext = "\u276f"
          ),
          theme = spotify_theme()
        )
      }
    
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
          tracks_table(data)
    }

    
# DATA---------------------------------------------------------------------------------------------------------------------
    #whatisthis <- read_rds("uris.rds")
    sevendaydonut <- read_rds("sevendaydonut2.rds")
    #volumeovertime <- read_csv("coronaconvo2.csv")
    #coronasent <- read_csv("coronasent.csv")
    hashtags_7days <- read_rds("hashtags_plot7day.rds")
   # coronaverbatims <- read_csv("coronaverbatims_l7d_wed8thmar.csv")
   # afinn <- readRDS("afinn.rds")
    apple_trending_apps1 <- read_csv("app_store_apps.csv")
    google_trending_apps1 <- read_csv("play_store_apps.csv")
    #uris <- readRDS("uris.rds")
    #urisclick <- readRDS("urisclick.rds")
    destinations_test_data <- readRDS("destinations_test.rds")
    kw_branded_top25 <- readRDS("kw_branded_top25.rds")
    kw_unbranded_top25 <- readRDS("kw_unbranded_top25.rds")
    alan_kohler_ms <- readRDS("alankohler.rds")
  
    #coronavirus <- readRDS("coronavirus2.rds")
    corona7daycases <- readRDS("corona7daycases.rds")
    sw_keywords_plot <- readRDS("sw_keywords_plot.rds")
    volume_chart <- readRDS("volumechart.rds")
    sentiment_timeline_plot  <- read_rds("sentiment_timeline_plot.rds")
    sw_mobile_plot <- readRDS("sw_mobile_plot.rds")
    sw_mobile_phrases_donut <- readRDS("mobile_phrases_donut.rds")
    sw_desktop_phrases_donut <- readRDS("desktop_phrases_donut.rds")
    rona_cloud <- readRDS("ronacloud.rds")
    
    sov_branded <- readRDS("branded_overall_sov.rds")
    sov_unbranded <- readRDS("unbranded_overall_sov.rds")
    
    corona_7day <- read_rds("corona7day.rds")
    
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
                                    tags$head(includeHTML("google-analytics.html"))
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
                                          Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets Dec 29, 2019 - May 24th, 2020. 
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
                                tags$li("Last week’s easing of the restrictions for the first time since lockdown and anti-lockdown 
                                        protests in Victoria whilst generating some interest have both failed to revive the COVID 
                                        social debate to its former peak, with conversations continuing to decline week on week. 
                                        A sign that the COVID “fever pitch” has truly abated amongst Australians, with most remaining 
                                        cautious about returning to a semi-normal life and increasingly weary of the ongoing crisis and its 
                                        media coverage.")
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
                                h5(tags$u("May 3rd - May 24th")),
                                      tags$li("With restrictions easing, parents debate whether schools should re-open."),
                                tags$li("Violence breaks out at anti-lockdown protests on Spring St in Melbourne."),
                                tags$li("Restrictions to ease again across the country on June 1, NT declared effectively virus free.")
                                       ))), 
                                  introBox(data.step = 2, 
                                          data.intro = "Click the Analysis button located on the bottom-left of every chart to see insights.", 
                                          bs_button(button_size = "small", "Analysis", button_type = "default") %>%
                                   bs_attach_collapse("volume_collapse"))),
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
                                  data.intro = "Here we analyse the <b>sentiment</b> of conversation over 
                                  time among Twitter, comments, blogs and forums."),
                                  bs_collapse(id = "sentiment_collapse",
                                              content = tags$div(class = "well",
                                tags$em(  
                                  tags$p("This chart displays the percentage of mentions of COVID-19 from Australians 
                                          only that are positive, negative and neutral. 
                                         Each date on the graph represents a 7 day period, beginning at the 
                                         labelled date. For example, the first point labelled 'Dec 29, 2019' represents
                                         the average weekly sentiment between the 29th of December and the 4th of January inclusive. 
                                         Data: Meltwater Explore; Sources: Blogs, Forums, Comments 
                                          and Tweets Dec 29, 2019 - May 24th, 2020. Note: Neutral sentiment refers to 
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
                                                             tags$em( tags$p("Sentiment in the 7 day period between the 17th and the 24th of May. 
                                                                      Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets. 
                                                                      Note: Neutral sentiment refers to mentions in which either 
                                                                      negative or positive keywords could not be identified.")), 
                                                             br(),
                                                            "While sentiment marginally improved following the easing of lockdown restrictions, 
                                                            in the past week this has taken a turn with a 2 point drop in positive sentiment 
                                                            (the lowest since January). ", 
                                                            tags$b(
                                                              "The worst of the health crisis has passed, yet the 
                                                            novelty of eased restrictions has begun to wane as the public re-adjusts to a 
                                                            new normal and comes to terms with both the devastating economic impact of 
                                                            lockdown and the possibility of a second wave of infections.")   
                                                             ,
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
                                                                                  tags$em(  tags$p("Trending hashtags between the 17th and the 24th of May.
                                                                                           Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets.")),
       
                                                                                    column(width = 12,
                                                                                           tags$li("This week international politics continued to dominate the 
                                                                                                   Australian Covid conversation. After weeks of feuding with ",
                                                                                                   tags$b("#Australia, #China "), "finally agreed to a virus inquiry at the ", tags$b("#WHO "), "Assembly."), br(),
                                                                                           tags$li(tags$b("#Trump "), "was meanwhile caught playing a leisurely game of golf as the 
                                                                                                   US death toll neared 100,000. The President also revealed he was 
                                                                                                   taking the unproven treatment ", tags$b("#hydroxychloroquine "), "(used for malaria) 
                                                                                                   to prevent coronavirus, despite concerns from the FDA."), br(),
                                                                                           tags$li("As the anti ", tags$b("#lockdown "), "movement grows, a violent outbreak at Melbourne’s ", 
                                                                                                   tags$b("#SpringSt "), "protest garnered attention with Premier Daniel Andrews receiving 
                                                                                                   criticism for his ‘dictator’ like measures to stop the spread."), br()
                                                                                           ,
                                                                                     br(), br()
                                                                                      ))),
                                                     bs_button("Analysis", button_type = "default") %>%
                                                         bs_attach_collapse("weekly_hashtags_collapse")
                                                     )
                                                    )
                                          ),
                                          br(), br(),
                                 column(width = 6,  h5("Text sentiment score - weekly snapshot"), 
                                         wellPanel(introBox(data.step = 6, 
                                                             data.intro = paste("This chart analyses the top 25 words contributing to positive or negative sentiment from Twitter, comments, 
                                                                                blogs and forums in the last week using the ", 
                                                                                tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"), " sentiment lexicon."),
                                                             plotlyOutput("contribution_plot")
                                                             
                                                     )
                                                   )
                                        ),
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
                                                                                                  " sentiment analysis lexicon. 7 day period between the 17th and the 24th of May. 
                                                                     Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets."))),                                                        
                                                                column(width = 6,
                                                                                   tags$em(tags$p("Top 25 contributing words to sentiment as determined by the", 
                                                                                                  tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"),
                                                                                        " sentiment analysis lexicon. 4 week period between the 27th of April and the 24th of May. 
                                                                     Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets."))), br(), 
                                                            
                                                                tags$li("The largest contributors to negative sentiment continue to be focused on virus casualties and spread 
                                                                        (e.g. lost, sick, risk, die, death, dead, died). ", tags$b("There is visibly still a high level of fear within 
                                                                        the community, with many in fear of a second wave of infections hitting as complacency rises 
                                                                        amidst easing lockdown measures.")), br(),
                                                                tags$li("While only accounting for a small portion of mentions, terms that generated positive sentiment continue to 
                                                                        reflect the ‘good news stories’ emerging from the ongoing crisis (e.g. help, care, support, thank, thanks, 
                                                                        love), ", tags$b("showing continuous consumer appetite for uplifting stories in these challenging times.")), br(),
                                                                tags$li(tags$b("Fake "), "has newly emerged this week, as ", tags$b("conspiracy "), "theories about the pandemic 
                                                                        continue to garner attention online. These range from the pandemic being a 
                                                                        complete ‘hoax’ and the belief that 5G is spreading coronavirus (the view of 1 in 8 Australians)."),
                                                                
                                                                )),
                      bs_button("Analysis", button_type = "default") %>%
                        bs_attach_collapse("contribution_collapse"))), br(), br(),
                      column(width = 12, 
                             h5("Most talked about brands in connection with Coronavirus (Australia VoC only) - weekly snapshot", align = "center"), 
                          wellPanel(introBox(data.step = 8, 
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
                                            Period May 11th to May 17th. Data: Meltwater. Brand Detection: OpenNLP."))), br(),
                                          column(width = 6,
                                                 h5("While debate surrounding the AFL continues, a number of heavyweight global brands 
                                                    dominated the Australian COVID-19 conversation last week, highlighting the 
                                                    consumer expectation of brands to “do the right thing” by the community 
                                                    in helping curve the spread in any way they can:"),
                                                 tags$li(tags$b("AFL "), "continues to generate negative attention as they announced the season will 
                                                         commence in 2020, despite the organisers deciding against compulsory flu vaccinations. 
                                                         Consumers also are frustrated that they are required to continue paying membership 
                                                         fees in the absence of games."), br(),
                                                 tags$li("Meanwhile in the US, the ", tags$b("NBA "), "is being praised for their tough approach to curbing 
                                                         the spread of the virus, announcing there will be a testing program for players should 
                                                         the season re-start."), br(),
                                        tags$li(tags$b("Nike "), "made headlines this week after it was revealed that 25 people contracted COVID-19 at their 
                                                Edinburgh conference in February, and are now being accused of an outbreak ‘cover up’. 
                                                They are also under fire for not providing staff with hand sanitizer or sick leave despite 
                                                a $6bn profit last year."), br(),
                                        tags$li(tags$b("Disney "), "has come under scrutiny for their blunt new policy, revealed days before a 
                                                number of resorts re-open across the US - “By visiting Walt Disney World Resort, 
                                                you voluntarily assume all risks related to exposure to COVID-19”."), br(),
                                        tags$li(tags$b("McDonald’s "), "was slammed online this week after reports that hundreds of 
                                                staff from a Victorian store were placed on unpaid leave for 2 weeks, 
                                                after coming in contact with an infected delivery driver at work."), br()
                                            ,
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
                                        ),
                                        column(width = 6, 
                                            wellPanel(style = "overflow-y:scroll; max-height: 700px",
                                                       h5("Scroll down to see more.", align = "center"),
                                        h6("NRL and AFL"),
                                           HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">What I don&#39;t get about these pathetic anti-vax <a href="https://twitter.com/NRL?ref_src=twsrc%5Etfw">@NRL</a> players is they&#39;re all covered in tattoos but won&#39;t get a tiny needle.</p>&mdash; IrmaGherd (Retired Nurse Thug) (@maureenchuck1) <a href="https://twitter.com/maureenchuck1/status/1259322306339287041?ref_src=twsrc%5Etfw">May 10, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                           HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Pardon my lingo, but if the AFL and NRL can start playing again, why the fk can I not get my brows done?</p>&mdash; GemmaTognini (@GemmaTognini) <a href="https://twitter.com/GemmaTognini/status/1260820975215448064?ref_src=twsrc%5Etfw">May 14, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                           HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">NRL player worried flu shot might brain his damage <a href="https://t.co/xcJbR81Tg3">https://t.co/xcJbR81Tg3</a></p>&mdash; 𝚃𝚑𝚎 𝙲𝚑𝚊𝚜𝚎𝚛 (@chaser) <a href="https://twitter.com/chaser/status/1260457614544179211?ref_src=twsrc%5Etfw">May 13, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        br(),
                                        h6("Wuhan Institute of Virology"),
                                            HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">The Wuhan Institute of Virology takes 10 days to notify the world of the sequence of coronavirus - and American taxpayers are supposed to keep funding them? After the State Dept said they weren’t being safe?<br><br>Looks like <a href="https://twitter.com/CBSNews?ref_src=twsrc%5Etfw">@CBSNews</a> is going all China First. <a href="https://t.co/TYqRBg23as">https://t.co/TYqRBg23as</a></p>&mdash; Matt Gaetz (@mattgaetz) <a href="https://twitter.com/mattgaetz/status/1259685433056940036?ref_src=twsrc%5Etfw">May 11, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        br(),
                                        h6("Virgin Airlines Australia"),
                                            HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Queensland Labor expresses interest in purchasing Virgin Airlines, but <a href="https://twitter.com/abcnews?ref_src=twsrc%5Etfw">@ABCNews</a> provides commentary from Dutton saying it&#39;s outrageous and reckless. Why the false balance with comments from a madman? Business people and the community are saying it&#39;s a good move. <a href="https://twitter.com/hashtag/AUSPOL?src=hash&amp;ref_src=twsrc%5Etfw">#AUSPOL</a></p>&mdash; Eddy Jokovich (@EddyJokovich) <a href="https://twitter.com/EddyJokovich/status/1260556080179892224?ref_src=twsrc%5Etfw">May 13, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        br(),
                                        h6('Coles, Woolworths and IGA'),
                                            HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Some IGA stores r getting on board with this major shift ppl r wanting. Isn&#39;t it about time u did the right thing for a change <a href="https://twitter.com/Coles?ref_src=twsrc%5Etfw">@Coles</a> <a href="https://twitter.com/woolworths?ref_src=twsrc%5Etfw">@woolworths</a> <a href="https://t.co/Pko1nr0CJ2">pic.twitter.com/Pko1nr0CJ2</a></p>&mdash; Alison (@Pussaluss04) <a href="https://twitter.com/Pussaluss04/status/1261116725073833985?ref_src=twsrc%5Etfw">May 15, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        br(),
                                        h6('Pfizer'),
                                            HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Incorrect.<br>Yes, pharmaceutical companies do make huge profits.<br>They do NOT make them from vaccines/antibiotics - these are used only very occasionally. <br>Big Pharma makes its profits from drugs that you take every day, for years (eg, ulcers, hypertension, Type II Diabetes, etc) <a href="https://t.co/5lugfDQYXD">https://t.co/5lugfDQYXD</a></p>&mdash; Dr Karl (@DoctorKarl) <a href="https://twitter.com/DoctorKarl/status/1260338915577233408?ref_src=twsrc%5Etfw">May 12, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
                                         )
                                        )
                                        )
                                        ),
                         bs_button("Analysis", button_type = "default") %>%
                          bs_attach_collapse("business_cloud"))), br(), br(),
                     
                     #  h5("Hello! This is only visible on the old server and not on the deployed link."),
                     #  h6("What do we think of this chart? I think it does a great job of explaining the impact and gradual return to normal from a consumer pov."),
                     #  column(width = 6, h6("Pros: "), tags$li("Consumer touchpoints"), tags$li("Progressive & optimistic narrative")),
                     #  column(width = 6, h6("Cons: "), 
                     #         tags$li("The data is from a news source, not social (see analysis container for source)"), 
                     #          tags$li("The original datasource is not accessible, we have access to the news source 
                     #                  reporting on the original Morgan Stanley data ($$$)"),
                     #         tags$li("The 'trough' period is not clearly defined, it is explained as just the 
                     #                 lowest point for each sector during lockdown measures."), br()),
                     #  column(width = 12, wellPanel(plotlyOutput("kohler_report", height = "500px"),
                     #                               bs_collapse("kohler_collapse", 
                     #                                           content = tags$div(class = "well", 
                     #                                                     tags$p("Change in Australian consumer behaviour from peak lockdown to May 18th, 2020. 
                     #                                                            Source: ", tags$a(href ="https://www.abc.net.au/news/2020-05-20/wednesday-finance-with-alan-kohler/12269202?nw=0",  
                     #                                                                              "Alan Kohler, ABC News Finance Report - May 20th, 2020")))),
                     #                               bs_button("Analysis", button_type = "default") %>%
                     #                                 bs_attach_collapse("kohler_collapse")
                     # ))
                     ))), 
                     ),
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
                                      data.intro = "This plot shows us the most popular search term variations including coronavirus from all search engine traffic.", 
                                      plotlyOutput("sw_keywords_plot")),
                                                bs_collapse(id = "sw_keywords_collapse", 
                                            content = tags$div(class = "well", 
                                                          tags$em(p("Top organic search keywords including ‘coronavirus’ ranked by volume 
                                                          of searches and destination (mobile and desktop traffic combined) for the period Feb 1st to Apr 30th; Data: SimilarWeb.")),
                                                          br(),
                                                          "From Feb until now, Australians have been mostly concerned with understanding 
                                                          the virus and its symptoms, assessing how it was spread (both locally and abroad) 
                                                          and the possibility of a vaccine. ", tags$b("They remain as anxious about personal safety 
                                                          with detection of the disease front of mind as they are about its eradication."), br(), br(),
                                                          
                                                          tags$li("Australians have been turning to government websites (health.gov.au, 
                                                                  cdc.gov, who.int) as their primary source of truth on COVID-19, 
                                                                  particularly when it came to researching symptoms and policy updates."), br(),
                                                          tags$li("Local news outlets (news.com.au, couriermail.com, abc.net.au, theguardian.com, 
                                                                  watoday.com) have been the preferred go-to destinations for the latest 
                                                                  developments on the spread of the disease at home and the development of a 
                                                                  vaccine, with a particular interest in news with a geographic lens (i.e. 
                                                                  NSW, QLD, Sydney, Perth and Melbourne)."), br(),
                                                          tags$li("Well-established reputable international news outlets  (straitstimes.com, 
                                                                  aljazeera.com, telegraph.co.uk) while less popular, were the top destinations 
                                                                  for those searching ‘how did coronavirus start’ and other key overseas 
                                                                  developments (i.e. total cases and the outbreak in Italy and Singapore).")
                                                    )
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
                                                                                                          
                                                                                                          tags$li("A number of shopping apps emerged on the charts this week 
                                                                                                                  (Gumtree, Cotton On, Shop Delivery, eBay, Dominos) as ", tags$b("consumers "),
                                                                                                                  "remain concerned about personal safety and continue to rely on 
                                                                                                                  online shopping and delivery despite the reopening of brick-and-mortar shops."), 
                                                                                                          br(),
                                                                                                          tags$li("Entertainment apps, particularly in gaming and social, continue 
                                                                                                                  to be popular choices as ", tags$b("consumers look for novel ways to distract 
                                                                                                                  themselves while at home.")), 
                                                                                                          br(),  
                                                                                                          tags$li("Utility apps remain popular also as ", tags$b("many Australians still work remotely, "),  
                                                                                                                  "particularly online conferencing services and mobile providers."), br()
                                                                                                          )
                                                                                            # ,      column(width = 6,
                                                                                            #               tags$em(tags$p("Top 10 upward trending apps on the Google Play store in the last 28 days. Data: SimilarWeb. ")), br(), 
                                                                                            #        )
                                                                                            )),
                                      bs_button("Analysis", button_type = "default") %>%
                                        bs_attach_collapse("apps_collapse"))), 
                                     br(), br(),
                                     column(width = 12, h5("Search top (paid and organic) keywords for banking category - branded"), align = "center",
                                            introBox(data.step = 13, data.intro = "This chart shows the top branded keywords including Coronavirus 
                                                     driving traffic to the Banking category in the last 28 days.",
                                                     wellPanel(
                                              plotlyOutput("sov_branded_out", height = "100px"),
                                              reactableOutput("better_keywords_branded")
                                                      ))
                                            
                                            ),
                                     column(width = 12, h5("Search top (paid and organic) keywords for banking category - unbranded"), align = "center",
                                            introBox(data.step = 14, data.intro = "This chart shows the top unbranded keywords including Coronavirus 
                                                     driving traffic to the Banking category in the last 28 days.", 
                                                     wellPanel(
                                               plotlyOutput("sov_unbranded_out", height = "100px"),
                                              reactableOutput("better_keywords_unbranded")
                                            ))
                                     ),
                                     column(width = 12, wellPanel(
                                       bs_collapse(id = "banking_collapse", content = tags$div(class = "well", 
                                                                                            column(width=12,
                                                                                                   tags$em(tags$p("Top branded/unbranded keywords including Coronavirus driving traffic to the Banking category in the last 28 days (desktop traffic only),
                                                                                                                  with the category defined as the Big 4 banks and St.George. Source: SimilarWeb."
                                                                                                   )),
                                                                                                   
                                                                                                   tags$li("In the last 28 days, ", tags$b("SME customers have been looking for information on how their
                                                                                                   bank can keep their business afloat during the COVID crisis."),
                                                                                                           "The government’s SME guarantee scheme was of particular interest to NAB,
                                                                                                   Westpac and ANZ customers, as it allows lenders to provide businesses with
                                                                                                   timely access to working capital to see them through the crisis."),
                                                                                                   br(),
                                                                                                   tags$li("", tags$b("As many Australians face financial hardship and revenue loss"), ", they are turning
                                                                                                           to their banks for support in the form of mortgage holds, credit card repayment relief and business loans."), br(),
                                                                                                   tags$li("Customers also seem confused as to whether ‘banks are still open’ due to social distancing restrictions."),
                                                                                                   br(),
                                                                                                   tags$li("Westpac customers were also concerned with their credit card travel insurance, ", tags$b("as international and
                                                                                                           interstate travel bans across the country force Australians to cancel their travel plans.")), br(),
                                                                                                   tags$li("Of note also is the traffic CBA is getting from unbranded coronavirus-related searches.
                                                                                                   The bank is currently enjoying the greatest SOV for unbranded Coronavirus-related terms (vs Westpac for branded terms)
                                                                                                           over its key competitors with the lion’s share (64.5%) of these terms sending traffic to its website."), br()
                                                                                            )
                                                                                            # ,      column(width = 6,
                                                                                            #               tags$em(tags$p("Top 10 upward trending apps on the Google Play store in the last 28 days. Data: SimilarWeb. ")), br(), 
                                                                                            #        )
                                       )),
                                       bs_button("Analysis", button_type = "default") %>%
                                         bs_attach_collapse("banking_collapse"))), 
                             )
                 )
                        
                 
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


              renderKeywordsBankingSectorTable <- function(data){
                tracks_table <- function(data) {
                  reactable(
                    data,
                    searchable = TRUE,
                    highlight = TRUE,
                    wrap = FALSE,
                    paginationType = "simple",
                    minRows = 10,
                    columns = list(
                      "Search Terms" = colDef(
                        minWidth = 300
                        ),
                      "Traffic Share" = colDef(
                        cell = function(value) {
                          value2 <- removeWords(value, c("anz.com.au", "nab.com.au", "commbank.com.au", "westpac.com.au", "stgeorge.com.au"))
                          value3 <- as.numeric(value2)
                          width <- paste0(value3 / max(kw_branded_top25$`_TrafficShare`) * 100, "%")
                          label <- format(value3, nsmall = 5)
                          bar_chart(label, value = value, width = width)
                        }
                      ),
                      "_TrafficShare" = colDef(show = FALSE)
                    ),
                    language = reactableLang(
                      searchPlaceholder = "Filter search terms",
                      noData = "No tracks found",
                      pageInfo = "{rowStart}\u2013{rowEnd} of {rows} terms",
                      pagePrevious = "\u276e",
                      pageNext = "\u276f"
                    ),
                    theme = spotify_theme()
                  )
                }
              
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
                
               tracks_table(data)
                
              }
            

                
              branded_table <- renderKeywordsBankingSectorTable(kw_branded_top25)

              output$better_keywords_branded <- renderReactable({
                branded_table
              })
              
              unbranded_table <- renderKeywordsBankingSectorTableUnbranded(kw_unbranded_top25)
              
              output$better_keywords_unbranded <- renderReactable({
                unbranded_table
              })
              
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
              
              output$kohler_report <- renderPlotly({
                alan_kohler_ms
              })
          
              
              output$sov_branded_out <- renderPlotly({
                sov_branded
              })
              
              output$sov_unbranded_out <- renderPlotly({
                sov_unbranded
              })
              
              
         }
    
shinyApp(ui = ui, server = server)
            

         
 