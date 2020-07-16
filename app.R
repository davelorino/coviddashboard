
library(shiny)
library(rintrojs)
library(dplyr)
library(readr)
library(plotly)
library(lubridate)
library(shinythemes)
library(magrittr)
library(bsplus)
library(wordcloud2)
library(reactable)
library(htmltools)
library(stringr)
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
        div(style = list(display = "flex", alignItems = "center"), 
            str_pad(paste(round(as.numeric(label)), "%"), 
                    width = 5, 
                    side = "left",
                    pad = "0"), 
            chart)
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
          minRows = 8,
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
    
    renderKeywordsBankingSectorTableBranded <- function(data){
      tracks_table <- function(data) {
        reactable(
          data,
          searchable = TRUE,
          highlight = TRUE,
          wrap = FALSE,
          paginationType = "simple",
          minRows = 4,
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
    
    alan_kohler_ms <- readRDS("alankohler.rds")
  
    #coronavirus <- readRDS("coronavirus2.rds")
    corona7daycases <- readRDS("corona7daycases.rds")
    sw_keywords_plot <- readRDS("sw_keywords_plot.rds")
    volume_chart <- readRDS("volumechart.rds")
    sentiment_timeline_plot  <- read_rds("sentiment_timeline_plot.rds")
    sw_mobile_plot <- readRDS("sw_mobile_plot.rds")
    sw_mobile_phrases_donut <- readRDS("mobile_phrases_donut.rds")
    sw_desktop_phrases_donut <- readRDS("desktop_phrases_donut.rds")
    rona_cloud <- readRDS("ronacloud2.rds")
    # kw_branded_top25 <- readRDS("kw_branded_top25.rds")
    # kw_unbranded_top25 <- readRDS("kw_unbranded_top25.rds")
    # sov_branded <- readRDS("branded_overall_sov.rds")
    # sov_unbranded <- readRDS("unbranded_overall_sov.rds")
    kw_branded_top25 <- readRDS("kw_top25_branded_may24.rds")
    kw_unbranded_top25 <- readRDS("kw_top25_unbranded_may24.rds")
    sov_branded <- readRDS("share_of_voice_branded_may24.rds")
    sov_unbranded <- readRDS("share_of_voice_unbranded_may24.rds")
    
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
                                          Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets Dec 29, 2019 - July 11th, 2020. 
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
                                tags$li("As May concluded, the easing of restrictions did little to revive waning consumer 
                                        interest in the dual COVID-19 health and economic crises, as mirrored by ongoing 
                                        decline in conversation. ", tags$b("The social debate had by then shifted to whether 
                                        Australians were becoming too complacent despite the possibility of another wave of infections.")), br(),
                                tags$li("As we moved into June, it appeared the pandemic was overshadowed by another crisis - the Black Lives Matter movement. 
                                        The death of George Floyd in the US sparked anti-police brutality protests around the world, and locally shone a light 
                                        on the overrepresentation of Indigineous Australians in incarceration. ", tags$b("Both the pandemic and BLM movement become linked 
                                        however, with fierce debate between Australian politicians and medical professionals as to whether public protests might 
                                        trigger a second wave of infections.")), br(),
                                tags$li("Less than a month since restrictions began to ease across the country, Australia has been met with a second-wave of 
                                        infections in Victoria. A new cluster of cases in NSW has been linked to travellers from Melbourne, which has 
                                        led to the closure of borders between  the neighbouring states. This turn of events has revived the COVID-19 
                                        social conversation again on the rise, though not back to its former peak of March. Melbourne has since gone 
                                        back into lockdown, ", tags$b("reigniting fear and anxiety amongst the public that the worst of the COVID-19 crisis may 
                                        not yet be over for Australia and a wider national lockdown could be imminent."))
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
                                tags$li("Restrictions to ease again across the country on June 1, NT declared effectively virus free."),
                                br(),
                                h5(tags$u("May 24th - June 14th")),
                                tags$li("Australian medical experts make a breakthrough in their ability to detect a rise in cases."),
                                tags$li("The murder of George Flloyd by a Minneapolis police officer sparks anti-police brutality protests around the world, despite social distancing restrictions."),
                                tags$li("For the first day since the start of the pandemic, Australia records zero new locally acquired cases."),
                                br(),
                                h5(tags$u("June 15h - July 12th")),
                                tags$li("Scientists draw links between the spread of diseases like COVID-19 from animals to humans, 
                                with the destruction of ecologies and habitats belonging to species that carry diseases."),  
                                tags$li("The Trump administration draws worldwide criticism for securing almost exclusive access 
                                to a pharmaceutical treatment that may minimise the symptoms of COVID-19."),
                                tags$li("Victoria deals with a second virus outbreak due to failures in hotel quarantining 
                                and social distancing, resulting in Melbourne returning to lockdown just one month after 
                                restrictions began to ease.") 
                                
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
                                          only that are positive and negative. 
                                         Each date on the graph represents a 7 day period, beginning at the 
                                         labelled date. For example, the first point labelled 'Dec 29, 2019' represents
                                         the average weekly sentiment between the 29th of December and the 4th of January inclusive. 
                                         Data: Meltwater Explore; Sources: Blogs, Forums, Comments 
                                          and Tweets Dec 29, 2019 - July 11th, 2020.")), 
                                  
                               column(width=12, 
                               br(),
                               tags$li("On the 5th of January, negative sentiment quickly spiked in correlation 
                               with the World Health Organisation’s global announcement of a new coronavirus outbreak. At
                               this early stage however, Australian consumer interest was low and largely driven by international 
                               news coverage.", tags$b("Australians were then mostly untroubled by the events unfolding overseas as yet 
                               unaware of the implications the outbreak would soon have on their daily lives.")),
                                br(),
                                tags$li("Negative sentiment has since stayed high, not falling below 43% since late January when the outbreak officially kicked off locally. 
                                Interestingly, while we witnessed a significant spike in mentions mid-March, this was not 
                                mirrored by a dramatic shift in consumer sentiment.", tags$b("This suggests that despite varying levels of 
                                interest since the start of the COVID-19 crisis, Australians have been mostly feeling anxious and 
                                uncertain about the its outcome and the future.")), 
                                br(),
                                tags$li("In March, we observed a sustained yet temporary increase in positive COVID-19 conversations online, 
                                        following the introduction of mandatory government measures to protect essential workers and encourage 
                                        the public to socially distance. ", tags$b("This slight improvement in consumer sentiment has reflected the widespread 
                                        support and empathy for measures promoting a new ‘preventative’ national mindset.")),
                                br(),
                                tags$li("Despite Australia’s success in flattening the curve, overall positive sentiment remained low in May 
                                        with no significant upside to date. ", tags$b("On the whole, Australians appeared mostly unaffected by the 
                                        good news around the containment of the virus or easing of the restrictions. "), "Instead, they became 
                                        more preoccupied with the escalation of the Black Lives Matter movement in June and the implications 
                                        this might have on Australia’s ability to contain viral spread in the community."),
                               br(),
                               tags$li("In early June, ", tags$b("it appeared that for the first time the public could see the light at the end of 
                                       the COVID-19 tunnel "), "- with Australia recording its first day with no locally acquired cases 
                                       since the initial outbreak on the 8th of June. This was matched by a temporary increase 
                                       in positive conversation online (the highest since January), however this quickly turned 
                                       when Victoria was hit with a second-wave of infections later in the month. ", tags$b("With Melbourne 
                                       returning to lockdown on July 7th, consumers appear to be losing hope again with many 
                                       anticipating other states will follow suit.")),
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
                                                             tags$em( tags$p("Sentiment in the 7 day period between the 5th and the 11th of July. 
                                                                      Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets.")), 
                                                             br(),
                                                             
                                                             tags$b("The nation appears to be moving at two-speeds in relation to the COVID-19 health crisis. "),
                                                             
                                                             "While Victoria grapples with a second-wave of infections and Melbournians begrudgingly 
                                                             return to a state of lockdown, the rest of Australia is continuing on their path back 
                                                             to normal life, albeit with a new sense of foreboding. As such, sentiment has taken a dip 
                                                             (by 5.15 points) in the past week as ", tags$b("the public is faced with the harsh reality that any 
                                                             success in flattening the curve is only temporary if met with complacency in practising social 
                                                             distancing measures."), 
                                                             
                                                  br())),
                                                            bs_button("Analysis", button_type = "default") %>%
                                                                bs_attach_collapse("weekly_sentiment_collapse")))),
                                   column(width = 6, h5("Trending hashtags - weekly snapshot"), 
                                          wellPanel(introBox(data.step = 5, 
                                                             
                                                             data.intro = "This chart analyses the top trending hashtags from Twitter in the last week.
                                                             ", 
                                                             plotlyOutput("hashtags_7days"),
                                                     bs_collapse(id = "weekly_hashtags_collapse",
                                                                 content = tags$div(class = "well",
                                                                                  tags$em(  tags$p("Trending hashtags between the 5th and the 11th of July.
                                                                                           Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets.")),
       
                                                                                    column(width = 12,
                                                                                           tags$li("The dominant theme in this week’s trending hashtags is the viral 
                                                                                                   outbreak in ", tags$b("#Victoria (#covid19vic, #covidvic), "), "with ", tags$b("#melbourne "),
                                                                                                   "returning to ", tags$b("#lockdown (#lockdownmelbourne, #melbournelockdown) "), 
                                                                                                   "just one month after restrictions were eased."), br(),
                                                                                           tags$li("Victorian Premier Daniel Andrews ", tags$b("(#istandwithdan, #springst, #vicpol) "), "is in 
                                                                                                   the media spotlight again after nine public housing towers were locked down 
                                                                                                   in Melbourne’s inner north-west, causing a divide amongst his supporters. 
                                                                                                   This follows his controversial hotel debacle, where a number of security 
                                                                                                   guards were accused of sleeping with quarantined guests."), br(),
                                                                                           tags$li("The investigation into the ", tags$b("#rubyprincess "), "outbreak continues to garner public interest, 
                                                                                                   as the inquiry uncovers NSW Health used an out-of-date arrival form template to dock 
                                                                                                   the ship. Rather than asking passengers if they had been overseas at all in the last 
                                                                                                   14 days, it only specified travel to “mainland China, Italy, Iran, South Korea, 
                                                                                                   Cambodia” which resulted in only 10 swab-tests being conducted amongst the ship’s 
                                                                                                   thousands of passengers."), br(),
                                                                                           tags$li(tags$b("#Trump "), "continues to dominate the online conversation as he moves to secure 
                                                                                                   almost all stocks of potential COVID-19 medication ‘remdesivir’ until 
                                                                                                   October, causing concern that Australians will miss out - sparking a war 
                                                                                                   of medicinal access between nations."), br(),
                                                                                          br()
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
                                                                                                  " sentiment analysis lexicon. 7 day period between the 5th and the 11th of July. 
                                                                     Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets."))),                                                        
                                                                column(width = 6,
                                                                                   tags$em(tags$p("Top 25 contributing words to sentiment as determined by the", 
                                                                                                  tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"),
                                                                                        " sentiment analysis lexicon. 4 week period between the 21st of June and the 11th of July.
                                                                     Data: Meltwater Explore; Sources: Blogs, Forums, Comments and Tweets."))), br(), 
                                                            
                                                                tags$li("Whilst the Black Lives Matter movement temporarily dominated the term 
                                                                        associations in early June, those linked to virus casualties have 
                                                                        re-appeared as the top contributors to negative sentiment in the past 
                                                                        month (e.g. worst, lost, death, died) ", tags$b("as public attention shifts back 
                                                                        to the outbreak in Victoria and fear of a national second-wave heightens.")), br(),
                                                                tags$li("While only accounting for a small portion of mentions still, terms 
                                                                        that generated positive sentiment continue to reflect the ‘good news stories’ 
                                                                        emerging from the ongoing crisis as Australians come together and help out 
                                                                        each other (e.g. support, help, care) and successfully overcome the outbreak 
                                                                        in parts of Australia (e.g. positive, good, better, greater, strong, free), 
                                                                        all ", tags$b("showing continuous consumer appetite for uplifting stories in these challenging times.")), br()
                                                                
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
                                            tags$em(tags$p("The most most talked about brands in connection with Coronavirus 
                                            in the last 7 days, ranked by VoC sentiment and volume of mentions (Australia only). 
                                            Brand mentions may be split out by both negative and positive sentiments where applicable. 
                                            Period 5th to the 11th of June. Data: Meltwater. Brand Detection: OpenNLP."))), br(),
                                          column(width = 6,
                                                 h5(tags$em("The Victorian State Government was in the spotlight this week over their 
                                                            response to the second outbreak and their measures in support of 
                                                            Small Businesses, together with Jetstar over its failure to 
                                                            adhere to the Public Health Order at Sydney airport. Both highlight 
                                                            Australians’ continuous expectation of public and private sector 
                                                            organisations to support the wider community in every way they can, 
                                                            without jeopardising the containment of the virus.")),
                                                 tags$li("The ", tags$b("World Health Organisation "), "is being criticised on the global stage 
                                                         for its response to coronavirus, particularly their early statement 
                                                         that the virus is not airborne as it is now acknowledging evidence 
                                                         to the contrary."), br(),
                                                 tags$li("With a small cluster of cases in NSW being linked to the second-wave of 
                                                         infections in Victoria, ", tags$b("Jetstar "), "has come under attack for not 
                                                         screening a planeload of passengers travelling from Melbourne to 
                                                         Sydney on arrival."), br(),
                                        tags$li("Despite controversy surrounding Premier Andrew’s lockdown response, the ", tags$b("Victorian 
                                                Government "), "has been praised for their announcement of a $534 million support package 
                                                for small businesses impacted by COVID-19 as they attempt to combat the state’s 
                                                significant economic losses."), br(),
                                        tags$li("A large takeout order from ", tags$b("KFC "), "in Melbourne led police to a house party in the suburbs, 
                                                where 16 people had broken restrictions and were caught hiding throughout the house, 
                                                resulting in $26,000 of fines."), br(),
                                        tags$li("After the government’s disastrous response to bushfires earlier this year, 
                                                Scott Morrison has appeared to redeem himself in the public eye with his handle 
                                                of the COVID-19 crisis. Despite this, his new popularity isn’t transferring to 
                                                the wider ", tags$b("Liberal Party"), ", losing the Eden-Monaro by-election to Labor."),
                                            
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
                                        h6("Victorian Government"),
                                        HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Public tenants will not be charged rent for two weeks.<br><br>We&#39;ll provide $1,500 cash payments for households with family members who are missing out on work.<br><br>Every other household will receive a $750 payment.</p>&mdash; Dan Andrews (@DanielAndrewsMP) <a href="https://twitter.com/DanielAndrewsMP/status/1279739499967246337?ref_src=twsrc%5Etfw">July 5, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),  
                                        br(),
                                        h6("World Health Organisation"),
                                        HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">FACT: <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> is NOT airborne. <br><br>The <a href="https://twitter.com/hashtag/coronavirus?src=hash&amp;ref_src=twsrc%5Etfw">#coronavirus</a> is mainly transmitted through droplets generated when an infected person coughs, sneezes or speaks.<br><br>To protect yourself:<br>-keep 1m distance from others<br>-disinfect surfaces frequently<br>-wash/rub your 👐<br>-avoid touching your 👀👃👄 <a href="https://t.co/fpkcpHAJx7">pic.twitter.com/fpkcpHAJx7</a></p>&mdash; World Health Organization (WHO) (@WHO) <a href="https://twitter.com/WHO/status/1243972193169616898?ref_src=twsrc%5Etfw">March 28, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        br(),
                                        h6("Jetstar"),
                                        HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">JUST IN: The ABC has obtained an email claiming Jetstar breached NSW coronavirus laws by letting all passengers on a flight from Melbourne to Sydney yesterday disembark without being screened via <a href="https://twitter.com/abcnews?ref_src=twsrc%5Etfw">@abcnews</a> <a href="https://twitter.com/hashtag/coronavirus?src=hash&amp;ref_src=twsrc%5Etfw">#coronavirus</a> <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a></p>&mdash; David Taylor (@DaveTaylorNews) <a href="https://twitter.com/DaveTaylorNews/status/1280723045586309120?ref_src=twsrc%5Etfw">July 8, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        br(),
                                        h6('KFC'),
                                            HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Ambulance officers noticed two people ordering around 20 KFC meals at 1.30am, with their car&#39;s registration leading police to a townhouse where there were 16 people &quot;hiding out the back&quot;. <a href="https://t.co/LwRNWaDhhl">https://t.co/LwRNWaDhhl</a></p>&mdash; The Age (@theage) <a href="https://twitter.com/theage/status/1281523574302478338?ref_src=twsrc%5Etfw">July 10, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        br(),
                                        h6('Morrison Government'),
                                        HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Scott Morrison does not appear to be a vote magnet in his home state.<br>* Lost Wentworth by-election 2018<br>* Lost NSW at the 2019 federal election to a Victorian trade union official. (Labor 24 seats, Coalition 22, Ind. 1)<br>* Lost Eden-Monaro by-election 2020<br>1/2 <a href="https://t.co/erkv8O0pIy">https://t.co/erkv8O0pIy</a></p>&mdash; George Megalogenis (@GMegalogenis) <a href="https://twitter.com/GMegalogenis/status/1279615578253127680?ref_src=twsrc%5Etfw">July 5, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>') 
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
                                            <br/><br/> Then, click 'Next' to continue the tour.", "Search & Mobile"),
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
                                                          of searches and destination (mobile and desktop traffic combined) for the period Mar 31st to May 31st; Data: SimilarWeb.")),
                                                          br(),
                                                          "To this day, Australians remain mostly concerned with understanding the virus symptoms, 
                                                          assessing the scale and impact of the outbreak (both locally and abroad) and the possibility 
                                                          of a vaccine. ", tags$b("They remain as anxious about personal safety (with a timely detection of the 
                                                          disease front of mind) as they are about its eradication."), br(), br(),
                                                          tags$li("Throughout the outbreak and until now, Australians have been turning to 
                                                                  government websites (health.gov.au, cdc.gov, who.int) as their primary 
                                                                  source of truth on COVID-19, particularly when it came to researching 
                                                                  symptoms and policy updates."), br(),
                                                          tags$li("Local news outlets (news.com.au, couriermail.com, abc.net.au, theguardian.com, 
                                                                  watoday.com) have been the preferred go-to destinations for the latest 
                                                                  developments on the spread of the disease at home and the development of a 
                                                                  vaccine, with a particular interest in news with a geographic lens (i.e. 
                                                                  NSW, QLD, Sydney, Perth and Melbourne)."), br(),
                                                          tags$li("Well-established reputable international news outlets (straitstimes.com, 
                                                                  aljazeera.com, telegraph.co.uk) while less popular, have been the top 
                                                                  destinations for those searching ‘how did coronavirus start’ and other key 
                                                                  overseas developments - notably in the UK and US, which have evolved to become 
                                                                  the new virus epi-centres."), br(),
                                                          tags$li("In the wake of the second wave of infections in Victoria, Australians 
                                                                  across states appear to be showing increasing concern over the development 
                                                                  of new clusters nearby and the possibility of a second outbreak developing 
                                                                  locally or nationally, as seen with some of the top search terms (e.g. 
                                                                  coronavirus sydney, coronavirus nsw, coronavirus perth)")
                                                    )
                                                ),
                                                 bs_button("Analysis", button_type = "default") %>%
                                                   bs_attach_collapse("sw_keywords_collapse"))), br(), br(),
                                      column(width = 12, wellPanel(
                                        HTML('<script type="text/javascript" src="https://ssl.gstatic.com/trends_nrtr/2213_RC01/embed_loader.js"></script> <script type="text/javascript"> trends.embed.renderWidget("US_cu_4Rjdh3ABAABMHM_en_en-AU", "fe_related_queries_c42508a0-7f03-4f36-a097-3d644d5ea101", {"guestPath":"https://trends.google.com:443/trends/embed/"}); </script>')
                                      , bs_collapse(id = "google_trends_collapse", content = tags$div(class = "well", 
                                                                                                       column(width = 12,
                                                                                                              tags$li("Trending searches in the last 7 days further indicate renewed concern over the possibility of the second outbreak in Victoria flowing into other states, with the spread of the virus being researched both on a local and national level (e.g. coronavirus victoria, australia coronavirus)."), br()
                                                                                                              ))), bs_button("Analysis", button_type = "default") %>%
                                                                                                                      bs_attach_collapse("google_trends_collapse"))),
                                     
                                      column(width = 6, align = "left", h5("Trending up apps, App Store/iPhone (last 28 days)"), introBox(data.step = 11, data.intro = "This chart shows the top 10 free trending apps across all categories in Australia over the last 28 days in the App store (iPhone only).", 
                                                                                                                                         wellPanel(reactableOutput("apple_table")))),
                                      column(width = 6, align = "left", h5("Trending up apps, Play Store (last 28 days)"), introBox(data.step = 12, data.intro = "This chart shows the top 10 free trending apps across all categories in Australia over the last 28 days in Google Play Store.", wellPanel(reactableOutput("google_trending_apps")))),
                                      br(),
                                     column(width = 12, wellPanel(
                                       bs_collapse(id = "apps_collapse", content = tags$div(class = "well", 
                                                     column(width=12,
                                                            tags$em(tags$p("Top Free Apps (across all categories) with the biggest increase 
                                                                           over the last 28 days in Store Rank or in Usage Rank, Australia only." 
                                                                           )), 
                                                            
                                                            tags$li("As restrictions ease in most States, we can see navigational and weather apps 
                                                                    return to the charts (WillyWeather, Waze) as people start venturing outside 
                                                                    freely for the first time since March. A haircut simulator (illus Face Hair Salon) 
                                                                    also featured, suggesting consumers are ", tags$b("once again becoming concerned with their 
                                                                    appearance as they return to public life.")), 
                                                            br(),
                                                            tags$li("A number of shopping and food delivery apps appeared on the charts this week 
                                                                    (Shop delivery, Menulog, The Iconic and Catch) as ", tags$b("some consumers continue to 
                                                                    rely on online shopping and delivery for both convenience and safety, 
                                                                    despite the reopening of brick-and-mortar shops.")), 
                                                            br(),  
                                                            tags$li("Entertainment apps, particularly in gaming (Stack Colors, Slots Heart of Vegas, 
                                                                    Athletics Mania and Love Pins), media streaming (Spotify, Stan) and social media 
                                                                    (Discord), continue to be popular choices as ", tags$b("consumers want to stay connected 
                                                                    to the outside world and look for ways to distract themselves while at home.")), br(),
                                                            tags$li("Entertainment apps, particularly in gaming (Twitch, 8 Ball Pool, Minecraft, 
                                                                    Burnout Masters and Pokemon Go), music streaming (Spotify) and social media 
                                                                    (Messenger, Linkedin, Facebook), continue to be popular choices as ", tags$b("consumers 
                                                                    want to stay connected to the outside world and look for ways to distract themselves 
                                                                    while at home.")),
                                                            tags$li("Dating apps (Bumble) have also re-emerged on the charts, ", tags$b("as consumers appear less 
                                                                    wary of interacting with strangers amidst easing restrictions.")),
                                                            br(),
                                                            tags$li("Utility apps remain popular also as ", tags$b("many Australians still work remotely "), "such as online chat apps (Google Meet)."), br()
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
                                                           
                                                           tags$li(tags$b("As many Australians continue to face financial hardship and revenue loss, "), 
                                                           "they are seeking support from their banks in the form of advice over renewing term deposits, 
                                                           early access to super and loan deferrals."),
                                                           br(),
                                                           tags$li("Searches for campaign messaging persisted this week, 
                                                                   notably interest for St.George’s ", tags$em("We’re here for you, "), 
                                                                   tags$b("demonstrating that customers are receptive to support 
                                                                   communications during this time.")),
                                                           br(),
                                                           tags$li(tags$b("SME customers are searching for information on how their bank can keep 
                                                                          their business afloat during the COVID crisis, and how they should 
                                                                          transform post-lockdown. "), "The government’s SME guarantee scheme remains 
                                                                          a popular choice amongst ANZ customers in particular as it allows 
                                                                          lenders to provide businesses with timely access to working capital 
                                                                          to see them through the crisis."), br(),
                                                           tags$li("Of note also is the traffic CBA is getting from unbranded coronavirus-related searches. 
                                                                   The bank is currently enjoying the greatest SOV for non-branded (26.32%) terms, 
                                                                   while Westpac dominates branded searches (50%)."), br()
                                                    )
                                                    # ,      column(width = 6,
                                                    #               tags$em(tags$p("Top 10 upward trending apps on the Google Play store in the last 28 days. Data: SimilarWeb. ")), br(), 
                                                                                            #        )
                                       )),
                                       bs_button("Analysis", button_type = "default") %>%
                                         bs_attach_collapse("banking_collapse")))
                                     # column(width = 6, wellPanel(
                                     #   HTML('<script type="text/javascript" src="https://ssl.gstatic.com/trends_nrtr/2213_RC01/embed_loader.js"></script> <script type="text/javascript"> trends.embed.renderWidget("US_cu_4Rjdh3ABAABMHM_en_en-AU", "fe_list_6676c6b0-213f-434e-99b2-7079c300945c_en-AU", {"guestPath":"https://trends.google.com:443/trends/embed/"}); </script> ')
                                     # )),
                                     
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
            

                
              branded_table <- renderKeywordsBankingSectorTableBranded(kw_branded_top25)

              output$better_keywords_branded <- renderReactable({
                branded_table
              })
              
              unbranded_table <- renderKeywordsBankingSectorTableUnbranded(kw_unbranded_top25)
              
              output$better_keywords_unbranded <- renderReactable({
                unbranded_table
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
            

         
 