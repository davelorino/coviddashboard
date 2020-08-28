
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
    sw_keywords_plot <- readRDS("sw_keywords_plot3.rds")
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
    emotions_cloud <- read_rds("emotioncloud2.rds")

# UI-------------------------------------------------------------------------------------------------------------
    
    ui = navbarPage(
                    
                   title = "Saatchi & Saatchi COVID-19 Pulse", theme = shinytheme("darkly"),
                   tabPanel(title = "Social",
                          
                       sidebarPanel(
                         style = "position:fixed;width:15%;",
                         img(src="Artboard1Logo.png", width="80%", height="80%"),
                                    br(), br(),
                                    actionButton("helpMe", "Tour"),
                                    br(), br(),
                         "Jump to:",
                         br(),
                         tags$a(href = "#volume_timeline", "Volume"),
                         br(),
                         tags$a(href = "#sentiment_timeline", "Sentiment"),
                         br(),
                         tags$a(href = "#emotions_cloud", "Emotions"),
                         br(),
                         tags$a(href = "#text_plot", "Text"),
                         br(),
                         tags$a(href = "#hashtags_plot", "Tweets"),
                         br(),
                         tags$a(href = "#brand_cloud", "Brands"),
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
                       div(id = "volume_timeline",  h4("Volume of COVID-19 conversation over time, Australia (VoC only)", align = "center")),
                      wellPanel(introBox(plotlyOutput("lineplot", height = "500px"), br(), 
                                         #tags$style(type = "text/css", "p { font-size: 12px; }"),
                                         tags$em(p("This chart displays the volume of mentions of COVID-19 on a weekly basis from Australians only. 
                                       Each point represents a 7 day period, beginning at the labelled date. For example, 
                                       the point labelled 'Dec 29, 2019' represents
                                         the ",  
                                                   tags$b("total weekly mentions "),  
                                                   "between the 29th of December and the 4th of January inclusive. 
                                          Data: NetBase; Sources: Blogs, Forums, Comments and Tweets Dec 29, 2019 - August 23rd, 2020. 
                                          Images on hover are selected from the top 10 of the week retweeted by people with < 1000 reach.")),
                                 data.step = 1, 
                                 data.intro = "Here we analyse the <b>volume</b> of conversation over time among Twitter, comments, 
                                 blogs and forums. <b>Hover</b> over the points to see what drove the conversation.",
                                 data.position = "bottom-left_aligned"),
                                 bs_collapse(
                                   id = "volume_collapse", 
                                   content = tags$div(class = "well", 
                                column(width = 12,  
                                      # tags$style(type = "text/css", "p { font-size: 12px; }")
                                                     ),
                              #  br(), br(),
                              tabsetPanel(selected = "Recent",  
                                tabPanel(title = "Past",
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
                                tags$li("Less than a month after easing restrictions  across the country, Australia was met with a 
                                        second-wave of infections in Victoria. Shortly after, a new cluster of cases in NSW was 
                                        linked to travellers from Melbourne, which led to the closure of borders between the 
                                        neighbouring states. This turn of events has led to a revival of the COVID-19 social conversation, 
                                        though not back to its former peak of March. Melbourne has since gone back into lockdown, ", 
                                        tags$b("reigniting fear and anxiety amongst the public that the worst of the COVID-19 crisis 
                                        may not yet be over for Australia.")), br(),
                                tags$li("By the end of July, the second outbreak was far from contained in Victoria, now home to more 
                                        than 70% of confirmed cases and 275 deaths to date, with Melbourne becoming the national 
                                        epicentre of the COVID-19 crisis as it entered Stage 4 lockdown. With small outbreaks 
                                        since appearing in other states and more border closures coming into force, the fear of 
                                        a nation-wide lockdown is yet to realise but remains present in everyone’s mind. 
                                        This major setback in the management of the Pandemic health crisis has been a key 
                                        driver of the COVID-19 conversation online, which has continued to rise steadily 
                                        in the last 7 weeks."), br()
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
                                h5(tags$u("June 15th - July 12th")),
                                tags$li("Scientists draw links between the spread of diseases like COVID-19 from animals to humans, 
                                with the destruction of ecologies and habitats belonging to species that carry diseases."),  
                                tags$li("The Trump administration draws worldwide criticism for securing almost exclusive access 
                                to a pharmaceutical treatment that may minimise the symptoms of COVID-19."),
                                tags$li("Victoria deals with a second virus outbreak due to failures in hotel quarantining 
                                and social distancing, resulting in Melbourne returning to lockdown just one month after 
                                restrictions began to ease."),
                                br(),
                                h5(tags$u("July 13th - August 9th")),
                                tags$li("Total confirmed cases rise to more than 400 a day in Victoria, with many suggesting 
                                        it’s because too many essential workers can’t afford to isolate when presenting with symptoms."),
                                tags$li("A small cluster of cases emerges in QLD after two travellers recently returned from Melbourne 
                                        lie on their border declaration pass, causing the state to close its borders to Victoria and NSW."),
                                tags$li("Face masks are made mandatory in Victoria’s COVID-19 hotspots and in a national-first, 
                                        metropolitan Melbourne moves into Stage 4 lockdown for 6 weeks from August 3rd."),
                                br())
                               ## End of tabPanel 'Past' 
                                       ),
                               tabPanel(title = "Recent",
                                        column(width = 6, br(), br(),
                                               tags$li("As Melbourne residents deal with the harsh realities of Stage 4 lockdown, 
                                                       the virus has continued to spread throughout the state, peaking on August 
                                                       4th with 693 confirmed cases recorded. While cases have since steadily declined, 
                                                       the rest of the nation continues to look on the situation in Victoria with a renewed 
                                                       sense of caution. The QLD Premier also threatened to keep their borders closed to other 
                                                       states until they can report zero community transmission. ", tags$b("As governments move to protect 
                                                       their constituents, fear-mongering and competition between the states in their containment 
                                                       of the pandemic has reignited ‘border wars’ and state-ism amongst Australians.")), br()
                                               ),
                                        column(width = 6,
                                               h4(tags$b(tags$u("Key Events"))),
                                               h5(tags$u("August 10th - August 23rd")), 
                                               tags$li("Prime Minister Scott Morrison announces that Australia has secured 
                                                       access to a promising vaccine being developed in Oxford University. 
                                                       If trials prove successful, the vaccine will be made mandatory to all Australians."), br(),
                                               tags$li("The State of Emergency in Victoria is extended until September 13th, allowing the 
                                                       government to continue isolation and social distancing measures in the state 
                                                       in an effort to curb the spread of COVID-19."), br(),
                                               tags$li("In a positive turn of events, it appears Australia has recorded its least deadly 
                                                       flu season in recent times as a result of the measures taken to curb the 
                                                       spread of the COVID-19 virus with only 36 lab confirmed deaths compared to 480 last winter."), br()))
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
                        div(id = "sentiment_timeline", h4("Sentiment of COVID-19 conversation over time, Australia (VoC only)", align = "center")),
                        wellPanel(introBox(plotlyOutput("sentiment_plot", height = "450px"),
                                           br(),
                                           tags$em(  
                                             tags$p("This chart displays the percentage of mentions of COVID-19 from Australians 
                                          only that are positive and negative. 
                                         Each date on the graph represents a 7 day period, beginning at the 
                                         labelled date. For example, the first point labelled 'Dec 29, 2019' represents
                                         the average weekly sentiment between the 29th of December and the 4th of January inclusive. 
                                         Data: NetBase; Sources: Blogs, Forums, Comments 
                                          and Tweets Dec 29, 2019 - August 23rd, 2020.")),
                                  data.step = 3,
                                  data.intro = "Here we analyse the <b>sentiment</b> of conversation over 
                                  time among Twitter, comments, blogs and forums."),
                                  bs_collapse(id = "sentiment_collapse",
                                              content = tags$div(class = "well",
                                 
                              tabsetPanel(selected = "Recent",
                                tabPanel(title = "Past",    
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
                              
                                , tags$style(type = "text/css", "p { font-size: 12px; }"))),
                               tabPanel(title = "Recent",
                                        column(width = 12,
                                               br(),
                                               tags$li(tags$b("As Victorians battle through a second and severe outbreak, their ordeal doesn’t appear to affect 
                                              the mood of the rest of the country. On the contrary, "), 
                                                       "positive sentiment was at an all-time high this month, hovering between 24-29% of total mentions. ",
                                                       tags$b("While the rest of Australia continues to warily work their way out of the crisis, 
                                              Victoria has not only been isolated physically from their sister states, but also in their collective psyche.")),
                                               br(), br()))
                                ))),
                                bs_button("Analysis", button_type = "default") %>%
                                  bs_attach_collapse("sentiment_collapse")),
                                  column(width = 6, h5("VoC sentiment - weekly snapshot"), 
                                         wellPanel(introBox(data.step = 4,
                                                            plotlyOutput("sevendaydonut"), br(),
                                                            tags$em( tags$p("Sentiment in the 7 day period between the 16th and the 23rd of August. 
                                                                      Data: NetBase; Sources: Blogs, Forums, Comments and Tweets.")),
                                                            data.intro = "This chart shows a snapshot of the consumer sentiment from Twitter, 
                                                            comments, blogs and forums over the last week.", 
                                                            bs_collapse(id = "weekly_sentiment_collapse",
                                                            content = tags$div(class = "well",
                                                             br(),
                                                             
                                                             tags$b("The nation continues to move at two-speeds in relation to the COVID-19 health 
                                                                    crisis. "), "As Metropolitan Melbourne remains in Stage 4 
                                                                    lockdown, the rest of Australia continues to enjoy the restricted freedoms of 
                                                                    a post-lockdown society, watching on with a sense of guilt and relief. 
                                                                    As such, sentiment appears unexpectedly high with 29.4% positive mentions - 
                                                                    the 2nd highest score since February.", 
                                                             
                                                  br())),
                                                            bs_button("Analysis", button_type = "default") %>%
                                                                bs_attach_collapse("weekly_sentiment_collapse")))),
                                   column(width = 6, div(id = "emotions_cloud", h5("Trending emotions - weekly snapshot")), 
                                          wellPanel(introBox(data.step = 5, 
                                                             
                                                             data.intro = "This chart shows the most frequently used emotional terms in the last week.", 
                                                           #  plotlyOutput("hashtags_7days"),
                                                           wordcloud2Output("emotion_cloud"), br(),
                                                           tags$em(  tags$p("Trending top positive and negative emotions between the 16th August and 23rd August. 
                                                                            Data: Netbase. Sources: Blogs, Forums, Comments and Tweets.")),
                                                           br(),
                                                     bs_collapse(id = "weekly_hashtags_collapse",
                                                                 content = tags$div(class = "well",
                                                                                    column(width = 12,
                                                                                           tags$li("Whilst social sentiment over the handling of the COVID-19 crisis remains 
                                                                                                   largely negative (with fear and the ongoing blaming game between states 
                                                                                                   the top negative emotions in the last 7 days), positive sentiment is mostly 
                                                                                                   driven by a celebration of the successes to date in containing the virus in 
                                                                                                   parts of the country and the belief that better days are to come with optimism 
                                                                                                   the top positive emotion."), 
                                                                                           br(),
                                                                                           br()
                                                                                      ))),
                                                     bs_button("Analysis", button_type = "default") %>%
                                                         bs_attach_collapse("weekly_hashtags_collapse")
                                                     )
                                                    )
                                          ),
                                          br(), br(),
                                 column(width = 6, div(id = "text_plot", h5("Text sentiment score - weekly snapshot")), 
                                         wellPanel(introBox(data.step = 6, 
                                                             data.intro = paste("This chart analyses the top 25 words contributing to positive or negative sentiment from Twitter, comments, 
                                                                                blogs and forums in the last week using the ", 
                                                                                tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"), " sentiment lexicon."),
                                                             plotlyOutput("contribution_plot"), br(),
                                                            tags$em(tags$p("Top 25 contributing words to sentiment as determined by the", 
                                                                           tags$a(href = "http://corpustext.com/reference/sentiment_afinn.html", "AFINN"),
                                                                           " sentiment analysis lexicon. 7 day period between the 16th and the 23rd of August. 
                                                                     Data: NetBase; Sources: Blogs, Forums, Comments and Tweets."))
                                                             
                                                     ),
                                                   bs_collapse(id = "contribution_collapse", content = tags$div(class = "well", 
                                                                                                                column(width=12
                                                                                                                       ),                                                        
                                                                                                                 br(), 
                                                                                                                
                                                        tags$li("Terms linked to virus casualties have remained the top contributors 
                                                                to negative sentiment in the past week ", tags$b("(lost, worst, infected, death, died) "),
                                                                "as the situation in Victoria continues to unfold. With Metropolitan Melbourne 
                                                                settling into a mandatory Stage 4 lockdown and a 6-week state of disaster, 
                                                                this has also generated a significant portion of negative conversation online (risk, crisis). "), br(),
                                                        tags$li("Whilst only accounting for a small portion of mentions still, terms contributing to positive 
                                                                sentiment continue to reflect the ‘good news stories’ emerging from the ongoing crisis 
                                                                as Australians come together and help out each other ", tags$b("(care, support, help) "), "and 
                                                                successfully overcome the outbreak in parts of Australia ", tags$b("(positive, good, better, 
                                                                greater, strong, free), "), "all ", tags$b("showing continuous consumer appetite for uplifting 
                                                                stories in these challenging times.")), br(),
                                                                                                                
                                                   )),
                                                   bs_button("Analysis", button_type = "default") %>%
                                                     bs_attach_collapse("contribution_collapse"), br()
                                                   )
                                        ),
                               column(width = 6, div(id = "hashtags_plot", h5("Trending hashtags - weekly snapshot")), 
                                          wellPanel(introBox(data.step = 7, 
                                                             data.intro = "This chart analyses the top trending hashtags from social media in the last week."),
                                                             #plotlyOutput("contribution_plot_30days")
                                                             plotlyOutput("hashtags_7days"), br(),
                                                    tags$em(  tags$p("Trending hashtags between the 16th and 23rd of August.
                                                                                           Data: NetBase; Sources: Blogs, Forums, Comments and Tweets.")),
                                                                         
                                      bs_collapse(id = "hashtagos_collapse", content = tags$div(class = "well", 
                                                                                                   column(width=12),                                                        
                                                                                                   br(), 
                                                                                                   
                                                                                                   tags$li("The dominant theme in this week’s trending hashtags is the 
                                                                                                           escalating health crisis in Australia, with Victoria its 
                                                                                                           epicentre ", tags$b("(#covid19vic, #stayathome) "), "as people are forced 
                                                                                                           to stay home and wear masks ", tags$b("(#wearamask, #ppe, #healthcareworkers) "), 
                                                                                                           "when out in public."), br(),
                                                                                                   tags$li("Nine cases of COVID-19 have been linked to the Network 10 reality show 
                                                                                                           The Masked Singer ", tags$b("(#maskedsingerau) "), "which is filmed in Melbourne, prompting 
                                                                                                           all 300 staff and viewers to get tested and go into 14 days of isolation."), br(),
                                                                                                   tags$li("Pauline Hanson has controversially announced Australia should follow in Sweden’s ", tags$b("(#sweden) "), "footsteps, by practising ‘herd immunity’."), br(),
                                                                                                   tags$li("Both local and international politics were again in the spotlight this week, 
                                                                                                           with Labor announcing a victory in the state election ", tags$b("(#ntvotes) "), "and the COVID-19 
                                                                                                           crisis? Not sure what hashtag we are refering to here? and ", tags$b("#Brexit "), "fueling fresh 
                                                                                                           calls for independence in Scotland."), br(),
                                                                                                    br()
                                                                                                   
                                      )),
                                      bs_button("Analysis", button_type = "default") %>%
                                        bs_attach_collapse("hashtagos_collapse"), br()
                               )),
                        column(width = 12, 
                            div(id = "brand_cloud", h5("Most talked about brands in connection with Coronavirus (Australia VoC only) - weekly snapshot", align = "center")), 
                          wellPanel(introBox(data.step = 8, 
                             data.intro = "This chart shows the brands and organisations that are being
                                                 talked about most by cosumers in the last week, and the sentiment towards that brand or organisation.", 
                                                wordcloud2Output("rona_cloud"), br(),
                             tags$em(tags$p("The most most talked about brands in connection with Coronavirus 
                                            in the last 7 days, ranked by VoC sentiment and volume of mentions (Australia only). 
                                            Brand mentions may be split out by both negative and positive sentiments where applicable. 
                                            Period 16th to the 23rd of August Data: NetBase. Brand Detection: OpenNLP."))
                             ,                  tags$script(HTML(
                                                  "$(document).on('click', '#canvas', function() {",
                                                  'word = document.getElementById("wcSpan").innerHTML;',
                                                  "Shiny.onInputChange('selected_word', word);",
                                                  "});"
                                                ))
                                  ),
                                    bs_collapse("business_cloud", 
                                                content = tags$div(class = "well",
                                
                                                                                          
                                        column(width = 12), br(),
                                          column(width = 6,
                                                 h5(tags$em("Brands, organisations and venues that fail to protect staff and customers from the virus continue to dominate the weekly social conversation and to be a source of negative sentiment.")),
                                                 tags$li("A ", tags$b("Starbucks "), "in South Korea was temporarily closed after an outbreak, however it has since been reported that the employees that wore masks escaped infection."), 
                                                 br(),
                                                 tags$li("Authorities are investigating two quarantine security guards from the ", tags$b("Marriott "), "Hotel in Circular Quay after they tested positive for coronavirus. They are yet unable to find a connection between the two cases."), 
                                                 br(),
                                        tags$li("Nine cases of COVID-19 have been linked to the Network 10 reality show ", tags$b("The Masked Singer "), "which is filmed in Melbourne, prompting all 300 staff and viewers to get tested and go into 14 days of isolation."), 
                                                 br(),
                                        tags$li("The ", tags$b("Brisbane Youth Detention Centre "), "has been linked to a cluster of 11 coronavirus infections, with reports it may be connected to a woman who returned from Melbourne last month."), br(),
                                        tags$li(tags$b("STA Travel "), "goes into administration, facing a huge downturn in revenue from travel bans. Angry customers are also being told they will not be eligible for refunds due to cancelled travel plans."), br(), br(),
                                            
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
                                        h6("Starbucks"),
                                        HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">A person sitting under an airconditioner infected 27 others with coronavirus at a Starbucks cafe in South Korea, but none of employees, who were wearing masks, got the virus <a href="https://t.co/7SYdKEglZT">https://t.co/7SYdKEglZT</a> <a href="https://t.co/VXA4Aw8uGv">pic.twitter.com/VXA4Aw8uGv</a></p>&mdash; Sam Kim (@samkimasia) <a href="https://twitter.com/samkimasia/status/1297116978964905984?ref_src=twsrc%5Etfw">August 22, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),  
                                        br(),
                                        h6('Marriot Hotel'),
                                        HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">A security guard working at Sydney’s Marriott Hotel, one of the hotels where returned travellers are placed into quarantine, has tested positive for COVID-19. News of the case came after NSW reported nine new cases in the latest 24-hour reporting period <a href="https://t.co/xwJ6RHDWHt">https://t.co/xwJ6RHDWHt</a> <a href="https://t.co/ZN8URmI2rY">pic.twitter.com/ZN8URmI2rY</a></p>&mdash; The Sydney Morning Herald (@smh) <a href="https://twitter.com/smh/status/1296997186627043328?ref_src=twsrc%5Etfw">August 22, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'), 
                                        br(),
                                        h6("The Masked Singer"),
                                        HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Kooky &amp; cute series, Masked Singer Australia has been shut down with crew &amp; judges self isolating after an outbreak of coronavirus on set. There are reports that seven dancers on the show have tested positive to COVID-19. <a href="https://t.co/glBhVP73C5">https://t.co/glBhVP73C5</a> via <a href="https://twitter.com/newscomauHQ?ref_src=twsrc%5Etfw">@newscomauHQ</a> <a href="https://twitter.com/hashtag/MaskedSingerAU?src=hash&amp;ref_src=twsrc%5Etfw">#MaskedSingerAU</a> <a href="https://t.co/cmvwlIDRuO">pic.twitter.com/cmvwlIDRuO</a></p>&mdash; Melissa Hoyer (@melissahoyer) <a href="https://twitter.com/melissahoyer/status/1297115600997642240?ref_src=twsrc%5Etfw">August 22, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        br(),
                                        h6("Brisbane Youth Detention Centre"),
                                            HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Worker at Brisbane Youth Detention Centre tests positive to coronavirus <a href="https://t.co/h8F1CdRLPp">https://t.co/h8F1CdRLPp</a></p>&mdash; Robert Maxwell (@RobMax4) <a href="https://twitter.com/RobMax4/status/1296313905191350274?ref_src=twsrc%5Etfw">August 20, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                                        br(),
                                        h6("STA"),
                                        HTML('<blockquote class="twitter-tweet"><p lang="en" dir="ltr">STA Travel goes into administration, leaving travellers in limbo <a href="https://t.co/p0OUY9rann">https://t.co/p0OUY9rann</a></p>&mdash; IRG (@intresilience) <a href="https://twitter.com/intresilience/status/1297064327006961665?ref_src=twsrc%5Etfw">August 22, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>') 
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
                                          "Jump to:",
                                          br(),
                                          tags$a(href = "#keywords", "Keywords"),
                                          br(),
                                          tags$a(href = "#trending_searches", "Trending Searches"),
                                          br(),
                                          tags$a(href = "#trending_apps", "Trending Apps"),
                                          br(),
                                          tags$a(href = "#branded_terms", "Branded Terms"),
                                          br(),
                                          tags$a(href = "#unbranded_terms", "Unbranded Terms"),
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
                                      br(),
                                      tags$em(p("Top organic search keywords including ‘coronavirus’ ranked by volume 
                                                          of searches and destination (mobile and desktop traffic combined) for the period May 1st to July 31st; Data: SimilarWeb.
                                                                     *Keywords with an appended asterisk are new to this period and were not present in the April - June period.")),
                                                bs_collapse(id = "sw_keywords_collapse", 
                                            content = tags$div(class = "well", 
                                               
                                                          br(),
                                                          "To this day, Australians remain focused on understanding the virus and keeping track of its impact both 
                                                          locally and abroad. ", tags$b("Personal safety, containment and eradication of the virus locally and overseas 
                                                          remain their primary concerns."), br(), br(),
                                                          tags$li("To date, global statistics site worldometers.info has been the primary source of information for 
                                                                  keep track of coronavirus developments around the world, particularly when researching live 
                                                                  statistics and the evolving situation in the US."), 
                                                          br(),
                                                          tags$li("Throughout the outbreak and until now, Australians have been turning to government websites 
                                                                  (health.gov.au, health.wa.gov.au) as their local source of truth on COVID-19, 
                                                                  particularly when it came to researching updates on both a national and state level."), 
                                                          br(),
                                                          tags$li("Local news outlets (7news.com.au, abc.net.au, smh.com.au) have been the preferred go-to 
                                                                  destination developments on the fast-moving spread of the disease in Victoria, 
                                                                  and overseas in the UK and China."), 
                                                          br()
                                                    
                                                )),
                                                 bs_button("Analysis", button_type = "default") %>%
                                                   bs_attach_collapse("sw_keywords_collapse"))), br(), br(),
                                      column(width = 12, wellPanel(
                                        HTML('<script type="text/javascript" src="https://ssl.gstatic.com/trends_nrtr/2213_RC01/embed_loader.js"></script> <script type="text/javascript"> trends.embed.renderWidget("US_cu_4Rjdh3ABAABMHM_en_en-AU", "fe_line_chart_c70bfabd-c15d-4487-89f2-f265c3afcddf", {"guestPath":"https://trends.google.com:443/trends/embed/"}); </script>')
                                      , bs_collapse(id = "google_trends_collapse", content = tags$div(class = "well", 
                                                                                                       column(width = 12,
                                                                                                              tags$li("Whilst remaining one of the most searched topics in Australia, search interest 
                                                                                                                      for Coronavirus continues to decline slowly yet steadily from its high peak of 
                                                                                                                      March, with other popular topics such as news and weather information taking 
                                                                                                                      over in the last 4 weeks. This shows that despite the ongoing second wave of 
                                                                                                                      infections in Victoria and clusters appearing in neighbouring states, Australians 
                                                                                                                      are now much better informed about the virus than they were at the start of the 
                                                                                                                      Pandemic crisis, diminishing the need for education-related searches. Media fatigue 
                                                                                                                      and information overload as the crisis drags on are also likely further impacting 
                                                                                                                      search interest."), br()
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
                                                            
                                                            tags$li("Health apps have been trending up this week (MyFitness Pal, Flo Period Tracker) 
                                                                    showing a greater consumer ", tags$b("focus on personal health and well-being as most 
                                                                    Australians return to public life in a post-lockdown world.")), 
                                                            br(),
                                                            tags$li("With house prices falling, interest rates at a record low and new Government incentives 
                                                                    rolled out in recent months in support of the property market, ", tags$b("realty apps have emerged 
                                                                    on the charts (realestate.com) for the first time in months as consumer demand is 
                                                                    stimulated"), ", in particular amongst first-home buyers who stand to benefit the most 
                                                                    from the latest Government schemes."), 
                                                            br(),  
                                                            tags$li("A new shopping app appeared on the charts this week (Alibaba) ", tags$b("as consumers continue to 
                                                                    rely on online shopping and delivery for both convenience and safety, particularly as 
                                                                    Victoria returns to lockdown.")), br(),
                                                            tags$li("Entertainment apps, particularly in gaming and gambling (No Deposit Bonuses, Crash Delivery!, 
                                                                    Cashman Casino, Kahoot!), media streaming (Foxtel Go) and social media (Discord, Reddit, Twitter, 
                                                                    Layout), continue to be popular choices as ", tags$b("Australians look for ways to stay connected and distract 
                                                                    themselves while in lockdown or as their life returns to semi-normal.")),
                                                            tags$li("Dating apps (Bumble) have also re-emerged on the charts, ", tags$b("as consumers appear less 
                                                                    wary of interacting with strangers amidst easing restrictions.")),
                                                            br(),
                                                            tags$li("Utility apps remain popular also as ", tags$b("many Australians still work remotely"), ", 
                                                                    such as online chat apps (Microsoft Outlook, Hangouts, Google Duo, My Optus)."), br()
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
                                                           
                                                           tags$li("As many Australians continue to face financial hardship and income loss, they are ",
                                                                   tags$b("seeking support from their banks mostly in the form of advice on COVID loans and 
                                                                   gaining early access to their super"), ", with Westpac and NAB driving most interest 
                                                                   in this regard. There also appears to be confusion over how ", tags$b("consumers can conduct 
                                                                   their day-to-day banking"), ", with queries on the best way to  deposit cheques or the 
                                                                   best time to visit local branches (opening hours)."),
                                                           br(),
                                                           tags$li(tags$b("SME customers are searching for information on how their bank can keep their business 
                                                                   afloat during the COVID crisis and help them transform post-lockdown. "), 
                                                                   "The government’s SME guarantee scheme remains a key area of interest, together with 
                                                                   equipment financing and COVID business loans."),
                                                           br(),
                                                           tags$li(tags$b("SME customers are searching for information on how their bank can keep 
                                                                          their business afloat during the COVID crisis, and how they should 
                                                                          transform post-lockdown. "), "The government’s SME guarantee scheme remains 
                                                                          a popular choice amongst ANZ customers in particular as it allows 
                                                                          lenders to provide businesses with timely access to working capital 
                                                                          to see them through the crisis."), br(),
                                                           tags$li("Of note also is the traffic Westpac is getting from coronavirus-related searches. 
                                                                   The bank is currently enjoying the greatest SOV for non-branded (45%) terms and branded searches (52%)."), br()
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
          
          output$emotion_cloud <- renderWordcloud2({
            emotions_cloud
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
            

         
 