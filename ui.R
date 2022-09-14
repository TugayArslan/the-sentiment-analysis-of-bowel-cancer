library(shiny)
library(shinydashboard)
library(forcats)
library(plotly)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)
library(dplyr)
library(shinycssloaders)
library(future)
library(promises)
library(janitor)
#library(wordcloud)
library(RColorBrewer)
#library(wordcloud2)
library(syuzhet)
library(colourpicker)
library(ggwordcloud)

#defaultW <- getOption("warn")
#options(warn = -1)

future::plan(future::multisession)

set.seed(1234)

# ---- Colors ----
ADMINLTE_COLORS <- list(
  "light-blue" = "#6699CC",
  "green"      = "#99C794",
  "red"        = "#EC5f67",
  "purple"     = "#C594C5",
  "aqua"       = "#a3c1e0",
  "yellow"     = "#FAC863",
  "navy"       = "#343D46",
  "olive"      = "#588b8b",
  "blue"       = "#4080bf",
  "orange"     = "#F99157",
  "teal"       = "#5FB3B3",
  "fuchsia"    = "#aa62aa",
  "lime"       = "#b0d4b0",
  "maroon"     = "#AB7967",
  "black"      = "#1B2B34",
  "gray-lte"   = "#D8DEE9",
  "primary"    = "#6699CC",
  "success"    = "#99C794",
  "danger"     = "#EC5f67",
  "info"       = "#a3c1e0",
  "warning"    = "#FAC863"
)
options("spinner.color" = ADMINLTE_COLORS$`gray-lte`)
options("spinner.color.background" = "#F9FAFB")
BASIC_COLORS <- c("primary", "info", "success", "danger", "warning")

# ---- Dates and Times ----
TZ_GLOBAL <- "Europe/London"        # Time zone where conference is taking place
Sys.setenv(TZ = TZ_GLOBAL)
tz_global <- function(tz = NULL) {
  if (!is.null(tz)) return(tz)
  tz <- Sys.getenv("TZ")
  if (tz == "") "UTC" else tz
}
today_tz <- function() today(tz_global())
TWEET_WALL_DATE_INPUTS <- c(
  "Today"       = "today",
  "Yesterday"   = "yesterday",
  "Past week"   = "past_week",
  "Past month"  = "past_month",
  "In 2017"     = "in_2017",
  "In 2018"     = "in_2018",
  "In 2019"     = "in_2019",
  "In 2020"     = "in_2020",
  "In 2021"     = "in_2021",
  "In 2022"     = "in_2022",
  "Since Covid" = "since_covid"
)
TWEET_WALL_DATE_RANGE <- function(inputId) {
  switch(
    inputId,
    "today"          = c(start = ymd("2022-07-11"),     end = ymd("2022-07-11")),
    "yesterday"      = c(start = ymd("2022-07-10"),     end = ymd("2022-07-10")),
    "past_week"      = c(start = ymd("2022-07-04"),     end = ymd("2022-07-11")),
    "past_month"     = c(start = ymd("2022-06-11"),     end = ymd("2022-07-11")),
    "in_2017"        = c(start = ymd("2017-01-01"),     end = ymd("2017-12-31")),
    "in_2018"        = c(start = ymd("2018-01-01"),     end = ymd("2018-12-31")),
    "in_2019"        = c(start = ymd("2019-01-01"),     end = ymd("2019-12-31")),
    "in_2020"        = c(start = ymd("2020-01-01"),     end = ymd("2020-12-31")),
    "in_2021"        = c(start = ymd("2021-01-01"),     end = ymd("2021-12-31")),
    "in_2022"        = c(start = ymd("2022-01-01"),     end = ymd("2022-12-31")),
    "since_covid"    = c(start = ymd("2020-01-01"),     end = ymd("2022-07-11")),
    NA
  )
}

# ---- UI Functions ----
twemoji <- function(runes, width = "20px") {
  runes <- tolower(runes)
  runes <- gsub(" ", "-", runes)
  runes <- sub("-fe0f$", "", runes) # seems to cause problems with twemoji :shrug:
  emojis <- glue::glue("https://cdnjs.cloudflare.com/ajax/libs/twemoji/11.2.0/2/svg/{runes}.svg")
  emojis <- glue::glue('<img src="{emojis}" width = "{width}">')
  paste(emojis)
}
tweetExplorerUI <- function(id, tweet_div_id = "tweetExplorer-tweet", collapsed = FALSE, status = "info") {
  ns <- NS(id)
  fluidRow(
    column(
      12,
      column(
        12,
        class = "col-md-push-9 col-md-3",
        box(
          width = "7 col-md-12",
          status = status,
          solidHeader = TRUE,
          title = "Options",
          collapsible = TRUE,
          collapsed = collapsed,
          selectInput(
            ns('view'),
            'Tweet Group',
            c('Popular',
              'Screening',
              "Covid/Corona",
              "All")
          ),
          uiOutput(ns('help_text')),
          uiOutput(ns('filters'))
        ),
        column(
          width = 7,
          class = "col-md-12",
          id = tweet_div_id,
          uiOutput(ns("tweet"))
        )
      ),
      box(
        width = "12 col-md-pull-3 col-md-9",
        status = "primary",
        DT::dataTableOutput(ns('tweets')))
    )
  )
}
ukroiExplorerUI <- function(id, tweet_div_id = "ukroiExplorer-tweet", collapsed = FALSE, status = "info") {
  ns <- NS(id)
  
  fluidRow(
    box(
      width = "12",
      status = status,
      column(
        2,
        selectInput(
          ns('view_ukroi'),
          'Regions',
          c('All',
            'United Kingdom',
            "England",
            "Scotland",
            "Wales",
            "Northern Ireland",
            "Republic of Ireland"))
      ),
      column(
        2,
        uiOutput(ns('help_text_ukroi'))
      ),
      column(
        5,
        checkboxGroupInput(ns('filter_binary_ukroi'), 'Tweet Filters',
                           choices = c("Quoted", "Replied", "Retweeted", "Favorited", "Screening", "Covid/Corona"),
                           inline = TRUE)
      ),
      column(
        3,
        uiOutput(ns('filter_text_ukroi'))
      )
    ),
    fluidRow(
      valueBoxOutput(
        ns("total_today_ukroi"),
        width = "3 col-lg-2"),
      valueBoxOutput(
        ns("tweeters_today_ukroi"),
        width = "3 col-lg-2"),
      valueBoxOutput(
        ns("total_favorites_ukroi"),
        width = "3 col-lg-2"),
      valueBoxOutput(
        ns("total_topic_ukroi"),
        width = "3 col-lg-2"),
      valueBoxOutput(
        ns("total_all_ukroi"),
        width = "3 col-lg-2"),
      valueBoxOutput(
        ns("rate_ukroi"),
        width = "3 col-lg-2")
      ),
    fluidRow(
      column(
        width = 8,
        offset = 2,
        class = "col-md-6 col-md-offset-0 col-lg-4",
        class = "text-center",
        tags$h4(HTML(twemoji("2764"), "Most Liked")),
        withSpinner(uiOutput(ns("dash_most_liked_ukroi")), proxy.height = "200px")
      ),
      column(
        width = 8,
        offset = 2,
        class = "col-md-6 col-md-offset-0 col-lg-4",
        class = "text-center",
        tags$h4(HTML(twemoji("1F31F"), "Most Retweet")),
        withSpinner(uiOutput(ns("dash_most_rt_ukroi")), proxy.height = "200px")
      ),
      column(
        width = 8,
        offset = 2,
        class = "col-md-6 col-md-offset-0 col-lg-4",
        class = "text-center",
        tags$h4(HTML(twemoji("1F389"), "Most Replied")),
        withSpinner(uiOutput(ns("dash_most_recent_ukroi")), proxy.height = "200px")
      )
    ),
    fluidRow(
      box(
        width = "5 col-lg-3",
        status = "info",
        title = "Top Tweeters",
        tags$div(
          class = "scroll-overflow-x",
          withSpinner(uiOutput(ns("top_tweeters_ukroi")))
        ),
        helpText(HTML("Total &#128259; RT (x2) and &#x2764; Favorites per user"))
      ),
      box(
        width = "5 col-lg-3",
        status = "danger",
        title = "Top Hashtags",
        withSpinner(uiOutput(ns("top_hashtags_ukroi"))),
        helpText("Times hashtag was used relative to most popular hashtag, excludes",
                 tags$code("cancer"), ",", tags$code("bowel"), ",", tags$code("colon"), ",", tags$code("colorectal"),
                 tags$code("bowelcancer"), ",", tags$code("coloncancer"), "and", tags$code("colorectalcancer")
        )
      ),
      box(
        width = "5 col-lg-3",
        status = "warning",
        title = "Top Words",
        withSpinner(uiOutput(ns("top_tweet_words_ukroi"))),
        helpText("Times word was used relative to most popular word")
      ),
      box(
        width = "5 col-lg-3",
        status = "success",
        title = "Top Emoji",
        withSpinner(uiOutput(ns("top_emojis_ukroi"))),
        helpText("Times emoji was used relative to most used emoji")
      )
    )
  )
}

dashboardPage(
  # Dashboard Page Setup ----------------------------------------------------
  title = "Sentiment Analysis of Bowel Cancer",
  skin  = "red",
  #theme = c("ocean-next/AdminLTE.css", "ocean-next/_all-skins.css", "custom.css"),
  dashboardHeader(
    #title = "DSA8030",
    title = tags$img(src='https://www.qub.ac.uk/schools/media/Media,973090,smxx.JPG', height = '51', width ='210')
  ),

  # Dashboard Sidebar -------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "tab_about", icon = icon("info")),
      menuItem("Northern Ireland", tabName = "tab_dashboard", icon = icon("tree-city")),
      menuItem("UK and Ireland", tabName = "tab_region", icon = icon("globe")),
      menuItem("Sentiment", tabName = "tab_sentiment", icon = icon("robot")),
      menuItem("Tweet Wall", tabName = "tab_tweet_wall", icon = icon("stream")),
      menuItem("Explore", tabName = "tab_explore", icon = icon("compass"))
    )
  ),

  # Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tabItems(
      # About - tab_about -------------------------------------------------------
      tabItem(
        "tab_about",
        fluidRow(
          # About - About Me - start ------------------------------------------------
          box(
            title = "About this project",
            status = "danger",
            width = "12",
            tags$p(
              class = "text-center",
              tags$img(class = "img-responsive img-rounded center-block", src = "qub.png")
            ),
            tags$h1(
              class = "text-center",
              tags$strong("The sentiment of Bowel Cancer"),
            ),
            tags$h2(
              class = "text-center",
              "MSc in Data Analytics/Science (DSA8030)",
            ),
            tags$h3(
              class = "text-center",
              "Faculty of Engineering and Physical Sciences",
            ),
            tags$h4(
              class = "text-center",
              "Author: Tugay Arslan"
            ),
            tags$h4(
              class = "text-center",
              "Supervisor: Dr. Felicity Lamrock"
            ),
            tags$p(
              "Bowel cancer is the 4th most common cancer in the UK. In Northern Ireland ",
              "approximately 1,200 people are diagnosed with bowel cancer each year, and ",
              "around 500 deaths.",
            ),
            tags$p(
              "In April 2010, the Northern Ireland Bowel Cancer Screening Programme (BCSP) ",
              "was launched to provide early detection and treatment of bowel cancer with ",
              "the aim of reducing the mortality and morbidity of the disease within in ",
              "Northern Ireland. The programme targets asymptomatic 60 to 75-year-olds and ",
              "screens this group every 2 years. Patients are given a home collection kit where ",
              "their stool sample is taken and tested to assess if further investigation via a ",
              "colonoscopy is needed. [1]"
            ),
            tags$p(
              "Other routes to diagnosis of bowel cancer are hospital via emergency or ",
              "General Practitioner (GP) referrals. [2]"
            ),
            tags$p(
              "In general, patients may find it hard to talk to their doctor about changes in ",
              "bowel habits, and due to COVID-19 doctors interact with patients differently ",
              "now via telephone triage."
            ),
            tags$p(
              "The goal of this project is to establish trends and patterns in social media ",
              "behaviours towards bowel cancer. By scraping data from Twitter and using ",
              "sentiment analysis, we will gain more insights into different views of bowel ",
              "cancer and possibly the impact that COVID-19 has had on this.",
            ),
            tags$p(
              "[1] ",
              tags$a(
                href = "https://www.publichealth.hscni.net/directorate-public-health/servicedevelopment-and-screening/bowel-cancer-screening",
                target = "_blank", "https://www.publichealth.hscni.net/directorate-public-health/servicedevelopment-and-screening/bowel-cancer-screening"
              ),
            ),
            tags$p(
              "[2] ",
              tags$a(
                href = "https://hscbusiness.hscni.net/pdf/Routes%20to%20Diagnosis%20Report%20-
%20Main%20Report%20Jan%202020.pdf",
                target = "_blank", "https://hscbusiness.hscni.net/pdf/Routes%20to%20Diagnosis%20Report%20-
%20Main%20Report%20Jan%202020.pdf"
              ),
            ),
            tags$h4(
              class = "text-center",
              "Project Objectives"
            ),
            tags$ul(
              tags$li("Scrape Twitter data from Department of Health, Public Health Agency and other relevant Northern Ireland Twitter accounts regarding bowel cancer"),
              tags$li("Data cleaning and pre-processing in R"),
              tags$li("Perform exploratory data analysis. For example describe how many tweets were posted and how often by each account. What are the most popular bowel cancer tweets over certain period of time."),
              tags$li("Sentiment analysis – natural language processing using the text mining package tm in R"),
              tags$li("Display findings using a Shiny app"),
              )
          ),
        ),
          # About - About Me - end --------------------------------------------------
        fluidRow(
          # About - About Dashboard - start -----------------------------------------
          box(
            title = "About this Dashboard",
            status = "primary",
            width = "12",
            tags$p(
              class = "text-center",
              tags$a(
                href = "https://rstudio.com",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "RStudio.svg",
                         style = "max-width: 100px; margin-left: 2em;"
                )
              ),
              tags$a(
                href = "https://shiny.rstudio.com",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "shiny.svg",
                         style = "max-width: 100px; margin-left: 2em;"
                )
              ),
              tags$a(
                href = "https://ggplot2.tidyverse.org",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "ggplot2.svg",
                         style = "max-width: 100px; margin-left: 2em;"
                )
              ),
              tags$a(
                href = "https://github.com/yihui/knitr",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "knitr.svg",
                         style = "max-width: 100px; margin-left: 2em;"
                )
              ),
              tags$a(
                href = "https://lubridate.tidyverse.org",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "lubridate.svg",
                         style = "max-width: 100px; margin-left: 2em;"
                )
              ),
              tags$a(
                href = "https://purrr.tidyverse.org",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "purrr.svg",
                         style = "max-width: 100px; margin-left: 2em;"
                )
              ),
              tags$a(
                href = "https://stringr.tidyverse.org",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "stringr.svg",
                         style = "max-width: 100px; margin-left: 2em;"
                )
              ),
              tags$a(
                href = "https://tidyverse.org",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "tidyverse.svg",
                         style = "max-width: 100px; margin-left: 2em;"
                )
              ),
            ),
            tags$p(
              "This dashboard was built in",
              tags$a(href = "https://r-project.org", target = "_blank", "R"),
              "and", tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"), "with",
              tags$strong("shiny,"),
              tags$strong("shinydashboard,"),
              tags$strong("dplyr,"),
              tags$strong("tidyverse,"),
              tags$strong("plotly,"),
              tags$strong("tidyverse,"),
              "the", tags$strong("tm,"),
              "and many more packages."
            ),
            tags$p(
              "The data scraped using ",
              tags$strong("snscrape"),
              " in ",
              tags$a(href = "https://www.python.org", target = "_blank", "Python"),
              "Targeting Northern Ireland's official tweet accounts and from people in Northern Ireland only dates between 2017-07-11 and 2022-07-11."
            ),
            tags$p(
              "UI design elements of this dashboard has been greatly used from ",
              HTML(paste0(tags$a(href = "https://garrickadenbuie.com", "garrickadenbuie.com", target = "_blank"), ".")),
            ),
            tags$p(
              class = "text-center",
              tags$a(
                href = "https://www.twitter.com",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "Logo blue.svg",
                         style = "max-width: 60px;"
                )
              ),
              tags$a(
                href = "https://www.r-project.org",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "Rlogo.svg",
                         style = "max-width: 60px;"
                )
              ),
              tags$a(
                href = "https://www.python.org",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "python.svg",
                         style = "max-width: 60px;"
                )
              )
            )
          )
          # About - About Dashboard - end -----------------------------------------
        )
      ),
      # Frontpage - tab_dashboard -----------------------------------------------
      tabItem(
        "tab_dashboard",
        tags$head(
          # Metadata <head> ---------------------------------------------------------
          HTML(glue::glue(
            '<meta property="og:title" content="Sentiment Analysis of Bowel Cancer">
            <meta property="og:description" content="Sentiment Analysis of Bowel Cancer">
            <link rel="apple-touch-icon" sizes="57x57" href="apple-icon-57x57.png">
            <link rel="apple-touch-icon" sizes="60x60" href="apple-icon-60x60.png">
            <link rel="apple-touch-icon" sizes="72x72" href="apple-icon-72x72.png">
            <link rel="apple-touch-icon" sizes="76x76" href="apple-icon-76x76.png">
            <link rel="apple-touch-icon" sizes="114x114" href="apple-icon-114x114.png">
            <link rel="apple-touch-icon" sizes="120x120" href="apple-icon-120x120.png">
            <link rel="apple-touch-icon" sizes="144x144" href="apple-icon-144x144.png">
            <link rel="apple-touch-icon" sizes="152x152" href="apple-icon-152x152.png">
            <link rel="apple-touch-icon" sizes="180x180" href="apple-icon-180x180.png">
            <link rel="icon" type="image/png" sizes="192x192"  href="android-icon-192x192.png">
            <link rel="icon" type="image/png" sizes="32x32" href="favicon-32x32.png">
            <link rel="icon" type="image/png" sizes="96x96" href="favicon-96x96.png">
            <link rel="icon" type="image/png" sizes="16x16" href="favicon-16x16.png">
            <link rel="manifest" href="manifest.json">
            <meta name="msapplication-TileColor" content="#6699CC">
            <meta name="msapplication-TileImage" content="ms-icon-144x144.png">
            <meta name="theme-color" content="#6699CC">
            '
          )),
          # Metadata <head> end -----------------------------------------------------
        ),
        fluidRow(
          # Frontpage - boxes - start -----------------------------------------------
          valueBoxOutput(
            "total_today",
            # "—", "Total Tweets",
            # color = "purple",
            # icon = icon("comment-dots"),
            width = "4 col-lg-2"),
          valueBoxOutput(
            "tweeters_today",
            # "—", "Total Tweeters",
            # color = "orange",
            # icon = icon("user-circle"),
            width = "4 col-lg-2"),
          valueBoxOutput(
            "total_favorites",
            # "—", "Total Likes",
            # color = "red",
            # icon = icon("heart"),
            width = "4 col-lg-2"),
          valueBoxOutput(
            "total_topic",
            # "—", "Total Retweets",
            # color = "teal",
            # icon = icon("comments"),
            width = "4 col-lg-2"),
          valueBoxOutput(
            "total_all",
            # "—", "Tweets with Hashtag",
            # color = "fuchsia",
            # icon = icon("r-project"),
            width = "4 col-lg-2"),
          valueBoxOutput(
            "rate",
            # "—", "Tweets/hr",
            # color = "green",
            # icon = icon("hourglass-half"),
            width = "4 col-lg-2")
          # Frontpage - boxes - end -------------------------------------------------
        ),
        fluidRow(
          # Frontpage - tweet volume plots - start ----------------------------------
          tabBox(
            width = 10,
            tabPanel(
              status = "primary",
              title = "Tweet Volume",
              withSpinner(plotlyOutput("plot_hourly_tweet_volume", height = "250px"))
            ),
            tabPanel(
              status = "success",
              title = "Tweets by Hour of Day",
              withSpinner(plotlyOutput("plot_tweets_by_hour", height = "250px"))
            ),
            tabPanel(
              status = "success",
              title = "Tweets by Day of Week",
              withSpinner(plotlyOutput("plot_tweets_by_day", height = "250px"))
            ),
            tabPanel(
              status = "success",
              title = "Tweets by Week of Year",
              withSpinner(plotlyOutput("plot_tweets_by_week", height = "250px"))
            ),
            tabPanel(
              status = "success",
              title = "Tweets by Month of Year",
              withSpinner(plotlyOutput("plot_tweets_by_month", height = "250px"))
            ),
            tabPanel(
              status = "success",
              title = "Tweets by Year",
              withSpinner(plotlyOutput("plot_tweets_by_year", height = "250px"))
            ),
            tabPanel(
              status = "success",
              title = "Institutions 2019 to 2020",
              withSpinner(plotlyOutput("plot_tweets_covid", height = "250px"))
            ),
            tabPanel(
              status = "success",
              title = "People 2019 to 2020",
              withSpinner(plotlyOutput("plot_tweets_covid_by_people", height = "250px"))
            )
          ),
          box(
            width = "3 col-lg-1",
            status = "success",
            title = "Top Emoji",
            withSpinner(uiOutput("top_emojis")),
            #helpText("Times emoji was used relative to most used emoji")
          ),
          box(
            width = "3 col-lg-1",
            status = "success",
            title = "",
            withSpinner(uiOutput("top_emojis_last")),
            #helpText("Times emoji was used relative to most used emoji")
          )
          # Frontpage - tweet volume plots - end ------------------------------------
        ),
        fluidRow(
          box(
            width = "5 col-lg-3",
            status = "info",
            title = "Top Tweeters",
            tags$div(
              class = "scroll-overflow-x",
              withSpinner(uiOutput("top_tweeters"))
            ),
            helpText(HTML("Total &#128490; Reply (x2) &#9996; Quote (x2) &#128259; RT and &#x2764; Favorites per user"))
          ),
          box(
            width = "5 col-lg-2",
            status = "danger",
            title = "Top Hashtags",
            withSpinner(uiOutput("top_hashtags")),
            helpText("Times hashtag was used relative to most popular hashtag, excludes",
                     tags$code("cancer"), ",", tags$code("bowel"), ",", tags$code("colon"), ",", tags$code("colorectal"),
                     tags$code("bowelcancer"), ",", tags$code("coloncancer"), "and", tags$code("colorectalcancer")
            )
          ),
          box(
            width = "5 col-lg-2",
            status = "danger",
            title = "Top Hashtags (Per Tweeter Type)",
            withSpinner(uiOutput("top_hashtags_topic")),
            helpText("Times tf-idf*1000 was used relative to most popular important hashtag")
          ),
          box(
            width = "5 col-lg-1",
            status = "warning",
            title = "Top Words",
            withSpinner(uiOutput("top_tweet_words")),
            helpText("Times word was used relative to most popular word")
          ),
          box(
            width = "5 col-lg-2",
            status = "warning",
            title = "Top Important Words (Per Tweeter Type)",
            withSpinner(uiOutput("sent_important_topic")),
            helpText("Times tf-idf*10000 was used relative to most popular important word")
          ),
          box(
            width = "5 col-lg-2",
            status = "warning",
            title = "Top Important Words (Per Year)",
            withSpinner(uiOutput("sent_important_year")),
            helpText("Times tf-idf*10000 was used relative to most popular important word")
          )
        ),
        fluidRow(
          # Frontpage - Most XX Tweets - start --------------------------------------
          column(
            width = 8,
            offset = 2,
            class = "col-md-6 col-md-offset-0 col-lg-4",
            class = "text-center",
            tags$h4(HTML(twemoji("2764"), "Most Liked")),
            withSpinner(uiOutput("dash_most_liked"), proxy.height = "200px")
          ),
          column(
            width = 8,
            offset = 2,
            class = "col-md-6 col-md-offset-0 col-lg-4",
            class = "text-center",
            tags$h4(HTML(twemoji("1F31F"), "Most Retweet")),
            withSpinner(uiOutput("dash_most_rt"), proxy.height = "200px")
          ),
          column(
            width = 8,
            offset = 2,
            class = "col-md-6 col-md-offset-0 col-lg-4",
            class = "text-center",
            tags$h4(HTML(twemoji("1F389"), "Most Replied")),
            withSpinner(uiOutput("dash_most_recent"), proxy.height = "200px")
          )
          # Frontpage - Most XX Tweets - end ----------------------------------------
        )
      ),

    # UK and Ireland - tab_region ---------------------------------------------
    tabItem(
      "tab_region",
      ukroiExplorerUI("ukroi_table", status = "success")
    ),

    # Sentiment - tab_sentiment -----------------------------------------------
    tabItem(
      "tab_sentiment",
      fluidRow(
        # Frontpage - boxes - start -----------------------------------------------
        box(
          width = "3 col-lg-4",
          status = "info",
          title = "Sentiment Distribution",
          withSpinner(plotlyOutput("plot_sent_freq"))
        ),
        box(
          width = "3 col-lg-4",
          status = "danger",
          title = "Word Cloud",
          withSpinner(plotOutput("plot_sent_wordcloud"))
        ),
        box(
          width = "3 col-lg-4",
          status = "success",
          title = "Sentiment by Tweeter Type",
          withSpinner(plotlyOutput("plot_sent_topic"))
        )
        # Frontpage - boxes - end -------------------------------------------------
      ),
      fluidRow(
        # Frontpage - tweet volume plots - start ----------------------------------
        tabBox(
          width = "4 col-lg-10",
          tabPanel(
            status = "primary",
            title = "Tweet Volume",
            withSpinner(plotlyOutput("plot_hourly_sent_volume"))
          ),
          tabPanel(
            status = "primary",
            title = "Tweet Sentiment",
            withSpinner(plotlyOutput("plot_hourly_sent_posneg"))
          ),
          tabPanel(
            status = "success",
            title = "Tweet Sentiment Monthly",
            withSpinner(plotlyOutput("plot_monthly_sent_posneg"))
          ),
          tabPanel(
            status = "success",
            title = "Tweets by Hour of Day",
            withSpinner(plotlyOutput("plot_sent_by_hour"))
          ),
          tabPanel(
            status = "success",
            title = "Tweets by Day of Week",
            withSpinner(plotlyOutput("plot_sent_by_day"))
          ),
          tabPanel(
            status = "success",
            title = "Tweets by Week of Year",
            withSpinner(plotlyOutput("plot_sent_by_week"))
          ),
          tabPanel(
            status = "success",
            title = "Tweets by Month of Year",
            withSpinner(plotlyOutput("plot_sent_by_month"))
          ),
          tabPanel(
            status = "success",
            title = "Tweets by Year",
            withSpinner(plotlyOutput("plot_sent_by_year"))
          )
        ),
        box(
          width = "4 col-lg-2",
          status = "warning",
          title = "Negative-Positive (Last 6 Years)",
          withSpinner(plotlyOutput("plot_sent_years"))
        )
        # Frontpage - tweet volume plots - end ------------------------------------
      ),
      fluidRow(
        column(width = 3, 
          box(
            width = NULL,
            status = "primary",
            title = "Top Negative Tweeters",
            withSpinner(uiOutput("sent_negtweeters"))
          ),
          box(
            width = NULL,
            status = "info",
            title = "Top Positive Tweeters",
            withSpinner(uiOutput("sent_postweeters")),
            helpText("Times positive tweeter was used relative to most positive tweeter")
          )
        ),
        column(width = 3, 
          box(
            width = NULL,
            status = "warning",
            title = "Top Negative Words",
            withSpinner(uiOutput("sent_negative"))
          ),
          box(
            width = NULL,
            status = "success",
            title = "Top Positive Words",
            withSpinner(uiOutput("sent_positive")),
            helpText("Times word was used relative to most negative/positive word")
          )
        ),
        column(width = 3, 
          box(
            width = NULL,
            status = "warning",
            title = "Top Negative Words (Per Tweeter Type)",
            withSpinner(uiOutput("sent_negative_topic"))
          ),
          box(
            width = NULL,
            status = "success",
            title = "Top Positive Words (Per Tweeter Type)",
            withSpinner(uiOutput("sent_positive_topic")),
            helpText("Times tf-idf*1000 was used relative to most negative/positive word")
          )
        ),
        column(width = 3, 
          box(
            width = NULL,
            status = "warning",
            title = "Top Negative Words (Per Year)",
            withSpinner(uiOutput("sent_negative_year"))
          ),
          box(
            width = NULL,
            status = "success",
            title = "Top Positive Words (Per Year)",
            withSpinner(uiOutput("sent_positive_year")),
            helpText("Times tf-idf*1000 was used relative to most negative/positive word")
          )
        )
      )
    ),

      # Tweet Wall - tab_tweet_wall ---------------------------------------------
      tabItem(
        "tab_tweet_wall",
        class = "text-center",
        tags$h1("Tweets about", "bowel cancer"),
        # Tweet Wall - twitter.js and masonry.css - start --------------------
        # twitter.js has to be loaded after the page is loaded (divs exist and jquery is loaded)
        tags$head(HTML(
        '
        <script>
        document.addEventListener("DOMContentLoaded", function(event) {
          var script = document.createElement("script");
          script.type = "text/javascript";
          script.src  = "twitter.js";
          document.getElementsByTagName("head")[0].appendChild(script);
        });
        </script>
        ')),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "masonry.css")),
        # Tweet Wall - twitter.js and masonry.css - end ----------------------
        fluidRow(
          column(
            # Tweet Wall - Controls - start -------------------------------------------
            12,
            class = "col-md-8 col-md-offset-2 col-lg-6 col-lg-offset-3",
            tags$form(
              class = "form-inline",
              tags$div(
                class = "form-group",
                tags$div(
                  class = "btn-toolbar btn-group-sm",
                  dateRangeInput("tweet_wall_daterange", "",
                                 start = "2022-07-11", end = "2022-07-11",
                                 min = "2017-07-11", max = "2022-07-11",
                                 weekstart = 1, separator = " to "),
                  shinyThings::dropdownButtonUI("tweet_wall_date_presets",
                                                TWEET_WALL_DATE_INPUTS,
                                                class = "btn-default")
                )
              )
            )
            # Tweet Wall - Controls - end ---------------------------------------------
          ),
          shinyThings::paginationUI("tweet_wall_pager", width = 12, offset = 0)
        ),
        withSpinner(uiOutput("tweet_wall_tweets")),
        shinyThings::pagerUI("tweet_wall_pager", centered = TRUE)
      ),

      # Explore - tab_explore ---------------------------------------------------
      tabItem(
        "tab_explore",
        fluidRow(
          tweetExplorerUI("tweet_table", status = "success")
        )
      )
    )
  )
)


