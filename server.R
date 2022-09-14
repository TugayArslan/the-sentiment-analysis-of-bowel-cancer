#--------------------- Server ----------------------#

#get_sentiments("afinn")
#get_sentiments("bing")
#get_sentiments("nrc")

#options(error=browser)
set.seed(1234)

BASIC_COLORS <- c("primary", "info", "success", "danger", "warning")
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
TZ_GLOBAL <- "Europe/London"
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
    "in_2017"        = c(start = ymd("2017-07-11"),     end = ymd("2017-12-31")),
    "in_2018"        = c(start = ymd("2018-01-01"),     end = ymd("2018-12-31")),
    "in_2019"        = c(start = ymd("2019-01-01"),     end = ymd("2019-12-31")),
    "in_2020"        = c(start = ymd("2020-01-01"),     end = ymd("2020-12-31")),
    "in_2021"        = c(start = ymd("2021-01-01"),     end = ymd("2021-12-31")),
    "in_2022"        = c(start = ymd("2022-01-01"),     end = ymd("2022-07-11")),
    "since_covid"    = c(start = ymd("2020-01-01"),     end = ymd("2022-07-11")),
    NA
  )
}
get_tweet_blockquote <- function(Url, ..., null_on_error = TRUE, theme = "light") {
  oembed <- list(...)$oembed
  if (!is.null(oembed) && !is.na(oembed)) return(unlist(oembed))
  oembed_url <- glue::glue("https://publish.twitter.com/oembed?url={Url}&omit_script=1&dnt=1&theme={theme}")
  bq <- possibly(httr::GET, list(status_code = 999))(URLencode(oembed_url))
  if (bq$status_code >= 400) {
    if (null_on_error) return(NULL)
    '<blockquote style="font-size: 90%">Sorry, unable to get tweet ¯\\_(ツ)_/¯</blockquote>'
  } else {
    httr::content(bq, "parsed")$html
  }
}
masonify_tweets <- function(tweets, id = NULL, class = NULL) {
  t_embed <-
    tweets %>%
    pmap(get_tweet_blockquote) %>%
    map(HTML) %>%
    map(tags$div, class = "tweet-item")
  
  tagList(
    tags$div(id = id,
             class = paste("masonry text-left", class),
             t_embed
    )
  )
}
twemoji <- function(runes, width = "20px") {
  runes <- tolower(runes)
  runes <- gsub(" ", "-", runes)
  runes <- sub("-fe0f$", "", runes) # seems to cause problems with twemoji :shrug:
  emojis <- glue::glue("https://cdnjs.cloudflare.com/ajax/libs/twemoji/11.2.0/2/svg/{runes}.svg")
  emojis <- glue::glue('<img src="{emojis}" width = "{width}">')
  paste(emojis)
}
#' Progress Bar
#'
#' From: https://github.com/rstudio/shinydashboard/issues/119
progressBar <- function(
    value = 0,
    label = FALSE,
    color = "aqua",
    size = NULL,
    striped = FALSE,
    active = FALSE,
    vertical = FALSE
) {
  stopifnot(is.numeric(value))
  if (value < 0 || value > 100)
    stop("'value' should be in the range from 0 to 100.", call. = FALSE)
  # if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
  #   stop("'color' should be a valid status or color.", call. = FALSE)
  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  text_value <- paste0(value, "%")
  if (vertical)
    style <- htmltools::css(height = text_value, `min-height` = "2em")
  else
    style <- htmltools::css(width = text_value, `min-width` = "2em")
  tags$div(
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    class = if (vertical) "vertical",
    class = if (active) "active",
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      class = if (striped) "progress-bar-striped",
      style = style,
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      tags$span(class = if (!label) "sr-only", text_value)
    )
  )
}
progressGroup <- function(text, value, min = 0, max = value, color = "aqua") {
  stopifnot(is.character(text))
  stopifnot(is.numeric(value))
  if (value < min || value > max)
    stop(sprintf("'value' should be in the range from %d to %d.", min, max), call. = FALSE)
  tags$div(
    class = "progress-group",
    tags$span(class = "progress-text", text),
    tags$span(class = "progress-number", sprintf("%d / %d", value, max)),
    progressBar(round(value / max * 100), color = color, size = "sm")
  )
}
progressBar_v <- function(x, colors) {
  if (length(colors) > length(x)) {
    colors <- rep(colors, ceiling(length(x)/length(colors)))
  }
  x <- purrr::map2(x, colors[seq_along(x)], ~ progressBar(.x, color = .y))
  map_chr(x, paste)
}
cache_profile_image <- function(profile_image_url, location = "www", default = "twitter-default-profile.jpg") {
  file_serve <- str_replace(profile_image_url, ".+/profile", "profile")
  file_local <- fs::path(location, file_serve)
  if (fs::file_exists(file_local)) {
    x <- list(result = file_serve)
  } else {
    fs::dir_create(fs::path_dir(file_local))
    x <- safely(download.file)(profile_image_url, file_local)
    # On fist download, the image won't be ready for the UI, so show default
    if (is.null(x$error)) x$result <- default
  }
  if (is.null(x$error)) x$result else default
}
tweetExplorer <- function(input, output, session, all_tweets, tzone = TZ_GLOBAL) {
  ns <- session$ns

  top_hashtags <- reactive({
    all_tweets() %>%
      select(Hastags) %>%
      mutate(tags = str_replace_all(Hastags, "\\[|\\]", "")) %>%
      mutate(keywords = gsub("\'","", tags)) %>%
      mutate(keywords = gsub(", ",",", keywords)) %>%
      mutate(Hastags = strsplit(keywords, ",")) %>%
      unnest(Hastags) %>%
      mutate(Hastags = tolower(Hastags)) %>%
      count(Hastags, sort = TRUE) %>%
      filter(!is.na(Hastags)) %>%
      filter(!str_detect(tolower(Hastags), "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer")) %>%
      slice(1:25) %>%
      pull(Hastags)
  })

  tweets <- reactive({
    screen_words <- "([Aa]wareness|[Ss]creen|[Dd]iagnos|[Tt]est|[Dd]etect|[Ss]ymptoms)"
    covid_words <- "([Cc]orona|[Cc]ovid|[Cc]ovid19|[Pp]andemi|[Ee]pidemi|[Ll]ockdown|[Qq]uarantine)"

    all_tweets <- all_tweets() %>%
      mutate(
        relates_screen = str_detect(Text, screen_words),
        relates_covid = str_detect(Text, covid_words),
        Hastags = map(Hastags, tolower)
      )

    x <- switch(
      input$view,
      'Popular' = all_tweets %>%
        arrange(desc(Retweet.Count + Like.Count)),#-map_int(mentions_screen_name, length)),
      'Screening' = all_tweets %>% filter(relates_screen),
      'Covid/Corona' = all_tweets %>% filter(relates_covid),
      all_tweets
    )

    if (input$view %in% c('All', 'Popular')) {
      if (length(input$filter_binary)) {
        for (filter_binary in input$filter_binary) {
          x <- switch(
            filter_binary,
            # 'Not Retweet' = filter(x, !is_retweet),
            'Quoted' = filter(x, Quote.Count > 0),
            'Replied' = filter(x, Reply.Count > 0),
            "Retweeted" = filter(x, Retweet.Count > 0),
            "Favorited" = filter(x, Like.Count > 0),
            x
          )
        }
      }
      if (length(input$filter_hashtag)) {
        x <- filter(x, !is.null(Hashtags))
        for (filter_hashtag in input$filter_hashtag) {
          x <- filter(x, map_lgl(Hashtags, function(h) filter_hashtag %in% h))
        }
      }
    }
    x
  })

  output$help_text <- renderUI({
    req(input$view)
    switch(
      input$view,
      'Popular' = helpText(HTML("&#x1F4AF;"),  "Most popular (retweets + favs) first"),
      'Screening' = helpText(HTML("&#x1F5E3;"), "Tweets that mention \"screening\", \"awareness\", etc."),
      'Covid/Corona' = helpText(HTML("&#x26D1;"),  "Tweets that mention \"covid\", \"corona\", etc."),
      'All' = helpText(HTML("&#x1F917;"), "All the tweets"),
      NULL
    )
  })

  hashtags_related <- reactive({
    req(input$view %in% c('All', 'Popular'))
    if (is.null(input$filter_hashtag) || input$filter_hashtag == '') return(top_hashtags())
    limit_to_tags <- related_hashtags %>%
      filter(tag %in% input$filter_hashtag) %>%
      pull(related) %>%
      unique()
    top_hashtags() %>%
      filter(`Top 10 Hashtags` %in% c(limit_to_tags, input$filter_hashtag)) %>%
      pull(`Top 10 Hashtags`)
  })

  output$filters <- renderUI({
    selected_hashtags <- isolate(input$filter_hashtag)
    selected_binary <- isolate(input$filter_binary)
    if (input$view %in% c('All', 'Popular')) {
      tagList(
        checkboxGroupInput(ns('filter_binary'), 'Tweet Filters',
                           choices = c("Quoted", "Replied", "Retweeted", "Favorited"),
                           selected = selected_binary,
                           inline = TRUE),
        selectizeInput(ns('filter_hashtag'), 'Hashtags', choices = c("", hashtags_related()), selected = selected_hashtags,
                       multiple = TRUE, options = list(plugins = list('remove_button')), width = "100%")
      )
    }
  })

  output$tweets <- DT::renderDataTable({
    tweets() %>%
      select(Datetime, Username, Text, Retweet.Count, Like.Count, Reply.Count, Quote.Count)
      #mutate(created_at = strftime(created_at, '%F %T', tz = tzone))
  },
  selection = 'single',
  style = "bootstrap",
  rownames = FALSE,
  colnames = c("Timestamp", "User", "Tweet", "RT", "Fav", "Reply", "Quote"),
  filter = 'top',
  options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 10)
  )

  output$tweet <- renderUI({
    req(tweets())
    x <- if (!is.null(input$tweets_rows_selected)) {
      tweets() %>%
        slice(input$tweets_rows_selected) %>%
        pmap_chr(get_tweet_blockquote) %>%
        HTML()
    } else {
      HTML('<blockquote>Choose a tweet from the table...</blockquote>')
    }

    tags$div(class = "tweet-item", x)
  })
}
ukroiExplorer <- function(input, output, session, all_tweets, tzone = TZ_GLOBAL) {
  ns <- session$ns
  
  tweets <- reactive({
    screen_words <- "([Aa]wareness|[Ss]creen|[Dd]iagnos|[Tt]est|[Dd]etect|[Ss]ymptoms)"
    covid_words <- "([Cc]orona|[Cc]ovid|[Cc]ovid19|[Pp]andemi|[Ee]pidemi|[Ll]ockdown|[Qq]uarantine)"
    
    all_tweets <- all_tweets() %>%
      mutate(
        relates_screen = str_detect(Text, screen_words),
        relates_covid = str_detect(Text, covid_words),
        Hastags = map(Hastags, tolower)
      )
    
    x <- switch(
      input$view_ukroi,
      'All' = all_tweets %>% filter(Place %in% c("England", "Scotland", "Wales", "Northern Ireland", "Republic of Ireland")),
      'United Kingdom' = all_tweets %>% filter(Place %in% c("England", "Scotland", "Wales", "Northern Ireland")),
      'England' = all_tweets %>% filter(Place == "England"),
      'Scotland' = all_tweets %>% filter(Place == "Scotland"),
      'Wales' = all_tweets %>% filter(Place == "Wales"),
      'Northern Ireland' = all_tweets %>% filter(Place == "Northern Ireland"),
      'Republic of Ireland' = all_tweets %>% filter(Place == "Republic of Ireland"),
      all_tweets
    )
    
    if (length(input$filter_binary_ukroi)) {
      for (filter_binary in input$filter_binary_ukroi) {
        x <- switch(
          filter_binary,
          # 'Not Retweet' = filter(x, !is_retweet),
          'Quoted' = filter(x, Quote.Count > 0),
          'Replied' = filter(x, Reply.Count > 0),
          "Retweeted" = filter(x, Retweet.Count > 0),
          "Favorited" = filter(x, Like.Count > 0),
          'Screening' = filter(x, relates_screen),
          'Covid/Corona' = filter(x, relates_covid),
          x
        )
      }
    }
    x
  })
  
  output$help_text_ukroi <- renderUI({
    req(input$view_ukroi)
    switch(
      input$view_ukroi,
      'United Kindom' = helpText("All the tweets within United Kingdom"),
      'England' = helpText("Tweets from England"),
      'Scotland' = helpText("Tweets from Scotland"),
      'Wales' = helpText("Tweets from Wales"),
      'Northern Ireland' = helpText("Tweets from Northern Ireland"),
      'Republic of Ireland' = helpText("Tweets from Republic of Ireland"),
      'All' = helpText("All the tweets"),
      NULL
    )
  })
  
  output$filter_text_ukroi <- renderUI({
    req(input$filter_binary_ukroi)
    switch(
      input$filter_binary_ukroi,
      'Quoted' = helpText("Tweets with > 0 quotes."),
      'Replied' = helpText("Tweets with > 0 replies."),
      'Retweeted' = helpText("Tweets with > 0 retweets."),
      'Favorited' = helpText("Tweets with > 0 favorites."),
      'Screening' = helpText("Tweets that has awareness/screen/diagnos/test/detect/symptoms words."),
      'Covid/Corona' = helpText("Tweets that has corona/covid/pandemi/epidemi/lockdown/quarantine words."),
      NULL
    )
  })
  
  output$total_today_ukroi <- renderValueBox({
    valueBox(
      tweets() %>% count(), "Total Tweets", icon = icon("comment-dots"),
      color = "purple"
    )
  })
  output$tweeters_today_ukroi <- renderValueBox({
    valueBox(
      tweets() %>% distinct(Username) %>% count(), "Total Tweeters", icon = icon("user-circle"),
      color = "orange"
    )
  })
  output$total_favorites_ukroi <- renderValueBox({
    valueBox(
      tweets() %>% pull(Like.Count) %>% sum(), "Total Likes", icon = icon("heart"),
      color = "green"
    )
  })
  output$total_topic_ukroi <- renderValueBox({
    valueBox(
      tweets() %>% pull(Retweet.Count) %>% sum(), "Total Retweets", icon = icon("retweet"),
      color = "red"
    )
  })
  output$total_all_ukroi <- renderValueBox({
    valueBox(
      tweets() %>% pull(Reply.Count) %>% sum(), "Total Replies", icon = icon("comments"),
      color = "teal"
    )
  })
  output$rate_ukroi <- renderValueBox({
    valueBox(
      tweets() %>% pull(Quote.Count) %>% sum(), "Total Quotes", icon = icon("quote-right"),
      color = "fuchsia"
    )
  })
  
  output$dash_most_liked_ukroi <- renderUI({
    tweets() %>%
      arrange(desc(Like.Count)) %>%
      slice(1) %>%
      pmap(get_tweet_blockquote) %>%
      .[[1]] %>%
      HTML()
  })
  output$dash_most_rt_ukroi <- renderUI({
    tweets() %>%
      arrange(desc(Retweet.Count)) %>%
      slice(1) %>%
      pmap(get_tweet_blockquote) %>%
      .[[1]] %>%
      HTML()
  })
  output$dash_most_recent_ukroi <- renderUI({
    tweets() %>%
      arrange(desc(Reply.Count)) %>%
      slice(1) %>%
      pmap(get_tweet_blockquote) %>%
      .[[1]] %>%
      HTML()
  })
  
  output$top_emojis_ukroi <- renderUI({
    emoji_regex <- "[\\uD83C-\\uDBFF\\uDC00-\\uDFFF\u2600-\u27ff]+"
    
    twe <- tweets() %>%
      select(Text) %>%
      tidytext::unnest_tokens(Text, Text, token = "tweets") %>%
      filter(str_detect(Text, emoji_regex)) %>%
      mutate(Text = str_remove_all(Text, "\\w")) %>%
      tidytext::unnest_tokens(Text, Text, token = "characters") %>%
      count(Text, sort = TRUE) %>%
      inner_join(emo::jis %>% select(runes, emoji, name), by = c("Text" = "emoji")) %>%
      filter(!str_detect(runes, "^1F3F[B-F]$")) %>%
      slice(1:10) %>%
      mutate(
        b = n,
        # use twemoji
        runes = str_replace(tolower(runes), " ", "-"),
        runes = twemoji(runes)
      )
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(twe))), ~ {
        progressGroup(HTML(twe$runes[[.x]]), twe$n[[.x]], max = max(twe$n), color = colors[.x])
      })
    )
  })
  output$top_hashtags_ukroi <- renderUI({
    twh <-
      tweets() %>%
      select(Hastags) %>%
      mutate(tags = str_replace_all(Hastags, "\\[|\\]", "")) %>%
      mutate(keywords = gsub("\'","", tags)) %>%
      mutate(keywords = gsub(", ",",", keywords)) %>%
      mutate(hastag = strsplit(keywords, ",")) %>%
      unnest(hastag) %>%
      mutate(hastag = tolower(hastag)) %>%
      count(hastag, sort = TRUE) %>%
      filter(!is.na(hastag)) %>%
      filter(!str_detect(tolower(hastag), "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer")) %>%
      mutate(hastag = paste0("#", hastag))
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(twh))), ~ {
        progressGroup(twh$hastag[[.]], twh$n[[.]], max = max(twh$n), color = colors[.])
      })
    )
  })
  output$top_tweeters_ukroi <- renderUI({
    tweets() %>%
      group_by(Username) %>%
      summarize(engagement = (sum(Retweet.Count) * 2 + sum(Like.Count))) %>%
      arrange(desc(engagement)) %>%
      ungroup() %>%
      slice(1:10) %>%
      mutate(
        engagement = scale(engagement, center = FALSE),
        engagement = engagement / max(engagement) * 100,
        profile_image_url = glue::glue("https://twitter.com/{Username}/profile_image?size=mini"),
        profile_image = map_chr(profile_image_url, cache_profile_image),
        profile_image_url = glue::glue('<div class="center-block"><img class="img-responsive img-circle" src="{profile_image}" alt={Username} style="max-height: 25px; min-width: 20px;"></div>'),
        profile_url = glue::glue("https://twitter.com/{Username}"),
        Username = glue::glue('<a href="{profile_url}" target="_blank">@{Username}</a>'),
        engagement = progressBar_v(engagement, rep(BASIC_COLORS[1:5], 2))
      ) %>%
      select(profile_image_url, Username, engagement) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("", "Screen Name", "Engagement"),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })
  output$top_tweet_words_ukroi <- renderUI({
    tw <- tweets() %>%
      select(Text) %>%
      mutate(
        Text = str_remove_all(Text, "@[[:alnum:]_]+\\b"),
        Text = str_remove_all(Text, "&\\w+;")
      ) %>%
      mutate(Text = tolower(Text)) %>%
      mutate(Text = gsub("@\\S+", " ", Text, perl=T)) %>%
      mutate(Text = gsub("#\\S+", " ", Text, perl=T)) %>%
      tidytext::unnest_tokens(word, Text) %>%
      filter(
        !word %in% c("http", "https", "t.co"),
        !str_detect(word, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
        nchar(word) >= 3
      ) %>%
      anti_join(tidytext::stop_words, by = "word") %>%
      count(word, sort = TRUE) %>%
      slice(1:10)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(tw$word[[.]], tw$n[[.]], max = max(tw$n), color = colors[.])
      })
    )
  })
}
removeMention <- function(x) gsub("@\\S+", " ", x, perl=T)
removeHashtag <- function(x) gsub("#\\S+", " ", x, perl=T)
removeURL <- function(x) gsub("(f|ht)tp(s?)://\\S+", " ", x, perl=T)
replacePunctuation <- function(x) gsub("[[:punct:]]", " ", x)

function(session, input, output) {
  tweets <- read.csv(file = './df_detailed.csv')
  tweets_ukroi <- read.csv(file = './df_ukireland.csv')
  
  tweets_most <- reactive({
    tweets %>%
      filter(between(as.Date(Datetime), as.Date('2017-07-11'), as.Date('2022-07-11'))) %>%
      filter(File != "pro-tweets-detailed.csv")
  })
  
  # tweets_sent <- tweets %>%
  #     filter(between(as.Date(Datetime), as.Date('2017-07-11'), as.Date('2022-07-11'))) %>%
  #     filter(File != "pro-tweets-detailed.csv") %>%
  #     select(-c("Conv..Id", "Language", "Retweeted.Tweet", "Quoted.Tweet", "Mentioned.Users", "Replied.Tweet", "Replied.User")) %>%
  #     mutate(DateHour = format(ymd_hms(Datetime), format="%Y-%m-%d %H:00")) %>%
  #     mutate(Hour = format(ymd_hms(Datetime), format="%H")) %>%
  #     mutate(Date = as.Date(Datetime)) %>%
  #     mutate(Day = lubridate::wday(Date, label=TRUE, abbr=FALSE)) %>%
  #     mutate(Week = lubridate::week(Date)) %>%
  #     mutate(Month = lubridate::month(Date, label=TRUE, abbr=FALSE)) %>%
  #     mutate(Year = lubridate::year(Date)) %>%
  #     mutate(type = str_split(File, "-", simplify = TRUE)[ , 1]) %>%
  #     mutate(topic = ifelse(type %in% c("belfast", "hashtag", "text"), "people", type))
  # 
  # tweets_freq <- reactive({
  #   temp <- tweets_sent %>%
  #     mutate(
  #       Text = str_remove_all(Text, "@[[:alnum:]_]+\\b"),
  #       Text = str_remove_all(Text, "&\\w+;")
  #     ) %>%
  #     mutate(Text = tolower(Text)) %>%
  #     mutate(Text = gsub("@\\S+", " ", Text, perl=T)) %>%
  #     mutate(Text = gsub("#\\S+", " ", Text, perl=T)) %>%
  #     mutate(Text = gsub("(f|ht)tp(s?)://\\S+", " ", Text, perl=T)) %>%
  #     mutate(Text = str_replace_all(Text, "’", "'")) %>%
  #     # See https://www.thoughtco.com/contractions-commonly-used-informal-english-1692651
  #     mutate(Text = str_replace_all(Text, "ain't", "not")) %>%
  #     mutate(Text = str_replace_all(Text, "can't", "not")) %>%
  #     mutate(Text = str_replace_all(Text, "won't", "not")) %>%
  #     mutate(Text = str_replace_all(Text, "shan't", "not")) %>%
  #     mutate(Text = str_replace_all(Text, "n't", " not ")) %>%
  #     mutate(Text = str_replace_all(Text, "covid19|covid2019", "covid")) %>%
  #     mutate(Text = str_replace_all(Text, "united kingdom", "unitedkingdom")) %>%
  #     mutate(Text = str_replace_all(Text, "northern ireland", "northernireland")) %>%
  #     tidytext::unnest_tokens(word, Text) %>%
  #     mutate(word = str_replace_all(word, "b'day", "birthday")) %>%
  #     mutate(word = str_replace_all(word, "o'clock", "clock")) %>%
  #     filter(
  #       !word %in% c("http", "https", "t.co"),
  #       !word %in% c("i.e", "e.g", "rt"),
  #       !str_detect(word, "^(\\d)"),
  #       !str_detect(word, ".com|.net|.uk|.org"),
  #       !str_detect(word, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
  #       nchar(word) >= 2
  #     ) %>%
  #     anti_join(tidytext::stop_words, by = "word") %>%
  #     mutate(word = str_replace_all(word, "'s|'ve", "")) %>%
  #     mutate(word = str_replace_all(word, "\\.|,", "")) %>%
  #     mutate(word = case_when(
  #       word %in% c("dr") ~ "doctor",
  #       word == "avg" ~ "average",
  #       word %in% c("yrs", "yr") ~ "year",
  #       word == "fav" ~ "favourite",
  #       word == "pls" ~ "please",
  #       word == "dm" ~ "message",
  #       word == "st" ~ "saint",
  #       word %in% c("ni", "ní", "northernireland") ~ "northern ireland",
  #       word %in% c("uk", "unitedkingdom") ~ "united kingdom",
  #       word %in% c("eu") ~ "europe",
  #       TRUE ~ word
  #     )) %>%
  #     filter(!str_detect(word, "[[:punct:]]"))
  #   
  #   comp_dict <- unique(temp$word)
  #   temp <- temp %>%
  #     mutate(sword = ifelse(nchar(word) >= 3, stemDocument(word), word))
  # 
  #   for (row in 1:nrow(temp)) {
  #     word <- NA
  #     if (nchar(temp[row, "word"]) >= 3) {
  #       word <- stemCompletion(temp[row, "sword"], comp_dict)
  #       temp[row, "fword"] <- word
  #     }
  #     if (is.na(word)) {
  #       temp[row, "fword"] <- temp[row, "word"]
  #     }
  #     print(paste("Row: ", row))
  #   }
  #   temp
  # })
  
  tweets_sfreq <- reactiveFileReader(1000, session, './df_nistemtext.csv', read.csv)
  tweets_nisword <- reactiveFileReader(1000, session, './df_nistemword.csv', read.csv)
  tweets_ni <- reactiveFileReader(1000, session, './df_nistem.csv', read.csv)
  
  tweets_sfreq_old <- reactive({
    #data <- tweets_ni %>% group_by(Tweet.Id) %>% mutate(ftext = paste0(fword, collapse = " ")) %>% distinct(Tweet.Id, .keep_all=TRUE)
    #temp <- get_nrc_sentiment(data$ftext) %>% mutate_at(vars(1:10), as.integer)
    #s <- cbind(data, temp)
    #s
    tweets_nistext <- read.csv(file = './df_nistemtext.csv')
    tweets_nistext
  })
  
  tweets_snfreq_old <- reactive({
    #tweets_nisword <- read.csv(file = './df_nistemword.csv')
    #temp <- get_nrc_sentiment(tweets_ni$fword) %>% mutate_at(vars(1:10), as.integer)
    #s <- cbind(tweets_ni, temp)
    #s
    data <- tweets_nisword %>% group_by(fword) %>% summarise(freq = n(), score = sum(negative)) %>% filter(score > 0)
    n <- data[order(data$freq, decreasing = TRUE), ]
    n
  })
  
  tweets_spfreq_old <- reactive({
    #tweets_nisword <- read.csv(file = './df_nistemword.csv')
    #temp <- get_nrc_sentiment(tweets_ni$fword) %>% mutate_at(vars(1:10), as.integer)
    #s <- cbind(tweets_ni, temp)
    #s
    data <- tweets_nisword %>% group_by(fword) %>% summarise(freq = n(), score = sum(positive)) %>% filter(score > 0)
    n <- data[order(data$freq, decreasing = TRUE), ]
    n
  })
  
  tweets_wfreq_old <- reactive({
    tweets_ni <- read.csv(file = './df_nistem.csv')
    data <- tweets_ni %>% count(fword) %>% rename(freq = n)
    w <- data[order(data$freq, decreasing = TRUE), ]
    w
  })
  
  tweets_most_ukroi <- reactive({
    tweets_ukroi %>%
      filter(between(as.Date(Datetime), as.Date('2017-07-11'), as.Date('2022-07-11'))) %>%
      filter(Username != "cjokiss1" & Username != "k_wiesendanger") %>%
      mutate(Place = case_when(
        grepl("england|london|manchester|liverpool|birmingham|york|newcastle|caster|chester|cester|upon|midland|wessex|kent|sussex|essex|plymouth|surrey|swindon|luton|bath|nottingham|leeds|sheffield|cornwall|bristol|derby|brighton|devon|exeter|windsor|cheshire|hampshire|bradford|lincolnshire|dorset|oxford|cambridge|berkshire|norfolk|suffolk|lancashire|durham|salford|cumbria|eastleigh|shrewsbury|southampton|thatcham|wallasey|richmond|buckinghamshire|hertfordshire|somerset|maidenhead|staffordshire|wirral|warwickshire|northumberland|skelmersdale|wiltshire|norwich|milton keynes|merseyside|shropshire|hackney|hampton|thornton|tyneside|stoke|stockport|peterborough|coventry|billingham", Location, ignore.case = TRUE) ~ "England",
        grepl("scotland|dundee|glasgow|aberdeen|edinburgh", Location, ignore.case = TRUE) ~ "Scotland",
        grepl("wales|aberystwyth|cardiff|swansea|newport", Location, ignore.case = TRUE) ~ "Wales",
        grepl("n.i|n.i.|northern ireland|antrim|newtonabbey|n.ireland|belfast|Béal Feirste|lisburn|derry|londonderry|armagh|newry", Location, ignore.case = TRUE) ~ "Northern Ireland",
        grepl("ireland|republic of ireland|dublin|Louth|Cork|galway", Location, ignore.case = TRUE) ~ "Republic of Ireland",
        grepl("united kingdom|u.k.|u.k|isle|bailiwick", Location, ignore.case = TRUE) ~ "Other UK",
        grepl("united states|u.s.|u.s|usa|u.s.a.|u.s.a|america|texas|utah|vermont|etatsunis|carolina|waukesha|california|mechanicsburg|waconia|ontario|niagara|florida|michigan|boston|santa monica|phoenix|san antonio|manitoba|milwaukee|virginia|bay area|pennsylvania|los angeles|philadelphia", Location, ignore.case = TRUE) ~ "US",
        grepl("europe|nederland|nijmegen|venice|napoli|italy|greece|nicosia|agios|netherlands|belgium|spain|sweden|france|germany|alemania|berlin|paris|madrid|catalonia|barcelona|munich|polska|españa|romania|slovenia", Location, ignore.case = TRUE) ~ "EU",
        grepl("philippines|nicaragua|manila|tanzania|whadjuk|lagos|nigeria|australia|tasmania|melbourne|canada|winnipeg|vancouver|mexico|taiwan|emirates|trinidad|africa|ukraine", Location, ignore.case = TRUE) ~ "Other",
        TRUE ~ "Unknown"
      ))
  })
  
  # # Dashboard Boxes ---------------------------------------------------------
  output$total_today <- renderValueBox({
    valueBox(
      tweets_most() %>% count(), "Total Tweets", icon = icon("comment-dots"),
      color = "purple"
    )
  })
  output$tweeters_today <- renderValueBox({
    valueBox(
      tweets_most() %>% distinct(Username) %>% count(), "Total Tweeters", icon = icon("user-circle"),
      color = "orange"
    )
  })
  output$total_favorites <- renderValueBox({
    valueBox(
      tweets_most() %>% pull(Like.Count) %>% sum(), "Total Likes", icon = icon("heart"),
      color = "green"
    )
  })
  output$total_topic <- renderValueBox({
    valueBox(
      tweets_most() %>% pull(Retweet.Count) %>% sum(), "Total Retweets", icon = icon("retweet"),
      color = "red"
    )
  })
  output$total_all <- renderValueBox({
    valueBox(
      tweets_most() %>% pull(Reply.Count) %>% sum(), "Total Replies", icon = icon("comments"),
      color = "teal"
    )
  })
  output$rate <- renderValueBox({
    valueBox(
      tweets_most() %>% pull(Quote.Count) %>% sum(), "Total Quotes", icon = icon("quote-right"),
      color = "fuchsia"
    )
  })

  # # Dashboard Plots ---------------------------------------------------------
  output$plot_hourly_tweet_volume <- renderPlotly({
    #validate(
    #  need(nrow(tweets_most()) > 0,
    #       "No tweets"
    #))

    tweets_most() %>%
      #tweets_just(created_at, is_topic) %>%
      #group_by(is_topic) %>%
      #tweets_volume() %>%
      #mutate(topic = if_else(is_topic, "topic", "all")) %>%
      #ungroup() %>%
      #rename(Date = by_time) %>%
      #select(-is_topic) %>%
      #spread(topic, n, fill = 0) %>%
      mutate(Date = as.Date(Datetime)) %>%
      #mutate(Date = lubridate::floor_date(as.POSIXct(Datetime), "1 hour")) %>%
      #mutate(Date = format(ymd_hms(Datetime), format="%Y-%m-%d %H:00")) %>%
      mutate(type = str_split(File, "-", simplify = TRUE)[ , 1]) %>%
      #mutate(topic = case_when(topic %in% c("belfast", "hashtag", "text") ~ "people",
      #                         TRUE                             ~ topic)) %>%
      mutate(topic = ifelse(type %in% c("belfast", "hashtag", "text"), "people", type)) %>%
      group_by(topic) %>%
      count(Date) %>%
      #ungroup() %>%
      #spread(topic, n, fill = 0) %>%
      plot_ly(x = ~ Date, y = ~ n, color = ~ topic, type = "scatter", mode = 'lines+markers') %>%
      #add_lines(y = ~topic, name = "", color = I(ADMINLTE_COLORS$teal)) %>%
      #{
      #  if (!is.null("#rstats")) {
      #    add_lines(., y = ~all, name = "#rstats", color = I(ADMINLTE_COLORS$purple))
      #  } else .
      #}%>%
      #config(displayModeBar = FALSE) %>%
      layout(
        xaxis = list(
          range = c(as.POSIXct('2017-07-11 00:00:00'), as.POSIXct('2022-07-11 00:00:00')),
          rangeselector = list(
            buttons = list(
              list(
                count = 1,
                label = "Today",
                step = "day",
                stepmode = "todate"),
              list(
                count = 2,
                label = "Yesterday",
                step = "day",
                stepmode = "backward"),
              list(
                count = 7,
                label = "Week",
                step = "day",
                stepmode = "backward"),
              list(
                count = 1,
                label = "Month",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 Year",
                step = "year",
                stepmode = "backward"),
              list(
                count = 2,
                label = "2 Year",
                step = "year",
                stepmode = "backward"),
              list(
                count = 3,
                label = "3 Year",
                step = "year",
                stepmode = "backward"),
              list(
                count = 4,
                label = "4 Year",
                step = "year",
                stepmode = "backward"),
              list(step = "all", label = "All"))),
          rangeslider = list(type = "date")),
        yaxis = list(title = "Tweets"),
        legend = list(orientation = 'h', x = 0.05, y = 0.9),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      ) %>%
      config(cloud = FALSE, mathjax = NULL)
  })

  output$plot_tweets_by_hour <- renderPlotly({
    #validate(
    #  need(nrow(tweets_most()) > 0,
    #       "No tweets"
    #))
    temp <- rbind(tweets_most(), tweets_most() %>% mutate(File = "total-tweets-detailed.csv"))
    
    temp %>%
      #tweets_just(created_at, is_topic) %>%
      #tweets_by_time(by = "1 hour") %>%
      #mutate(hour = hour(by_time)) %>%
      #group_by(hour, is_topic) %>%
      #count() %>%
      #ungroup() %>%
      #mutate(topic = if_else(is_topic, "topic", "all")) %>%
      #select(-is_topic) %>%
      #spread(topic, n, fill = 0) %>%
      #mutate(Hour = format(ymd_hms(Datetime), format="%H")) %>%
      #mutate(Date = as.Date(Datetime)) %>%
      mutate(Date = format(ymd_hms(Datetime), format="%Y-%m-%d %H:00:00")) %>%
      mutate(type = str_split(File, "-", simplify = TRUE)[ , 1]) %>%
      #mutate(topic = case_when(topic %in% c("belfast", "hashtag", "text") ~ "people",
      #                         TRUE                             ~ topic)) %>%
      mutate(topic = ifelse(type %in% c("belfast", "hashtag", "text"), "people", type)) %>%
      #count(topic, Hour) %>%
      count(topic, Date) %>%
      mutate(Hour = format(ymd_hms(Date), format="%H")) %>%
      mutate(Date = as.Date(Date)) %>%
      spread(Hour, n, fill = 0) %>%
      gather("Hour", "n", -topic, -Date) %>%
      group_by(topic, Hour) %>% 
      summarise(mean = round(mean(n), 3), total = sum(n), sd = round(sd(n), 3)) %>% 
      #spread(Hour, n, fill = 0) %>%
      #adorn_totals(c("row","col")) %>%
      #adorn_percentages("row") %>% 
      #adorn_pct_formatting(rounding = "half up", digits = 0) %>%
      #adorn_ns() %>%
      #gather("Hour", "n", -topic, -Total) %>%
      #mutate(freq = as.integer(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      plot_ly(x = ~ Hour, y = ~ mean, color = ~ topic, type = "bar", text = ~ total) %>%
      #add_bars(y = ~topic, name = "useR!2019", color = I(ADMINLTE_COLORS$teal)) %>%
      #plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Average # of Tweets"),
        xaxis = list(title = glue::glue("Hour of the Day ({TZ_GLOBAL})")),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_tweets_by_day <- renderPlotly({
    #validate(
    #  need(nrow(tweets_most()) > 0,
    #       "No tweets"
    #))
    temp <- rbind(tweets_most(), tweets_most() %>% mutate(File = "total-tweets-detailed.csv"))
    
    temp %>%
      #tweets_just(created_at, is_topic) %>%
      #tweets_by_time(by = "1 hour") %>%
      #mutate(hour = hour(by_time)) %>%
      #group_by(hour, is_topic) %>%
      #count() %>%
      #ungroup() %>%
      #mutate(topic = if_else(is_topic, "topic", "all")) %>%
      #select(-is_topic) %>%
      #spread(topic, n, fill = 0) %>%
      mutate(Date = as.Date(Datetime)) %>%
      mutate(type = str_split(File, "-", simplify = TRUE)[ , 1]) %>%
      #mutate(topic = case_when(topic %in% c("belfast", "hashtag", "text") ~ "people",
      mutate(topic = ifelse(type %in% c("belfast", "hashtag", "text"), "people", type)) %>%
      count(topic, Date) %>%
      mutate(Day = lubridate::wday(Date, label=TRUE, abbr=FALSE)) %>%
      spread(Day, n, fill = 0) %>%
      #adorn_totals(c("row","col")) %>%
      #adorn_percentages("row") %>% 
      #adorn_pct_formatting(rounding = "half up", digits = 0) %>%
      #adorn_ns() %>%
      gather("Day", "n", -topic, -Date) %>%
      group_by(topic, Day) %>% 
      summarise(mean = round(mean(n), 3), total = sum(n), sd = round(sd(n), 3)) %>% 
      #mutate(freq = as.integer(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      plot_ly(x = ~ Day, y = ~ mean, color = ~ topic, type = "bar", text = ~ total) %>%
      #add_bars(y = ~topic, name = "useR!2019", color = I(ADMINLTE_COLORS$teal)) %>%
      #plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Average # of Tweets"),
        xaxis = list(title = glue::glue("Day of the Week ({TZ_GLOBAL})"), categoryorder = "array", categoryarray = c("Monday", 
                                                                                                                     "Tuesday", 
                                                                                                                     "Wednesday",
                                                                                                                     "Thursday",
                                                                                                                     "Friday",
                                                                                                                     "Saturday",
                                                                                                                     "Sunday")
        ),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_tweets_by_week <- renderPlotly({
    #validate(
    #  need(nrow(tweets_most()) > 0,
    #       "No tweets"
    #))
    temp <- rbind(tweets_most(), tweets_most() %>% mutate(File = "total-tweets-detailed.csv"))
    
    temp %>%
      #tweets_just(created_at, is_topic) %>%
      #tweets_by_time(by = "1 hour") %>%
      #mutate(hour = hour(by_time)) %>%
      #group_by(hour, is_topic) %>%
      #count() %>%
      #ungroup() %>%
      #mutate(topic = if_else(is_topic, "topic", "all")) %>%
      #select(-is_topic) %>%
      #spread(topic, n, fill = 0) %>%
      mutate(Date = format(as.Date(Datetime), format="%Y")) %>%
      mutate(Week = lubridate::week(as.Date(Datetime))) %>%
      mutate(type = str_split(File, "-", simplify = TRUE)[ , 1]) %>%
      #mutate(topic = case_when(topic %in% c("belfast", "hashtag", "text") ~ "people",
      mutate(topic = ifelse(type %in% c("belfast", "hashtag", "text"), "people", type)) %>%
      count(topic, Date, Week) %>%
      spread(Week, n, fill = 0) %>%
      #adorn_totals(c("row","col")) %>%
      #adorn_percentages("row") %>% 
      #adorn_pct_formatting(rounding = "half up", digits = 0) %>%
      #adorn_ns() %>%
      gather("Week", "n", -topic, -Date) %>%
      group_by(topic, Week) %>% 
      summarise(mean = round(mean(n), 3), total = sum(n), sd = round(sd(n), 3)) %>%
      #mutate(freq = as.integer(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      plot_ly(x = ~ Week, y = ~ mean, color = ~ topic, type = "bar", text = ~ total) %>%
      #add_bars(y = ~topic, name = "useR!2019", color = I(ADMINLTE_COLORS$teal)) %>%
      #plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Average # of Tweets"),
        xaxis = list(title = glue::glue("Week of the Year ({TZ_GLOBAL})"), categoryorder = "array", categoryarray = array(seq(1, 53, by=1))
        ),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_tweets_by_month <- renderPlotly({
    #validate(
    #  need(nrow(tweets_most()) > 0,
    #       "No tweets"
    #))
    temp <- rbind(tweets_most(), tweets_most() %>% mutate(File = "total-tweets-detailed.csv"))
    
    temp %>%
      #tweets_just(created_at, is_topic) %>%
      #tweets_by_time(by = "1 hour") %>%
      #mutate(hour = hour(by_time)) %>%
      #group_by(hour, is_topic) %>%
      #count() %>%
      #ungroup() %>%
      #mutate(topic = if_else(is_topic, "topic", "all")) %>%
      #select(-is_topic) %>%
      #spread(topic, n, fill = 0) %>%
      mutate(Date = format(as.Date(Datetime), format="%Y")) %>%
      mutate(Month = lubridate::month(as.Date(Datetime), label=TRUE, abbr=FALSE)) %>%
      mutate(type = str_split(File, "-", simplify = TRUE)[ , 1]) %>%
      #mutate(topic = case_when(topic %in% c("belfast", "hashtag", "text") ~ "people",
      mutate(topic = ifelse(type %in% c("belfast", "hashtag", "text"), "people", type)) %>%
      count(topic, Date, Month) %>%
      spread(Month, n, fill = 0) %>%
      #adorn_totals(c("row","col")) %>%
      #adorn_percentages("row") %>% 
      #adorn_pct_formatting(rounding = "half up", digits = 0) %>%
      #adorn_ns() %>%
      gather("Month", "n", -topic, -Date) %>%
      group_by(topic, Month) %>% 
      summarise(mean = round(mean(n), 3), total = sum(n), sd = round(sd(n), 3)) %>%
      #mutate(freq = as.integer(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      plot_ly(x = ~ Month, y = ~ mean, color = ~ topic, type = "bar", text = ~ total) %>%
      #add_bars(y = ~topic, name = "useR!2019", color = I(ADMINLTE_COLORS$teal)) %>%
      #plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Average # of Tweets"),
        xaxis = list(title = glue::glue("Month of the Year ({TZ_GLOBAL})"), categoryorder = "array", categoryarray = c("January", 
                                                                                                                     "February", 
                                                                                                                     "March",
                                                                                                                     "April",
                                                                                                                     "May",
                                                                                                                     "June",
                                                                                                                     "July",
                                                                                                                     "August",
                                                                                                                     "September",
                                                                                                                     "October",
                                                                                                                     "November",
                                                                                                                     "December")
                     ),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_tweets_by_year <- renderPlotly({
    #validate(
    #  need(nrow(tweets_most()) > 0,
    #       "No tweets"
    #))
    temp <- rbind(tweets_most(), tweets_most() %>% mutate(File = "total-tweets-detailed.csv"))
    
    temp %>%
      #tweets_just(created_at, is_topic) %>%
      #tweets_by_time(by = "1 hour") %>%
      #mutate(hour = hour(by_time)) %>%
      #group_by(hour, is_topic) %>%
      #count() %>%
      #ungroup() %>%
      #mutate(topic = if_else(is_topic, "topic", "all")) %>%
      #select(-is_topic) %>%
      #spread(topic, n, fill = 0) %>%
      #mutate(Date = format(as.Date(Datetime), format="%Y")) %>%
      mutate(Year = lubridate::year(as.Date(Datetime))) %>%
      mutate(type = str_split(File, "-", simplify = TRUE)[ , 1]) %>%
      #mutate(topic = case_when(topic %in% c("belfast", "hashtag", "text") ~ "people",
      mutate(topic = ifelse(type %in% c("belfast", "hashtag", "text"), "people", type)) %>%
      count(topic, Year) %>%
      spread(Year, n, fill = 0) %>%
      #adorn_totals(c("row","col")) %>%
      #adorn_percentages("row") %>% 
      #adorn_pct_formatting(rounding = "half up", digits = 0) %>%
      #adorn_ns() %>%
      gather("Year", "n", -topic) %>% #, -Total
      group_by(topic, Year) %>% 
      summarise(mean = round(mean(n), 3), total = sum(n)) %>%
      #mutate(freq = as.integer(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      plot_ly(x = ~ Year, y = ~ mean, color = ~ topic, type = "bar", text = ~ total) %>%
      #add_bars(y = ~topic, name = "useR!2019", color = I(ADMINLTE_COLORS$teal)) %>%
      #plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Average # of Tweets"),
        xaxis = list(title = glue::glue("Year ({TZ_GLOBAL})")),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_tweets_covid <- renderPlotly({
    
    tweets_most() %>%
      mutate(Date = format(as.Date(Datetime), format="%Y")) %>%
      filter(Date %in% c(2019, 2020, 2021)) %>%
      mutate(Month = lubridate::month(as.Date(Datetime), label=TRUE, abbr=FALSE)) %>%
      mutate(type = str_split(File, "-", simplify = TRUE)[ , 1]) %>%
      mutate(topic = ifelse(type %in% c("belfast", "hashtag", "text"), "people", type)) %>%
      filter(topic != "people") %>%
      count(Date, Month) %>%
      spread(Month, n, fill = 0) %>%
      gather("Month", "n", -Date) %>%
      mutate(Month = factor(Month, levels = c("January","February","March","April","May","June",
                                              "July","August","September","October","November","December"))) %>%
      complete(Date, Month, fill = list(n = 0)) %>%
      group_by(Date, Month) %>% 
      summarise(mean = round(mean(n), 3), sd = round(sd(n), 3)) %>%
      arrange(Month) %>%
      plot_ly(x = ~ Month, y = ~ mean, color = ~ Date, type = "scatter", mode = 'lines+markers', error_y = list(array = ~sd,
                                                                                                                color = ~Date)) %>%
      #plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "# of Tweets"),
        xaxis = list(title = glue::glue("Month of the Year ({TZ_GLOBAL})")),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_tweets_covid_by_people <- renderPlotly({
    
    tweets_most() %>%
      mutate(Date = format(as.Date(Datetime), format="%Y")) %>%
      filter(Date %in% c(2019, 2020, 2021)) %>%
      mutate(Month = lubridate::month(as.Date(Datetime), label=TRUE, abbr=FALSE)) %>%
      mutate(type = str_split(File, "-", simplify = TRUE)[ , 1]) %>%
      mutate(topic = ifelse(type %in% c("belfast", "hashtag", "text"), "people", type)) %>%
      filter(topic == "people") %>%
      count(topic, Date, Month) %>%
      spread(Month, n, fill = 0) %>%
      gather("Month", "n", -Date, -topic) %>%
      mutate(Month = factor(Month, levels = c("January","February","March","April","May","June",
                                              "July","August","September","October","November","December"))) %>%
      complete(topic, Date, Month, fill = list(n = 0)) %>%
      group_by(Date, Month) %>% 
      summarise(mean = round(mean(n), 3), sd = round(sd(n), 3)) %>%
      arrange(Month) %>%
      plot_ly(x = ~ Month, y = ~ mean, color = ~ Date, type = "scatter", mode = 'lines+markers', error_y = list(array = ~sd,
                                                                                                                color = ~Date)) %>%
      #plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "# of Tweets"),
        xaxis = list(title = glue::glue("Month of the Year ({TZ_GLOBAL})")),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$top_emojis <- renderUI({
    emoji_regex <- "[\\uD83C-\\uDBFF\\uDC00-\\uDFFF\u2600-\u27ff]+"
    
    twe <- tweets_most() %>%
      select(Text) %>%
      tidytext::unnest_tokens(Text, Text, token = "tweets") %>%
      filter(str_detect(Text, emoji_regex)) %>%
      mutate(Text = str_remove_all(Text, "\\w")) %>%
      tidytext::unnest_tokens(Text, Text, token = "characters") %>%
      count(Text, sort = TRUE) %>%
      inner_join(emo::jis %>% select(runes, emoji, name), by = c("Text" = "emoji")) %>%
      filter(!str_detect(runes, "^1F3F[B-F]$")) %>%
      slice(1:10) %>%
      mutate(
        b = n,
        # use twemoji
        runes = str_replace(tolower(runes), " ", "-"),
        runes = twemoji(runes)
      )
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    first <- twe %>% slice(1:5)
    
    tags$div(
      map(seq_len(min(10, nrow(first))), ~ {
        progressGroup(HTML(first$runes[[.x]]), first$n[[.x]], max = max(twe$n), color = colors[.x])
      })
    )
  })
  
  output$top_emojis_last <- renderUI({
    emoji_regex <- "[\\uD83C-\\uDBFF\\uDC00-\\uDFFF\u2600-\u27ff]+"
    
    twe <- tweets_most() %>%
      select(Text) %>%
      tidytext::unnest_tokens(Text, Text, token = "tweets") %>%
      filter(str_detect(Text, emoji_regex)) %>%
      mutate(Text = str_remove_all(Text, "\\w")) %>%
      tidytext::unnest_tokens(Text, Text, token = "characters") %>%
      count(Text, sort = TRUE) %>%
      inner_join(emo::jis %>% select(runes, emoji, name), by = c("Text" = "emoji")) %>%
      filter(!str_detect(runes, "^1F3F[B-F]$")) %>%
      slice(1:10) %>%
      mutate(
        b = n,
        # use twemoji
        runes = str_replace(tolower(runes), " ", "-"),
        runes = twemoji(runes)
      )
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    last <- twe %>% slice(6:10)
    
    tags$div(
      map(seq_len(min(10, nrow(last))), ~ {
        progressGroup(HTML(last$runes[[.x]]), last$n[[.x]], max = max(twe$n), color = colors[.x])
      })
    )
  })
  
  output$top_hashtags_topic <- renderUI({
    twh <-
      tweets_most() %>%
      select(Hastags, File) %>%
      mutate(type = str_split(File, "-", simplify = TRUE)[ , 1]) %>%
      mutate(topic = ifelse(type %in% c("belfast", "hashtag", "text"), "people", type)) %>%
      mutate(tags = str_replace_all(Hastags, "\\[|\\]", "")) %>%
      mutate(keywords = gsub("\'","", tags)) %>%
      mutate(keywords = gsub(", ",",", keywords)) %>%
      mutate(hastag = strsplit(keywords, ",")) %>%
      unnest(hastag) %>%
      mutate(hastag = tolower(hastag)) %>%
      filter(!is.na(hastag)) %>%
      filter(!str_detect(tolower(hastag), "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer")) %>%
      mutate(hastag = paste0("#", hastag)) %>%
      count(topic, hastag, sort = TRUE) %>% 
      tidytext::bind_tf_idf(hastag, topic, n)
    
    twh_idf <- twh[order(twh$tf_idf, decreasing = TRUE), ] %>%
      mutate(n = as.integer(tf_idf * 1000)) %>%
      slice(1:10)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(twh_idf))), ~ {
        progressGroup(paste(twh_idf$hastag[[.]], "-", twh_idf$topic[[.]]), twh_idf$n[[.]], max = max(twh_idf$n), color = colors[.])
      })
    )
  })
  
  output$top_hashtags <- renderUI({
    twh <-
      tweets_most() %>%
      select(Hastags) %>%
      mutate(tags = str_replace_all(Hastags, "\\[|\\]", "")) %>%
      mutate(keywords = gsub("\'","", tags)) %>%
      mutate(keywords = gsub(", ",",", keywords)) %>%
      mutate(hastag = strsplit(keywords, ",")) %>%
      unnest(hastag) %>%
      mutate(hastag = tolower(hastag)) %>%
      count(hastag, sort = TRUE) %>%
      filter(!is.na(hastag)) %>%
      filter(!str_detect(tolower(hastag), "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer")) %>%
      mutate(hastag = paste0("#", hastag))
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(twh))), ~ {
        progressGroup(twh$hastag[[.]], twh$n[[.]], max = max(twh$n), color = colors[.])
      })
    )
  })
  
  output$top_tweeters <- renderUI({
    tweets_most() %>%
      group_by(Username) %>%
      summarize(engagement = (sum(Reply.Count) * 2 + sum(Quote.Count) * 2 + sum(Like.Count) + sum(Retweet.Count))) %>%
      arrange(desc(engagement)) %>%
      ungroup() %>%
      slice(1:10) %>%
      mutate(
        engagement = scale(engagement, center = FALSE),
        engagement = engagement / max(engagement) * 100,
        profile_image_url = glue::glue("https://twitter.com/{Username}/profile_image?size=mini"),
        profile_image = map_chr(profile_image_url, cache_profile_image),
        profile_image_url = glue::glue('<div class="center-block"><img class="img-responsive img-circle" src="{profile_image}" alt={Username} style="max-height: 25px; min-width: 20px;"></div>'),
        profile_url = glue::glue("https://twitter.com/{Username}"),
        Username = glue::glue('<a href="{profile_url}" target="_blank">@{Username}</a>'),
        engagement = progressBar_v(engagement, rep(BASIC_COLORS[1:5], 2))
      ) %>%
      select(profile_image_url, Username, engagement) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("", "Screen Name", "Engagement"),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })
  
  output$top_tweet_words <- renderUI({
    tw <- tweets_most() %>%
      select(Text) %>%
      mutate(
        Text = str_remove_all(Text, "@[[:alnum:]_]+\\b"),
        Text = str_remove_all(Text, "&\\w+;")
      ) %>%
      mutate(Text = tolower(Text)) %>%
      mutate(Text = gsub("@\\S+", " ", Text, perl=T)) %>%
      mutate(Text = gsub("#\\S+", " ", Text, perl=T)) %>%
      tidytext::unnest_tokens(word, Text) %>%
      filter(
        !word %in% c("http", "https", "t.co"),
        !str_detect(word, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
        nchar(word) >= 3
      ) %>%
      anti_join(tidytext::stop_words, by = "word") %>%
      count(word, sort = TRUE) %>%
      slice(1:10)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(tw$word[[.]], tw$n[[.]], max = max(tw$n), color = colors[.])
      })
    )
  })
  
  # # Dashboard Tweets --------------------------------------------------------
  output$dash_most_liked <- renderUI({
    #validate(
    #  need(nrow(tweets_most()) > 0,
    #  "No tweets"
    #))
    
    tweets_most() %>%
      arrange(desc(Like.Count)) %>%
      slice(1) %>%
      pmap(get_tweet_blockquote) %>%
      .[[1]] %>%
      HTML()
  })
  
  output$dash_most_rt <- renderUI({
    #validate(
    #  need(nrow(tweets_most()) > 0,
    #  "No tweets"
    #))
    
    tweets_most() %>%
      arrange(desc(Retweet.Count)) %>%
      slice(1) %>%
      pmap(get_tweet_blockquote) %>%
      .[[1]] %>%
      HTML()
  })
  
  output$dash_most_recent <- renderUI({
    #validate(
    #  need(nrow(tweets_most()) > 0,
    #  "No tweets"
    #))
    
    tweets_most() %>%
      arrange(desc(Reply.Count)) %>%
      slice(1) %>%
      pmap(get_tweet_blockquote) %>%
      .[[1]] %>%
      HTML()
  })
  
  # # UK and Ireland --------------------------------------------------------------
  callModule(ukroiExplorer, "ukroi_table", reactive({ tweets_most_ukroi() }), tzone = tz_global())
  
  # # Sentiment ------------------------------------------------------
  # # Sentiment Boxes ---------------------------------------------------------
  output$plot_sent_freq <- renderPlotly({
    req(tweets_sfreq())
    # validate(
    #  need(nrow(tweets_sfreq()) > 0,
    #  "No tweets"
    # ))
    
    freq <- tweets_sfreq() %>% select(anger:positive) %>% colSums(.) %>% as.data.frame(.)
    freq["Sentiment"] <- rownames(freq)
    rownames(freq) <- 1:nrow(freq)
    names(freq)[1] <- 'Score'

    p <- freq %>% plot_ly(labels = ~Sentiment, values = ~Score) %>%
        add_pie(hole = 0.6) %>%
        layout(title = '',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = FALSE, cloud = FALSE, mathjax = NULL)
    p
  })
  
  output$plot_sent_topic <- renderPlotly({
    req(tweets_sfreq())
    # validate(
    #   need(nrow(tweets_sfreq()) > 0,
    #        "No tweets"
    #   ))
    
    freq <- tweets_sfreq() %>% group_by(topic) %>% summarise_at(vars(anger:positive), sum) %>% as.data.frame(.)
    freq <- freq %>% adorn_totals(c("row")) %>%
                    adorn_percentages("row") %>% 
                    adorn_pct_formatting(rounding = "half up", digits = 2) %>%
                    adorn_ns() %>%
                    gather("Sentiment", "n", -topic) %>%
                    mutate(Proportion = as.double(str_split(n, "%", simplify = TRUE)[ , 1]))

    p <- freq %>% plot_ly(x = ~Proportion, y = ~topic, color = ~Sentiment, text = ~ n, type = 'bar', orientation = 'h') %>%
      layout(title = '',
             xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             barmode = 'stack',
             hovermode = "compare",
             showlegend = FALSE) %>%
      add_annotations(xref = 'x', yref = 'paper',
                      x = c(2, 10, 19, 25, 32, 45, 65, 78, 84, 94),
                      y = 1.09,
                      text = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'negative', 'positive', 'sadness', 'surprise', 'trust'),
                      font = list(family = 'Arial', size = 12, color = 'rgb(67, 67, 67)'),
                      textangle = 315,
                      valign = "bottom",
                      showarrow = FALSE) %>%
      config(displayModeBar = FALSE, cloud = FALSE, mathjax = NULL)
    p
  })
  
  output$plot_hourly_sent_volume <- renderPlotly({
    req(tweets_nisword())
    
    tweets_nisword() %>%
      mutate(Date = as.Date(Datetime)) %>%
      group_by(Date) %>%
      summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), fear = sum(fear), joy = sum(joy),
                sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), negative = sum(negative), positive = sum(positive)) %>% 
      plot_ly(x = ~ Date) %>%
      add_lines(y = ~anger, name = "Anger", color = I(ADMINLTE_COLORS$red)) %>%
      add_lines(y = ~anticipation, name = "anticipation", color = I(ADMINLTE_COLORS$purple)) %>%
      add_lines(y = ~disgust, name = "disgust", color = I(ADMINLTE_COLORS$blue)) %>%
      add_lines(y = ~fear, name = "fear", color = I(ADMINLTE_COLORS$orange)) %>%
      add_lines(y = ~joy, name = "joy", color = I(ADMINLTE_COLORS$teal)) %>%
      add_lines(y = ~sadness, name = "sadness", color = I(ADMINLTE_COLORS$fuchsia)) %>%
      add_lines(y = ~surprise, name = "surprise", color = I(ADMINLTE_COLORS$lime)) %>%
      add_lines(y = ~trust, name = "trust", color = I(ADMINLTE_COLORS$maroon)) %>%
      add_lines(y = ~negative, name = "negative", color = I(ADMINLTE_COLORS$black)) %>%
      add_lines(y = ~positive, name = "positive", color = I(ADMINLTE_COLORS$green)) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        xaxis = list(
          range = c(as.POSIXct('2017-07-11 00:00:00'), as.POSIXct('2022-07-11 00:00:00')),
          rangeselector = list(
            buttons = list(
              list(
                count = 1,
                label = "Today",
                step = "day",
                stepmode = "todate"),
              list(
                count = 2,
                label = "Yesterday",
                step = "day",
                stepmode = "backward"),
              list(
                count = 7,
                label = "Week",
                step = "day",
                stepmode = "backward"),
              list(
                count = 1,
                label = "Month",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 Year",
                step = "year",
                stepmode = "backward"),
              list(
                count = 2,
                label = "2 Year",
                step = "year",
                stepmode = "backward"),
              list(
                count = 3,
                label = "3 Year",
                step = "year",
                stepmode = "backward"),
              list(
                count = 4,
                label = "4 Year",
                step = "year",
                stepmode = "backward"),
              list(step = "all", label = "All"))),
          rangeslider = list(type = "date")),
        yaxis = list(title = "Tweets"),
        legend = list(orientation = 'h', x = 0.05, y = 0.9),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      ) %>%
      config(cloud = FALSE, mathjax = NULL)
  })
  
  output$plot_hourly_sent_posneg <- renderPlotly({
    req(tweets_nisword())
    
    tweets_nisword() %>%
      mutate(Date = as.Date(Datetime)) %>%
      group_by(Date) %>%
      summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), fear = sum(fear), joy = sum(joy),
                sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), negative = sum(negative), positive = sum(positive)) %>% 
      mutate(sentiment = positive - negative) %>%
      plot_ly(x = ~Date, y = ~sentiment, type = 'bar') %>% #mode='lines+markers'
      config(displayModeBar = TRUE) %>%
      layout(
        xaxis = list(
          range = c(as.POSIXct('2017-07-11 00:00:00'), as.POSIXct('2022-07-11 00:00:00')),
          rangeselector = list(
            buttons = list(
              list(
                count = 1,
                label = "Today",
                step = "day",
                stepmode = "todate"),
              list(
                count = 2,
                label = "Yesterday",
                step = "day",
                stepmode = "backward"),
              list(
                count = 7,
                label = "Week",
                step = "day",
                stepmode = "backward"),
              list(
                count = 1,
                label = "Month",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 Year",
                step = "year",
                stepmode = "backward"),
              list(
                count = 2,
                label = "2 Year",
                step = "year",
                stepmode = "backward"),
              list(
                count = 3,
                label = "3 Year",
                step = "year",
                stepmode = "backward"),
              list(
                count = 4,
                label = "4 Year",
                step = "year",
                stepmode = "backward"),
              list(step = "all", label = "All"))),
          rangeslider = list(type = "date")),
        yaxis = list(title = "Tweets"),
        legend = list(orientation = 'h', x = 0.05, y = 0.9),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      ) %>%
      config(cloud = FALSE, mathjax = NULL)
  })
  
  output$plot_monthly_sent_posneg <- renderPlotly({
    req(tweets_nisword())
    
    tweets_nisword() %>%
      group_by(Year, Month) %>%
      summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), fear = sum(fear), joy = sum(joy),
                sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), negative = sum(negative), positive = sum(positive)) %>% 
      mutate(sentiment = positive - negative) %>%
      mutate(Year = factor(Year, levels = c("2017","2018","2019","2020","2021","2022"))) %>%
      plot_ly(x = ~ Month, y = ~ sentiment, color = ~ Year, type = "bar") %>%
      plotly::config(displayModeBar = TRUE) %>%
      layout(
        yaxis = list(title = "Sentiment Score"),
        xaxis = list(title = glue::glue("Month of the Year ({TZ_GLOBAL})"), categoryorder = "array", categoryarray = c("January", 
                                                                                                                       "February", 
                                                                                                                       "March",
                                                                                                                       "April",
                                                                                                                       "May",
                                                                                                                       "June",
                                                                                                                       "July",
                                                                                                                       "August",
                                                                                                                       "September",
                                                                                                                       "October",
                                                                                                                       "November",
                                                                                                                       "December")
        ),
        #barmode = 'stack',
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_sent_by_hour <- renderPlotly({
    req(tweets_nisword())
    
    tweets_nisword() %>%
      mutate(Date = format(ymd_hms(Datetime), format="%Y-%m-%d %H:00:00")) %>%
      mutate(Hour = format(ymd_hms(Datetime), format="%H")) %>%
      group_by(Date, Hour) %>%
      summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), fear = sum(fear), joy = sum(joy),
                sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), negative = sum(negative), positive = sum(positive)) %>% 
      group_by(Hour) %>%
      summarise(anger = mean(anger), anticipation = mean(anticipation), disgust = mean(disgust), fear = mean(fear), joy = mean(joy),
                sadness = mean(sadness), surprise = mean(surprise), trust = mean(trust), negative = mean(negative), positive = mean(positive)) %>% 
      adorn_totals(c("row","col")) %>%
      adorn_percentages("row") %>% 
      adorn_pct_formatting(rounding = "half up", digits = 2) %>%
      adorn_ns() %>%
      gather("Sentiment", "n", -Hour, -Total) %>%
      mutate(freq = as.double(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      plot_ly(x = ~ Hour, y = ~ freq, color = ~ Sentiment, type = "bar", text = ~ n, hovertemplate = paste("%{yaxis.title.text}: <b>%{text}</b><br>", 
                                                                                                       "%{xaxis.title.text}: %{x}<br>",
                                                                                                       "<extra></extra>")) %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Proportion of Tweets (%)"),
        xaxis = list(title = glue::glue("Hour of the Day ({TZ_GLOBAL})")),
        barmode = 'stack',
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_sent_by_day <- renderPlotly({
    req(tweets_nisword())
    
    tweets_nisword() %>%
      mutate(Date = as.Date(Datetime)) %>%
      group_by(Date, Day) %>%
      summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), fear = sum(fear), joy = sum(joy),
                sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), negative = sum(negative), positive = sum(positive)) %>% 
      group_by(Day) %>%
      summarise(anger = mean(anger), anticipation = mean(anticipation), disgust = mean(disgust), fear = mean(fear), joy = mean(joy),
                sadness = mean(sadness), surprise = mean(surprise), trust = mean(trust), negative = mean(negative), positive = mean(positive)) %>% 
      adorn_totals(c("row","col")) %>%
      adorn_percentages("row") %>% 
      adorn_pct_formatting(rounding = "half up", digits = 2) %>%
      adorn_ns() %>%
      gather("Sentiment", "n", -Day, -Total) %>%
      mutate(freq = as.double(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      plot_ly(x = ~ Day, y = ~ freq, color = ~ Sentiment, type = "bar", text = ~ n, hovertemplate = paste("%{yaxis.title.text}: <b>%{text}</b><br>", 
                                                                                                      "%{xaxis.title.text}: %{x}<br>",
                                                                                                      "<extra></extra>")) %>%
      #add_bars(y = ~topic, name = "useR!2019", color = I(ADMINLTE_COLORS$teal)) %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Proportion of Tweets (%)"),
        xaxis = list(title = glue::glue("Day of the Week ({TZ_GLOBAL})"), categoryorder = "array", categoryarray = c("Monday", 
                                                                                                                     "Tuesday", 
                                                                                                                     "Wednesday",
                                                                                                                     "Thursday",
                                                                                                                     "Friday",
                                                                                                                     "Saturday",
                                                                                                                     "Sunday")
        ),
        #barmode = 'stack',
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_sent_by_week <- renderPlotly({
    req(tweets_nisword())
    
    tweets_nisword() %>%
      group_by(Year, Week) %>%
      summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), fear = sum(fear), joy = sum(joy),
                sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), negative = sum(negative), positive = sum(positive)) %>% 
      group_by(Week) %>%
      summarise(anger = mean(anger), anticipation = mean(anticipation), disgust = mean(disgust), fear = mean(fear), joy = mean(joy),
                sadness = mean(sadness), surprise = mean(surprise), trust = mean(trust), negative = mean(negative), positive = mean(positive)) %>% 
      adorn_totals(c("row","col")) %>%
      adorn_percentages("row") %>% 
      adorn_pct_formatting(rounding = "half up", digits = 2) %>%
      adorn_ns() %>%
      gather("Sentiment", "n", -Week, -Total) %>%
      mutate(freq = as.double(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      plot_ly(x = ~ Week, y = ~ freq, color = ~ Sentiment, type = "bar", text = ~ n, hovertemplate = paste("%{yaxis.title.text}: <b>%{text}</b><br>", 
                                                                                                       "%{xaxis.title.text}: %{x}<br>",
                                                                                                       "<extra></extra>")) %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Proportion of Tweets (%)"),
        xaxis = list(title = glue::glue("Week of the Year ({TZ_GLOBAL})"), 
                     categoryorder = "array", categoryarray = array(seq(1, 53, by=1)),
                     textangle = 90
        ),
        barmode = 'stack',
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_sent_by_month <- renderPlotly({
    req(tweets_nisword())
    
    tweets_nisword() %>%
      group_by(Year, Month) %>%
      summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), fear = sum(fear), joy = sum(joy),
                sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), negative = sum(negative), positive = sum(positive)) %>% 
      group_by(Month) %>%
      summarise(anger = mean(anger), anticipation = mean(anticipation), disgust = mean(disgust), fear = mean(fear), joy = mean(joy),
                sadness = mean(sadness), surprise = mean(surprise), trust = mean(trust), negative = mean(negative), positive = mean(positive)) %>% 
      adorn_totals(c("row","col")) %>%
      adorn_percentages("row") %>% 
      adorn_pct_formatting(rounding = "half up", digits = 2) %>%
      adorn_ns() %>%
      gather("Sentiment", "n", -Month, -Total) %>%
      mutate(freq = as.double(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      plot_ly(x = ~ Month, y = ~ freq, color = ~ Sentiment, type = "bar", text = ~ n, hovertemplate = paste("%{yaxis.title.text}: <b>%{text}</b><br>", 
                                                                                                        "%{xaxis.title.text}: %{x}<br>",
                                                                                                        "<extra></extra>")) %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Proportion of Tweets (%)"),
        xaxis = list(title = glue::glue("Month of the Year ({TZ_GLOBAL})"), categoryorder = "array", categoryarray = c("January", 
                                                                                                                       "February", 
                                                                                                                       "March",
                                                                                                                       "April",
                                                                                                                       "May",
                                                                                                                       "June",
                                                                                                                       "July",
                                                                                                                       "August",
                                                                                                                       "September",
                                                                                                                       "October",
                                                                                                                       "November",
                                                                                                                       "December")
        ),
        barmode = 'stack',
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_sent_by_year <- renderPlotly({
    req(tweets_nisword())
    
    tweets_nisword() %>%
      group_by(Year) %>%
      summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), fear = sum(fear), joy = sum(joy),
                sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), negative = sum(negative), positive = sum(positive)) %>% 
      adorn_totals(c("row","col")) %>%
      adorn_percentages("row") %>% 
      adorn_pct_formatting(rounding = "half up", digits = 2) %>%
      adorn_ns() %>%
      gather("Sentiment", "n", -Year, -Total) %>%
      mutate(freq = as.double(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      plot_ly(x = ~ Year, y = ~ freq, color = ~ Sentiment, type = "bar", text = ~ n, hovertemplate = paste("%{yaxis.title.text}: <b>%{text}</b><br>", 
                                                                                                       "%{xaxis.title.text}: %{x}<br>",
                                                                                                       "<extra></extra>")) %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Proportion of Tweets (%)"),
        xaxis = list(title = glue::glue("Year ({TZ_GLOBAL})")),
        #barmode = 'stack',
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
  output$plot_sent_years <- renderPlotly({
    req(tweets_sfreq())
    data <- tweets_sfreq() %>% 
      group_by(Year) %>% 
      summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), fear = sum(fear), joy = sum(joy),
                sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), negative = sum(negative), positive = sum(positive)) %>% 
      #adorn_totals(c("row")) %>%
      adorn_percentages("row") %>% 
      adorn_pct_formatting(rounding = "half up", digits = 2) %>%
      adorn_ns() %>%
      gather("Sentiment", "n", -Year) %>%
      filter(Sentiment == "negative" | Sentiment == "positive") %>%
      mutate(Proportion = as.double(str_split(n, "%", simplify = TRUE)[ , 1])) %>%
      mutate(Count = as.integer(str_extract(n, "(?<=\\().*(?=\\))")))
    
    p <- data %>% plot_ly(x = ~Count, y = ~Year, color = ~Sentiment, text = ~n, type = 'bar', orientation = 'h',colors = c("red", "green")) %>%
      layout(title = '',
             xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             margin = list(l = 0, r = 10, b = 50, t = 10),
             #barmode = 'stack',
             hovermode = "compare",
             showlegend = FALSE) %>%
      add_annotations(xref = 'paper', yref = 'y',
                      y = c(2017, 2018, 2019, 2020, 2021, 2022),
                      x = -0.10,
                      text = c('2017', '2018', '2019', '2020', '2021', '2022'),
                      font = list(family = 'Arial', size = 12, color = 'rgb(67, 67, 67)'),
                      textangle = 315,
                      valign = "bottom",
                      showarrow = FALSE) %>%
      config(displayModeBar = FALSE, cloud = FALSE, mathjax = NULL)
    p
  })
  
  output$sent_postweeters <- renderUI({
    req(tweets_nisword())
    data <- tweets_nisword() %>% 
      filter(!fword %in% c("united kingdom", "northern ireland")) %>%
      group_by(Username) %>% 
      summarise(freq = n(), positive = sum(positive), negative = sum(negative)) %>% 
      mutate(score = positive/freq)
    tweets_spfreq <- data[order(data$score, decreasing = TRUE), ]
    
    tweets_spfreq %>%
      slice(1:5) %>%
      mutate(
        score = scale(score, center = FALSE),
        score = score / max(score) * 100,
        profile_image_url = glue::glue("https://twitter.com/{Username}/profile_image?size=mini"),
        profile_image = map_chr(profile_image_url, cache_profile_image),
        profile_image_url = glue::glue('<div class="center-block"><img class="img-responsive img-circle" src="{profile_image}" alt={Username} style="max-height: 25px; min-width: 20px;"></div>'),
        profile_url = glue::glue("https://twitter.com/{Username}"),
        Username = glue::glue('<a href="{profile_url}" target="_blank">@{Username}</a>'),
        score = progressBar_v(score, rep(BASIC_COLORS[1:5], 2))
      ) %>%
      select(profile_image_url, Username, score) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("", "Screen Name", "Positive Score"),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })
  
  output$sent_negtweeters <- renderUI({
    req(tweets_nisword())
    data <- tweets_nisword() %>% 
      filter(!fword %in% c("united kingdom", "northern ireland")) %>%
      group_by(Username) %>% 
      summarise(freq = n(), positive = sum(positive), negative = sum(negative)) %>% 
      mutate(score = negative/freq)
    tweets_spfreq <- data[order(data$score, decreasing = TRUE), ]
    
    tweets_spfreq %>%
      slice(1:5) %>%
      mutate(
        score = scale(score, center = FALSE),
        score = score / max(score) * 100,
        profile_image_url = glue::glue("https://twitter.com/{Username}/profile_image?size=mini"),
        profile_image = map_chr(profile_image_url, cache_profile_image),
        profile_image_url = glue::glue('<div class="center-block"><img class="img-responsive img-circle" src="{profile_image}" alt={Username} style="max-height: 25px; min-width: 20px;"></div>'),
        profile_url = glue::glue("https://twitter.com/{Username}"),
        Username = glue::glue('<a href="{profile_url}" target="_blank">@{Username}</a>'),
        score = progressBar_v(score, rep(BASIC_COLORS[1:5], 2))
      ) %>%
      select(profile_image_url, Username, score) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("", "Screen Name", "Negative Score"),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })
  
  output$sent_important_topic <- renderUI({
    req(tweets_nisword())
    data <- tweets_nisword() %>% count(topic, fword, sort = TRUE) %>% tidytext::bind_tf_idf(fword, topic, n)
    tweets_spfreq <- data[order(data$tf_idf, decreasing = TRUE), ]
    
    tw <- tweets_spfreq %>%
      filter(
        !fword %in% c("united kingdom", "northern ireland"),
        !str_detect(fword, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
        nchar(fword) >= 3
      ) %>%
      mutate(freq = as.integer(tf_idf * 10000)) %>%
      slice(1:10)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(paste(tw$fword[[.]], "-", tw$topic[[.]]), tw$freq[[.]], max = max(tw$freq), color = colors[.])
      })
    )
  })
  
  output$sent_important_year <- renderUI({
    req(tweets_nisword())
    data <- tweets_nisword() %>% count(Year, fword, sort = TRUE) %>% tidytext::bind_tf_idf(fword, Year, n)
    tweets_spfreq <- data[order(data$tf_idf, decreasing = TRUE), ]
    
    tw <- tweets_spfreq %>%
      filter(
        !fword %in% c("united kingdom", "northern ireland"),
        !str_detect(fword, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
        nchar(fword) >= 3
      ) %>%
      mutate(freq = as.integer(tf_idf * 10000)) %>%
      slice(1:10)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(paste(tw$fword[[.]], "-", tw$Year[[.]]), tw$freq[[.]], max = max(tw$freq), color = colors[.])
      })
    )
  })
  
  output$sent_negative <- renderUI({
    req(tweets_nisword())
    data <- tweets_nisword() %>% group_by(fword) %>% summarise(freq = n(), score = sum(negative)) %>% filter(score > 0)
    tweets_snfreq <- data[order(data$freq, decreasing = TRUE), ]
    
    tw <- tweets_snfreq %>%
      filter(
        !fword %in% c("united kingdom", "northern ireland", "mum"),
        !str_detect(fword, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
        nchar(fword) >= 3
      ) %>%
      slice(1:5)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(tw$fword[[.]], tw$freq[[.]], max = max(tw$freq), color = colors[.])
      })
    )
  })
  
  output$sent_negative_topic <- renderUI({
    req(tweets_nisword())
    data <- tweets_nisword() %>% filter(negative - positive > 0) %>% count(topic, fword, sort = TRUE) %>% tidytext::bind_tf_idf(fword, topic, n)
    tweets_snfreq <- data[order(data$tf_idf, decreasing = TRUE), ]
    
    tw <- tweets_snfreq %>%
      filter(
        !fword %in% c("united kingdom", "northern ireland", "mum"),
        !str_detect(fword, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
        nchar(fword) >= 3
      ) %>%
      mutate(freq = as.integer(tf_idf * 1000)) %>%
      slice(1:5)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(paste(tw$fword[[.]], "-", tw$topic[[.]]), tw$freq[[.]], max = max(tw$freq), color = colors[.])
      })
    )
  })
  
  output$sent_negative_year <- renderUI({
    req(tweets_nisword())
    data <- tweets_nisword() %>% filter(negative - positive > 0) %>% count(Year, fword, sort = TRUE) %>% tidytext::bind_tf_idf(fword, Year, n)
    tweets_snfreq <- data[order(data$tf_idf, decreasing = TRUE), ]
    
    tw <- tweets_snfreq %>%
      filter(
        !fword %in% c("united kingdom", "northern ireland", "mum"),
        !str_detect(fword, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
        nchar(fword) >= 3
      ) %>%
      mutate(freq = as.integer(tf_idf * 1000)) %>%
      slice(1:5)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(paste(tw$fword[[.]], "-", tw$Year[[.]]), tw$freq[[.]], max = max(tw$freq), color = colors[.])
      })
    )
  })
  
  output$sent_positive <- renderUI({
    req(tweets_nisword())
    data <- tweets_nisword() %>% group_by(fword) %>% summarise(freq = n(), score = sum(positive)) %>% filter(score > 0)
    tweets_spfreq <- data[order(data$freq, decreasing = TRUE), ]
    
    tw <- tweets_spfreq %>%
      filter(
        !fword %in% c("united kingdom", "northern ireland", "patient"),
        !str_detect(fword, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
        nchar(fword) >= 3
      ) %>%
      slice(1:5)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(tw$fword[[.]], tw$freq[[.]], max = max(tw$freq), color = colors[.])
      })
    )
  })
  
  output$sent_positive_topic <- renderUI({
    req(tweets_nisword())
    data <- tweets_nisword() %>% filter(positive - negative > 0) %>% count(topic, fword, sort = TRUE) %>% tidytext::bind_tf_idf(fword, topic, n)
    tweets_snfreq <- data[order(data$tf_idf, decreasing = TRUE), ]
    
    tw <- tweets_snfreq %>%
      filter(
        !fword %in% c("united kingdom", "northern ireland", "mum", "patient", "increase"),
        !str_detect(fword, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
        nchar(fword) >= 3
      ) %>%
      mutate(freq = as.integer(tf_idf * 1000)) %>%
      slice(1:5)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(paste(tw$fword[[.]], "-", tw$topic[[.]]), tw$freq[[.]], max = max(tw$freq), color = colors[.])
      })
    )
  })
  
  output$sent_positive_year <- renderUI({
    req(tweets_nisword())
    data <- tweets_nisword() %>% filter(positive - negative > 0) %>% count(Year, fword, sort = TRUE) %>% tidytext::bind_tf_idf(fword, Year, n)
    tweets_snfreq <- data[order(data$tf_idf, decreasing = TRUE), ]
    
    tw <- tweets_snfreq %>%
      filter(
        !fword %in% c("united kingdom", "northern ireland", "mum"),
        !str_detect(fword, "cancer|bowel|colon|colorectal|bowelcancer|coloncancer|colorectalcancer"),
        nchar(fword) >= 3
      ) %>%
      mutate(freq = as.integer(tf_idf * 1000)) %>%
      slice(1:5)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(paste(tw$fword[[.]], "-", tw$Year[[.]]), tw$freq[[.]], max = max(tw$freq), color = colors[.])
      })
    )
  })
  
  output$plot_sent_wordcloud <- renderPlot({
    req(tweets_ni())
    data <- tweets_ni() %>% count(fword) %>% rename(freq = n)
    tweets_wfreq <- data[order(data$freq, decreasing = TRUE), ] %>%
      filter(freq >= 5) %>% 
      head(350) %>%
      mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))
    
    ggplot(tweets_wfreq, aes(label = fword, color = fword, size = freq, angle = angle)) +
      geom_text_wordcloud_area(area_corr = TRUE, rm_outside = TRUE, 
                               #mask = png::readPNG(system.file("twitter.png", 
                               #                                 package = "ggwordcloud"))
                               ) +
      scale_size_area(max_size = 24) +
      theme_minimal()
  })
  
  # # Tweet Wall --------------------------------------------------------------
  tweets_wall <- reactive({
    tweets_most() %>%
      #filter(
      #  created_at >= input$tweet_wall_daterange[1],
      #  created_at < input$tweet_wall_daterange[2] + 1
      #)
      filter(between(as.Date(Datetime), as.Date(input$tweet_wall_daterange[1]), as.Date(input$tweet_wall_daterange[2])))
  })
  
  tweet_wall_page_break = 20
  tweet_wall_n_items <- reactive({ nrow(tweets_wall()) })
  tweet_wall_page <- shinyThings::pager("tweet_wall_pager",
                                        n_items = tweet_wall_n_items,
                                        page_break = tweet_wall_page_break)
  
  output$tweet_wall_tweets <- renderUI({
    s_page_items <- tweet_wall_page() %||% 1L
    
    validate(need(
      nrow(tweets_wall()) > 0,
      "No tweets in selected date range. Try another set of dates."
    ))
    
    tweets_wall() %>%
      slice(s_page_items) %>%
      masonify_tweets()
  })
  
  tweet_wall_date_preset <- shinyThings::dropdownButton("tweet_wall_date_presets",
                                                        options = TWEET_WALL_DATE_INPUTS)
  
  observe({
    req(tweet_wall_date_preset())
    update_dates <- TWEET_WALL_DATE_RANGE(tweet_wall_date_preset())
    if (any(is.na(update_dates))) return(NULL)
    update_dates <- strftime(update_dates, "%F", tz = TZ_GLOBAL, usetz = TRUE) %>% unname()
    updateDateRangeInput(session, "tweet_wall_daterange", start = update_dates[1], end = update_dates[2], max = ymd("2022-07-11"))
  })
  
  # # TweetExplorer -----------------------------------------------------------
  callModule(tweetExplorer, "tweet_table", reactive({ tweets_most() }), tzone = tz_global())
}
