## Построение анимации 10-матчевого скользящего среднего рейтингов НБА.

### Используя NBA API получаем данные боксскора каждой игры каждой команды

```r
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)
library(ggthemes)
library(gganimate)
library(rlang)
##Getting data via NBA API.
##Required link
adv_box_team <- "https://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=2018-19&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&VsConference=&VsDivision="
##Adding headers
request_headers <- c(
  "accept-encoding" = "gzip, deflate, sdch",
  "accept-language" = "en-US,en;q=0.8",
  "cache-control" = "no-cache",
  "connection" = "keep-alive",
  "host" = "stats.nba.com",
  "pragma" = "no-cache",
  "upgrade-insecure-requests" = "1",
  "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9"
)
#Getting a response
request <- GET(adv_box_team, add_headers(request_headers))
#Convert to js.file to list
boxscore_data <- fromJSON(content(request, as = "text"))
#Convert to tibble data and assigning column names
table <- tbl_df(data.frame(boxscore_data$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
names(table) <- toupper(boxscore_data$resultSets$headers[[1]])
```

### Выбираем нужные данные и переименовываем столбцы с названием рейтингов

```r
##Select the columns you want to analyze
rating <- table %>%
  select(TEAM_ID,
         TEAM_ABBREVIATION,
         TEAM_NAME,
         GAME_ID,
         GAME_DATE,
         MATCHUP,
         WL,
         E_OFF_RATING,
         E_DEF_RATING,
         E_NET_RATING)
## Renaming columns with E_OFF_RATING on ORTG
rating1 <- rating %>%
rename_at(vars(starts_with("E_")),
          list(~str_c(str_sub(., start = 3, end = 3), 
                       str_sub(., start = 7, end = 7),
                       str_sub(., start = 9, end = 9),
                       str_sub(., start = 12, end = 12))))
```

Таблица:

|TEAM_ID|	TEAM_ABBREVIATION|	TEAM_NAME|	GAME_ID|	GAME_DATE|	MATCHUP|	WL|	ORTG|	DRTG|	NRTG|
|---|---|---|---|---|---|---|---|---|---|
|1610612742|	DAL|	Dallas Mavericks|	0021801227|	2019-04-10T00:00:00|	DAL @ SAS|	L|	97.6|	106.1|	-8.5|
|1610612754|	IND|	Indiana Pacers|	0021801220|	2019-04-10T00:00:00|	IND @ ATL|	W|	122.5|	116.8|	5.7|
|1610612744|	GSW|	Golden State Warriors|	0021801225|	2019-04-10T00:00:00|	GSW @ MEM|	L|	113.4|	129.5|	-16.1|
|1610612765|	DET|	Detroit Pistons|	0021801223|	2019-04-10T00:00:00|	DET @ NYK|	W|	118.8|	93.7|	25.1|
|1610612741|	CHI|	Chicago Bulls|	0021801224|	2019-04-10T00:00:00|	CHI @ PHI|	L|	108.3|	128.1|	-19.7|
|1610612737|	ATL|	Atlanta Hawks|	0021801220|	2019-04-10T00:00:00|	ATL vs. IND|	L|	116.8|	122.5|	-5.7|
|1610612755|	PHI|	Philadelphia 76ers|	0021801224|	2019-04-10T00:00:00|	PHI vs. CHI|	W|	128.1|	108.3|	19.7|
|1610612753|	ORL|	Orlando Magic|	0021801222|	2019-04-10T00:00:00|	ORL @ CHA|	W|	130.1|	121.4|	8.6|
|1610612760|	OKC|	Oklahoma City Thunder|	0021801226|	2019-04-10T00:00:00|	OKC @ MIL|	W|	116.8|	102.4|	14.4|
|1610612751|	BKN|	Brooklyn Nets|	0021801221|	2019-04-10T00:00:00|	BKN vs. MIA|	W|	103.6|	84.2|	19.4|

### Создание функции **rolling_offnet_rating_nba** для построения графиков.

Получаем значение скользящего среднего для ныжных команды и рейтинга.

```r
##Return the entered value in the function argument in the type quosure
   quo_rating <- enquo(variable)
   quo_col1 <- enquo(col1)
   quo_col2 <- enquo(col2)
##Changing the data type of multiple columns
   test1 <- rating1 %>%
     mutate(GAME_DATE = as.Date(ymd_hms(GAME_DATE))) %>%
     mutate_at(vars(ORTG:NRTG), list(~as.numeric)) %>%
     arrange(GAME_DATE)
##The calculation of the moving average  
   team <- test1 %>%
     filter(TEAM_ABBREVIATION == "DAL") %>%
     mutate(RATING = rollmeanr(ORTG, k = 10, fill= NA)) %>%
     na.omit(test1)
```

Получаем среднее, 10-ое и 21-ое (10 снизу) значения ретйинга по Лиге.

```r
##Create a table with the average ratings of each team
   league <- test1 %>%
     group_by(TEAM_NAME) %>%
     summarise(ORTG = mean(ORTG),
               DRTG = mean(DRTG),
               NRTG = mean(NRTG))
##The average, 10 and 21 ratings in the entire League.   
  average <- league %>%
    mutate(average = mean(!! quo_rating)) %>%
    select(average) %>%
    unique() %>%
    .$average
  
   top10 <- league %>%
     arrange(desc(!! quo_rating)) %>%
     select(!! quo_rating) %>%
     slice(10)
   top10 <- top10[[1]]
  bottom10 <- league %>%
    arrange(desc(!! quo_rating)) %>%
    select(!! quo_rating) %>%
    slice(21)
  bottom10 <- bottom10[[1]]
##Getting the date of the first rollaverage  
  data <- team %>%
    select(GAME_DATE) %>%
    arrange(GAME_DATE)
  data <- data[[1,1]]
```

Получаем значения цветов и их названия

```r
table_color <- read_delim("./data/Team_color.csv", delim = ";", col_names = TRUE)
##Getting color and color_name selected color
  color1 <- table_color %>%
    filter(TEAM_ABBREVIATION == name) %>%
    select(!! quo_col1)
  color1 <- color1[[1]]
  color2 <- table_color %>%
    filter(TEAM_ABBREVIATION == name) %>%
    select(!! quo_col2)
  color2 <- color2[[1]]
  name1 <- paste0("name_", quo_name(quo_col1))
  name2 <- paste0("name_", quo_name(quo_col2))
  name_color1 <- table_color %>%
    filter(TEAM_ABBREVIATION == name) %>%
    select(name1)
  name_color1 <- name_color1[[1]]
  name_color2 <- table_color %>%
    filter(TEAM_ABBREVIATION == name) %>%
    select(name2)
  name_color2 <- name_color2[[1]]
```

Получаем максимальное значение рейтинга для добавления `geom_text` на графике.

```r
##The maximum value of the rating
  max <- team %>%
    filter(RATING == max(RATING)) %>%
    select(RATING)
  max <- max[[1]]
```

### Построение статичного графика

```r
##Building and save a static chart
  Sys.setlocale("LC_ALL", "C")
  gg <- ggplot(team, aes(GAME_DATE, RATING)) +
     geom_hline(yintercept = c(top10, bottom10), col = c("red", "blue")) +
     annotate(geom = "text", x = as.Date(data) + 2, y = top10 - 0.2,
              label = "TOP 10", col = "red") +
     annotate(geom = "text", x = as.Date(data) + 2, y = bottom10 + 0.2,
              label = "BOTTOM 10", col = "blue") +
      geom_line(size = 2, col = if_else(team$RATING > average, color1, color2)) +
     theme_tufte() +
     labs(title = paste0(team$TEAM_NAME, " 10-Game Rolling ", quo_name(quo_rating)),
          subtitle = paste0(paste0(name_color1, " - above average ", quo_name(quo_rating)),
                            "\n", paste0(name_color2, " - below average ",quo_name(quo_rating))),
          caption = "Source: BBall Index Data & Tools\nTelegram: @NBAatlantic, twitter: @vshufinskiy")
   theme(plot.title = element_text(size = 12, hjust = 0.5),
         plot.caption = element_text(size = 10),
         plot.subtitle = element_text(size = 9))
   ggsave(paste0(unique(team$TEAM_NAME), quo_name(quo_rating), ".jpeg"), gg, width = 8, units = "in")
```

### Построение анимации

```r
##Building animations
  anim <- gg +
    theme(plot.title = element_text(hjust = 0.5, size = 25),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 15),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18)) +
    geom_text(aes(x = as.Date(data), y = max + 0.5),
              label = paste0(quo_name(quo_rating)," ", round(team$RATING, digits = 1)), size = 6,
              col = if_else(team$RATING > average, color1, color2)) +
    transition_reveal(GAME_DATE) +
    labs(title = paste0(team$TEAM_NAME, " 10-Game Rolling ", quo_name(quo_rating)),
         subtitle = paste0(paste0(name_color1, " - above average ",quo_name(quo_rating)),
                           "\n", paste0(name_color2, " - below average ",quo_name(quo_rating)),
                           "\n", "Date: {frame_along}"),
         caption = paste0("Source: stats.nba.com\nTelegram: @NBAatlantic, twitter: @vshufinskiy"))
##Save results  
  animate(anim, fps = 30, duration = 15, width = 1280, height = 720,
          renderer = gifski_renderer(paste0(unique(team$TEAM_ABBREVIATION), "_", quo_name(quo_rating), ".gif")))
```

### Результат

![GIF](DEN_DRTG.gif)

![GIF](DEN_NRTG.gif)
