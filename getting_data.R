library(httr)
library(jsonlite)
library(tidyverse)

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
