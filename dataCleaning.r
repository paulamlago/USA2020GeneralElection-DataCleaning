fileDirectory <-dirname(rstudioapi::getActiveDocumentContext()$path)
csv_usa <- file.path(fileDirectory, '2020 November General Election - Turnout Rates.csv')
usa_elections <- read.csv(csv_usa)

