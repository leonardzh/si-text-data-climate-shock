cities <- read.csv('Updated citie.csv')
news <- read.csv("news_raw_contents.csv",header = FALSE,sep='|')
typeof(news)
df_news <- data.frame(news)
all_cities <- df_news[(df_news$V4 != ''),4]
# Q? how to convert factor to string
i <- sapply(df_news, is.factor)
df_news[i] <- lapply(df_news[i], as.character)
# Q? 
lst <- lapply(all_cities, function(vec) unique(unlist(strsplit(vec, ",", perl = T))))

# text data cleaning 
# weather data
# NOAA
# clean news
# sentiment analysis