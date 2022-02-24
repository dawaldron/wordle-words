setwd('C:/Users/dwald/OneDrive/ZenBook backup/Blog/Posts/Wordle Stats')
library(data.table)
library(magrittr)
library(RcppMsgPack)
library(forecast)
library(arm)

# load guesses data
dt_guesses <- fread('guesses.tsv') %>%
  .[, pct := as.numeric(gsub('%','',percent)) / 100] %>%
  .[, pct := pct / sum(pct), wordleID] %>%
  dcast(wordleID ~ paste0('g', guesses), value.var = 'pct')
dt_guesses[, g1_3 := g1 + g2 + g3]
dt_guesses[, g6_x := g6 + gX]

# load game data
dt_game <- fread('game.tsv')

# word frequencies from https://github.com/rspeer/wordfreq/tree/master/wordfreq/data
l_en <- msgpack_read('large_en.msgpack')
l_en[[1]] <- NULL
dt_zipf <- lapply(1:length(l_en), function(i) {
  if (length(l_en[[i]]) > 0) {
    dt <- data.table(word = toupper(unlist(l_en[[i]])))
    dt[, zipf := 9-((i-1) / 100)]
    return(dt)
  } else {
    return(NULL)
  }
}) %>%
  rbindlist() %>%
  .[nchar(word) == 5]

# select wordle answer words
dt_zipf.answer <- dt_zipf[word %in% dt_game[, answer]]

# indicate letter used multiple times
dt_zipf.answer[, doubleLetter := sapply(word, function(x) length(unique(strsplit(x, '')[[1]]))) < 5]

# calculate number of common words within one letter
c_zipf.common <- dt_zipf[zipf > 3, word]

calcLeven <- function(word, wordlist) {
  wordlist <- wordlist[wordlist != word]
  c(sum(substr(wordlist,2,5) == substr(word,2,5)),
    sum(paste0(substr(wordlist,1,1),substr(wordlist,3,5)) == paste0(substr(word,1,1),substr(word,3,5))),
    sum(paste0(substr(wordlist,1,2),substr(wordlist,4,5)) == paste0(substr(word,1,2),substr(word,4,5))),
    sum(paste0(substr(wordlist,1,3),substr(wordlist,5,5)) == paste0(substr(word,1,3),substr(word,5,5))),
    sum(substr(wordlist,1,4) == substr(word,1,4))) %>%
    max()
}

dt_zipf.answer[, close := sapply(dt_zipf.answer[, word], calcLeven, c_zipf.common)]

# calculate English text frequency of word's letters (from wikipedia)
dt_letterfreq <- fread('letterfreq.tsv') %>%
  .[, letter := toupper(letter)]

calcFreq <- function(word, letter) {
  data.table(letter = strsplit(word, '')[[1]]) %>%
    dt_letterfreq[., on = 'letter'] %>%
    .[order(freq)] %>%
    .[letter, freq]
}

dt_zipf.answer[, letterfreqMax := sapply(word, calcFreq, letter = 5)]
dt_zipf.answer[, letterfreqMed := sapply(word, calcFreq, letter = 3)]
dt_zipf.answer[, letterfreqMin := sapply(word, calcFreq, letter = 1)]

# scrabble scores
calcScrabble <- function(word) {
  data.table(letter = strsplit(word, '')[[1]]) %>%
    dt_letterfreq[., on = 'letter'] %>%
    .[, sum(scrabble)]
}

dt_zipf.answer[, scrabbleScore := sapply(word, calcScrabble)]


# merge word data to game data
dt_game <- dt_game %>%
  .[dt_zipf.answer, on = c('answer' = 'word'), nomatch = 0]
dt_game <- dt_game[order(wordleID)]

dt_game <- dt_guesses[dt_game, on = 'wordleID']

# time series decomposition
ts_results = ts(dt_game[!is.na(results), results], frequency = 7)
decompose_results = decompose(ts_results, "multiplicative")
plot(decompose_results)

dt_decomp <- data.table(results_seas = decompose_results$seasonal,
                        results_trend = decompose_results$trend,
                        results_rand = decompose_results$random)

dt_game[!is.na(results), results_seas := decompose_results$seasonal]
dt_game[!is.na(results), results_trend := decompose_results$trend]
dt_game[!is.na(results), results_rand := decompose_results$random - 1]


# model percent of players using six guesses (6/6 and X/6 scores)
fit <- lm((g6_x * 100) ~ scale(-zipf) + scale(scrabbleScore) + (close > 2) + doubleLetter, dt_game)
names(fit$coefficients) <- c('Intercept','Word obscurity','Scrabble score','Similar to other common words','Duplicate letter')
summary(fit)
coefplot(fit, CI = 2)

# write data for plot
fit$coefficients
summary(fit)$coefficients %>%
  data.table(coefficient = row.names(.), .) %>%
  fwrite('coefplot.csv')

# zipf vs 6 guesses correlation
fit_zipf <- lm(g6_x ~ zipf, dt_game)
summary(fit_zipf)
dt_fitzipf <- data.table(t(fit_zipf$coefficients), r2 = summary(fit_zipf)$r.squared)
fwrite(dt_fitzipf, 'model_zipf.csv')

# 6 guesses vs total players correlation
fit_report6 <- lm(results_rand ~ g6_x, dt_game)
summary(fit_report6)
dt_fitreport6 <- data.table(t(fit_report6$coefficients), r2 = summary(fit_report6)$r.squared)
fwrite(dt_fitreport6, 'model_report6.csv')

# 3 guesses vs total players correlation
fit_report3 <- lm(results_rand ~ g1_3, dt_game)
summary(fit_report3)
dt_fitreport3 <- data.table(t(fit_report3$coefficients), r2 = summary(fit_report3)$r.squared)
fwrite(dt_fitreport3, 'model_report3.csv')

# write main wordle data
fwrite(dt_game[!is.na(results)], 'wordle-data.csv')
