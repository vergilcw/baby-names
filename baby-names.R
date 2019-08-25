options(stringsAsFactors=F)
# install.packages(c('dplyr', 'XML', 'ggvis', 'tidyr', 'httr', 'stringdist', 'readr'))
library(dplyr)
library(XML)
library(reshape2)
library(ggvis)
library(tidyr)
library(httr)
library(stringdist)
library(readr)

# library(shiny)

#get latest baby names (using code from hadley wickham's babynames package):
#https://github.com/hadley/babynames/blob/master/data-raw/applicants.R


urlbirths <- "http://www.ssa.gov/oact/babynames/numberUSbirths.html"
tabs <- GET(urlbirths)
tabs <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)

ssa <- tabs[[2]]
names(ssa) <- c("year", "M", "F", "total")
ssa$total <- NULL
ssa <- ssa[-1, ]

ssa$year <- as.integer(as.character(ssa$year))
ssa$M <- as.integer(gsub(",", "", ssa$M))
ssa$F <- as.integer(gsub(",", "", ssa$F))

applicants <- ssa %>%
  tbl_df() %>%
  gather(sex, applicants, M:F) %>%
  mutate(sex = as.character(sex))

#https://github.com/hadley/babynames/blob/master/data-raw/names.R
if (!file.exists("data-raw/names")) {
  tmp <- tempfile(fileext = ".zip")
  download.file("http://www.ssa.gov/oact/babynames/names.zip", tmp, quiet = TRUE)
  unzip(tmp, exdir = "data-raw/names")
  unlink(tmp)
}

# From: http://www.ssa.gov/oact/babynames/limits.html
all <- dir("data-raw/names", "\\.txt$", full.names = TRUE)
year <- as.numeric(gsub("[^0-9]", "", basename(all)))

data <- lapply(all, read.csv,
               colClasses = c("character", "character", "integer"),
               header = FALSE)

one <- bind_rows(data)
names(one) <- c("name", "sex", "n")
one$year <- rep(year, vapply(data, nrow, integer(1)))

babynames1 <- one %>%
  tbl_df() %>%
  select(year, sex, name, n) %>%
  arrange(year, sex, desc(n)) %>%
  left_join(applicants) %>%
  mutate(perc = n / applicants * 100) %>%
  select(-applicants)

#remove all exact-match biblical names:


bible_names <- readLines('biblical-names.txt') %>%
  c(readLines('biblical-place-names.txt')) %>%
  strsplit(' ') %>%
  unlist() %>%
  unique()

babynames2 <- babynames1 %>%
  filter(!name %in% bible_names)
  #loss of 2.7%
    #filtering with soundex removes quite a few names.  I'll stick with a simple
    #exact match for now
  # mutate(biblename = name[amatch(name, bible_names, method = 'soundex')])
  # filter(ain(name, bible_names, method = 'soundex'))



# Names Analysis ----------------------------------------------------------


#also:

#plot popularity of some names
babynames1 %>%
  filter(name %in% c(
    # 'August',
    # 'Kai',
    # 'Rafael',
    # 'Milo',
    #'Jesse',
    'Levon',
    'Kepler',
    'Carver'
    # 'Loyal'
    # 'Fox'
    # 'Sim',
    # 'Gibson',
    # 'Emil',
    # 'Henrik'
    # 'Liam',
    # 'Noah',
    # 'John',
    # 'Michael',
    # 'Jason',
    # 'James',
    # 'Owen',
    # 'Curtis'
                     ),
         sex == 'M') %>%
  group_by(name) %>%
  ggvis(~year, ~perc) %>%
  layer_lines(stroke = ~name, strokeWidth := 3) %>%
  add_axis("y", title = "percent") %>%
  add_axis("x", title = "year",  format = "####")


#find names that were popular in 1880-1920 that are not popular today

#method 1:
name_era_spread <- babynames2 %>%
  filter(sex == 'M') %>%
  mutate(era = ifelse(year <= 1940, 'old',
                      ifelse( year >= 1980, 'new', 'middle'))) %>%
  filter(era %in% c('old', 'new')) %>%
  group_by(era, name) %>%
  summarize(perc = mean(perc, na.rm = TRUE)) %>%
  ungroup %>%
  spread(key = era, value = perc) %>%
  filter(!is.na(new)) %>%
  mutate(diff_perc = (old - new)/old) %>%
  arrange(desc(diff_perc))


name_era_spread <- babynames2 %>%
  filter(sex == 'M') %>%
  mutate(era = ifelse(year <= 1940, 'old',
                      ifelse( year >= 1980, 'new', 'middle'))) %>%
  filter(era %in% c('middle', 'new')) %>%
  group_by(era, name) %>%
  summarize(perc = mean(perc, na.rm = TRUE)) %>%
  ungroup %>%
  spread(key = era, value = perc) %>%
  filter(!is.na(new)) %>%
  mutate(diff_perc = (old - new)/old) %>%
  arrange(desc(diff_perc))

#print out first list
# name_era_spread$name %>%
#   head(2000) %>%
#   paste(collapse = '  ') %>%
#   writeLines(con = 'namefile.txt')

alreadyseen <- readLines('namefile.txt') %>%
  strsplit('  ') %>%
  unlist

alreadyseen2 <- readLines('namefile2.txt') %>%
  strsplit('  ') %>%
  unlist

alreadyseen3 <- readLines('babyname-wizard-already-considered.txt') %>%
  strsplit(' ') %>%
  unlist

alreadyseen_final <- bind_rows(tibble(name = alreadyseen),
                               tibble(name = alreadyseen2),
                               tibble(name = alreadyseen3)) %>%
  distinct()


#too many somewhat popular names.  Let's remove top 1000 names
top1000boy <- read_csv('top-1000-boynames.csv')

name_era_spread %>%
  anti_join(top1000boy) %>%
  anti_join(alreadyseen_final) %>%
  pull(name) %>%
  head(1200) %>%
  paste(collapse = '  ') %>%
  writeLines(con = 'namefile3.txt')


#method 2:
name_era <- babynames2 %>%
  filter(sex == 'M') %>%
  mutate(era = ifelse(year <= 1920, 'old',
                      ifelse( year >= 1980, 'new', 'middle'))) %>%
  filter(era %in% c('old', 'new')) %>%
  group_by(era, name) %>%
  summarize(perc = mean(perc, na.rm = TRUE)) %>%
  ungroup

names_old <- name_era %>%
  filter(era == 'old') %>%
  select(name, perc_old = perc)

names_new <- name_era %>%
  filter(era == 'new') %>%
  select(name, perc_new = perc)

#first check for names that haven't been used in last 35 years:
names_gone <- left_join(names_old, names_new, by = 'name') %>%
  filter(is.na(perc_new))

names_gap <- left_join(names_old, names_new, by = 'name') %>%
  filter(!is.na(perc_new)) %>%
  mutate(difference = perc_old / perc_new) %>%
  arrange(desc(difference)) %>%
  filter(perc_new < 0.01)



#make an animation of plots of the top 10 boys' names popularity in each
# year since 1980 (keep colors the same for each name via factor)

#first make a list of only top 10 boys' names:

top10 <- babynames %>%
  filter(sex == 'M') %>%
  group_by(year) %>%
  top_n(10, perc) %>%
  ungroup %>%
  select(name) %>%
  unique

#45 names
top10_timeline <-
  left_join(top10, babynames %>% filter(sex=='M')) %>%
  mutate(name = as.factor(name))

plots <- lapply(1880:2014, function(x, bbnames) {
  bbn = filter(bbnames, year %in% x) %>%
    top_n(10, perc)



  bbnames %>%
    filter(name %in% bbn$name) %>%
    group_by(name) %>%
    ggvis(~year, ~perc) %>%
    layer_lines(stroke = ~name, strokeWidth:=3) %>%
    add_axis("y", title="percent") %>%
    add_axis("x", title="year",  format="####")

}, bbnames = top10_timeline)






