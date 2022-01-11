## Script to fix/optimize RDA files.
## Andy Wills
library(tidyverse)
library(stringi)

## jarrold10 dataset has non-ASCII characters in it, this is not allowed in an
## R package. The following code converts them to ASCII. As the 3rd line
## shows, they now appear as NA
fnam <- "../pkg/data/jarrold10.rda" 
load(fnam)
jarrold10$response <- stri_trans_general(jarrold10$response, "latin-ascii")
jarrold10$response <- iconv(jarrold10$response, from="UTF-8", to="ASCII")
unique(jarrold10$response)
save(jarrold10, file=fnam)

## healey14 has 102 columns! Convert to long format
fnam <- "../pkg/data/healey14.rda" 
load(fnam)

## Data is missing a 'listN' variable and the number of lists and sessions
## varies for some subjects. So, only way to add list number is via a loop

subject <- healey14$subject[1]
session <- healey14$session[1]
listN <- 0
healey14$listN <- 0
for(rw in 1:nrow(healey14)) {
    if(subject == healey14$subject[rw] & session == healey14$session[rw]) {
        listN <- listN + 1
    } else {
        print(rw)
        listN <- 1
        subject <- healey14$subject[rw]
        session <- healey14$session[rw]
    }
    healey14$listN[rw] <- listN
}

## Dataset has four distinct components.
## Split into these, and pivot each to long format

word <- healey14 %>% select(subject:word16, listN) %>% add_column(type = "word")

wordL <- word %>% pivot_longer(word1:word16,
                               names_to = "pos",
                               names_prefix = "word",
                               values_to = "value")

recword <- healey14 %>% select(subject:session, recword1:recword28, listN) %>%
    add_column(type = "recword")

recwordL <- recword %>% pivot_longer(recword1:recword28,
                               names_to = "pos",
                               names_prefix = "recword",
                               values_to = "value")

recinpos <- healey14 %>% select(subject:session, recinpos1:recinpos28, listN) %>%
    add_column(type = "recinpos")

recinposL <- recinpos %>% pivot_longer(recinpos1:recinpos28,
                               names_to = "pos",
                               names_prefix = "recinpos",
                               values_to = "value")

rectime <- healey14 %>% select(subject:session, rectime1:rectime28, listN) %>%
    add_column(type = "rectime")

rectimeL <- rectime %>% pivot_longer(rectime1:rectime28,
                               names_to = "pos",
                               names_prefix = "rectime",
                               values_to = "value")

## Combine back into one long-format dataset

healey14 <- bind_rows(wordL, recwordL, recinposL, rectimeL)

## Fix data types
healey14$pos <- as.integer(healey14$pos)
healey14$listN <- as.integer(healey14$listN)

## The four types of data are actually in two categories - one about presented
## words, and three about the recall of words. For human readability, it is
## useful to classify and order in this way
healey14$inout <- "output"
healey14$inout[healey14$type == "word"] <- "input"
healey14 <- healey14 %>% arrange(subject, session, listN, inout, pos, type)

## inout was useful for ordering but adds redundancy to dataset. Remove
healey14 <- healey14 %>% select(-inout)

## The matrix representation of the dataset was not particularly suited to it
## because they number of recalled items varied by list. 
## Now it's in long-format, we can cut out the unused rows
healey14 <- healey14 %>% filter(value != 0)

## Finally, the package is based on dataframes rather than tibbles, so
## convert to df and save
healey14 <- as.data.frame(healey14)
save(healey14, file=fnam)
