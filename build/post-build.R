## Script to fix/optimize RDA files.
## Andy Wills
library(tidyverse)
library(stringi)

## jarrold10 dataset has non-ASCII characters in it, this is not allowed in an
## R package. The following code converts them to ASCII. As the 3rd line
## shows, they now appear as a form of question mark
fnam <- "../pkg/data/jarrold10.rda" 
load(fnam)
jarrold10$response <- stri_trans_general(jarrold10$response, "latin-ascii")
unique(jarrold10$response)
save(jarrold10, file=fnam)
