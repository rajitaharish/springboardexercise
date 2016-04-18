getwd()

setwd("C:/Users/HARISH/Downloads")

getwd()

refine <- read.csv("refine_original.csv", header = TRUE,stringsAsFactors = FALSE)


dim(refine)


names(refine)

refine$company
agrep("phillips",x = refine$company, value = FALSE)
agrep("azko",x = refine$company, value = TRUE)

new_company <- function(a)
  {  a <- tolower(a)
  b <- nchar(a)
  first_2 <- substr(a,1,2)
  last_2 <- substr(a, b-1,b)
  if(last_2 == 'ps')
  {return('phillips')}
  else if(first_2 == 'ak')
  {return('azko')}
  else if(first_2 == 'va')
  {return('van houten')}
  
  else {return('unilever')}}

new_company(refine$company[9])
new_company(refine$company[22])

refine$company <- sapply(refine$company,FUN = new_company)

refine
help("function")
help("nchar")

product_code <- strsplit(refine$Product.code...number, split = '-')


refine$product_code <- sapply(product_code, FUN = function(x) x[1])

refine$product_number <- sapply(product_code, FUN = function(x) x[2])

refine$product_number <- as.integer(refine$product_number)

refine


lookup_table <- c('p' = 'Smartphone', 'v' = 'TV', 'x' = 'Laptop', 'q' = 'Tablet')


refine$product_category <- factor(lookup_table[refine$product_code])

refine$product_category

refine$full_address <- paste(refine$address, refine$city, refine$country,sep = ', ')

names(refine)

model.matrix(refine)

library(dplyr)
refine <- refine %>%
  mutate(company_philips = ifelse(company == "philips", 1, 0))

refine <- refine %>%
  mutate(company_azko = ifelse(company == "azko", 1, 0))

refine <- refine %>%
  mutate(company_vanhouten = ifelse(company == "van houten", 1, 0))

refine <- refine %>%
  mutate(company_unilever = ifelse(company == "unilever", 1, 0))

refine <- refine %>%
  mutate(product_smartphone = ifelse(company == "smartphone", 1, 0))

refine <- refine %>%
  mutate(product_tv = ifelse(company == "TV", 1, 0))

refine <- refine %>%
  mutate(product_laptop = ifelse(company == "Laptop", 1, 0))

refine <- refine %>%
  mutate(product_tablet = ifelse(company == "tablet", 1, 0))




names(refine)

write.csv(x = refine, file = 'refine_clean.csv')


