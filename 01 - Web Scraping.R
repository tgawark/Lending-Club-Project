#Install package
install.packages("rvest")

#Loading the rvest package
library('rvest')

#Specifying the Vanguard URL
url <- 'https://personal.vanguard.com/us/funds/snapshot?FundIntExt=INT&FundId=0914&ps_disable_redirect=true#tab=1a'

#Reading the HTML code from the website
webpage <- read_html(url)

rank_data_html <- html_node(webpage,'td')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)