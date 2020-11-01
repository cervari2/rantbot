### Read in Messages
# Reading datafile
library("rjson")


# data wrangling
library(tidyverse)

# text processing
library(tidytext)
library(textclean)
library(tokenizers)

# markov chain
library(markovchain)

path_to_messages<-"" # path to messages
friends_name <- "" ## friends name, according to Messenger

  
message_files<-list.files(path=path_to_messages,pattern = ".json$")
rants.array<-c() # initialize rant array

for (i in message_files) { ## Go through all files
  json_file <- sprintf("%s%s",path_to_messages,i) ### set to path
  json_data <- fromJSON(paste(readLines(json_file, warn=FALSE),collapse = "")) # read in data. ignore EOF errors
  messages.json<-json_data1$messages
  
  for (j in 1:length(messages.json)) { # Go through all messages
    if (messages.json[[j]]$sender_name == friends_name) { ## only from ranter
      rants.array<-c(rants.array,messages.json[[j]]$content) ## only use rants
    }  
  }
}

rants.df<-as.data.frame(rants.array) # change to dataframe for easier handling
colnames(rants.df)<-c("rants") # label column
rants.df$rants<-as.character(rants.df$rants)

rants.array <- rants.df %>% # on this df... 
  pull(rants) %>% ## get the rant column ...
  strsplit(" ") %>%  # split by space...
  unlist() # and unlist


badchars<-c("Ã°âÿââ¶") # put in bad characters here.
badchars.string<-paste(unlist(strsplit(badchars,split="")),collapse = '|') # build into a string
rants.array<-rants.array[grep(badchars.string,rants.array,invert = TRUE)] # remove bad strings
rants.array<-rants.array[grep("u00",rants.array,invert = TRUE)] # remove  emojis
rants.array<-gsub("\\p{P}", "", rants.array, perl=TRUE) # remove slashes




st<-Sys.time()
rants.markov <- markovchainFit(rants.array) # This takes hours
print(Sys.time()-st)



rantbot <- function(num = 100, first_word = "i", n = 2) {
  for (i in 1:num) {
    set.seed(i+100)
    markovchainSequence(n = n, # generate 2 additional random words
                        markovchain = rants.markov$estimate,
                        t0 = tolower(first_word), include.t0 = T) %>% 
      # joint words
      paste(collapse = " ") %>% # join generated words with space
      # create proper sentence form
      str_replace_all(pattern = " ,", replacement = ",") %>% 
      str_replace_all(pattern = " [.]", replacement = ".") %>% 
      str_replace_all(pattern = " [!]", replacement = "!") %>% 
      str_to_sentence() %>% # start every sentences with capitalization
      print()
    
    
  }
  
}

rantbot(num= 100, first_word = "iran", n=20)



# Use Markov chain
# https://rpubs.com/nabiilahardini/text-generator

# Method 2

# Method 3
