# Reads data from raw tweets

createList <- function(fname) {
  
  # Reads data
  galaxy <- 
    read.table(fname, sep = ",", quote = "\"", stringsAsFactors = FALSE) %>% 
    set_colnames(c("id", "screenname", "tweet")) %>%
    tbl_df()
  
  # Extracts poster information
  retweeterPoster <- 
    galaxy %>%
    mutate(is_retweeted = stri_detect_regex(tweet, "(RT|via)((?:\\b\\W*@\\w+)+)")) %>%
    filter(is_retweeted) %>%
    rowwise() %>%
    do({
      # Gets retwitter
      who_retweet <- 
        stri_extract_first_regex(.$tweet, "(RT|via)((?:\\b\\W*@\\w+)+)")[[1]] %>%
        stri_extract_first_regex("@[a-zA-Z0-9_]{1,}") %>%
        stri_replace_all_fixed("@", "")
      
      # Returns pair
      data_frame(who_post = .$screenname, who_retweet = who_retweet, 
                 combi = stri_c(sort(c(.$screenname, who_retweet)), collapse = " "))
    }) %>%
    ungroup() %>%
    group_by(combi) %>%
    summarize(from = min(who_post, who_retweet), 
              to = max(who_post, who_retweet), 
              weight = n()) %>%
    ungroup() %>%
    select(-combi)
  
  # Returns results
#  saveRDS(retweeterPoster, file = dataFile)
  retweeterPoster
}

