library(foreach)
library(doParallel)

library(dplyr)
library(magrittr)

no_cores <- detectCores() - 1 # leave one core 
cl <- makeCluster(no_cores,type = "FORK") # make clusters, specify "fork"
registerDoParallel()  # register a parallel backend



tmp <- foreach(
  i = 1:10000
  # .packages = c("sentimentr", "dplyr", "magrittr")
) %dopar% {
  bru_text[i, ] %>%
    mutate(sentence = sentimentr::get_sentences(text)) %$%
    sentiment_by(sentence, list(review_id))
}

do.call(rbind,tmp)


bru_text_sentiment_final <- do.call(rbind,bru_text_sentiment)


parallel::stopCluster(cl)
bru_text_sentiment2 <- bru_text_sentiment
ls_tmp <- ls(bru_text_sentiment,bru_text_sentiment2)
bru_text_sentiment3 <- data.table::rbindlist(ls_tmp)

for (j in 1:36) {
  tmp<-foreach(i:(j*10000)) %dopar%
    bru_text[i, ] %>%
    mutate(sentence = sentimentr::get_sentences(text)) %$%
    sentiment_by(sentence, list(review_id))
}