#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'




getSentiment <- function(text, dictionary="bing",scale=FALSE,score.type=sum) {
  if(dictionary=="vader"){
     used.dictionary<-sysdata$lex_vader
   }  else if (dictionary=="bing"){
    used.dictionary<-sysdata$lex_bing
   }  else if (dictionary=="afinn"){
    used.dictionary<-sysdata$lex_afinn
   }  else{
    stop("The specified sentiment lexicon is not currently available.
         See help for more information")
   }
  colnames(used.dictionary)<-c("words","sentiment")

  if(scale){
       used.dictionary$sentiment<-
         plotrix::rescale(x = used.dictionary$sentiment, newrange = c(-1,1))
  }

  if(is.na(text)){
    return(NA)
  }

  words<-unlist(strsplit(text,split = "\\s+"))
  if(length(words)==0){
    return(0)
  }
  sent.values<-used.dictionary[used.dictionary$words %in% words,]$sentiment
  return(score.type(sent.values))
}




getCustomSentiment<-function(text,custom.dictionary,
                             scale=FALSE,
                             score.type=sum ){
  if(mode(custom.dictionary)!="list"){
    stop("The specified sentiment lexicon is not in the correct format.
         See help for more information")

  }

  if(length(custom.dictionary)!=2){
    stop("The specified sentiment lexicon
          does not have the necessary number of columns (2).
          See help for more information")

  }
  colnames(custom.dictionary)<-c("words","sentiment")
  m.custom.words<-mode(custom.dictionary$words)
  if(m.custom.words!="character"){
    stop("The specified sentiment lexicon does not
          have the necessary requirements.
          Make sure that the words column is the first.
          See help for more information")

  }

  if(scale){

    custom.dictionary$sentiment<-
      plotrix::rescale(x = custom.dictionary$sentiment, newrange = c(-1,1))
  }


  words<-unlist(strsplit(text,split = "\\s+"))
  sent.values<-custom.dictionary[custom.dictionary$words %in% words,]$sentiment
  return(score.type(sent.values))

}














