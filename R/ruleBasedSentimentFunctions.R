getVaderRuleBasedSentiment<-function(text,compound=TRUE){
 text<-iconv(text,to="UTF-8")
 text<-gsub('\\p{So}|\\p{Cn}', '', text, perl = TRUE)
 text<-gsub("\\s+", " ",text)

 if(is.na(text)){
   return(NA)
 }
 if(text=="" || text==" "){
   return(NA)
 }
 return(PolarityScores(text,compound))


}





Negated<-function(input.words,include_nt=TRUE){
  input.words<-tolower(input.words)
  if(any(input.words %in% sysdata$sentimentVars$negation.list)){
    return(TRUE)
  }
  if(any(grepl("n't",input.words))){
    return(TRUE)
  }
  if("least" %in% input.words){
    index<-which(input.words=="least")
    if(index>1 && input.words[index-1]!="at"){
      return(TRUE)
    }
  }
  return(FALSE)
}




NormalizeScore<-function(score,alpha=15){
  norm.score<-score/sqrt((score*score)+alpha)
  if(norm.score<(-1.0)){
    return(-1.0)
  }
  else if(norm.score>1.0){
    return(1.0)
  }
  else{
    return(norm.score)
  }
}


AllCapDifferential<-function(input.words){
  is.different<-FALSE

  all.caps.words<-0
  all.caps.words<-grepl("^[[:upper:]]+$", input.words)
  if(!is.null(input.words) &&
     all.caps.words>0 &&
     all.caps.words<length(input.words)){
    is.different<-TRUE
  }
  return(is.different)
}




AuxScalarIncDec<-function(word,valence,is_cap_diff,scalar){
  if(valence<0){
    scalar<-scalar*(-1)

  }
  if(grepl("^[[:upper:]]+$", word) && is_cap_diff){
    if(valence>0){
      scalar<-scalar+sysdata$sentimentVars$caps.increase
    }
    else{
      scalar<-scalar-sysdata$sentimentVars$caps.increase
    }
  }
  return(scalar)
}

ScalarIncDec<-function(word,valence,is_cap_diff){
  scalar<-0
  word.lower<-tolower(word)
  if(word.lower %in%sysdata$sentimentVars$boost.increase.list){
    scalar<-sysdata$sentimentVars$boost.increase
    return(AuxScalarIncDec(word,valence,is_cap_diff,scalar))
  }
  else if(word.lower %in% sysdata$sentimentVars$boost.decrease.list){
    scalar<-sysdata$sentimentVars$boost.decrease
    return(AuxScalarIncDec(word,valence,is_cap_diff,scalar))

  }
  else{
    scalar<-0
  }
  return(scalar)


}




WordsPlusPunct<-function(text){
 text.no.punctuation<-tm::removePunctuation(text)
 words.only<-unlist(strsplit(text.no.punctuation,"\\s+"))
 words.only<-words.only[length(words.only)>1]
 punct.list<- c(".", "!", "?", ",", ";", ":", "-", "'", "\"",
              "!!", "!!!", "??", "???", "?!?", "!?!", "?!?!", "!?!?")
 words.c<-rep(words.only,times=length(punct.list))
 words.c2<-unlist(
   lapply(words.only,FUN=rep,times=length(punct.list)))

 words.p.before<-apply(
   expand.grid(punct.list,words.only), 1, paste, collapse="")
 words.p.after<-apply(
   expand.grid(words.only,punct.list), 1, paste, collapse="")


 words.punct.dict<-data.frame(
                     mod=c(words.p.after,words.p.before),
                     original=c(words.c,words.c2)
                    )
 return(words.punct.dict)

}


WesAux<-function(word,words.punct.dict){
  if(word %in% words.punct.dict$mod)
    return(as.character(words.punct.dict[words.punct.dict$mod==word,]$original))
  else(
    return(word)
  )
}


WordsAndEmoticons<-function(text){

  wes<-unlist(strsplit(text,"\\s+"))
  words.punct.dict<-WordsPlusPunct(text)
  wes<-wes[nchar(wes)>1]
  wes<-unlist(lapply(wes,WesAux,words.punct.dict))
  return(wes)
}




PolarityScores<-function(text,compound){

  sentiments<-c()
  wae<-WordsAndEmoticons(text)
 for(i in seq_along(wae)){
    valence<-0
    item<-wae[i]
    if(i<length(wae) && tolower(wae[i])=="kind" && tolower(wae[i+1])=="of"
       || (!(is.null(item)) &&
           (tolower(wae[i])%in%sysdata$sentimentVars$boost.increase.list
            || tolower(wae[i])%in%sysdata$sentimentVars$boost.decrease.list))){
        sentiments<-c(sentiments,valence)

    }

    sentiments<-SentimentValence(valence,text,item,i,sentiments)

  }
 sentiments<-ButCheck(wae,sentiments)
 valence_dict<-ScoreValence(sentiments,text)
 if(compound){
   return(as.double(valence_dict["compound"]))
 }
 return(valence_dict)
}

ButCheck<-function(wae,sentiments){
  if("but" %in% wae || "BUT" %in% wae){

    bi<-try(head(which(wae=="but"),1))
    if(length(bi)==0){
      bi<-try(head(which(wae=="BUT"),1))
    }
    for(si in seq_along(sentiments)){

      if(si<bi){
        sentiments[si]<-sentiments[si]*0.5
      }
      else if(si>bi){
        sentiments[si]<-sentiments[si]*1.5

      }
    }
  }
  return(sentiments)
}



SentimentValence<-function(valence,text,item,i,sentiments){
  wae<-WordsAndEmoticons(text)
  is.cap.diff<-AllCapDifferential(wae)
  item.lowercase<-tolower(item)
  if((!is.null(item)) && item.lowercase %in% sysdata$lex_vader$X1){
    valence<-head(sysdata$lex_vader[sysdata$lex_vader$X1==item.lowercase,]$X2,1)
    if(grepl("^[[:upper:]]+$",item)){
      if(valence>0){
        valence<-valence+sysdata$sentimentVars$caps.increase
      }
      else{
        valence<-valence-sysdata$sentimentVars$caps.increase

      }
    }

    for(start_i in c(1:3)){
        if(i>start_i &&
           !((tolower(wae[i-start(start_i)]))
             %in% sysdata$lex_vader$X1 )){

          s<-ScalarIncDec(wae[i-start_i],valence,is.cap.diff)
          if(start_i==2 && s!=0){
            s<-s*0.95
          }
          if(start_i==3 && s!=0){
            s<-s*0.9
          }
          valence<-valence+s
          valence<-NeverCheck(valence,wae,start_i,i)
          if(start_i==3){
             valence<-IdiomsCheck(valence,wae,i)
           }
        }
      }
    }
    valence<-LeastCheck(valence,wae,i)


  sentiments<-c(sentiments,valence)
  return(sentiments)
}

NeverCheck<-function(valence,wae,start_i,i){
  if(start_i==1){
    if(Negated(wae[i-1])){
      valence<-valence*sysdata$sentimentVars$n.scalar
    }
  }
  if(start_i==2){
    if(wae[i-2]=="never" && (wae[i-1]=="so" || wae[i-1]=="this")){
      valence<-valence*1.5

    }
    else if(Negated(wae[i-(start_i)])){
      valence<-valence*sysdata$sentimentVars$n.scalar
    }
  }
  if(start_i==3){
    if(wae[i-3]=="never"
       && (wae[i-2]=="so" || wae[i-2]=="this")
       || (wae[i-1]=="so" || wae[i-1]=="this") ){
      valence<-valence*1.25

    }
    else if(Negated(wae[i-(start_i)])){
      valence<-valence*sysdata$sentimentVars$n.scalar
    }
  }
  return(valence)
}

LeastCheck<-function(valence,wae,i){
  if(i>2 &&
     !(tolower(wae[i-1]) %in% sysdata$lex_vader$X1)
     && tolower(wae[i-1])=="least"){
    if(tolower(wae[i-2])!="at"
       && tolower(wae[i-2])!="very"){
      valence<-valence*sysdata$sentimentVars$n.scalar
    }
  }
  else if(i>1
          && !(tolower(wae[i-1]) %in% sysdata$lex_vader$X1)
          && tolower(wae[i-1])=="least"){
    valence<-valence*sysdata$sentimentVars$n.scalar
  }
  return(valence)
}

ScoreValence<-function(sentiments,text){
  if(exists("sentiments")){
    sum.s<-sum(sentiments)
    punc.emph.ampl<-0
    punc.emph.ampl<-PunctuationEmphasis(sum.s,text)
    if(sum.s>0){
      sum.s<-sum.s+punc.emph.ampl
    }
    else if(sum.s<0){
      sum.s<-sum.s-punc.emph.ampl

    }

   compound<-NormalizeScore(sum.s)
   scores.dif<-SiftSentimentScores(sentiments)
   if(scores.dif["pos.sum"]>abs(scores.dif["neg.sum"])){
     scores.dif["pos.sum"]<-scores.dif["pos.sum"]+punc.emph.ampl
   }
   else if(scores.dif["pos.sum"]<abs(scores.dif["neg.sum"])){
     scores.dif["neg.sum"]<-scores.dif["neg.sum"]-punc.emph.ampl
   }
   return(c(compound=compound,scores.dif))
  }
}


SiftSentimentScores<-function(sentiments){
  pos.sum<-0
  neu.sum<-0
  neg.sum<-0
  for (sentiment.score in sentiments){
    if(sentiment.score>0){
      pos.sum<-pos.sum+sentiment.score+1
    }
    if(sentiment.score<0){
      neg.sum<-neg.sum+(sentiment.score-1)
    }
    if(sentiment.score==0){
      neu.sum<-neu.sum+1
    }



  }
  return(c(pos.sum=pos.sum,neu.sum=neu.sum,neg.sum=neg.sum))


}

PunctuationEmphasis<-function(sum.s,text){
  ep.amplifier<-AmplifyEp(text)
  qm.amplifier<-AmplifyQm(text)
  punct.emph.amplifier<-ep.amplifier+qm.amplifier
  return(punct.emph.amplifier)
}


AmplifyEp<-function(text){
  ep<-as.vector(gregexpr('!', text)[[1]])
  if(ep[1]!=-1){
    ep.count<-length(ep)

  }
  else{
    ep.count<-0
  }
  if(ep.count>4){
    ep.count<-4
  }
  ep.amplifier<-ep.count*0.292
  return(ep.amplifier)
}


AmplifyQm<-function(text){
  qm<-as.vector(gregexpr(pattern = '\\?', text)[[1]])
  if(qm[1]!=-1){
    qm.count<-length(qm)

  }
  else{
    qm.count<-0
  }
  qm.amplifier<-0
  if(qm.count>1){
    if(qm.count<=3){
        qm.amplifier<-qm.count*0.18
    }
    else{
      qm.amplifier<-0.96

    }
  }

  return(qm.amplifier)
}


##teste
IdiomsCheck<-function(valence,wae,i){
  onezero<-paste(wae[i-1],wae[i],sep = " ")
  twoonezero<-paste(wae[i-2],wae[i-1],wae[i],sep = " ")
  twoone<-paste(wae[i-2],wae[i-1],sep = " ")
  threetwoone<-paste(wae[i-3],wae[i-2],wae[i-1],sep = " ")
  threetwo<-paste(wae[i-3],wae[i-2],sep = " ")
  sequences<-c(onezero,twoonezero,twoone,threetwoone,threetwo)
  for(seq in sequences){
    if(seq %in% sysdata$sentimentVars$special.case.idioms$words){
      valence<-sysdata$sentimentVars$special.case.idioms[sysdata$sentimentVars$special.case.idioms$words==seq,2]
      break
    }
  }
  if(length(wae)-1>i){

    zeroone<-paste(wae[i],wae[i+1],sep = " ")


    if(zeroone %in%
       sysdata$sentimentVars$special.case.idioms$words){
      valence<-sysdata$sentimentVars$special.case.idioms[sysdata$sentimentVars$special.case.idioms$words==zeroone,2]

    }
  }
  if(length(wae)-1>i+1){
    zeroonetwo<-paste(wae[i],wae[i+1],wae[i+2],sep = " ")
    if(zeroonetwo %in%
       sysdata$sentimentVars$special.case.idioms$words){
      valence<-sysdata$sentimentVars$special.case.idioms[sysdata$sentimentVars$special.case.idioms$words==zeroonetwo,2]

    }
  }
  if(threetwo %in% sysdata$sentimentVars$boost.decrease.list
     || twoone %in% sysdata$sentimentVars$boost.decrease.list){
    valence<-valence+sysdata$sentimentVars$boost.decrease
  }
  return(valence)

}

