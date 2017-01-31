\name{getVaderRuleBasedSentiment}
\alias{getVaderRuleBasedSentiment}
\title{Vader Rule Based Sentiment Function}
\usage{
getVaderRuleBasedSentiment(text, compound=TRUE)
}
\arguments{
\item{text}{The text which should be analyze.}
\item{compound}{Whether the result should be return in a compound form or distributed by sentiment class}
}

\description{
The \code{getVaderRuleBasedSentiment} is a function which performs sentiment analysis using the Vader dictionary and rule based system. Vader was designed for social media sentiment analysis and deals with increase boost words ("very","most", "amazingly",...), decrease boost words ("almost", "barely", "partly",...) and negations ("not","ain't","wont",..). It also considers repeated punctuation, capitalization and sentiment emoticons.

For more information:

Hutto, C.J. & Gilbert, E.E. (2014). VADER: A Parsimonious Rule-based Model for Sentiment Analysis of Social Media Text. Eighth International Conference on Weblogs and Social Media (ICWSM-14). Ann Arbor, MI, June 2014.
}
\examples{
\dontrun{

getVaderRuleBasedSentiment("Today only kinda sux! But I'll get by, lol", compound=F)

getVaderRuleBasedSentiment("The book was kind of good.", compound = T)

}
}
