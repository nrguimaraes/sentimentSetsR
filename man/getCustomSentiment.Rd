\name{getCustomSentiment}
\alias{getCustomSentiment}
\title{Get Custom Sentiment Function}
\usage{
getCustomSentiment(text,custom.dictionary,scale=FALSE,score.type=sum)
}
\arguments{
\item{text}{The text which should be analyze.}
\item{custom.dictionary}{The custom dictionary that should be used to perform the sentiment analysis task. The dictionary must have two columns where the first is the sentiment words and the second the sentiment scores}
\item{scale}{A boolean to determine if the dictionary should be scaled to a [-1,1] range}
\item{score.type}{The function to be used in the final sentiment score of the text. By default it is used the sum of all the values of the sentiment words. But any function that accepts a numeric vector can be used.}
}

\description{
The \code{getSentiment} is a highly customizable function that performs a sentiment analysis on the text provided using a custom sentiment dictionary. This is ideal for language or domain specific sentiment analysis. The custom dictionary must be composed of a data frame of two columns where in the first column are the sentiment words and the second the sentiment scores (for each word).  The final sentiment score of the text is calculated using the function provided in score.type. The input is a vector with the sentiment words scores.
}
\examples{
\dontrun{

getCustomSentiment("Gosto muito de pizza!",
                        dictionary=sentiment.portuguese,scale=T,score.type=mean)

getCustomSentiment("The movie was awesome and the plot was unpredictable!",
                        dictionary=sentiment.movies,scale=F,score.type=sum)

getCustomSentiment("The car was beautiful but the steering was unpredictable",
                        dictionary=sentiment.cars,scale=F,score.type=mean)

}
}

