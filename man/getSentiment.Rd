\name{getSentiment}
\alias{getSentiment}
\title{Get Sentiment Function}
\usage{
getSentiment(text,dictionary="bing",scale=FALSE,score.type=sum)
}
\arguments{
\item{text}{The text which should be analyze.}
\item{dictionary}{The dictionary that should be used to perform the sentiment analysis task.}
\item{scale}{A boolean to determine if the dictionary should be scaled to a [-1,1] range}
\item{score.type}{The function to be used in the final sentiment score of the text. By default it is used the sum of all the values of the sentiment words. But any function that accepts a numeric vector can be used.}
}

\description{
The \code{getSentiment} is a highly customizable function that performs a sentiment analysis on the text provided using a pre-selected sentiment dictionary. The available dictionaries are "afinn", "bing" and "vader". The "afinn" dictionary scores range from a -5 to 5 whether "vader" ranges from -4 to 4. In Bing each word has only a positive (1) or negative (-1) value.

For more information on each dictionary:

Bing: Minqing Hu and Bing Liu. 2004. Mining and summarizing customer reviews. In Proceedings of the tenth ACM SIGKDD international conference on Knowledge discovery and data mining (KDD '04). ACM, New York, NY, USA, 168-177. DOI=http://dx.doi.org/10.1145/1014052.1014073

Afinn: Finn Arup Nielsen. A new {ANEW}: evaluation of a word list for sentiment analysis in microblogs. Proceedings of the ESWC2011 Workshop on 'Making Sense of Microposts': Big things come in small packages. May 2011

Vader: Hutto, C.J. & Gilbert, E.E. (2014). VADER: A Parsimonious Rule-based Model for Sentiment Analysis of Social Media Text. Eighth International Conference on Weblogs and Social Media (ICWSM-14). Ann Arbor, MI, June 2014.


The final sentiment score of the text is calculated using the function provided in score.type.
}
\examples{
\dontrun{
getSentiment("I like you so much!!!", dictionary="afinn",scale=T,score.type=mean)

getSentiment("I love the new star wars movie but I hated the ending!",
                    dictionary="vader",scale=F,score.type=sum)

}
}

