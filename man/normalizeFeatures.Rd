\name{normalizeFeatures}
\alias{normalizeFeatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalization of expression features
}
\description{
Normalization of expression features. Several methods available in the package \code{CONOR} can be used.
}
\usage{
normalizeFeatures(x, method=c("none", "scale", "eb", "gq", "xpn"))
}
\arguments{
  \item{x}{
An object result of a call to \code{matchFeatures}
}
  \item{method}{
Several methods are available:
"none": No normalization is done
"scale": Each expression feature is scaled to have zero mean and standard deviation 1
"eb": empirical bayes method (eb) See \code{CONOR} documentation for details
"gq": gene quantile method (gq) See \code{CONOR} documentation for details
"eb": xpn method (xpn) See \code{CONOR} documentation for details.
}
}
\details{
No further normalization is needed on the copy number, as log2 ratios are comparable between platforms.
}
\value{
A list of the same format as \code{matchFeatures}, but with \code{train.Exp} anfd \code{Exp} normalized.
}
\references{
Curtis et al. The genomic and transcriptomic architecture of 2,000
breast tumours reveals novel subgroups. Nature 2012; 486:346-352.
Rudy and Valafar. Empirical comparison of cross-platform normalization methods for gene 
expression data. BMC Bioinformatics, 2011; 12:467.
Johnson et al. Adjusting batch effects in microarray expression  data using empirical Bayes methods. 
Biostatistics, 2007: 8(1)
Xia et al. WebArrayDB: cross-platform microarray data analysis and public data repository. 
Bioinformatics, 2009: 25(18).
Shabalin et al. Merging two gene-expression studies via cross-platform normalization. Bioinformatics,
  2008; 24:9.
}
\author{
Oscar M Rueda
}
\note{
Original functions from the \code{CONOR} package are used.
}

\seealso{
\code{\link{xpn}}, \code{\link{gq}}, \code{\link{eb}}
}
\examples{
require(iC10TrainingData)
data(train.CN)
data(train.Exp)
features <- matchFeatures(Exp=train.Exp,
Exp.by.feat="probe", ref="hg18")
features <- normalizeFeatures(features, "scale")
}