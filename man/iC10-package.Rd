\name{iC10-package}
\alias{iC10-package}
\docType{package}
\title{
A copy number and expression-based classifier for breast tumours.
}
\description{
iC10 implements the classifier described in the METABRIC paper 'The genomic and transcriptomic architecture of 2,000 breast tumours reveals novel subgroups' (Curtis et al., Nature 2012). It uses copy number and/or expression form breast cancer data, trains a pamr classifier (Tibshirani et al.) with the features available and predicts the iC10 group.
}
\details{
\tabular{ll}{
Package: \tab iC10\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-22\cr
License: \tab What license is it under?\cr
}
data(train.CN)
data(train.Exp)
features <- matchFeatures(Exp=train.Exp, Exp.by.feat="probe")
features <- normalizeFeatures(features, "scale")
res <- iC10(features)
summary(res)
goodnessOfFit(res, newdata=features)
compare(res, ic10=1:5, newdata=features)
compare(res, ic10=6:10, newdata=features)
}
\author{
Oscar M Rueda

Maintainer: <Oscar.Rueda@cruk.cam.ac.uk>
}
\references{
Curtis et al. The genomic and transcriptomic architecture of 2,000
breast tumours reveals novel subgroups. Nature 2012; 486:346-352.
Tibshirani et al. Diagnosis of multiple cancer types by shrunken
centroids of gene expression. PNAS 2002; 99(10):6567-6572.
}

\seealso{
\code{pamr}, \code{CONOR}
}
\examples{
require(iC10TrainingData)
data(train.CN)
data(train.Exp)
features <- matchFeatures(Exp=train.Exp, Exp.by.feat="probe")
features <- normalizeFeatures(features, "scale")
res <- iC10(features)
summary(res)
goodnessOfFit(res, newdata=features)
compare(res, iC10=1:2, newdata=features)
compare(res, iC10=2:4, newdata=features)
}