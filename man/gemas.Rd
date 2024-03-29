% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataSets.R
\docType{data}
\name{gemas}
\alias{gemas}
\title{GEMAS geochemical data set}
\format{
A data frame with 2108 observations and 30 variables
}
\usage{
data(gemas)
}
\description{
Geochemical data set on agricultural and grazing land soil
}
\details{
\itemize{
 \item{\code{COUNTRY }}{country name}
 \item{\code{longitude }}{longitude in WGS84}
 \item{\code{latitude }}{latitude in WGS84}
 \item{\code{Xcoord }}{UTM zone east}
 \item{\code{Ycoord }}{UTM zone north}
 \item{\code{MeanTemp}}{Annual mean temperature}
 \item{\code{AnnPrec }}{Annual mean precipitation}
 \item{\code{soilclass }}{soil class}
 \item{\code{sand }}{sand}
 \item{\code{silt }}{silt}
 \item{\code{clay }}{clay}
 \item{\code{Al }}{Concentration of aluminum (in mg/kg)}
 \item{\code{Ba }}{Concentration of barium (in mg/kg)}
 \item{\code{Ca }}{Concentration of calzium (in mg/kg)}\
 \item{\code{Cr }}{Concentration of chromium (in mg/kg)}
 \item{\code{Fe }}{Concentration of iron (in mg/kg)}
 \item{\code{K }}{Concentration of pottasium (in mg/kg)}
 \item{\code{Mg }}{Concentration of magnesium (in mg/kg)}
 \item{\code{Mn }}{Concentration of manganese (in mg/kg)}
 \item{\code{Na }}{Concentration of sodium (in mg/kg)}
 \item{\code{Nb }}{Concentration of niobium (in mg/kg)}
 \item{\code{Ni }}{Concentration of nickel (in mg/kg)}
 \item{\code{P }}{Concentration of phosphorus (in mg/kg)}
 \item{\code{Si }}{Concentration of silicium (in mg/kg)}
 \item{\code{Sr }}{Concentration of strontium (in mg/kg)}
 \item{\code{Ti }}{Concentration of titanium (in mg/kg)}
 \item{\code{V }}{Concentration of vanadium (in mg/kg)}\
 \item{\code{Y }}{Concentration of yttrium (in mg/kg)}
 \item{\code{Zn }}{Concentration of zinc (in mg/kg)}
 \item{\code{Zr }}{Concentration of zirconium (in mg/kg)}
 \item{\code{LOI }}{Loss on ignition (in wt-percent)}
}

The sampling, at a density of 1 site/2500 sq. km, was completed at the beginning of 2009 by collecting 2211 samples of agricultural soil (Ap-horizon, 0-20 cm, regularly ploughed fields), and 2118 samples from land under permanent grass cover (grazing land soil, 0-10 cm), according to an agreed field protocol.
All GEMAS project samples were shipped to Slovakia for sample preparation, where they were air dried, sieved to <2 mm using a nylon screen, homogenised and split to subsamples for analysis. They were analysed for a large number of chemical elements. In this sample, the main elements by X-ray fluorescence are included as well as the composition on sand, silt, clay.
}
\examples{

data(gemas)
str(gemas)
## sample sites
\dontrun{
require(ggmap)
map <- get_map("europe", source = "stamen", maptype = "watercolor", zoom=4)
ggmap(map) + geom_point(aes(x=longitude, y=latitude), data=gemas)
map <- get_map("europe", zoom=4)
ggmap(map) + geom_point(aes(x=longitude, y=latitude), data=gemas, size=0.8)
}
}
\references{
Reimann, C., Birke, M., Demetriades, A., Filzmoser, P. and O'Connor, P. (Editors), 2014. Chemistry of Europe's agricultural soils - Part A: Methodology and interpretation of the GEMAS data set. Geologisches Jahrbuch (Reihe B 102), Schweizerbarth, Hannover, 528 pp. + DVD 
Reimann, C., Birke, M., Demetriades, A., Filzmoser, P. & O'Connor, P. (Editors), 2014. Chemistry of Europe's agricultural soils - Part B: General background information and further analysis of the GEMAS data set. Geologisches Jahrbuch (Reihe B 103), Schweizerbarth, Hannover, 352 pp.
}
\author{
GEMAS is a cooperation project between the EuroGeoSurveys Geochemistry Expert Group and Eurometaux. Integration in R, Peter Filzmoser and Matthias Templ.
}
\keyword{data}
