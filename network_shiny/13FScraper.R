library(scrapeR)

CIK <- "1547007" #CIK number for Dorsal
url <- paste("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=",CIK,sep="")

table <- readHTMLTable(url, stringsAsFactors = F)
table <- table[[3]]

df2 <- table[!duplicated(table$Filings), ]
H <- df2$Filings == "13F-HR"
df2 <- df2[H, ]

file.no <- gsub("Quarterly report filed by institutional managers, HoldingsAcc-no: ", "", df2[1, 3])
file.no <- sub("Â.*", "", file.no)
file.no <- gsub("-", "", file.no)

url2 <- paste("http://www.sec.gov/Archives/edgar/data/",CIK,"/",file.no,"/xslForm13F_X01/infotable.xml",sep="")

holdings <- readHTMLTable(url2, stringsAsFactors = F)
holdings <- holdings[[4]]
holdings.vec <- as.matrix(holdings[-1:-3, 3])