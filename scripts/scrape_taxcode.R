library(RSelenium)
library(rvest)
library(XML)

#sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1

url='http://www.wpc-edi.com/reference/codelists/healthcare/health-care-provider-taxonomy-code-set/'

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

remDr$open()
remDr$navigate(url)

webElement <- remDr$findElement(using = "css selector", "#maincolumn > div.item-page > table > tbody:nth-child(1) > tr > td > div > iframe")
remDr$switchToFrame(webElement)
webElement1<-remDr$findElement(using = "css selector", 'html> frameset > frame')
remDr$switchToFrame(webElement1)
webElement2<-remDr$findElement(using = "css selector", 'html>body ')

contentPage <- remDr$getPageSource()


t=htmlParse(contentPage[[1]])

lowest=xpathSApply(t,'//*[@id="flx"]', xmlValue)
middlle=xpathSApply(t,'//div[@id="RYO_HTML1"]/ul/ul/ul/li', xmlValue)
general=xpathSApply(t,'//div[@id="RYO_HTML1"]/ul/ul/li', xmlValue)

lowest=gsub('?|\\[definition\\]',"", lowest)
lowest=gsub('Ã‚Â', '', lowest)  
v1 <- sub("-.*", "",lowest)
v2 <- sub(".*-", "", lowest)
taxcodel<-cbind.data.frame(v1,v2)
names(taxcodel)=c("desc","code")



middlle=gsub('?|\\[definition\\]',"", middlle)
middlle=gsub('Ã‚Â', '', middlle)  
v1 <- sub("-.*", "",middlle)
v2 <- sub(".*-", "", middlle)
taxcodem<-cbind.data.frame(v1,v2)
names(taxcodem)=c("desc","code")

general=gsub('?|\\[definition\\]',"", general)
general=gsub('Ã‚Â', '', general)  
v1 <- sub("-.*", "",general)
v2 <- sub(".*-", "", general)
taxcodeg<-cbind.data.frame(v1,v2)
names(taxcodeg)=c("desc","code")





remDr$closeServer()
rm(remDr)


