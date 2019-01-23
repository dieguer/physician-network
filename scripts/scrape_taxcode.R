library(RSelenium)

url='http://www.wpc-edi.com/reference/codelists/healthcare/health-care-provider-taxonomy-code-set/'

driver<- rsDriver(browser=c("firefox"))
remDr <- driver[["client"]]
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

lowest=gsub('Â|\\[definition\\]',"", lowest)
v1 <- sub("-.*", "",lowest)
v2 <- sub(".*-", "", lowest)


taxcodel<-cbind.data.frame(v1,v2)

names(taxcodel)=c("desc","code")


middlle=gsub('Â|\\[definition\\]',"", middlle)
v1 <- sub("-.*", "",middlle)
v2 <- sub(".*-", "", middlle)

taxcodem<-cbind.data.frame(v1,v2)

names(taxcodel)=c("desc","code")


taxcodel<-cbind.data.frame(v1,v2)

names(taxcodel)=c("desc","code")



