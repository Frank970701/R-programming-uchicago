#read html document
library(tidyverse)
library(rvest)
HTML<-read_html('http://uscode.house.gov/table3/111_273.htm')
table<-html_node(HTML,'table')
rows<-html_nodes(table,'tr')

#basic loop framework(method1)
bill_data=data.frame()
for (i in 1:length(rows)){
  .row<-rows[i]
  print(.row)
  if(html_attr(.row,'class') %in% c('table3row_even','table3row_odd')){
    .cell<-html_nodes(.row,'td') #for basic content
    .cell2<-html_nodes(.row,'td a') #for url
    print(.cell)
    print(.cell2)
    .actsection<-html_text(.cell[1])
    .statutesatlargepage<-html_text(.cell[2])
    .legislation_url<-html_attr(.cell2,'href')[1]
    .unitedstatescodetitle<-html_text(.cell[3])
    .unitedstatescodesection<-html_text(.cell[4])
    .unitedstatescodesection_url<-html_attr(.cell2,'href')[2]
    .unitedstatescodestatus<-html_text(.cell[5])
    .output <- setNames(c(.actsection, .statutesatlargepage, .legislation_url,.unitedstatescodetitle,.unitedstatescodesection,.unitedstatescodesection_url,.unitedstatescodestatus), c("actsection", "statutesatlargepage", "legislation_url",'unitedstatescodetitle','unitedstatescodesection','unitedstatescodesection_url','unitedstatescodestatus'))
    bill_data<-bind_rows(bill_data,.output)
  }
}
glimpse(bill_data)

#basic loop framework(method2)
bill_data=data.frame()
for (i in 1:length(rows)){
  .row<-rows[i]
  print(.row)
  if(!is.na(html_attr(.row,'class'))){
    if(html_attr(.row,'class')=='table3row_even'|html_attr(.row,'class')=='table3row_odd'){
      .cell<-html_nodes(.row,'td') #for basic content
      .cell2<-html_nodes(.row,'td a') #for url
      print(.cell)
      print(.cell2)
      .actsection<-html_text(.cell)[1]
      .statutesatlargepage<-html_text(.cell)[2]
      .legislation_url<-html_attr(.cell2,'href')[1]
      .unitedstatescodetitle<-html_text(.cell)[3]
      .unitedstatescodesection<-html_text(.cell)[4]
      .unitedstatescodesection_url<-html_attr(.cell2,'href')[2]
      .unitedstatescodestatus<-html_text(.cell)[5]
      .output <- setNames(c(.actsection, .statutesatlargepage, .legislation_url,.unitedstatescodetitle,.unitedstatescodesection,.unitedstatescodesection_url,.unitedstatescodestatus), c("actsection", "statutesatlargepage", "legislation_url",'unitedstatescodetitle','unitedstatescodesection','unitedstatescodesection_url','unitedstatescodestatus'))
      bill_data<-bind_rows(bill_data,.output)}
  }
}
glimpse(bill_data)

#build loop function
get_bill<-function(x,y){
  HTML<-read_html(paste('http://uscode.house.gov/table3/',x,'_',y,'.htm',sep = ''))
  bill_data=data.frame()
  table<-html_node(HTML,'table')
  rows<-html_nodes(table,'tr')
  for (i in 1:length(rows)){
    .row<-rows[i]
    print(.row)
    if(html_attr(.row,'class') %in% c('table3row_even','table3row_odd')){
      .cell<-html_nodes(.row,'td') #for basic content
      .cell2<-html_nodes(.row,'td a') #for url
      print(.cell)
      print(.cell2)
      .actsection<-html_text(.cell[1])
      .statutesatlargepage<-html_text(.cell[2])
      .legislation_url<-html_attr(.cell2,'href')[1]
      .unitedstatescodetitle<-html_text(.cell[3])
      .unitedstatescodesection<-html_text(.cell[4])
      .unitedstatescodesection_url<-html_attr(.cell2,'href')[2]
      .unitedstatescodestatus<-html_text(.cell[5])
      .output <- setNames(c(.actsection, .statutesatlargepage, .legislation_url,.unitedstatescodetitle,.unitedstatescodesection,.unitedstatescodesection_url,.unitedstatescodestatus), c("actsection", "statutesatlargepage", "legislation_url",'unitedstatescodetitle','unitedstatescodesection','unitedstatescodesection_url','unitedstatescodestatus'))
      bill_data<-bind_rows(bill_data,.output)
    }
  }
  return(bill_data)
}

#function test
test_111_292<-get_bill(111,292)
glimpse(test_111_292)
test_111_346<-get_bill(111,346)
glimpse(test_111_346)
test_112_194<-get_bill(112,194)
glimpse(test_112_194)