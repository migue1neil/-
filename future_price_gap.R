setwd("C:/Users/Neil/Desktop/期貨與選擇權") # 設定工作目錄

#學習篩選欄位後抓取、ifelse()、
library(tidyr) #整理數據使用
library(lubridate) #日期模組
library(data.table) #把df格式轉換為data.table格式，基本上操作方式一樣，但在處理大量資料(萬筆)的時候，速度有明顯差異
library(ggplot2)  #畫圖

future_gap = read.csv("期貨正逆價差.csv",stringsAsFactors = FALSE) 
future_gap = as.data.table(future_gap)
future_gap$年月日 = ymd(future_gap$年月日) # 將年月日從字串轉換為日期格式
colnames(future_gap) = c("證券代碼","年月日","加權指數收盤價","台指期收盤價") #重新命名欄位名稱

#取代逗號再把數據從文字轉換成數字
future_gap$加權指數收盤價 = gsub(",","",future_gap$加權指數收盤價)  
future_gap$台指期收盤價 = gsub(",","",future_gap$台指期收盤價)
future_gap$加權指數收盤價 = as.numeric(future_gap$加權指數收盤價)
future_gap$台指期收盤價 = as.numeric(future_gap$台指期收盤價)

#很白癡的方法，因為抓下來的資料很酷，把數據拆成兩部分再用merge 方法合併，
market_closed = future_gap[!(future_gap$加權指數收盤價 == ""),]  #df[row,col]
market_closed = market_closed[,-4]
future_index = future_gap[!(future_gap$台指期收盤價 == ""),]
future_index = future_index[,-3]

New_future_gap = merge(market_closed,future_index , by = "年月日" ,all = TRUE) #根據年月日合併資料，保留所有數據
New_future_gap$價差 = New_future_gap$台指期收盤價 - New_future_gap$加權指數收盤價 #新增一個欄位做數據加減

New_future_gap$正價差 = ifelse(New_future_gap$價差>0  ,1,0) #將價差轉換成01虛擬變數
stat = table(New_future_gap$正價差)
p = round( mean(New_future_gap$正價差), digits = 3) #出現正價差的機率

ggplot(New_future_gap, aes(x = 年月日, y = 價差))+ #畫圖
  geom_line(aes(y = 價差))
New_future_gap$正價差 = ifelse(New_future_gap$價差>0 , 1, 0) #將價差轉換成01虛擬變數

gap_prb = function(date){ #設計一個函式，輸入日期之後可以吐出正逆價差次數，以及出現正價差的機率
date = as.character(date)
df = New_future_gap[(New_future_gap$年月日 >= ymd(date)),]
tmp = table(df$正價差)
p = round( mean(df$正價差), digits = 3) #出現正價差的機率
cat("逆價差的次數為: ",tmp[1],"正價差的次數為: ",tmp[2],"正價差的機率為: ",p*100 , "%")
}

gap_prb(20050101) #我想要看2005年以後的正逆價差情形
