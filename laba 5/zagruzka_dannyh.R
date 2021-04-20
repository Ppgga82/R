# Загрузка данных WDI
library('httr')
library('jsonlite')
library('XML')
library('RCurl')
library('WDI')
library('data.table')

# Индикатор показателя Общий объем выбросов парниковых газов (тыс. т эквивалента CO2)
indicator.code <- 'EN.ATM.GHGT.KT.CE'

# Парсим данные с WDI
data <- data.table(WDI(indicator = indicator.code, start = 2012, end = 2012))

# Загружаем данные в csv файл
write.csv(data, file = './data/dannye_WDI.csv', row.names = F)

# Портал открытых данные РФ

# API ключ для работы с порталом
API.key <- '087b07d503be6b5fa0d1cbfb7326deb6'
# Ссылка для работы с API
URL.base <- 'http://data.gov.ru/api/'

# Функция для работы с API портала открытых данных РФ
getOpenDataRF <- function(api.params, url.base = URL.base, api.key = API.key){
  par <- paste0(api.params, collapse = '/')
  url <- paste0(url.base, par, '/?access_token=', api.key)
  message(paste0('Загружаем ', url, ' ...'))
  resp <- GET(url)
  fromJSON(content(resp, 'text'))
}

# Используем id из задания, чтобы получить нужные нам данные
dataset_id <- '3460012716-zhkhregistryoverhaul'

# Задаем параметры и получаем данные
params <- c('dataset', dataset_id)
dataset <- getOpenDataRF(params)

# Количество версий таблицы
params <- c(params, 'version')
versions <- getOpenDataRF(params)

nrow(versions)

# Загружаем последнюю версию в объект doc
mrv <- versions[nrow(versions), 1]
params <- c(params, mrv)
content <- c(params, 'content')
doc <- getOpenDataRF(content)

# Оставляем данные с городом Суровикино
doc <- doc[grep('г. Суровикино', doc$Address), c('Year2', 'Wall', 'TotalArea1', 'Address')]

# Находим координаты с помощь имеющихся адрессов через API Yandex карт
# Ключ API для работы с Яндекс картами
API.key <- 'a31dfb4a-5efb-4ccd-bc06-e12a7ce2f2b9'
URL.base <- 'https://geocode-maps.yandex.ru/1.x/'

# Функция для работы с API Yandex Карт
getYandexMaps <- function(api.params, url.base = URL.base, api.key = API.key){
  par <- paste0(api.params, collapse = '&')
  url <- paste0(url.base, '?format=xml&apikey=', api.key, par)
  message(paste0('Загружаем ', url, ' ...'))
  doc.ya <- content(GET(url), 'text', encoding = 'UTF-8')
  rootNode <- xmlRoot(xmlTreeParse(doc.ya, useInternalNodes = TRUE))
  coords <- xpathSApply(rootNode, "//*[name()='Envelope']/*", xmlValue)
  coords <- lapply(strsplit(coords, ' '), as.numeric)
  coords <- c((coords[[1]][1] + coords[[2]][1])/2, (coords[[1]][2] + coords[[2]][2])/2)
  names(coords) <-c('lat', 'long')
  coords
}

params <-paste0('&geocode=', gsub(pattern =' ', replacement ='+',
                                  curlEscape(doc$Address[1])))

# Парсим координаты
coords <- sapply(as.list(doc$Address), function(x){
  params <- paste0('&geocode=', gsub(curlEscape(x), pattern = ' ',
                                     replacement = '+'))
  getYandexMaps(params)
})

df.coords <- as.data.frame(t(coords))
colnames(df.coords) <- c('long', 'lat')

#Добавляем координаты в основной фрейм данных
doc <- cbind(doc, df.coords)
# Сохраняем данные в файл
write.csv2(doc, file = './data/dannye_data_gov.csv', row.names = F)
