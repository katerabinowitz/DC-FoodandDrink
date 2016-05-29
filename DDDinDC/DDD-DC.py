import pandas as pd
import numpy as np
import urllib
from bs4 import BeautifulSoup as Soup, Tag
import urllib2

### Scrape biggest US cities ###
wiki = "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population"
header = {'User-Agent': 'Mozilla/5.0'} #Needed to prevent 403 error on Wikipedia
req = urllib2.Request(wiki,headers=header)
page = urllib2.urlopen(req)
soup = Soup(page)
 
city = []
state = []
pop=[]
 
table = soup.find("table", { "class" : "wikitable sortable" })
 
for row in table.findAll("tr"):
    cells = row.findAll("td")
    #For each "tr", assign each "td" to a variable.
    if len(cells) == 9:
        city.append(cells[1].find(text=True))
        state.append(cells[2].find(text=True))
        pop.append(cells[3].find(text=True))

cityState =[]
for each in range(0,len(city)):
        cityState.append(city[each] + ', ' + state[each])

cityDF=pd.DataFrame({'cityState':cityState, 'pop':pop})
city50=cityDF.head(50)

### Read in DDD Date###
url = "https://raw.githubusercontent.com/mcriqui/DDD/master/FINAL_csv.csv"
webpage = urllib.urlopen(url)
DDD= pd.read_csv(webpage)

DDD["City"]=DDD["City"].str.strip()
DDD["State"]=DDD["State"].str.strip()

def f(row):
  if row['City'] == "Queens":
    val= "New York"
  elif row['City'] == "Brooklyn":
    val= "New York"
  elif row['City'] == "Manhattan":
    val= "New York"
  elif row['City'] == "Bronx":
    val= "New York"
  elif row['City'] == "Long Island":
    val= "New York"
  else: 
   	val=row['City']
  return val
DDD['City'] = DDD.apply(f, axis=1)

def s(row):
  if row['State'] == "DC":
    val= "District of Columbia"
  else: 
   	val=row['State']
  return val
DDD['State'] = DDD.apply(s, axis=1)

def r(row):
  if row['Rating'] == "Closed":
    val=None
  else:
   	val=row['Rating']
  return val
DDD['Rating'] = DDD.apply(r, axis=1)

DDD["cityState"] = DDD["City"] + ", " + DDD["State"]

DDD["Rating"] = DDD["Rating"].astype(float)

city50DDD = pd.merge(city50, DDD, how='left', on=['cityState'])

countDDD=city50DDD[['Resturant','City']].groupby(['City']).agg(['count'])
avgRating=city50DDD[['Rating','City']].groupby(['City']).agg(['mean'])

countDDD.to_csv('countDDD.csv')
avgRating.to_csv('avgRating.csv')

