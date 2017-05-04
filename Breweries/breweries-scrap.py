from selenium import webdriver
from bs4 import BeautifulSoup
import random
import time
import pandas as pd

driver = webdriver.Chrome()
driver.get("https://www.brewersassociation.org/directories/breweries/")

driver.find_element_by_xpath('//*[@id="country"]/a').click()
driver.find_element_by_xpath('//*[@id="country_select"]/li[1]').click()

seconds = 5 + (random.random() * 5)
time.sleep(seconds)

html = driver.page_source
soup = BeautifulSoup(html)

Name=[]
Names=[]
Street=[]
Streets=[]
BrewType=[]
BrewTypes=[]
BrewType2=[]
BrewTypes2=[]
Address=[]
Addresses=[]
Towns=[]
Towns2=[]
Name=soup.findAll("li", {"class":"name"})
for element in Name:
 	Names.append(element.get_text().encode('utf8'))

Street=soup.findAll("li", {"class":"address"})
for element in Street:
 	Streets.append(element.get_text().encode('utf8'))

BrewType=soup.findAll("li", {"class":"address"})
for element in BrewType:
 	BrewTypes.append(element.find_next('li').find_next('li').get_text().encode('utf8'))

BrewType2=soup.findAll("li", {"class":"address"})
for element in BrewType2:
 	BrewTypes2.append(element.find_next('li').find_next('li').find_next('li').get_text().encode('utf8'))

Section=soup.findAll('li', {"class":"address"})
for town in Section:
	Towns.append(town.find_next('li').get_text().encode('utf8'))

Section2=soup.findAll('li', {"class":"name"})
for town2 in Section2:
	Towns2.append(town2.find_next('li').get_text().encode('utf8'))

# print len(Names)
# print len(Streets)
# print len(BrewTypes)
# print len(Towns)
# # print len(Towns2)

brewery_data = pd.DataFrame({'Names':Names, 'Streets':Streets, 'Towns':Towns, 'TownsPlanning':Towns2, 'Type':BrewTypes, 'Type2':BrewTypes2})

brewery_data.to_csv('brewery_data113.csv')
