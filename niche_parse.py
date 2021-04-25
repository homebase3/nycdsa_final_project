# %% load in libraries
from bs4 import BeautifulSoup
import pandas as pd
import time
from selenium import webdriver
import random

# %% set up selenium and login
from selenium import webdriver
driver = webdriver.Firefox()

# %% get counties
urlbase = 'https://www.niche.com/places-to-live/search/best-counties/?page='

driver.get(urlbase)
# wait for it to load
time.sleep(3)

#soup page
soup = BeautifulSoup(driver.page_source,'html.parser')

# %%
Counties = []
Links = []
r = list(range(1,127))
random.shuffle(r)
for i in r:
    driver.get(urlbase + str(i))
    # wait for it to load
    sleep_time = random.uniform(3,10)
    time.sleep(sleep_time)
    #soup page
    soup = BeautifulSoup(driver.page_source,'html.parser')

    Counties += [a.text for a in soup.find_all('h2',{'class':"search-result__title"})]
    Links += [a['href'] for a in soup.find_all('a',{'class':"search-result__link"})]

# %%
dict_ = {}
dict_['County'] = Counties
dict_['Link'] = Links
df = pd.DataFrame.from_dict(dict_)
df.to_csv('county_names.csv')
