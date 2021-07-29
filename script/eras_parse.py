# %% load in libraries
from bs4 import BeautifulSoup, Comment
import pandas as pd
import time
from selenium import webdriver
import random
import numpy as np
from selenium.webdriver.common.action_chains import ActionChains
import re
from gazpacho import Soup

# %% set up selenium

from selenium import webdriver
driver = webdriver.Firefox()

# %%
driver.get('https://services.aamc.org/eras/erasstats/par/index.cfm?NAV_ROW=PAR')

# %%
soup = BeautifulSoup(driver.page_source,'html.parser')
spec = soup.find_all('h3')[0].text
set1 = 19
first = [i.text.strip() for i in soup.find_all('td')]
arr = np.array(first[set1:-2]).reshape((int(len(first[set1:-2])/6),6))
second = [i.a['href']if i.a != None else None for i in soup.find_all('td')][set1+3::6]
arr2 = np.array(second).reshape((len(second),1))
df1 = pd.DataFrame(arr, columns = ["State","City", "Blank", "Program name","ID","Status"])
df2 = pd.DataFrame(arr2, columns = ["ERAS link"])
pd.concat([df1,df2], axis = 1).to_csv(spec.replace("/","-") + '.csv')
