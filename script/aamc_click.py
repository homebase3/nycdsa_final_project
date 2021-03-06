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
driver.get('https://www.residencyexplorer.org/Explore')

# %% first 350

els = driver.find_elements_by_xpath("//input[@class = 'myCompare']")
# %%
for a in els[:(min(len(els),300))]:
    # action.move_to_element(a).perform()
    driver.execute_script("arguments[0].scrollIntoView();", a)
    mean = max(np.random.uniform(0.05,0.1),1)
    sd = max(np.random.uniform(0.01,0.05),0.05)
    time.sleep(max(np.random.normal(mean,sd),0.223124))
    a.click()
# %%
for a in els[50:(min(len(els),100))]:
    # action.move_to_element(a).perform()
    driver.execute_script("arguments[0].scrollIntoView();", a)
    mean = max(np.random.uniform(0.05,0.1),1)
    sd = max(np.random.uniform(0.01,0.05),0.05)
    time.sleep(max(np.random.normal(mean,sd),0.223124))
    a.click()
# %%
for a in els[100:(min(len(els),400))]:
    # action.move_to_element(a).perform()
    driver.execute_script("arguments[0].scrollIntoView();", a)
    mean = max(np.random.uniform(0.05,0.1),1)
    sd = max(np.random.uniform(0.01,0.05),0.05)
    time.sleep(max(np.random.normal(mean,sd),0.223124))
    a.click()
# %%
for a in els[150:(min(len(els),215))]:
    # action.move_to_element(a).perform()
    driver.execute_script("arguments[0].scrollIntoView();", a)
    mean = max(np.random.uniform(0.05,0.1),1)
    sd = max(np.random.uniform(0.01,0.05),0.05)
    time.sleep(max(np.random.normal(mean,sd),0.223124))
    a.click()
# %%
for a in els[400:(min(len(els),500))]:
    # action.move_to_element(a).perform()
    driver.execute_script("arguments[0].scrollIntoView();", a)
    mean = max(np.random.uniform(0.05,0.1),1)
    sd = max(np.random.uniform(0.01,0.05),0.05)
    time.sleep(max(np.random.normal(mean,sd),0.223124))
    a.click()

# %%
for a in els[500:(min(len(els),600))]:
    # action.move_to_element(a).perform()
    driver.execute_script("arguments[0].scrollIntoView();", a)
    mean = max(np.random.uniform(0.05,0.1),1)
    sd = max(np.random.uniform(0.01,0.05),0.05)
    time.sleep(max(np.random.normal(mean,sd),0.223124))
    a.click()

# %%
for a in els[600:(min(len(els),700))]:
    # action.move_to_element(a).perform()
    driver.execute_script("arguments[0].scrollIntoView();", a)
    mean = max(np.random.uniform(0.05,0.1),1)
    sd = max(np.random.uniform(0.01,0.05),0.05)
    time.sleep(max(np.random.normal(mean,sd),0.223124))
    a.click()

# %%
soup = BeautifulSoup(driver.page_source,'html.parser')
spec = soup.find_all('span', attrs={'class': 'noprint'})[0].text
arr = [[spec, i.a.text, i.a['href']] for i in soup.find_all('td', attrs={'data-field': 'Program'})]
arr2 = [[i.text] for i in soup.find_all('td', attrs={'data-field': 'City'})]
arr3 = [[i.text] for i in soup.find_all('td', attrs={'data-field': 'State'})]
df1 = pd.DataFrame(arr, columns = ["Specialty","Program", "Link suffix"])
df2 = pd.DataFrame(arr2, columns = ["City"])
df3 = pd.DataFrame(arr3, columns = ["State"])
pd.concat([df1,df2,df3], axis = 1).to_csv(spec.replace("/","-") + '.csv')
