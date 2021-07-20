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
driver = webdriver.Chrome()

# %%
driver.get('https://www.residencyexplorer.org/Explore')

# %% first 350

els = driver.find_elements_by_xpath("//input[@class = 'myCompare']")
# %%
for a in els[:(min(len(els),100))]:
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
for a in els[100:(min(len(els),150))]:
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
[i.a.text for i in soup.find_all('td', attrs={'data-field': 'Program'})]
