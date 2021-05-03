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
for a in els[0:(min(len(els),350))]:
    # action.move_to_element(a).perform()
    driver.execute_script("arguments[0].scrollIntoView();", a)
    a.click()

# %% first 350
for a in els[(min(len(els),350)):]:
    # action.move_to_element(a).perform()
    driver.execute_script("arguments[0].scrollIntoView();", a)
    a.click()

# %%
els[0:(min(len(els),350))]
