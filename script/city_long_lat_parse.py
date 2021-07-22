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


# %% soup page
for i in range(8,9):
    driver.get('https://www.latlong.net/category/cities-236-15-' + str(i+1) +'.html')
    soup = BeautifulSoup(driver.page_source,'html.parser')
    full = [i.text for i in soup.find_all('td')[5:-2]]
    split = [[full[num*3],full[num*3+1],full[num*3+2]] for num in range(int(len(full)/3))]
    pd.DataFrame(split, columns = ["Place name","Latitude","Longitude"]).to_csv("CityZip" + str(i+1) + '.csv')
