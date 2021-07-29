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
driver.get('https://residency.doximity.com/specialties')

# %%
soup = BeautifulSoup(driver.page_source,'html.parser')
links =['https://residency.doximity.com' + i['href'] for i in soup.find_all('a')[5:-9]]
specs = [i.text for i in soup.find_all('a')[5:-9]]

# %%
outdict = {}
outdict['Specialty'] = []
outdict['Program name'] = []
outdict['Doximity link'] = []
for i, link in enumerate(links):
    driver.get(link)
    soup = BeautifulSoup(driver.page_source,'html.parser')
    program_links =['https://residency.doximity.com' + i['href'] for i in soup.find_all('a')][6:-7]
    programs = [i.text for i in soup.find_all('a')][6:-7]

    outdict['Specialty'] += [specs[i]]*len(programs)
    outdict['Program name'] += programs
    outdict['Doximity link'] += program_links

# %%
links[1]

# %%
pd.DataFrame.from_dict(outdict).to_csv()
