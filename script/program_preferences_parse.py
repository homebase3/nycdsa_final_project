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
driver = webdriver.Chrome('/usr/bin/chromedriver')

# %%
driver.get('file:///home/justin/Documents/Data%20Science%20Bootcamp/Final%20project/nycdsa_final_project/data/residents/2020-PD-Survey_sub.pdf')

# 13,20,27,34,41,48,55,62,69, 76, 83, 90, 97, 104,111,118, 125, 132, 139, 146, 153, 160, 167
# %% soup page
soup = BeautifulSoup(driver.page_source,'html.parser')
# soup.find_all('span',attrs={'style': (lambda s: 'left' in s)})
lis_ = [i.text for i in soup.select("[style*='left'],[style*='transform']")]
ser_ = pd.Series(lis_)
percent_ser = [i for i in ser_ if "%" in i]
percent_ser = ["11%"if i == "%" else i for i in percent_ser]


# split = [[full[num*3],full[num*3+1],full[num*3+2]] for num in range(int(len(full)/3))]
# pd.DataFrame(split, columns = ["Place name","Latitude","Longitude"]).to_csv("CityZip" + str(i+1) + '.csv')
