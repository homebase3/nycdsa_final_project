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
driver.get('https://www.residencyexplorer.org/Program/GetById/3635')

#%%
soup = BeautifulSoup(driver.page_source,'html.parser')

# %%
itemdict = {}

# %% applications
for i in range(0,4):
    try:
        driver.find_element_by_xpath("//*[name()='circle'][position()=" + str(i+1) + "]").click()
        soup = BeautifulSoup(driver.page_source,'html.parser')
        itemdict[str(2017 + i) + " total applications"] = int(soup.find('div',{'class':'k-tooltip k-chart-tooltip'}).text)
    except:
        itemdict[str(2017 + i) + " total applications"] = None
# menu

# %% Positions
try:
    itemdict["Positions offered in " + "2020"] = int(soup.find('th',string = '# of Positions Offered:').next_element.next_element.next_element.text)
except:
    pass

try:
    itemdict["Positions filled in " + "2020"] = int(soup.find('th',string = '# of Positions Filled:').next_element.next_element.next_element.text)
except:
    pass

# %% Selectivity
# # try:
# pull = soup.find('th',string = '# of Positions Offered in the<br/>NRMP Main Residency Match:')
# itemdict["Positions offered in " + "2019"] = int(pull)
# # except:
# #     pass
soup = BeautifulSoup(driver.page_source,'html.parser')
try:
    pull = soup.select_one('th:-soup-contains("Positions Offered")').next_element.next_element.next_element.text.strip()
    itemdict["Positions Offered in 2019"] = int(pull)
except:
    pass

try:
    pull1 = soup.select_one('th:-soup-contains("Applicants")').next_element.next_element.next_element
    pull2 = pull1.next_element.next_element.next_element
    try:
        itemdict["Number of Applications in 2019"] = int(pull1.text.strip())
    except:
        pass
    try:
        itemdict["Number of Applications in 2019 vs. Specialty Average"] = itemdict["Number of Applications in 2019"]/int(pull2.text.strip()) - 1
    except:
        pass
except:
    pass

try:
    pull1 = soup.select_one('th:-soup-contains("Interviewed")').next_element.next_element.next_element.next_element.next_element
    pull2 = pull1.next_element.next_element.next_element
    try:
        itemdict["% of Applicants Interviewed"] = float(pull1.text.strip())/100
    except:
        pass
    try:
        itemdict["% of Applicants Interviewed vs. Specialty Average"] = itemdict["% of Applicants Interviewed"]*100/float(pull2.text.strip()) - 1
    except:
        pass
except:
    pass



# try:
#     pull = soup.find('th',string = r'*# of Applicants interviewed:*').next_element.next_element.next_element.text.strip()
#     itemdict["Positions offered in " + "2019"] = int(pull)
# except:
#     pass

# %%
# pull = soup.find('th', text = re.compile('(\# of Positions Offered in the)')).next_element.next_element.next_element.text.strip()
# itemdict["Positions offered in 2019"] = int(pull)

# %%
m_bars = [20.5, 84.5, 147.5,210.5,274.5,337.5,401.5,464.5]
levels_base = ['0','1-2','3-4','5-6','7-8','9-10','11-12','13+']
levels_vol = ['0','1-3','4-6','7-9','10-12','13-15','16-18','19+']
levels = {}
levels['workExp'] = levels_base
levels['volExp'] = levels_vol
levels['researchExp'] = levels_base
levels['pubExp'] = levels_base


# %%
element = driver.find_element_by_xpath("//h5[contains(text(),'Your characteristics compared to applicants who ma')]")
driver.execute_script("arguments[0].scrollIntoView(true);", element)

# %% scrape percentiles, using gazpacho as parser
full = 72.5
soup = Soup(driver.page_source)
for key,val in levels.items():
    tot = 0
    for i, val2 in enumerate(val):
        try:
            pull = soup.find("div",{'id':key},partial = False).find('path',{'d':'M'+str(m_bars[i])},partial = True)[0].attrs['d'].split(" ")[1]
            calc_ = full - float(pull)
            itemdict[key + "_" + val2] = calc_
            tot += calc_

        except Exception	Brookwood Baptist Health Program	97%							Birmingham	AL	Southern
 as err:
            itemdict[key + "_" + val2] = 0
    for i, val2 in enumerate(val):
        itemdict[key + "_" + val2] /= tot

# % scrape step scores
soup = BeautifulSoup(driver.page_source,'html.parser')
comments = soup.find_all(text=lambda text:isinstance(text, Comment))
try:
    pull = comments[comments.index(' START Step1 TABLE ')].next_element.next_element.find('strong').text
    itemdict['Step 1 score range'] = pull
except:
    pass

# %%





# %%
itemdict


# %%
itemdict = {}

# %%
for key,val in levels.items():
    for i, val2 in enumerate(val):
        for el in driver.find_elements_by_xpath("(//div[@id='"+key+"']//*[local-name()='svg']//*[name()='g']//*[name()='path' and contains(@d,'M"+str(m_bars[i])+"')])"):
            try:


# %%
itemdict


# %%

soup.find("div",{'id':'workExp'},partial = False).find('path',{'d':'M20.5'},partial = True)[1]

# %%
soup = Soup(driver.page_source)
soup.find("div",{'id':key},partial = False).find('path',{'d':"M84.5"},partial = True)

# %%
