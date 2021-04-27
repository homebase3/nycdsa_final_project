# %% load in libraries
from bs4 import BeautifulSoup
import pandas as pd
import time
from selenium import webdriver
import random

# %% set up selenium
from selenium import webdriver
driver = webdriver.Firefox()

# %% login
url1 = 'https://www.doximity.com/residency/'
driver.get(url1)

# %% get links to each specialty
soup = BeautifulSoup(driver.page_source,'html.parser')
fields_full = [a.get_text() for a in soup.find_all('option')]
keys = [a['value'] for a in soup.find_all('option')]

#%%
urlbase = 'https://www.doximity.com/residency/programs?specialtyKey=&'
urltail = '&trainingEnvironmentKey=all&intendedFellowshipKey='

#%%
sortkeys = ['reputation','research_rank','program_size_approved','percent_grad_subspecialize']
sortvals = ['reputation','research output','program size','percent subspecialize']

#%%
driver.get(urlbase+keys[0]+'sortByKey='+sortkeys[0]+urltail)
soup = BeautifulSoup(driver.page_source,'html.parser')

# %%
names = []
links = []
fields = []
ranks = []
sorttypes = []
for sortkey, sortval in zip(sortkeys,sortvals):
    for key, field in zip(keys,fields_full):
        try:
            time.sleep(3)
            driver.get(urlbase+key+'sortByKey='+sortkey+urltail)
        except:
            continue
        res = 0
        print(sortkey,sortval,key,field)
        # while res == 0:
        #     try:
        #         time.sleep(1)
        #         driver.find_element_by_xpath("//div[@class='residency-show-more-results']").click()
        #     except:
        #         soup = BeautifulSoup(driver.page_source,'html.parser')
        #         souped = soup.find_all('a',{'class':"residency-result-program-title"})
        #         names += [a.text for a in souped]
        #         links += ['https://www.doximity.com' + a['href'] for a in souped]
        #         fields += [field]*len(names)
        #         ranks += range(len(names))
        #         sorttypes += [sortval]*len(names)
        #     finally:
        #         res = 1

# %%
# make dict to save
dict_ = {}
dict_['Field'] = fields
dict_['Program'] = names
dict_['Link'] = links
dict_['Rank'] = ranks
dict_['Sort type'] = sorttypes
df = pd.DataFrame.from_dict(dict_)
# df.to_csv('best_counties_buy_house.csv')

#%%
list(zip(keys,fields))
