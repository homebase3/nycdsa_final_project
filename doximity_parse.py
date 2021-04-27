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
specialties = [a.get_text() for a in soup.find_all('option')]
addresses = [a['value'] for a in soup.find_all('option')]
specialties = specialties[1:-4]
addresses = addresses[1:-4]

#%%
urlbase = 'https://www.doximity.com/residency/programs?specialtyKey=&'
urltail = '&trainingEnvironmentKey=all&intendedFellowshipKey='

#%%
sortkeys = ['reputation','research_rank','program_size_approved','percent_grad_subspecialize','alphabetical']
sortvals = ['reputation','research output','program size','percent subspecialize', 'alphabetical']

#%%
driver.get(urlbase+addresses[0]+'sortByKey='+sortkeys[0]+urltail)
soup = BeautifulSoup(driver.page_source,'html.parser')
time.sleep(1)
driver.find_element_by_xpath("//div[@class='dropdown-menu-select']").click()
time.sleep(1)
driver.find_element_by_xpath("//li[@class='dropdown-menu-options-item'][position()=2]").click()
# time.sleep(1)
driver.find_element_by_xpath("//option[position()=2]").click()

# %%

# %%

names = []
links = []
fields = []
ranks = []
sorttypes = []
for i, val in enumerate(specialties):
    #navigate to specialty
    time.sleep(1)
    driver.find_element_by_xpath("//div[@class='dropdown-menu-select']").click()
    time.sleep(1)
    driver.find_element_by_xpath("//li[@class='dropdown-menu-options-item'][position()="+str(i+1)+"]").click()
    for j, val2 in enumerate(sortvals):
        #navigate to sorttype
        try:
            time.sleep(1)
            driver.find_element_by_xpath("//option[position()="+str(j+1)+"]").click()
        except:
            break
        res = 0
        while res == 0:
            try:
                time.sleep(1)
                driver.find_element_by_xpath("//div[@class='residency-show-more-results']").click()
            except:
                soup = BeautifulSoup(driver.page_source,'html.parser')
                souped = soup.find_all('a',{'class':"residency-result-program-title"})
                names += [a.text.strip() for a in souped]
                links += ['https://www.doximity.com' + a['href'] for a in souped]
                fields += [val]*len(names)
                ranks += range(1,len(names)+1)
                sorttypes += [val2]*len(names)
                res = 1

# %%
# driver.get(links[1])
ranks


# %%
names = []
links = []
fields = []
ranks = []
sorttypes = []
for sortkey, sortval in zip(sortkeys,sortvals):
    for address, specialty in zip(addresses,specialties):
        try:
            time.sleep(3)
            driver.get(urlbase+address+'sortByKey='+sortkey+urltail)
        except:
            pass
        res = 0
        print(sortkey,sortval,address,specialty)
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
df.to_csv('best_counties_buy_house.csv')

#%%
addresses
