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

#%% navigate to base page
driver.get(urlbase+addresses[0]+'sortByKey='+sortkeys[0]+urltail)
soup = BeautifulSoup(driver.page_source,'html.parser')

# %% soup specialties
names = []
links = []
fields = []
ranks = []
sorttypes = []
for i, val in list(enumerate(specialties))[7:]:
    #navigate to specialty
    time.sleep(1)
    driver.find_element_by_xpath("//div[@class='dropdown-menu-select']").click()
    time.sleep(1)
    driver.find_element_by_xpath("//li[@class='dropdown-menu-options-item'][position()="+str(i+1)+"]").click()
    time.sleep(1)
    soup = BeautifulSoup(driver.page_source,'html.parser')
    options = [a.text for a in soup.find_all('option')]
    options = options[:options.index("All Training Sites")]
    print(options)
    for j, val2 in enumerate(options):
        #navigate to sorttype
        time.sleep(1)
        driver.find_element_by_xpath("//option[position()="+str(j+1)+"]").click()

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
                fields += [val]*len(souped)
                ranks += list(range(1,len(souped)+1))
                sorttypes += [val2]*len(souped)
                res = 1

# %%

# make dict to save
dict_ = {}
dict_['Field'] = fields
dict_['Program'] = names
dict_['Link'] = links
dict_['Rank'] = ranks
dict_['Sort type'] = sorttypes
df = pd.DataFrame.from_dict(dict_)
df.to_csv("specialties_doximity_2.csv")

# %%
list(enumerate(specialties))

#%%
souped = soup.find_all('a',{'class':"residency-result-program-title"})
[a.text.strip() for a in souped]


#%%
soup = BeautifulSoup(driver.page_source,'html.parser')
options = [a.text for a in soup.find_all('option')]
options[:options.index("All Training Sites")]
