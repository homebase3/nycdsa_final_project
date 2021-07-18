
# %% load in libraries
from bs4 import BeautifulSoup
import pandas as pd
import time
from selenium import webdriver

# %% set up selenium and login
from selenium import webdriver
driver = webdriver.Firefox()
url1 = 'https://freida.ama-assn.org/search/list?spec=43236&page=1'
driver.get(url1)

# %% get program names for each specialty
specialties = pd.read_csv('specialties.csv')
df = pd.DataFrame({'Specialty':[],'School': [], 'Link': []})
for index, row in specialties.iterrows():
    urlbase = 'https://freida.ama-assn.org/search/list?spec=' + str(row[1]) + '&page='
    site = 'https://freida.ama-assn.org'
    for i in range(1,100):
        url = urlbase + str(i)
        # load page
        driver.get(url)
        # wait for it to load
        time.sleep(3)

        #soup page
        soup = BeautifulSoup(driver.page_source,'html.parser')
        # check for blank pages
        for div in soup.find_all('div',{'class':['search-list__count']}):
            count_text = div.text.strip().split()

        else:
            links = []
            for a in soup.find_all('a',{'class':['search-result-card__title']}):
                links += [site + a['href']]
            schools = []
            for a in soup.find_all('a',{'class':['search-result-card__title']}):
                schools += [a.text]
            dict_ = {'Specialty':[row[0]]*len(schools),'School': schools,'Link':links}
            df_it = pd.DataFrame(dict_)
            df = df.append(df_it)

            if int(count_text[5]) == int(count_text[3]):
                break

# %% save program names for each specialty
df.to_csv('names.csv')
