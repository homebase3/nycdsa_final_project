# %% load in libraries
from bs4 import BeautifulSoup
import pandas as pd
import time
from selenium import webdriver
import random

# %% set up selenium and login
from selenium import webdriver
driver = webdriver.Firefox()

# %% get counties
urlbase = 'https://www.niche.com/places-to-live/search/best-counties/?page='

driver.get(urlbase)
# wait for it to load
time.sleep(3)

#soup page

# %%
Counties = []
Links = []
r = list(range(1,127))
random.shuffle(r)
for i in r:
    driver.get(urlbase + str(i))
    # wait for it to load
    sleep_time = random.uniform(3,10)
    time.sleep(sleep_time)
    #soup page
    soup = BeautifulSoup(driver.page_source,'html.parser')

    Counties += [a.text for a in soup.find_all('h2',{'class':"search-result__title"})]
    Links += [a['href'] for a in soup.find_all('a',{'class':"search-result__link"})]

# %%
dict_ = {}
dict_['County'] = Counties
dict_['Link'] = Links
df = pd.DataFrame.from_dict(dict_)
df.to_csv('county_names.csv')

# %%
soup = BeautifulSoup(driver.page_source,'html.parser')

# %% read in county list
counties = pd.read_csv('county_names.csv')
counties = counties.sample(n=5)

details = pd.DataFrame.from_dict({})

# %% parse counties
for index, row in counties.iterrows():

    # load page
    url = row[2]
    driver.get(url)
    sleep_time = random.uniform(1,2)
    time.sleep(sleep_time)
    soup = BeautifulSoup(driver.page_source,'html.parser')

    # initialize iteration
    itemdict = {}
    itemdict['Link'] = url

    #grades
    grades = [a.get_text(separator = "\n").split("\n") for a in soup.find_all('div',{'class': 'profile-grade--two'})]
    for lis_ in grades:
        itemdict[lis_[0]] = lis_[2]

    # summary
    itemdict['sunmary'] = soup.find('span',{'class':'bare-value'}).text

    #scalars
    scalars = [a.get_text(separator = "\n").split("\n") for a in soup.find_all('div',{'class':'scalar__value'})]

    itemdict['Population'] = number_strip(scalars[0][0])
    itemdict['Median Home Value'] = number_strip(scalars[1][0][1:])
    itemdict['Median Rent'] = number_strip(scalars[2][0][1:])
    itemdict['Median Household Income'] = number_strip(scalars[3][0][1:])
    try:
        itemdict['Median Home Value vs. National'] = number_strip(scalars[1][0][1:])/number_strip(scalars[1][2][1:]) - 1
    except:
        itemdict['Median Home Value vs. National'] = None
    try:
        itemdict['Median Rent vs. National'] =  number_strip(scalars[2][0][1:])/number_strip(scalars[2][2][1:]) - 1
    except:
        itemdict['Median Rent vs. National']  = None
    try:
        itemdict['Median Household Income vs. National'] = number_strip(scalars[3][0][1:])/number_strip(scalars[3][2][1:]) - 1
    except:
        itemdict['Median Household Income vs. National'] = None

    # facts
    fact_list = [a.get_text(separator = "\n").split("\n") for a in soup.find_all('li',{'class':'fact__table__row'})]
    for lis_ in fact_list:
        itemdict[lis_[0]] = number_strip(lis_[1])
        if len(lis_) == 4:
            try:    print(fact_list)
                itemdict[lis_[0] + ' vs. National'] = number_strip(lis_[1])/number_strip(lis_[3]) - 1
            except:
                itemdict[lis_[0] + ' vs. National'] = None


    # ratings
    overall_rating = soup.find('div',{'class':'review__stars'}).get_text(separator = "\n").split("\n")
    itemdict["Overall rating"] = number_strip(overall_rating[1])
    itemdict["Total reviews"] = number_strip(overall_rating[-1].split(' ')[0])

    rating_counts = [a.get_text(separator = "\n").split("\n") for a in soup.find_all('div',{'class':'review__chart__item__total'})]
    for i in range(5):
        try:
            itemdict[str(5-i) + '-star ratings'] = rating_counts[i][0]
        except:
            itemdict[str(5-i) + '-star ratings'] = None

    # move to residents dataframe
    driver.get(url + 'residents/')
    sleep_time = random.uniform(1,2)
    time.sleep(sleep_time)
    soup = BeautifulSoup(driver.page_source,'html.parser')
    scalars = [a.get_text(separator = "\n").split("\n") for a in soup.find_all('div',{'class':'scalar--three'})]

    for lis_ in scalars:
        itemdict[lis_[0]] = number_strip(lis_[-1])

    fact_list = [a.get_text(separator = "\n").split("\n") for a in soup.find_all('li',{'class':'fact__table__row'})]
    for lis_ in fact_list:
        itemdict[lis_[0]] = number_strip(lis_[1])
        if len(lis_) == 4:
            try:
                itemdict[lis_[0] + ' vs. National'] = number_strip(lis_[1])/number_strip(lis_[3]) - 1
            except:
                itemdict[lis_[0] + ' vs. National'] = None

    #save results
    df_item = pd.DataFrame([itemdict])
    details = details.append(df_item)

# scalars

# %%
details
