# %% load in libraries
from bs4 import BeautifulSoup
import pandas as pd
import time
from selenium import webdriver
import random

# %% set up selenium and login
from selenium import webdriver
driver = webdriver.Firefox()

# %% read in county list
counties = pd.read_csv('county_names.csv')
counties = counties.tail(counties.shape[0]-75-1335-416-381-137)
# counties = counties.head(1)
details = pd.DataFrame.from_dict({})
# counties.shape[0]

# %%
def number_strip(text):
    if text.find("%") != -1:
        return float(text.replace("%",""))/100
    else:
        out = text.replace("$","").replace(",","")
        try:
            return int(out)
        except:
            return float(out)
        return None

# %% parse counties
for index, row in counties.iterrows():

    # load page
    url = row[2]
    driver.get(url)
    sleep_time = random.uniform(0,1)
    time.sleep(sleep_time)
    soup = BeautifulSoup(driver.page_source,'html.parser')

    # initialize iteration
    itemdict = {}
    itemdict['Link'] = url

    #grades
    try:
        itemdict['Overall Grade'] = soup.find('div',{'class': 'overall-grade__niche-grade'}).get_text(separator = "\n").split("\n")[1]
    except:
        itemdict['Overall Grade'] = None

    grades = [a.get_text(separator = "\n").split("\n") for a in soup.find_all('div',{'class': 'profile-grade--two'})]
    for lis_ in grades:
        try:
            itemdict[lis_[0]] = lis_[2]
        except:
            itemdict[lis_[0]] = None

    # summary
    try:
        itemdict['Summary'] = soup.find('span',{'class':'bare-value'}).text
    except:
        itemdict['Summary'] = None

    #scalars
    scalars = [a.get_text(separator = "\n").split("\n") for a in soup.find_all('div',{'class':'scalar__value'})]

    try:
        itemdict['Population'] = number_strip(scalars[0][0])
    except:
        itemdict['Population'] = None

    try:
        itemdict['Median Home Value'] = number_strip(scalars[1][0][1:])
    except:
        itemdict['Median Home Value'] = None
    try:
        itemdict['Median Rent'] = number_strip(scalars[2][0][1:])
    except:
        itemdict['Median Rent'] = None

    try:
        itemdict['Median Household Income'] = number_strip(scalars[3][0][1:])
    except:
        itemdict['Median Household Income'] = None

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
        try:
            itemdict[lis_[0]] = number_strip(lis_[1])
        except:
            itemdict[lis_[0]] = None
        if len(lis_) == 4:
            try:
                itemdict[lis_[0] + ' vs. National'] = number_strip(lis_[1])/number_strip(lis_[-1]) - 1
            except:
                itemdict[lis_[0] + ' vs. National'] = None
    # ratings
    try:
        overall_rating = soup.find('div',{'class':'review__stars'}).get_text(separator = "\n").split("\n")
        itemdict["Overall rating"] = number_strip(overall_rating[1])
        itemdict["Total reviews"] = number_strip(overall_rating[-1].split(' ')[0])
    except:
        itemdict["Overall rating"] = None
        itemdict["Total reviews"] = None


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
        try:
            itemdict[lis_[0]] = number_strip(lis_[-1])
        except:
            itemdict[lis_[0]] = None

    fact_list = [a.get_text(separator = "\n").split("\n") for a in soup.find_all('li',{'class':'fact__table__row'})]
    for lis_ in fact_list:
        try:
            itemdict[lis_[0]] = number_strip(lis_[1])
        except:
            itemdict[lis_[0]] = None
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
details.to_csv('county_details_8.csv')

# %%
soup = BeautifulSoup(driver.page_source,'html.parser')
