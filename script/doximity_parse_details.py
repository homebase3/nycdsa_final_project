# %% load in libraries
from bs4 import BeautifulSoup
import pandas as pd
import time
from selenium import webdriver
import random
import numpy as np

# %% set up selenium
from selenium import webdriver
driver = webdriver.Firefox()

# %%
driver.get('https://www.doximity.com/residency/programs/009b631d-3390-4742-b583-820ccab9a18b-university-of-california-san-francisco-anesthesiology')

# %%
specialties_df = pd.read_csv('specialties_doximity.csv')
specialties_links = list(np.unique(specialties_df['Link']))
details = pd.DataFrame.from_dict({})

# %%
specialties_links
# %%
for link in specialties_links:
    # initialize item dictionary
    itemdict = {}
    itemdict['Link'] = link

    # get page
    driver.get(link)
    time.sleep(4)

    #pull data from page
    soup = BeautifulSoup(driver.page_source,'html.parser')
    stats = [a.text.strip() for a in soup.find_all('strong',{'class':'metric-box-number'})]
    stats_labels = [a.text.replace("‡","").strip() for a in soup.find_all('p',{'class':'metric-box-label'})]
    for i in range(len(stats)):
        try:
            itemdict[stats_labels[i]] =[stats[i]]
        except:
            itemdict[stats_labels[i]] = None

    graph_list = [list(a.children) for a in soup.find_all('div',{'class':'simple-table-cell residency-rotation-column'})]
    for i, val in enumerate(graph_list):
        reverse_ = list(reversed(val))
        reverse_ = [rev['aria-label'].split(' ') for rev in reverse_]
        for j, val2 in enumerate(reverse_):
            try:
                itemdict["Year "+str(i)+ ", site "+str(j)+": institution name"] = [" ".join(val2[:-2])]
            except:
                itemdict["Year "+str(i)+ ", site "+str(j)+": institution name"] = None
            try:
                itemdict["Year "+str(i)+ ", site "+str(j)+": institution months"] = [float(val2[-2])]
            except:
                itemdict["Year "+str(i)+ ", site "+str(j)+": institution months"] = None


    # Fellowship programs
    try:
        itemdict["Accredited Fellowship Programs"] = [[a.text.strip() for a in soup.find_all('li',{'class':'col-1-3 residency-program-fellowship-program-list-item'})]]
    except:
        itemdict["Accredited Fellowship Programs"] = None
    # subspcialty data
    try:
        total_subspecializing = soup.find('h2',{'class':'tooltip-top'}).text.replace("*","").strip().split(' ')[0]
        total_subspecializing = float(total_subspecializing.replace("%",""))/100
        sub_specialties = [a.text.strip() for a in soup.find_all('div',{'class':'residency-program-subspeicalty-diagram-label'})]
        sub_speciaties_percentage = list(soup.find_all('div',{'class':'residency-program-subspeicalty-bar'}))
        sub_specialties_percentage = [float(a['style'].replace("%","").replace(";","").replace("width: ","").strip())/100 for a in sub_speciaties_percentage]
        sum_ssp = sum(sub_specialties_percentage)
        sub_specialties_percentage_abs = [i/sum_ssp * total_subspecializing for i in sub_specialties_percentage]
    except:
        pass
    try:
        for i,val in enumerate(sub_specialties):
            try:
                itemdict[val] = [sub_specialties_percentage_abs[i]]
            except:
                itemdict[val] = None
    except:
        pass

    # Alumni satistfaction
    soup = BeautifulSoup(driver.page_source,'html.parser')
    try:
        totals = soup.find('span',{'class':'residency-program-review-subheader'}).get_text().split(' ')
    except:
        pass
    try:
        itemdict['Rating count'] = [int(totals[0])]
    except:
        itemdict['Rating count'] = None
    try:
        itemdict['Review count'] = [int(totals[3])]
    except:
        itemdict['Review count'] = None

    # ratings
    rating_categories = [a.text for a in soup.find_all('span',{'class':'residency-program-star-rating-title'})]
    ratings_helper = [list(a.children) for a in soup.find_all('span',{'class':'star-rating residency-rating'})]
    ratings = []
    for val in ratings_helper:
        classes = []
        for val2 in val:
            try:
                classes += val2['class']
            except:
                pass
        ratings += [classes.count('svg-icon-star-full') + 0.5 * classes.count('svg-icon-star-half')]
    for i,val in enumerate(rating_categories):
        try:
            itemdict[val] = [ratings[i]]
        except:
            itemdict[val] = None

    # adjust dataframe
    # print(itemdict)
    df_item = pd.DataFrame.from_dict(itemdict)
    # print(df_item)
    details = details.append(df_item)
    # print(details.shape[0])

# %%
details
# details.to_csv('doximity_details_3_1.csv')

# %%
df_item =  pd.DataFrame.from_dict(itemdict)
details.append(df_item).append(df_item)
