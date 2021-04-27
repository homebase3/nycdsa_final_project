# %% load in libraries
from bs4 import BeautifulSoup
import pandas as pd
import time
from selenium import webdriver
import random

# %% set up selenium
from selenium import webdriver
driver = webdriver.Firefox()

# %%
driver.get('https://www.doximity.com/residency/programs/009b631d-3390-4742-b583-820ccab9a18b-university-of-california-san-francisco-anesthesiology')

# %% try assign function
def try_assign(dict,key,val):
    try:
        dict[key] = val
    except:
        dict[key] = None
# %%
itemdict = {}
soup = BeautifulSoup(driver.page_source,'html.parser')
stats = [a.text.strip() for a in soup.find_all('strong',{'class':'metric-box-number'})]
stats_labels = [a.text.replace("‡","").strip() for a in soup.find_all('p',{'class':'metric-box-label'})]
for i in range(len(stats)):
    try_assign(itemdict, stats_labels[i],stats[i])

graph_list = [list(a.children) for a in soup.find_all('div',{'class':'simple-table-cell residency-rotation-column'})]
for i, val in enumerate(graph_list):
    reverse_ = list(reversed(val))
    reverse_ = [rev['aria-label'].split(' ') for rev in reverse_]
    for j, val2 in enumerate(reverse_):
        try_assign(itemdict,"Year "+str(i)+ ", site "+str(j)+": institution name"," ".join(val2[:-2]))
        try_assign(itemdict,"Year "+str(i)+ ", site "+str(j)+": institution months",float(val2[-2]))


# %%
try_assign(itemdict, "Accredited Fellowship Programs",[a.text.strip() for a in soup.find_all('li',{'class':'col-1-3 residency-program-fellowship-program-list-item'})])

#%%
total_subspecializing = soup.find('h2',{'class':'tooltip-top'}).text.replace("*","").strip().split(' ')[0]
total_subspecializing = float(total_subspecializing.replace("%",""))/100
sub_specialties = [a.text.strip() for a in soup.find_all('div',{'class':'residency-program-subspeicalty-diagram-label'})]
sub_speciaties_percentage = list(soup.find_all('div',{'class':'residency-program-subspeicalty-bar'}))
sub_specialties_percentage = [float(a['style'].replace("%","").replace(";","").replace("width: ","").strip())/100 for a in sub_speciaties_percentage]
sum_ssp = sum(sub_specialties_percentage)
sub_specialties_percentage_abs = [i/sum_ssp * total_subspecializing for i in sub_specialties_percentage]

for i,val in enumerate(sub_specialties):
    try_assign(itemdict,val,sub_specialties_percentage[i])

# %%


# %%
itemdict
