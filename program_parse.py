# load in libraries
from bs4 import BeautifulSoup
import pandas as pd
import time
from selenium import webdriver

# %% set up selenium
from selenium import webdriver
driver = webdriver.Firefox()
url1 = 'https://freida.ama-assn.org/search/list?spec=43236&page=1'
driver.get(url1)

# %% define standard parse function
def standard_parse(dict_,lis_):
    for val1,val2 in zip(lis_[::2],lis_[1::2]):
        if val1 in dict_.keys():
            dict_[val1] = [dict_[val1][0] + "_" + val2]
        else:
            dict_[val1] = [val2]

# %% initialize dataframes
names = pd.read_csv('names.csv')
names = names.tail(names.shape[0]-28)
# names = names.head(3)

details = pd.DataFrame({})
# %% parse programs
for index, row in names.iterrows():
    # print(row[1])

    # load page
    driver.get(row[2])
    time.sleep(3)
    soup = BeautifulSoup(driver.page_source,'html.parser')


    # initialize itemdict
    itemdict = {}
    itemdict['Link'] = row[2]

    # perform base parse
    table_first = [val.text.strip() for val in soup.find_all('td')]
    del table_first[-4]
    standard_parse(itemdict,table_first)

    # get contact info. If not exist, page is blank, move on
    try:
        contacts = [list(a.stripped_strings) for a in soup.find_all('small',{'class':['contact-info__contacts__details']})]
        itemdict['Program director'] = '\n'.join(contacts[0])
        itemdict['Person to contact for more information about the program'] = '\n'.join(contacts[1])
    except:
        itemdict['Program director'] = None
        itemdict['Person to contact for more information about the program'] = None

    # perform ID and location parse
    try:
        special = [a.get_text(separator = ", ") for a in soup.find_all('small',{'class':"ng-star-inserted"})]
        itemdict['ID'] = special[0][4:]
        itemdict['Location'] = special[1]
        itemdict['Sponsor'] = special[2]
    except:
        continue

    for i,val in enumerate(special[3:-1]):
        itemdict['Participant '+ str(i)] = val

    # introduction
    try:
        itemdict['Intro'] = [a.text.strip() for a in soup.find_all('div',{'class':['special_features ng-star-inserted']})][0]
    except:
        itemdict['Intro'] = None

    # go to second tab
    #get program length to adjust parse
    length = int(itemdict['Required length'][0])

    try:
        driver.find_element_by_xpath("//div[@data-test='program-sub-nav__item'][position()=2]").click()
    except:
        # adjust dataframe
        df_item = pd.DataFrame.from_dict(itemdict)
        details = details.append(df_item)

        continue
    time.sleep(0.25)
    soup = BeautifulSoup(driver.page_source,'html.parser')

    table_second = [val.text.strip() for val in soup.find_all('td')]
    end_second = list(reversed(table_second)).index('Full-time paid') + 1
    table_third = table_second[-end_second:]
    del table_second[-end_second:]
    table_fourth = table_third[9:]
    del table_third[9:]
    try:
        end_fourth = list(reversed(table_fourth)).index('% Male') -1
    except:
        try:
            end_fourth = list(reversed(table_fourth)).index('1') + 1
        except:
            end_fourth = len(table_fourth)

    table_fifth = table_fourth[-end_fourth:]
    del table_fourth[-end_fourth:]
    del table_fifth[-1]

    standard_parse(itemdict,table_second)

    for i,val in enumerate(table_third[::3]):
        itemdict[val.strip()+'_Physician'] = [table_third[3*i+1]]
        itemdict[val.strip()+'_Non-physician'] = [table_third[3*i+2]]

    standard_parse(itemdict,table_fourth)
    try:
        for i,val in enumerate(table_fifth[::3]):
            itemdict['Year most taxing schedule And frequency per year_Year ' + val] = [table_fifth[3*i+1]]
            itemdict['Beeper or home call (Weeks/Year)_Year ' + val] = [table_fifth[3*i+2]]
    except:
        pass
    #move to third tab
    try:
        driver.find_element_by_xpath("//div[@data-test='program-sub-nav__item'][position()=3]").click()
    except:
        # adjust dataframe
        df_item = pd.DataFrame.from_dict(itemdict)
        details = details.append(df_item)
        continue

    time.sleep(0.25)
    soup = BeautifulSoup(driver.page_source,'html.parser')

    table_sixth = [val.text.strip() for val in soup.find_all('td')]
    try:
        end_sixth = table_sixth.index('Specialty Details')+2
    except:
        end_sixth = 6

    table_seventh = table_sixth[end_sixth:]
    #
    del table_sixth[end_sixth:]
    end_seventh = table_seventh.index('Salary paid by a non-profit institution')
    table_eigth = table_seventh[end_seventh:]
    del table_seventh[end_seventh:]


    standard_parse(itemdict,table_sixth)

    # print(table_sixth)
    # print(table_seventh)
    for i,val in enumerate(table_seventh[::4]):
        itemdict['Salary compensation_Year ' + val] = table_seventh[4*i+1]
        itemdict['Vacation days_Year ' + val] = table_seventh[4*i+2]
        itemdict['Sick days_Year ' + val] = table_seventh[4*i+3]

    standard_parse(itemdict,table_eigth)

    # adjust dataframe
    df_item = pd.DataFrame.from_dict(itemdict)
    details = details.append(df_item)


# %%
details.to_csv('programs.csv')
details


# %% load page
driver.get('https://freida.ama-assn.org/program/4204921199')
time.sleep(5)
soup = BeautifulSoup(driver.page_source,'html.parser')

# %%
special = [a.get_text(separator = ", ") for a in soup.find_all('small',{'class':"ng-star-inserted"})]
itemdict['ID'] = special[0][4:]
itemdict['Location'] = special[1]
itemdict['Sponsor'] = special[2]

for i,val in enumerate(special[3:-1]):
    itemdict['Participant '+ str(i)] = val

# %% initialize itemdict
itemdict = {}
itemdict['Link'] = row[2]
print(row[2])

table_first = [val.text.strip() for val in soup.find_all('td')]
del table_first[-4]
# for val1,val2 in zip(table_first[::2],table_first[1::2]):
#     itemdict[val1.strip()] = val2.strip()
standard_parse(itemdict,table_first)
print(itemdict)
# %%
# small
contacts = [list(a.stripped_strings) for a in soup.find_all('small',{'class':['contact-info__contacts__details']})]
print(contacts)
itemdict
# itemdict['Program director'] = '\n'.join(contacts[0])
# itemdict['Person to contact for more information about the program'] = '\n'.join(contacts[1])

# introduction
try:
    itemdict['Intro'] = [a.text for a in soup.find_all('div',{'class':['special_features ng-star-inserted']})][0]
except:
    itemdict['Intro'] = ''

# go to second tab
#get program length to adjust parse
length = int(itemdict['Required length'][0])

driver.find_element_by_xpath("//div[@data-test='program-sub-nav__item'][position()=2]").click()
time.sleep(0.5)
soup = BeautifulSoup(driver.page_source,'html.parser')

# %%
table_second = [val.text.strip() for val in soup.find_all('td')]
end_second = list(reversed(table_second)).index('Full-time paid') + 1
table_third = table_second[-end_second:]
del table_second[-end_second:]
table_fourth = table_third[9:]
del table_third[9:]
try:
    end_fourth = list(reversed(table_fourth)).index('% Male') -1
except:
    end_fourth = list(reversed(table_fourth)).index('1') + 1
table_fifth = table_fourth[-end_fourth:]
del table_fourth[-end_fourth:]
del table_fifth[-1]


table_fourth


# %%

standard_parse(itemdict,table_second)

for i,val in enumerate(table_third[::3]):
    itemdict[val.strip()+'_Physician'] = [table_third[3*i+1]]
    itemdict[val.strip()+'_Non-physician'] = [table_third[3*i+2]]

standard_parse(itemdict,table_fourth)

print(table_fifth)
try:
    for i,val in enumerate(table_fifth[::3]):
        itemdict['Year most taxing schedule And frequency per year_Year ' + val] = [table_fifth[3*i+1]]
        itemdict['Beeper or home call (Weeks/Year)_Year ' + val] = [table_fifth[3*i+2]]
itemdict

# %%
#move to third tab
driver.find_element_by_xpath("//div[@data-test='program-sub-nav__item'][position()=3]").click()

# %%

time.sleep(0.5)
soup = BeautifulSoup(driver.page_source,'html.parser')

table_sixth = [val.text.strip() for val in soup.find_all('td')]
try:
    end_sixth = table_sixth.index('Specialty Details')+2
except:
    end_sixth = 6

table_seventh = table_sixth[end_sixth:]
#
del table_sixth[end_sixth:]
end_seventh = table_seventh.index('Salary paid by a non-profit institution')
table_eigth = table_seventh[end_seventh:]
del table_seventh[end_seventh:]

table_seventh

# %%


standard_parse(itemdict,table_sixth)

table_seventh

for i,val in enumerate(table_seventh[::4]):
    itemdict['Salary compensation_Year ' + val] = table_seventh[4*i+1]
    itemdict['Vacation days_Year ' + val] = table_seventh[4*i+2]
    itemdict['Sick days_Year ' + val] = table_seventh[4*i+3]


#
# standard_parse(itemdict,table_eigth)



# [val.text for val in soup.find_all('tr')]

# %%
itemdict
