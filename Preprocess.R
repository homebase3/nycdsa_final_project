library(tidyverse)
library(readxl)
library(data.table)
library(janitor)

# read in aamc data files
file_list <- list.files("data/AAMC")

for (num in seq_along(file_list)) {
  read_excel(paste0("data/AAMC/",file_list[num])) %>% 
    colnames(.)[1] %>% 
    colnames(.) %>% 
    gsub(".*: ","",.) -> specialty_name
  
  read_excel(paste0("data/AAMC/",file_list[num]),skip = 1)  %>% 
    colnames(.) %>% 
    .[-1] -> ID
  
  read_excel(paste0("data/AAMC/",file_list[num]), skip = 1) %>% 
    t(.) %>% 
    as.data.frame(.) %>% 
    row_to_names(row_number = 1) %>% 
    mutate(Specialty = specialty_name, .before = 1) %>% 
    mutate(ID = ID, .before = 1)-> out
  
  if(num == 1) {
    dat <- out
  } else {
    dat <- bind_rows(dat,out)
  }
}

# Remove duplicates and check to ensure have the correct number of programs in each speciality
dat <- unique(dat)
dat %>% 
  group_by(Specialty) %>% 
  summarize(count = n()) %>% 
  View(.)

# Remove duplicates


dat %>% 
  filter(Specialty == "Orthopaedic Surgery") %>% 
  .$`Residency program name` %>% 
  unique(.) -> included

full_list <- c('Jack Hughston Memorial Hospital Program',
                'Wake Forest University School of Medicine Program',
                'University of North Carolina Hospitals Program',
                'Carolinas Medical Center Program',
                'Duke University Hospital Program',
                'Univ of North Dakota School of Medicine and Health Sciences Program',
                'OhioHealth/Doctors Hospital Program',
                'Kettering Health Network Program',
                'Western Reserve Hospital  Program',
                'Cleveland Clinic Foundation/South Pointe Hospital Program',
                'One Brooklyn Health System/Kingsbrook Jewish Medical Center Program',
                'St Elizabeth Youngstown Hospital Program',
                'Case Western Reserve University/University Hospitals Cleveland Medical Center/Regional Program',
                'Summa Health System/NEOMED Program',
                'University of Cincinnati Medical Center/College of Medicine Program',
                'Case Western Reserve University/University Hospitals Cleveland Medical Center Program',
                'Akron General Medical Center/NEOMED Program',
                'Ohio State University Hospital Program',
                'Wright State University Program',
                'Cleveland Clinic Foundation Program',
                'University of Toledo Program',
                'Mercy St Vincent Medical Center Program',
                'New York Presbyterian Hospital (Columbia Campus) Program',
                'Montefiore Medical Center/Albert Einstein College of Medicine Program',
                'Stony Brook Medicine/University Hospital Program',
                'RowanSOM/Jefferson Health/Virtua Our Lady of Lourdes Hospital Program',
                'Rutgers Health/Monmouth Medical Center Program',
                "St Joseph's University Medical Center Program",
                'Rutgers Health/Robert Wood Johnson Medical School Program',
                'Rutgers Health/New Jersey Medical School Program',
                'University of New Mexico School of Medicine Program',
                'Nassau University Medical Center Program',
                'Zucker School of Medicine at Hofstra/Northwell at Plainview Hospital Program',
                'Icahn School of Medicine at Mount Sinai Program',
                'NYU Long Island School of Medicine Program',
                'Zucker School of Medicine at Hofstra/Northwell at Lenox Hill Hospital Program',
                'NYU Grossman School of Medicine/NYU Langone Orthopedic Hospital Program',
                'Hospital for Special Surgery/Cornell Medical Center Program',
                'University at Buffalo Program',
                'University of Rochester Program',
                'SUNY Upstate Medical University Program',
                'Albany Medical Center Program',
                'Westchester Medical Center Program',
                'Maimonides Medical Center Program',
                'SUNY Downstate Health Sciences University Program',
                'Zucker School of Medicine at Hofstra/Northwell Program',
                'Mount Carmel Health System Program',
                'Oklahoma State University Center for Health Sciences Program',
                'University of Oklahoma Health Sciences Center Program',
                'Samaritan Health Services - Corvallis Program',
                'Baylor University Medical Center Program',
                'University of Texas at Austin Dell Medical School Program',
                'University of Texas Southwestern Medical Center Program',
                'University of Texas Medical Branch Hospitals Program',
                'University of Texas Health Science Center at Houston Program',
                'Texas A&M College of Medicine-Scott and White Medical Center (Temple) Program',
                'William Beaumont Army Medical Center/Texas Tech University (El Paso) Program',
                'John Peter Smith Hospital (Tarrant County Hospital District) Program',
                'Baylor College of Medicine Program',
                'University of Texas Health Science Center San Antonio Joe and Teresa Lozano Long School of Medicine Program',
                'Texas Tech University Health Sciences Center at Lubbock Program',
                'San Antonio Uniformed Services Health Education Consortium (SAUSHEC) Program',
                'University of Utah Health Program',
                'University of Vermont Medical Center Program',
                'Naval Medical Center (Portsmouth) Program',
                'Virginia Commonwealth University Health System Program',
                'University of Virginia Medical Center Program',
                'University of Washington Program',
                'Madigan Army Medical Center Program',
                'Marshall University School of Medicine Program',
                'West Virginia University Program',
                'Methodist Hospital (Houston) Program',
                'Rutgers Health/Jersey City Medical Center Program',
                'University of Tennessee/Campbell Clinic Program',
                'Vanderbilt University Medical Center Program',
                'Oregon Health & Science University Program',
                'Philadelphia College of Osteopathic Medicine Program',
                'Wellspan Health/York Hospital Program',
                'UPMC Pinnacle Hospitals Program',
                'Lake Erie College of Osteopathic Medicine Program',
                'Albert Einstein Healthcare Network Program',
                'Sidney Kimmel Medical College at Thomas Jefferson University/TJUH Program',
                'University of Pennsylvania Health System Program',
                'Temple University Hospital Program',
                'UPMC Medical Education Program',
                'Penn State Milton S Hershey Medical Center Program',
                'Allegheny Health Network Medical Education Consortium (AGH) Program',
                "St Luke's University Hospital Program",
                'Geisinger Health System Program',
                'UPMC Medical Education (Hamot)  Program',
                'University of Puerto Rico Program',
                'Brown University Program',
                'Prisma Health/University of South Carolina SOM Greenville (Greenville) Program',
                'Medical University of South Carolina Program',
                'Prisma Health/University of South Carolina SOM Columbia (Columbia) Program',
                'East Tennessee State University/Quillen College of Medicine Program',
                'University of Tennessee College of Medicine at Chattanooga Program',
                'Medical College of Wisconsin Affiliated Hospitals Program',
                'Inspira Health Network/Inspira Medical Center Vineland Program',
                'Dartmouth-Hitchcock/Mary Hitchcock Memorial Hospital Program',
                'MedStar Health/Georgetown University Hospital Program',
                'George Washington University Program',
                'Howard University Program',
                'Broward Health Program',
                'HCA Healthcare/USF Morsani College of Medicine GME: Largo Medical Center Program',
                'University of Central Florida/HCA Healthcare GME (Ocala) Program',
                'University of South Florida Morsani Program',
                'University of Florida College of Medicine Jacksonville Program',
                'University of Miami/Jackson Health System Program',
                'University of Connecticut Program',
                'University of Florida Program',
                'Emory University School of Medicine Program',
                'Medical College of Georgia Program',
                'Dwight David Eisenhower Army Medical Center Program',
                'WellStar Atlanta Medical Center Program',
                'University of Hawaii Program',
                'Tripler Army Medical Center Program',
                'Franciscan Health Olympia Fields Program',
                'McGaw Medical Center of Northwestern University Program',
                'University of Illinois College of Medicine at Chicago Program',
                'Orlando Health Program',
                'Yale-New Haven Medical Center Program',
                'University of Colorado Program',
                'Los Angeles County-Harbor-UCLA Medical Center Program',
                'USA Health Program',
                'University of Alabama Medical Center Program',
                'Mayo Clinic College of Medicine and Science (Arizona) Program',
                'University of Arizona College of Medicine-Tucson Program',
                'University of Arizona College of Medicine-Phoenix Program',
                'University of Arkansas for Medical Sciences (UAMS) College of Medicine Program',
                'Riverside University Health System Program',
                'Community Memorial Health System Program',
                'Valley Consortium for Medical Education Program',
                'Cedars-Sinai Medical Center Program',
                'University of California (San Francisco)/Fresno Program',
                'University of California (San Francisco) Program',
                'Loma Linda University Health Education Consortium Program',
                'University of California (Irvine) Program',
                'UCLA David Geffen School of Medicine/UCLA Medical Center Program',
                'Stanford Health Care-Sponsored Stanford University Program',
                'University of California (San Diego) Medical Center Program',
                'University of California Davis Health Program',
                'University of Southern California/LAC+USC Medical Center Program',
                "St Mary's Hospital and Medical Center Program",
                'Naval Medical Center (San Diego) Program',
                'Loyola University Medical Center Program',
                'Southern Illinois University Program',
                'University of Chicago Program',
                'Rush University Medical Center Program',
                'Henry Ford Macomb Hospital Program',
                'McLaren Health Care/Macomb/MSU Program',
                'Beaumont Health (Royal Oak and Taylor) Program',
                'Ascension Providence/MSUCHM Program',
                'Henry Ford Hospital/Wayne State University Program',
                'McLaren Health Care/Flint/MSU Program',
                'University of Michigan Health System Program',
                'Western Michigan University Homer Stryker MD School of Medicine Program',
                'Spectrum Health/Michigan State University Program',
                'Detroit Medical Center/Wayne State University Program',
                'University of Minnesota Program',
                'Mayo Clinic College of Medicine and Science (Rochester) Program',
                'University of Mississippi Medical Center Program',
                "Kansas City University GME Consortium (KCU-GME Consortium)/St Mary's Program",
                'University of Missouri-Kansas City School of Medicine Program',
                'St Louis University School of Medicine Program',
                'Washington University/B-JH/SLCH Consortium Program',
                'University of Missouri-Columbia Program',
                'University of Nebraska Medical Center College of Medicine Program',
                'University of Nevada Las Vegas (UNLV) School of Medicine Program',
                'OPTI West/Valley Hospital Medical Center Program',
                'McLaren Health Care/Greater Lansing/MSU Program',
                'Cooper Medical School of Rowan University/Cooper University Hospital Program',
                'McLaren Health Care/Oakland/MSU Program',
                'Ascension Genesys Hospital Program',
                'Indiana University School of Medicine Program',
                'University of Iowa Hospitals and Clinics Program',
                'University of Kansas School of Medicine Program',
                'University of Kansas (Wichita) Program',
                'University of Louisville School of Medicine Program',
                'University of Kentucky College of Medicine Program',
                'Louisiana State University (Shreveport) Program',
                'Louisiana State University Program',
                'Ochsner Clinic Foundation Program',
                'Tulane University Program',
                'Sinai Hospital of Baltimore Program',
                'Johns Hopkins University Program',
                'National Capital Consortium Program',
                'MedStar Health/Union Memorial Hospital Program',
                'University of Maryland Program',
                "Massachusetts General Hospital/Brigham and Women's Hospital/Harvard Medical School Program",
                'Tufts Medical Center Program',
                'University of Massachusetts Program',
                'Boston University Medical Center Program',
                'Metro Health University of Michigan Health (Metro Health) Program',
                'Beaumont Health (Farmington Hills and Dearborn) Program',
                'Ascension Macomb-Oakland Hospital Program',
                'University of Wisconsin Hospitals and Clinics Program')