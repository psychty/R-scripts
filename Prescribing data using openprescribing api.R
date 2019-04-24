
# Open prescribing 

library(tidyverse)

# cite - "OpenPrescribing.net, EBM DataLab, University of Oxford, 2019"

# caveats # https://ebmdatalab.net/limitations-of-nhs-england-prescribing-data/

# BNF codes ####
# grab the latest BNF codes datafile here https://apps.nhsbsa.nhs.uk/infosystems/data/showDataSelector.do?reportId=126

BNF <- read_csv("~/R-scripts/20190301_1551434673212_BNF_Code_Information.csv", col_types = cols(.default = col_character()))

BNF_sub_paragraphs <- BNF %>% 
  select(-c(`BNF Presentation Code`, `BNF Presentation`, `BNF Product Code`, `BNF Product`, `BNF Chemical Substance Code`, `BNF Chemical Substance`)) %>% 
  unique()

BNF_paragraphs <- BNF_sub_paragraphs %>% 
  select(-c(`BNF Subparagraph Code`, `BNF Subparagraph`)) %>% 
  unique()

BNF_sections <- BNF_paragraphs %>% 
  select(-c(`BNF Paragraph Code`, `BNF Paragraph`)) %>% 
  unique()

BNF_chapters <- BNF_sections %>% 
  select(-c(`BNF Section Code`, `BNF Section`)) %>% 
  unique()

# Retrieve total spending and items by CCG or practice on a particular chemical, presentation or BNF section. (Spending is calculated using the actual_cost field in the HSCIC data, items using the total_items field.)

code_x = "0104"
# https://ebmdatalab.net/prescribing-data-bnf-codes/

# spending_by_ccg or spending_by_practice

spending_acute_diarrhoea_09G <- read_csv(paste0("https://openprescribing.net/api/1.0/spending_by_practice/?code=", code_x, "&org=09G&format=csv"), col_types = cols(actual_cost = col_double(),date = col_date(format = ""),items = col_double(), quantity = col_double(), row_id = col_character(),row_name = col_character())) %>% 
  mutate(Code = code_x) %>% 
  left_join(BNF_sections, by = c("Code" = "BNF Section Code"))

spending_acute_diarrhoea_all_ccgs <- read_csv(paste0("https://openprescribing.net/api/1.0/spending_by_ccg/?code=", code_x, "&org=&format=csv"), col_types = cols(actual_cost = col_double(),date = col_date(format = ""),items = col_double(), quantity = col_double(), row_id = col_character(),row_name = col_character())) %>% 
  mutate(Code = code_x) %>% 
  left_join(BNF_sections, by = c("Code" = "BNF Section Code")) %>% 
  group_by(date) %>% 
  arrange(desc(quantity)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(decile = ntile(quantity, 10)) %>% 
  mutate(percentile = ntile(quantity, 100))


paste0("The most recently available data are for ", format(max(spending_acute_diarrhoea_all_ccgs$date), "%B %Y"), ".")
# what are the most recent six time periods 

# We don't need to do this, it was just the way the charts are ordered on the website (average rank over past six months). We do however want to calculat the percentile and decile as in df_1
six_month_time <- spending_acute_diarrhoea_all_ccgs %>% 
  select(date) %>%
  unique() %>% 
  arrange(desc(date)) %>% 
  top_n(6)

df_1 <- spending_acute_diarrhoea_all_ccgs %>% 
#  filter(date %in% six_month_time$date) %>% 
  group_by(date) %>% 
  arrange(desc(quantity)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(decile = ntile(quantity, 10)) %>% 
  mutate(percentile = ntile(quantity, 100))

# Stephen Black had a greta idea to produce an area plot of the prescribing for a particular Practice or CCG grouped by BNF chapter name. https://public.tableau.com/profile/matt.black#!/vizhome/summaryprescribingdatav1_1/Notes

# coloured by chapter, boxes by section - data included in tooltip = "Analgesics is part of the Central Nervous System BNF chapter This sections consists of 4 paragraphs The section contains 57 different drugs with 1,007 different formulations Over this time period the NHS has spent	 	£2,700.56M on these drugs  and has issued 375.54M prescriptions at an average cost of £7.19 That's an average of 5,689,997 prescriptions and £40.92M per month.

# We will have to explore a way of calling all prescribing for a particular practice from the api.
# If we just leave the code= blank it will return all items. Instead I think we have to use a loop to capture each area and each relevant code, storing each into an overall dataframe.

names(df_x)

combined_df <- data.frame(actual_cost = numeric(), date = character(), items = numeric(),quantity = numeric(), row_id = character(), row_name = character(), Code = character(), `BNF Chapter` = character(), `BNF Chapter Code` = character(), `BNF Section` = character(), Rank = numeric(), decile = numeric(), percentile = numeric(), check.names = FALSE)

org_x = ""

for(i in 1:length(unique(BNF_sections$`BNF Section Code`))){
  code_x = BNF_sections$`BNF Section Code`[i]

  # if(code_x %in% c("0108","0210", "0213", "0305", "0306")) {
  #   print(paste0("Skipping ", code_x))
  #   next
  # }  
    
  
df_x <- read_csv(paste0("https://openprescribing.net/api/1.0/spending_by_ccg/?code=", code_x, "&org=", org_x,"&format=csv"), col_types = cols(actual_cost = col_double(),date = col_date(format = ""),items = col_double(), quantity = col_double(), row_id = col_character(),row_name = col_character())) %>% 
    mutate(Code = code_x) %>% 
    left_join(BNF_sections, by = c("Code" = "BNF Section Code")) %>% 
    mutate(date = as.character(date)) %>% 
    group_by(date) %>% 
    arrange(desc(quantity)) %>% 
    mutate(Rank = row_number()) %>% 
    mutate(decile = ntile(quantity, 10)) %>% 
    mutate(percentile = ntile(quantity, 100))

combined_df <- combined_df %>% 
  bind_rows(df_x)

}

spending_acute_diarrhoea_all_ccgs <- read_csv(paste0("https://openprescribing.net/api/1.0/spending_by_ccg/?code=", code_x, "&org=&format=csv"), col_types = cols(actual_cost = col_double(),date = col_date(format = ""),items = col_double(), quantity = col_double(), row_id = col_character(),row_name = col_character())) %>% 
  mutate(Code = code_x) %>% 
  left_join(BNF_sections, by = c("Code" = "BNF Section Code")) %>% 
  group_by(date) %>% 
  arrange(desc(quantity)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(decile = ntile(quantity, 10)) %>% 
  mutate(percentile = ntile(quantity, 100))



# Organisation details - https://openprescribing.net/api/1.0/org_details/?

list_size <- read_csv("https://openprescribing.net/api/1.0/org_details/?org_type=practice&org=09G&keys=total_list_size&format=csv")

# You could also include register size from QOF - note that age ranges sometimes differ (e.g. diabetes is 17+ whilst prescribing data will be all ages)
# https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data/2017-18

# NHS recommends some items are not routinely prescribed https://www.england.nhs.uk/wp-content/uploads/2017/11/items-which-should-not-be-routinely-precscribed-in-pc-ccg-guidance.pdf

#Over the counter medicines which should not be routinely prescribed 
OTC <- read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2017-07/Dataset.csv") %>% 
  mutate(Time_period = "2016")

Drugs <- read_csv("https://openprescribing.net/api/1.0/bnf_code/?q=seratonin&format=csv")

# BNF Extraction
# Characters 1 & 2 show the BNF Chapter
# 3 & 4 show the BNF Section
#. 5 & 6 show the BNF paragraph
#. 7 shows the BNF sub-paragraph
#. 8 & 9 show the Chemical Substance
#. 10 & 11 show the Product
#. 12 &13 show the Strength and Formulation
#. 14 & 15 show the equivalent
# The 'equivalent' is defined as follows: If the presentation is a generic, the 14th and 15th character will be the same as the 12th and 13th character. . Where the product is a brand the 14th and 15th digit will match that of the generic equivalent, unless the brand does not have a generic equivalent in which case A0 will be used. 

Prescribing_July$BNF_chapter <- substr(Prescribing_July$BNF_code, 1, 2)
Prescribing_July$BNF_section <- substr(Prescribing_July$BNF_code, 3, 4)
Prescribing_July$BNF_paragraph <- substr(Prescribing_July$BNF_code, 5, 6)
Prescribing_July$BNF_sub_paragraph <- substr(Prescribing_July$BNF_code, 7, 7)
Prescribing_July$BNF_chemical_substance <- substr(Prescribing_July$BNF_code, 8, 9)
Prescribing_July$BNF_product <- substr(Prescribing_July$BNF_code, 10, 11)
Prescribing_July$BNF_strength_formulation <- substr(Prescribing_July$BNF_code, 12, 13)
Prescribing_July$BNF_equivalent <- substr(Prescribing_July$BNF_code, 14, 15)

# Items 
# This gives the number of items for this presentation that were dispensed in the specified month. A prescription item refers to a single supply of a medicine, dressing or appliance prescribed on a prescription form. If a prescription form includes three medicines it is counted as three prescription items. 
# Item figures do not provide any indication of the length of treatment or quantity of medicine prescribed. (The quantity is given in the 'Quantity' field, described below).
# Patients with a long term condition usually get regular prescriptions. Whilst many prescriptions are for one month, (28 or 30 days supply), others will be for various lengths of treatment and quantity

# The net ingredient cost (NIC) is the basic price of a drug i.e. the price listed in the Drug Tariff or price lists. NIC refers to the basic cost of the drug and does not include any dispensing costs, fees or discount. It does not include any adjustment for income obtained where a prescription charge is paid at the time the prescription is dispensed or where the patient has purchased a pre-payment certificate. The figures are in £s and pence.

# Actual Cost
# From July 2012 onwards, the formula used to calculate 'Actual Cost' has been changed to include the new reimbursement payments which will be charged back to practices from dispensed prescriptions. 
# Actual Cost = (Net Ingredient Cost less discount) + Payment for Consumables (previously known as Container Allowance) + Payment for Containers + Out of Pocket Expenses
# Prior to July 2012 this Actual Cost was defined as the Net Ingredient Cost less the average discount percentage received by pharmacists calculated from the previous month, plus container allowance. This is the estimated cost to the NHS, which is lower than NIC.
# Community pharmacists are reimbursed for medicines they have dispensed on the basis of the NIC less a deduction related to the discount that they are assumed to have received from their suppliers (for details see the Drug Tariff Part V - Deduction Scale). A container allowance is then added (see Drug Tariff Part IV). The figures are in £s and pence.
# Note: electronic Drug Tariff can be found at:  http://www.ppa.org.uk/ppa/edt_intro.htm


# Quantity
# The quantity of a drug dispensed is measured in units depending on the formulation of the product, which is given in the drug name. Quantities should not be added together across preparations because of different strengths and formulations.

# Where the formulation is tablet, capsule, ampoule, vial etc the quantity will be the number of tablets, capsules, ampoules, vials etc
# where the formulation is a liquid the quantity will be the number of MLS
# Where the formulation is a solid form (eg. Cream, gel, ointment) the quantity will be the number of grammes.


# denominators for rates ####

# You can use "list size" which tells you how many patients a practice covers, but this can be problematic, because different practices will have different kinds of patients, some with lots of older people, and so on.

# To account for this the NHS uses imperfect but useful "adjusted" denominators called STAR-PUs, which try to account for the age and sex structure of the practice's population. These STAR-PUs are specific to specific disease areas, because they try to account for different rates of usage -- in different age bands of the population - for specific treatment. So for the STAR-PU for cardiovascular disease prevention prescribing, for example, gives you extra points for every man aged 40-50, even more for men aged 50-60, and so on; but less for women in the same bands, and very little for younger people.
Generating these STAR-PUs for each practice, each disease area, and each month, takes coder time, so we currently only have the STAR-PU for antibiotics.
When using the data ourselves we tend to use more thoughtful approaches to try to "bake in" population prevalence or need for a particular condition, or to explore different prescribing patterns. For example, we often use whole classes of drug as the denominator in our analyses, as in the video walkthroughs; or we compare the use of one drug against the use of another. When looking at whether a practice is using a lot of Nexium (an expensive "proton pump inhibitor" pill for treating ulcers) we might look at "Nexium prescribing" versus "all proton pump inhibitor prescribing" (example).
Play around and let us know if you find anything interesting, or develop any interesting methods.
