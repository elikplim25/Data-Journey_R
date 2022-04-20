# ############################################################################################################
# 
# This R script scrapes the procurement information on Ghana's Public Procurement Authority's website
# and stores it as .csv file.
# 
# All that is required is changing the page number on line 24 to reflect the total number of pages on the site.
# 
# And that's pretty much all there's to it!
# 
# Efficiency improvement suggestions welcome: datalabgh@gmail.com
#
# ############################################################################################################

library('rvest')
library('stringr')
library('tibble')

# website base url
base_url <- 'http://tenders.ppa.gov.gh/contracts'

# total number of pages on site
# total number of pages on site. 
# 552 at the last count. change this to reflect new information)
total_site_pages <- '552'

# initialise an empty character vector to store contract urls
ppa_contract_title_url_vec = character()

# initialise vector to store retrieved contract details
ppa_contracts_vec = character()

# placeholders for contract detail vectors (17 of them)
Awarding_Agency = character()
Tender_Package_No = character()
Tender_Type = character()
Contract_Type = character()
Lot_No = character()
Tender_Description = character()
Approval_Auth = character()
Justification = character()
Contract_Date = character()
Completion_Date = character()
Contract_Currency = character()
Contract_Award_Price = character()
Contract_Awarded_To = character()
Company_Email = character()
Company_Address = character()
Company_Tel = character()
Supplier_No = character()

# loop through links on webpage per page
for(i in 1:as.integer(total_site_pages)){
  
  # tell user where you are in execution
  print('=======================')
  print(paste0("Reading page ", i, " of ", as.integer(total_site_pages)," of website..."))
  print('=======================')
  
  
  # read page url
  ppa_contracts <- read_html(paste0(base_url, '?page=', i))
  
  # read clickable title permalinks
  ppa_contract_titles <- ppa_contracts %>%
                         html_nodes('.list-title a') %>%
                         html_attr('href') %>%
                         str_squish() # remove all white spaces
  
  # loop through titles
  for(j in 1:length(ppa_contract_titles)){

    # read details of each contract from permalink
    ppa_contract_details_url <- read_html(ppa_contract_titles[j])

    # read nodes of contract details
    ppa_contract_details <- html_nodes(ppa_contract_details_url, xpath = ".//dl") %>% # details stored in dl nodes
                            html_text() %>%
                            str_squish() # remove all white spaces

    # begin loop over contract details vector
    for(k in 1:length(ppa_contract_details)){

      # split details but use only first occurence of ":".
      ppa_contract_detail <- unlist(strsplit(sub(":", "\010", ppa_contract_details[k]), '\010'))[2]

      # vector of contract details
      ppa_contracts_vec <- c(ppa_contracts_vec, ppa_contract_detail)

      # get individual vectors of contract details (17 of them)
      if(length(ppa_contracts_vec) == 17){
        Awarding_Agency = c(Awarding_Agency, ppa_contracts_vec[1])
        Tender_Package_No = c(Tender_Package_No, ppa_contracts_vec[2])
        Tender_Type = c(Tender_Type, ppa_contracts_vec[3])
        Contract_Type = c(Contract_Type, ppa_contracts_vec[4])
        Lot_No = c(Lot_No, ppa_contracts_vec[5])
        Tender_Description = c(Tender_Description, ppa_contracts_vec[6])
        Approval_Auth = c(Approval_Auth, ppa_contracts_vec[7])
        Justification = c(Justification, ppa_contracts_vec[8])
        Contract_Date = c(Contract_Date, ppa_contracts_vec[9])
        Completion_Date = c(Completion_Date, ppa_contracts_vec[10])
        Contract_Currency = c(Contract_Currency, ppa_contracts_vec[11])
        Contract_Award_Price = c(Contract_Award_Price, ppa_contracts_vec[12])
        Contract_Awarded_To = c(Contract_Awarded_To, ppa_contracts_vec[13])
        Company_Email = c(Company_Email, ppa_contracts_vec[14])
        Company_Address = c(Company_Address, ppa_contracts_vec[15])
        Company_Tel = c(Company_Tel, ppa_contracts_vec[16])
        Supplier_No = c(Supplier_No, ppa_contracts_vec[17])

        # empty vector and start again for another set
        ppa_contracts_vec = character()
      }
    }
  }
}

# create final dataframe of contract details
ppa_contracts_df <- tibble(Awarding_Agency,Tender_Package_No, Tender_Type, Contract_Type,
                           Lot_No, Tender_Description, Approval_Auth, Justification,
                           Contract_Date, Completion_Date, Contract_Currency, Contract_Award_Price,
                           Contract_Awarded_To, Company_Email, Company_Address, Company_Tel,
                           Supplier_No)

# rename column headers
names(ppa_contracts_df) <- c("Awarding Agency","Tender Package No","Tender Type","Contract Type",
                             "Lot #","Tender Description","Approval Auth","Justification",
                             "Contract Date","Completion Date","Contract Currency","Contract Award Price",
                             "Contract Awarded To","Company Email","Company Address","Company Tel",
                             "Supplier #")

# tell user where you are in execution
print("Writing data frame to file...")

# write to file (csv)
write.csv(x = ppa_contracts_df, file = "ppa_contracts.csv")

# tell user where you are in execution
print('=======================')
print("Done scraping data from website. Written to file.")
print('=======================')