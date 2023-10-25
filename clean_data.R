library(data.table)
library(magrittr)
library(readxl)
library(tibble)
library(purrr)
library(salesforcer)
library(stringi)



data <- read_excel("State Auditor's Report - Unified-2023-09-08-12-46-57.xlsx",1) %>% 
  janitor::clean_names() %>% 
  as.data.table() %>% 
  .[,.(
    entity_name,
    
    primary_first_name = first_name_3,
    primary_last_name = last_name_4,
    primary_title = title_5,
    primary_email = email_6,
    primary_phone = phone_7,
    entity_counties,
    
    billing_city,
    billing_street,
    billing_state_province,
    billing_zip_postal_code,
    
    shipping_city,
    shipping_street,
    mailing_street,
    mailing_state_province,
    mailing_zip_postal_code,
    
    govt_lvl = entity_type,
    entity_last_modified_date,
    entity_status,
    registration_approved_date,
    
    board_member = board_member_board_members_name,
    bm_name = first_name_23,
    bm_last_name = last_name_24,
    bm_active = active,
    bm_designation = designation,
    bm_email = email_27,
    bm_phone = phone_28,
    bm_title = title_29
    
  )]
  
standard_entity_name <- ""

impute_entity_name <- function(entity_name_p){
  if(entity_name_p %>% is.na()){
    return(standard_entity_name)
  }else{
    standard_entity_name <<- entity_name_p
    return(entity_name_p)
  }
}

convert_to_osa_titlecase <- function(name_str){
  name_str %>% 
    stri_trans_totitle() %>% 
    stri_replace_all_regex(" With ", " with ") %>% 
    stri_replace_all_regex(" Of "  , " of ") %>% 
    stri_replace_all_regex(" And " , " and ") %>% 
    stri_replace_all_regex(" For " , " for ") %>% 
    stri_replace_all_regex(" And " , " and ") %>% 
    stri_replace_all_regex(" At ", " at ") %>% 
    stri_replace_all_regex(" The ", " the ") %>% 
    stri_replace_all_regex(" On ", " on ") %>% 
    
    #fix title case issues:
    stri_replace_all_regex(" Cpas", " CPAs") %>% 
    stri_replace_all_regex(" Cpa", " CPA") %>% 
    stri_replace_all_regex(" Cfp", " CFP") %>% 
    stri_replace_all_regex("Arup Lab", "ARUP Lab") %>% 
    stri_replace_all_regex(" Md", " MD") %>% 
    stri_replace_all_regex(" Pc", " PC") %>% 
    stri_replace_all_regex(" Pplc", " PPLC") %>% 
    stri_replace_all_regex(" Pllc", " PLLC") %>% 
    stri_replace_all_regex(" Od", " OD") %>% 
    stri_replace_all_regex(" Dpm", " DPM") %>%
    stri_replace_all_regex(" Do", " DO") %>% 
    stri_replace_all_regex(" Np", " NP") %>% 
    stri_replace_all_regex("Af Cnms", "AF CNMS") %>% 
    
     
    stri_replace_all_regex("Bdo Usa Llp", "BDO USA") %>% 
    stri_replace_all_regex("Bjb$", "BJB") %>% 
    stri_replace_all_regex("Chp$", "CHP") %>% 
    stri_replace_all_regex(" Usa", " USA") %>% 
    stri_replace_all_regex("Ihc", "IHC") %>% 
    stri_replace_all_regex("Imed", "IMED") %>% 
    stri_replace_all_regex("Ldsh", "LDSH") %>% 
    stri_replace_all_regex("Uuhsc", "University of Utah Health Sciences Center") %>% 
    stri_replace_all_regex("Uvpm", "Utah Valley Pain Management") %>%
    stri_replace_all_regex("Uvrmc ", "Utah Valley Hospital") %>%
    stri_replace_all_regex("Imurs ", "IMURS") %>%
    stri_replace_all_regex("Lds ", "LDS ") %>% 
    
    stri_replace_all_regex("Clr Vision PC", "CLR Vision PC") %>% 
    stri_replace_all_regex("Cns Home Health Plus", "CNS Home Health Plus") %>% 
    stri_replace_all_regex("Cs Lewis Academy", "CS Lewis Academy") %>% 
    stri_replace_all_regex("Centro De La Familia De Utah", "Centro de la Familia de Utah") %>% 
    stri_replace_all_regex("Comunidad Materna En Utah", "Comunidad Materna en Utah") %>% 
    stri_replace_all_regex("Iaa Draper", "IAA Draper") %>% 
    stri_replace_all_regex("Kpmg", "KPMG") %>% 
    stri_replace_all_regex("Kt&T", "KT and T") %>% 
    stri_replace_all_regex("Kt&T Ventures", "KT and T Ventures") %>% 
    stri_replace_all_regex("Kuen", "KUEN") %>% 
    stri_replace_all_regex("Osna - Aosa", "OSNA - AOSA") %>% 
    stri_replace_all_regex("Rites", "RITES") %>% 
    stri_replace_all_regex("Rha - Community Services of Utah", "RHA - Community Services of Utah") %>% 
    stri_replace_all_regex(" Va Clinic", " Veteran Affairs Clinic") %>% 
    stri_replace_all_regex("Slc", "Salt Lake City") %>% 
    stri_replace_all_regex("Tkj$", "TKJ") %>% 
    stri_replace_all_regex("Tmj ", "TMJ ") %>% 
    stri_replace_all_regex("Us Bank", "US Bank") %>% 
    stri_replace_all_regex("Us Synthetic Clinic", "US Synthetic Clinic") %>% 
    stri_replace_all_regex("Ffelp ", "FFELP ") %>% 
    stri_replace_all_regex("Wsrp ", "WRSP") %>% 
    stri_replace_all_regex("Utopia", "UTOPIA") %>% 
    stri_replace_all_regex("Ycc Family Crisis Center", "YCC Family Crisis Center") %>% 
    stri_replace_all_regex("Ymca of Northern Utah", "YMCA of Northern Utah") %>% 
    stri_replace_all_regex("Ccd Smiles", "CCD Smiles") %>% 
    stri_replace_all_regex("Cchd Charitable Corporation", "CCHD Charitable Corporation") %>% 
    stri_replace_all_regex("Aaa Fair Credit Foundation", "AAA Fair Credit Foundation")
  
  
  
}

data2 <- data[,`:=`(impute_entity_name = entity_name %>% map_chr(impute_entity_name))]


board_members <- data2[,.(entity_name = impute_entity_name,
                          board_member,
                          bm_name,
                          bm_last_name,
                          bm_active,
                          bm_designation,
                          bm_email,
                          bm_phone,
                          bm_title)]

entities <- data2[,.(
  entity_name,
  
  primary_first_name,
  primary_last_name,
  primary_title,
  primary_email,
  primary_phone,
  entity_counties,
  
  billing_city,
  billing_street,
  billing_state_province,
  billing_zip_postal_code,
  
  shipping_city,
  shipping_street,
  mailing_street,
  mailing_state_province,
  mailing_zip_postal_code,
  
  govt_lvl,
  entity_last_modified_date,
  entity_status,
  registration_approved_date
)] %>% 
  .[!is.na(entity_name)] %>% 
  unique()




# Get existing information -----------------------------------------------------

sf <-  sf_auth(username = "alexnielson@utah.gov",
             password = 'Muckdweller2023@',
             security_token = 'tj9SnVNoOZ0Fa9dXMIV46gwW')

temp_soql <- paste0(" SELECT 
                        Name,
                        Id,
                        Transparency_ID__c,
                        Entity_Status__c,
                        Is_Solely_Fee_Assessing__c,
                        ShippingStreet,
                        ShippingCity,
                        ShippingState,
                        ShippingPostalCode,
                        BillingStreet,
                        BillingCity,
                        BillingState,
                        BillingPostalCode,
                        Mailing_county__c,
                        Tax_Holding_County__c,
                        FINET_Vendor_Code__c,
                        Fiscal_Year_End_New__c,
                        RecordType.Name
                        
                      FROM Account
                    
                    ")


sf_account <- sf_query(temp_soql)%>%
  as.data.table() %>% 
  .[,.( 
    sf_name           = Name,
    non_taxing_entity = Is_Solely_Fee_Assessing__c,
    sf_id             = Id,
    aws_id            = Transparency_ID__c,
    govt_lvl = RecordType.Name,
    entity_status     = Entity_Status__c,
    ship_street       = ShippingStreet,
    ship_city         = ShippingCity,
    ship_state        = ShippingState,
    ship_pc           = ShippingPostalCode,
    bill_street       = BillingStreet,
    bill_city         = BillingCity,
    bill_state        = BillingState,
    bill_pc           = BillingPostalCode,
    mailing_county    = Mailing_county__c,
    tax_holding_county_id= Tax_Holding_County__c,
    finet_vendor_code = FINET_Vendor_Code__c,
    fiscal_yr_end = Fiscal_Year_End_New__c
  )] #%>% 
  # .[!(entity_status %in% c("Inactive", "Dissolved"))] 


tax_holding_entity <- sf_account[,.(tax_holding_county_id = sf_id,
                                    tax_holding_county= sf_name)]

sf_account <- merge(sf_account, tax_holding_entity, all.x=T, by="tax_holding_county_id")

# sf_account_flags <- sf_account[sf_name %>%
                                 # stri_detect_regex("[^[:alnum:][:blank:]-]") | sf_name %>% stri_detect_regex("(SSD)|(CD)|(PID)")] %>% 
  # .[!(govt_lvl %in% c("Health Provider", "CPA Firm"))] %>% 
sf_account2 <- sf_account %>%   
.[sf_name != "salesforce.com"] %>% 
  .[,.(sf_id, govt_lvl, entity_status, sf_name)] %>% 
  .[,`:=`(new_name = fcase(
    #Manual Name Changes
    sf_name == "Family Support & Advocacy Ctr of SE UT", "Family Support and Advocacy Center of Southeast Utah",
    sf_name == "Green Hills Est. Water and Sewer Imp. District", "Green Hills Water and Sewer Improvement District",
    sf_name == "North Salt Lake, City of", "North Salt Lake City",
    sf_name == "St. Anne's Center DBA: Lantern House", "Saint Annes Center",
    sf_name == "West Jordan Fairway Estates SS Rec. District", "West Jordan Fairway Estates Special Service Recreation District",
    sf_name == "Utah Alliance of Economic Development Prof.", "Utah Alliance of Economic Development Professionals",
    sf_name == "Delta-Sutherland-Oasis Cemetery Maintenance", "Delta-Sutherland-Oasis Cemetery Maintenance District",
    sf_name == "Hanksville", "Hanksville Town",
    sf_name == "High Country Special Improvement District", "High County Special Improvement District",
    sf_name == "Independence", "Independence Town",
    sf_name == "MOSAIC Inter-Faith Ministries dba", "Mosaic Inter-Faith Ministries",
    sf_name == "Mosquito Abatement District- Davis", "Mosquito Abatement District-Davis",
    sf_name == "Spanish Valley Wtr and Sewer Improv. District", "Spanish Valley Water and Sewer Improvement District",
    sf_name == "Taylor-West Weber Water Improvement Dist", "Taylor-West Weber Water Improvement District",
    sf_name == "Utah County Health Dept", "Utah County Health Department",
    sf_name == "Utah Symphony|Utah Opera", "Utah Symphony and Opera",
    sf_name == "Delta-Sutherland-Oasis Cemetery Maint.", "Delta-Sutherland-Oasis Cemetery Maintenance District",
    sf_name == "American Preparatory Academy- Utah Charter Academies", "American Preparatory Academy - Utah Charter Academies",
    sf_name == "Legislative Research and General Counsel, Office of", "Legislative Research and General Counsel",
    sf_name == "P3+", "P3 Plus",
    sf_name == "Park City Performances DBA Egyptian Theater", "Park City Performances - Egyptian Theater",
    sf_name == "UAMMI (Utah Advanced Materials and Manufacturing Initiative)", "Utah Advanced Materials and Manufacturing Initiative",
    sf_name == "UNIVERSITY HEALTH CARE PARK CITY MTN RESORT", "University Health Care Park City Mountain Resort",
    sf_name == "USU STUDENT HEALTH & WELLNESS", "Utah State University Student Health and Wellness",
    sf_name == "Utah Fund of Funds II", "Utah Fund of Funds 2",
    sf_name == "Utah Municipal Finance Cooperative II", "Utah Municipal Finance Cooperative 2",
    sf_name == "Utah System of Higher Education-Utah Board of Higher Education", "Utah System of Higher Education - Utah Board of Higher Education",
    sf_name == "WPR Road and Fire District", "Wasatch Peaks Ranch Road and Fire District",
    sf_name == "WPR Utility District", "Wasatch Peaks Ranch Utility District",
    sf_name == "High County Special Improvement District", "High Country Special Improvement District",
    sf_name == "Juab County Spec Serv Fire Protection District", "Juab County Special Service Fire Protection District",
    sf_name == "UNIV. OF UTAH HOSP. IM- HEMATOLOGY", "University of Utah Hospital IM - Hematology",
    # Weird Comma behaviors:
    sf_name %>% stri_detect_regex(", Department of"), sf_name %>% stri_replace_all_regex(", Department of","") %>% paste0("Department of ",.),
    sf_name %>% stri_detect_regex(", Department"), sf_name %>% stri_replace_all_regex(", Department","") %>% paste0("Department of ",.),
    sf_name %>% stri_detect_regex(", Dept. of"), sf_name %>% stri_replace_all_regex(", Dept. of","") %>% paste0("Department of ",.),
    
    #Weird "-" behaviors:
    sf_name %>% stri_detect_regex("(?<!\\s)-\\s"), sf_name %>% stri_replace_all_regex("(?<!\\s)-\\s"," - "),
    
    
    #Incorporations, LLCs
    sf_name %>% stri_detect_regex(", Incorporated"), sf_name %>% stri_replace_all_regex(", Incorporated", ""),
    sf_name %>% stri_detect_regex(", Inc\\."), sf_name %>% stri_replace_all_regex(", Inc\\.", ""),
    sf_name %>% stri_detect_regex(", Inc"), sf_name %>% stri_replace_all_regex(", Inc", ""),
    sf_name %>% stri_detect_regex(", INC"), sf_name %>% stri_replace_all_regex(", INC", ""),
    sf_name %>% stri_detect_regex(", LLC"), sf_name %>% stri_replace_all_regex(", LLC", ""),
    sf_name %>% stri_detect_regex(", LLP"), sf_name %>% stri_replace_all_regex(", LLP", ""),
    sf_name %>% stri_detect_regex(" LLC"), sf_name %>% stri_replace_all_regex(" LLC", ""),
    sf_name %>% stri_detect_regex(" LLP"), sf_name %>% stri_replace_all_regex(" LLP", ""),
    sf_name %>% stri_detect_regex("Inc\\."), sf_name %>% stri_replace_all_regex("Inc\\.", ""),
    sf_name %>% stri_detect_regex(" Inc$"), sf_name %>% stri_replace_all_regex(" Inc$",""),
    sf_name %>% stri_detect_regex(" INC$"), sf_name %>% stri_replace_all_regex(" INC$",""),
  
    # Abbreviations
    sf_name %>% stri_detect_regex("Serv\\."), sf_name %>% stri_replace_all_regex("Serv\\.", "Service"),
    sf_name %>% stri_detect_regex("SSD"), sf_name %>% stri_replace_all_regex("SSD", "Special Service District"),
    sf_name %>% stri_detect_regex("PID"), sf_name %>% stri_replace_all_regex("PID", "Public Infrastructure District"),
    sf_name %>% stri_detect_regex("Spec\\."), sf_name %>% stri_replace_all_regex("Spec\\.", "Special"),
    sf_name %>% stri_detect_regex("St\\."), sf_name %>% stri_replace_all_regex("St\\.", "Saint"),
    sf_name %>% stri_detect_regex("Mt\\."), sf_name %>% stri_replace_all_regex("Mt\\.", "Mount"),
    sf_name %>% stri_detect_regex("No\\."), sf_name %>% stri_replace_all_regex("No\\.", ""),
    sf_name %>% stri_detect_regex("Maint\\."), sf_name %>% stri_replace_all_regex("Maint\\.", "Maintenance"),
    sf_name %>% stri_detect_regex("Imp\\."), sf_name %>% stri_replace_all_regex("Imp\\.", "Improvement"),
    sf_name %>% stri_detect_regex("Lt\\."), sf_name %>% stri_replace_all_regex("Lt\\.", "Lieutenant"),
    sf_name %>% stri_detect_regex("Improv\\."), sf_name %>% stri_replace_all_regex("Improv\\.",""),
    sf_name %>% stri_detect_regex("EMS"), sf_name %>% stri_replace_all_regex("EMS", "Emergency Medical Services"),
    sf_name %>% stri_detect_regex("Wtr"), sf_name %>% stri_replace_all_regex("Wtr", "Water"),
    sf_name %>% stri_detect_regex(" MBA "), sf_name %>% stri_replace_all_regex(" MBA "," Municipal Building Authority "),
    sf_name %>% stri_detect_regex("U OF U "), sf_name %>% stri_replace_all_regex("U OF U ","University of Utah "),
    sf_name %>% stri_detect_regex("UNIV\\. OF UTAH "), sf_name %>% stri_replace_all_regex("UNIV\\. OF UTAH ","University of Utah "),
    sf_name %>% stri_detect_regex("HOSP\\. "), sf_name %>% stri_replace_all_regex("HOSP\\. ","Hospital "),
    sf_name %>% stri_detect_regex("UOU "), sf_name %>% stri_replace_all_regex("UOU ","University of Utah "),
    
    #Special Characters
    sf_name %>% stri_detect_regex("&"), sf_name %>% stri_replace_all_regex(" & ", " and "),
    sf_name %>% stri_detect_regex("\\s&(?!\\s)"), sf_name %>% stri_replace_all_regex("\\s&(?!\\s)", " and "),
    sf_name %>% stri_detect_regex("/"), sf_name %>% stri_replace_all_regex("/", "-"),
    sf_name %>% stri_detect_regex("#"), sf_name %>% stri_replace_all_regex("#", " "),
    sf_name %>% stri_detect_regex("\\|"), sf_name %>% stri_replace_all_regex("\\|", "-"),
    sf_name %>% stri_detect_regex("\\("), sf_name %>% stri_replace_all_regex("\\(.+\\)", ""),
    sf_name %>% stri_detect_regex("'"), sf_name %>% stri_replace_all_regex("'", ""),
    sf_name %>% stri_detect_regex("’"), sf_name %>% stri_replace_all_regex("’", ""),
    sf_name %>% stri_detect_regex(","), sf_name %>% stri_replace_all_regex(",", ""),
    sf_name %>% stri_detect_regex("\\."), sf_name %>% stri_replace_all_regex("\\.",""),
    sf_name %>% stri_detect_regex(" IV$"), sf_name %>% stri_replace_all_regex(" IV$"," 4"),
    sf_name %>% stri_detect_regex(" III$"), sf_name %>% stri_replace_all_regex(" III$"," 3"),
    sf_name %>% stri_detect_regex(" II$"), sf_name %>% stri_replace_all_regex(" II$"," 2"),
    sf_name %>% stri_detect_regex(" I$"), sf_name %>% stri_replace_all_regex(" I$"," 1"),
    
    default= "NEEDS RULE"
  ) %>% 
    
    fifelse( stri_detect_regex(.,"St\\."),  stri_replace_all_regex(.,"St\\.", "Saint"),.) %>% 
    fifelse( stri_detect_regex(.,"Mt\\."),  stri_replace_all_regex(.,"Mt\\.", "Mount"),.) %>% 
    fifelse( stri_detect_regex(.,"St George"),  stri_replace_all_regex(.,"St George", "Saint George"),.) %>% 
    fifelse( stri_detect_regex(.,"ST GEORGE"),  stri_replace_all_regex(.,"ST GEORGE", "Saint George"),.) %>%
    fifelse( stri_detect_regex(.,"ST\\. GEORGE"),  stri_replace_all_regex(.,"ST\\. GEORGE", "Saint George"),.) %>%
    fifelse( stri_detect_regex(.,"St Marks"),  stri_replace_all_regex(.,"St Marks", "Saint Marks"),.) %>% 
    fifelse( stri_detect_regex(.,"ST MARKS"),  stri_replace_all_regex(.,"ST MARKS", "Saint Marks"),.) %>% 
    fifelse( stri_detect_regex(.,"ST\\. MARKS"),  stri_replace_all_regex(.,"ST\\. MARKS", "Saint Marks"),.) %>% 
    fifelse( stri_detect_regex(.,"&"),  stri_replace_all_regex(.,"&", " and "),.) %>% 
    fifelse( stri_detect_regex(.,"/"),  stri_replace_all_regex(.,"/", "-"),.) %>% 
    fifelse( stri_detect_regex(.,"#"),  stri_replace_all_regex(.,"#", " "),.) %>% 
    fifelse( stri_detect_regex(.,"\\|"),  stri_replace_all_regex(.,"\\|", "-"),.) %>% 
    fifelse( stri_detect_regex(.,"\\("),  stri_replace_all_regex(.,"\\(.+\\)", ""),.) %>% 
    fifelse( stri_detect_regex(.,"Spec Serv"), stri_replace_all_regex(.,"Spec Serv","Special Service"),.) %>% 
    fifelse( stri_detect_regex(.,"Serv\\."),  stri_replace_all_regex(.,"Serv\\.", "Service"),.) %>% 
    fifelse( stri_detect_regex(.,"SSD"),  stri_replace_all_regex(.,"SSD", "Special Service District"),.) %>% 
    fifelse( stri_detect_regex(.,"PID"),  stri_replace_all_regex(.,"PID", "Public Infrastructure District"),.) %>% 
    fifelse( stri_detect_regex(.,"PUD"),  stri_replace_all_regex(.,"PID", "Planned Unit Development"),.) %>% 
    fifelse( stri_detect_regex(.,"Spec\\."),  stri_replace_all_regex(.,"Spec\\.", "Special"),.) %>% 
    fifelse( stri_detect_regex(.,"Comm\\. Dev\\."),  stri_replace_all_regex(.,"Comm\\. Dev\\.", "Community Development"),.) %>%
    fifelse( stri_detect_regex(.,"No\\."),  stri_replace_all_regex(.,"No\\.", ""),.) %>%
    fifelse( stri_detect_regex(.,"'"),  stri_replace_all_regex(.,"'", ""),.) %>%
    fifelse( stri_detect_regex(.,"’"),  stri_replace_all_regex(.,"’", ""),.) %>%
    fifelse( stri_detect_regex(.,"Maint\\."),  stri_replace_all_regex(.,"Maint\\.", "Maintenance"),.) %>% 
    fifelse( stri_detect_regex(.,","),  stri_replace_all_regex(.,",", ""),.) %>%
    fifelse( stri_detect_regex(.,"Lt\\."),  stri_replace_all_regex(.,"Lt\\.", "Lieutenant"),.) %>%
    fifelse( stri_detect_regex(.," MBA "),  stri_replace_all_regex(.,"MBA", "Municipial Building Authority"),.) %>%
    fifelse( stri_detect_regex(.," IV$"),  stri_replace_all_regex(.," IV$", " 4"),.) %>%
    fifelse( stri_detect_regex(.," III$"),  stri_replace_all_regex(.," III$", " 3"),.) %>%
    fifelse( stri_detect_regex(.," II$"),  stri_replace_all_regex(.," II$", " 2"),.) %>%
    fifelse( stri_detect_regex(.," I$"),  stri_replace_all_regex(.," I$", " 1"),.) %>% 
    # fifelse( stri_detect_regex(.,""),  stri_replace_all_regex(.,"", ""),.) %>% 
    # fifelse( stri_detect_regex(.,""),  stri_replace_all_regex(.,"", ""),.) %>% 
    
    stri_replace_all_regex("\\s+"," ") %>% 
    stri_trim_both() %>% 
    stri_replace_all_regex("\\.","") 
  )] %>% 
  .[,`:=`(new_clean_name = fifelse(new_name=="NEEDS RULE", sf_name, new_name) %>% 
      convert_to_osa_titlecase() %>% 
        stri_trim_both())] %>% 
  .[order(sf_name)]
  
  




temp_soql <- paste0(" 
          SELECT 
            Name,
            AccountId,
            Role__c,
            Email,
            Phone,
            Is_Primary__c,
            IsDeleted,
            IsDeactivated__c
          FROM Contact")

sf_contact <- sf_query(temp_soql)%>%
  as.data.table() 


# update the salesforce objects we have so that names comply with good standards
# General Naming Rules/Conventions:
# 1. No special characters. only alphanumeric characters.   
# 2. No acronyms for SSD, SD, CD, PID, etc. 
# 3. Spell all words out fully, do not abbreviate. "Dist" ==> "District"





# First pass using exact matches for entities

entities2 <- entities %>% 
  .[, entity_name := fifelse(
    entity_name %>% stri_detect_regex("City [oO]f "),
    entity_name %>% 
      stri_replace_all_regex("City [oO]f","") %>% 
      paste0(.," City") %>%
      stri_trim_both(),
    entity_name
  )] %>% 
  .[,clean_entity_name := fcase(
  entity_name == "Academy for Math, Engineering & Science", "Academy for Math Engineering and Science",
  entity_name == 'Alf Engen Ski Museum Foundation', "Alf Engen Ski Museum",
  entity_name == "Alta Community Enrichment - Alta Arts Council", "Alta Community Enrichment",
  entity_name == "American Preparatory Academy – Draper 1", "American Preparatory Academy - Utah Charter Academies",
  entity_name == "American Preparatory Academy – Draper 2", "American Preparatory Academy - Utah Charter Academies",
  entity_name == "American Preparatory Academy – Draper 3", "American Preparatory Academy - Utah Charter Academies",
  entity_name == "American Preparatory Academy – Salem", "American Preparatory Academy - Utah Charter Academies",
  entity_name == "American Preparatory Academy – The Accelerated School", "American Preparatory Academy - Utah Charter Academies",
  entity_name == "American Preparatory Academy – The School for New Americans", "American Preparatory Academy - Utah Charter Academies",
  entity_name == "American West Heritage Foundation", "American West Heritage Center",
  entity_name == "American West Symphony & Chorus", "American West Symphony and Chorus of Sandy",
  entity_name == "American Festival Chorus and Orchestra", "American Festival Chorus",
  entity_name == "Apple Valley", "Apple Valley Town",
  entity_name == "Ascent Academies of Utah - Farmington", "Ascent Academies of Utah",
  entity_name == "Ascent Academies of Utah - Lehi", "Ascent Academies of Utah",
  entity_name == "Ascent Academies of Utah - West Jordan", "Ascent Academies of Utah",
  entity_name == "Ashley Valley Water & Sewer Improvement District", "Ashley Valley Water and Sewer Improvement District",
  entity_name == "Asian Association of Utah, dba Refugee and Immigrant Center", "Asian Association of Utah",
  entity_name == "Assist Inc", "Assist",
  entity_name == "Athlos Academy", "Athlos Academy of Utah",
  entity_name == "Avon Cemetery District", "Avon Cemetery Maintenance District",
  entity_name == "Auto Mall & Retail Public Infrastructure District", "Auto Mall and Retail Public Infrastructure District",
  entity_name == "Bear Lake Community Health Center, Inc.", "Bear Lake Community Health Center",
  entity_name == "Bear River Associations of Governments", "Bear River Association of Governments",
  entity_name == "Bear River Mental Health Services, Inc.", "Bear River Mental Health Service",
  entity_name == "Bear River Town Corporation", "Bear River City",
  entity_name == "Beaver Dam Special Service District", "Beaver Dam Village Special Service District",
  entity_name == "Beaver Fire District No. 1", "Beaver Fire District 1",
  entity_name == "Beaver County Fire District #2", "Beaver County Fire District 2",
  entity_name == "Beaver County Special Service District #7", "Beaver County Special Service District 7",
  entity_name == "Beaver County Waste Management Service District #5", "Beaver County Waste Management Service District 5",
  entity_name == "Benson Culinary Improvement District", "Benson Culinary Water Improvement District",
  entity_name == "Big Brothers Big Sisters Of Utah", "Big Brothers - Big Sisters of Utah",
  entity_name == "Big Water Municipal", "Big Water Town",
  entity_name == "Birch Creek Service Ranch, Inc.", "Birch Creek Service Ranch",
  entity_name == "BJB, Inc.", "Bjb",
  entity_name == "Blackhawk Arena MBA #2", "Blackhawk Arena Municpial Building Authority 2",
  entity_name == "Blacksmith Fork Soil Conservation District", "Blacksmith Fork Conservation District",
  entity_name == "Bluff Water Works Special Service District", "Bluff Water Works District",
  entity_name == "Bountiful Davis Art Center Foundation", "Bountiful Davis Arts Center",
  entity_name == "Box Elder County and Perry City Flood Control Drainage Special Service District", "Box Elder County and Perry City Flood Control",
  entity_name == "Box Elder County Grouse Creek Solid Waste", "Box Elder County Solid Waste",
  entity_name == "Box Elder County Service Area No. 2", "Box Elder County Service Area 2",
  entity_name == "Box Elder Special Service District", "Box Elder County Special Service District",
  entity_name == "Boys & Girls Clubs of Greater Salt Lake","Boys and Girls Clubs of Greater Salt Lake",
  entity_name == "Boys & Girls Club of Northern Utah","Boys and Girls Clubs of Northern Utah",
  entity_name == "Boys & Girls Clubs of Utah County","Boys and Girls Clubs of Utah County",
  entity_name == "Boys & Girls Clubs of Weber-Davis","Boys and Girls Clubs of Weber-Davis",
  # entity_name == "Boys & Girls Clubs of Weber-Davis", "Boys and Girls Clubs of Weber Davis",
  entity_name == "Brian Head", "Brian Head Town",
  entity_name == "Brigham City Corporation", "Brigham City",
  entity_name == "Cache County Drainage District #3", "Cache County Drainage District 3",
  entity_name == "Cache County Drainage District #6", "Cache County Drainage District 6",
  entity_name == "Cache County School District", "Cache School District",
  entity_name == "Cache Refugee and Immigrant Connection (CRIC)", "Cache Refugee and Immigrant Connection",
  entity_name == "C.S. Lewis Academy", "CS Lewis Academy",
  # entity_name == "CBA Center", "",
  # entity_name == "CCD Smiles", "",
  # entity_name == "CCHD Charitable Corporation", "",
  entity_name == "COLOR COUNTRY RESOURCE CONSERVATION AND DEVELOPMENT COUNCIL and DBA Five County Community Foundation", "Color Country Resource Conservation and Development Council",
  entity_name == "Cache Valley Center for the Arts-Cache Arts", "Cache Valley Center for the Arts",
  # entity_name == "Cache Valley Unitarian Universalists", "",
  entity_name == "Canyon Creek Women's Crisis Center", "Canyon Creek Womens Crisis Center",
  # entity_name == "Canyonlands Field Institute", "",
  entity_name == "Canyons School District Education Foundation", "Canyons School District",
  # entity_name == "Carbon County Economic Development Agency", "",
  entity_name == "Carbon County Recreation and Transportation Special Service District", "Carbon County Recreation and Transportation Special Service District",
  entity_name == "Carbon County School District", "Carbon School District",
  entity_name == "Castleland Resource Conservation and Development Council, INC", "Castleland Resource Conservation and Development Council",
  entity_name == "Catholic Community Services Utah", "Catholic Community Services of Utah",
  entity_name == "Cedar City Corporation", "Cedar City",
  entity_name == "Central Box Elder Fire District", "Central Box Elder Fire Special Service District",
  entity_name == "Central Utah Educational Services (CUES)", "Central Utah Educational Services",
  entity_name == "Central Davis County Sewer District", "Central Davis Sewer District",
  entity_name == "Central Utah Mental Health Substance Abuse Center, Dba Central Utah Counseling Center", "Central Utah Counseling Center",
  entity_name == "Center for Education, Business, and the Arts Interlocal Agency", "Center for Education Business and the Arts Interlocal Agency",
  entity_name == "Davis Performing Arts Association dba CenterPoint Legacy Theatre", "Centerpoint Legacy Theatre",
  entity_name == "Central Utah Mental Health Substance Abuse Center, dba Central Utah Counseling Center", "Central Utah Counseling Center",
  
  entity_name == "Cerebral Palsy of Utah dba Foundations for Independence", "Cerebral Palsy of Utah",
  
  entity_name == "Chelsey Public Infrastructure District No. 1", "Chelsey Public Infrastructure District 1",
  entity_name == "Child and Family Support Center of Cache County (dba The Family Place)", "Child and Family Support Center of Cache County",
  entity_name == "Children and Youth Services, Inc.", "Children and Youth Services",
  entity_name == "Children's Service Society of Utah", "Childrens Service Society of Utah", 
  entity_name == "Citizens Against Physical and Sexual Abuse (CAPSA)", "Citizens Against Physical and Sexual Abuse",
  entity_name == "Clarkston Town Corporation", "Clarkston Town",
  entity_name == "Clearfield City Corporation", "Clearfield City",
  entity_name == "Clinton City Corp", "Clinton City",
  entity_name == "Clinton Sanitary Sewer Special Service District", "Clinton Sanitary Sewer Special Service District",
  entity_name == "Columbus Foundation, Inc.", "Columbus Foundation",
  entity_name == "Community Action Services and Food Bank, Inc.", "Community Action Services",
  entity_name == "Community Education Partnership of West Valley City, Inc.", "Community Education Partnership of West Valley City",
  entity_name == "Community Health Centers, Inc.", "Community Health Centers",
  entity_name == "Community Wireless of Park City dba as KPCW", "Community Wireless of Park City",
  # entity_name == "Community Development Renewal Agency of Santaquin City", "",
  # entity_name == "Community Development and Renewal Agency for the City of Cottonwood Heights", "",
  # entity_name == "Community Development and Renewal Agency of Herriman City", "",
  # entity_name == "Community Development and Renewal Agency of Midway City", "",
  # entity_name == "Community Development and Renewal Agency of West Point City", "",
  # entity_name == "Community Wireless of Park City dba as KPCW", "",
  entity_name == "Comunidades Unidas", "Comunidades Unidas-Communities United",
  entity_name == "Copper Canyon P.U.D. Special Service District", "Copper Canyon PUD Special Service District",
  entity_name == "Corinne Drainage Maintenance District #1", "Corinne Drainage District 1",
  entity_name == "Cornish Cemetery  District", "Cornish Cemetery Maintenance District",
  entity_name == "Cornish Town Corporation", "Cornish Town",
  entity_name == "Coral Junction Public Infrastructure District No. 2", "Coral Junction Public Infrastructure District 2",
  entity_name == "Cottonwood Improvement District (formerly Salt Lake County Cottonwood Sanitary District)", "Cottonwood Improvement District",
  entity_name == "Crescent Cemetery", "Crescent Cemetery Maintenance District",
  entity_name == "DDI VANTAGE, Inc.", "DDI Vantage",
  entity_name == "Daggett County Service Area No. 1", "Daggett County Service Area 1",
  entity_name == "Davis Behavioral Health, Inc", "Davis Behavioral Health",
  entity_name == "Davis Conservation District", "Davis County Conservation District",
  entity_name == "Davis Performing Arts Association Dba Centerpoint Legacy Theatre", "Davis Performing Arts Association",
  entity_name == "Deseret Peak SSD", "Deseret Peak Special Service District",
  entity_name == "Deseret Oasis Special Service District", "Deseret-Oasis Special Service District",
  entity_name == "Deweyville Town Corporation", "Deweyville Town",
  entity_name == "Discovery Gateway Children's Museum", "Discovery Gateway",
  entity_name == "Dixie Montessori Academy", "Dixie Montessori Academy",
  entity_name == "Downtown Alliance Inc.", "Downtown Alliance",
  entity_name == "Duchesne County Mosquito Abatement", "Duchesne County Mosquito Abatement District",
  entity_name == "Duchesne County School District", "Duchesne School District",
  entity_name == "Duchesne Conservation District", "Duchesne County Conservation District",
  entity_name == "Duchesne County Special Service District #2", "Duchesne County Special Service District 2",
  entity_name == "Duchesne County Special Service District #3", "Duchesne County Special Service District 3",
  entity_name == "Dutch John", "Dutch John Town",
  entity_name == "Eagle Condor Humanitarian/Foundation", "Eagle Condor Humanitarian",
  entity_name == "East Lewiston Drainage District #4", "East Lewiston Drainage District 4",
  entity_name == "Eastern Utah Early Intervention, Inc.", "Eastern Utah Early Intervention",
  entity_name == "EDU-Care DBA Adventures In Learning", "Educare",
  entity_name == "Elk Ridge", "Elk Ridge City",
  entity_name == "Elwood Town RDA", "Elwood Town Redevelopment Agency",
  entity_name == "Egyptian Theatre Foundation, Inc.", "Park City Performances - Egyptian Theater", 
  entity_name == "Emery County School District", "Emery School District",
  entity_name == "Emery County Special Service District #1", "Emery County Special Service District 1",
  entity_name == "Endeavor Hall Charter School", "Endeavor Hall",
  entity_name == "English Language Center of Cache Valley, Inc", "English Language Center of Cache Valley",
  entity_name == "Ensign Peak Services, Inc", "Ensign Peak Services",
  entity_name == "Enterprise Valley Medical Clinic Inc.", "Enterprise Valley Medical Clinic",
  entity_name == "Enterprise & Iron Conservation District", "Enterprise and Iron Conservation District",
  entity_name == "Entheos Academy Magna", "Entheos Academy",
  entity_name == "Entheos", "Entheos Academy",
  entity_name == "Entrada Institute, Inc.", "Entrada Institute",
  entity_name == "Eureka City Corporation", "Eureka City",
  entity_name == "Excelsior Academy Charter School", "Excelsior Academy",
  entity_name == "Family Promise Salt Lake", "Family Promise - Salt Lake",
  entity_name == "Family Support Center of Washington County Utah, Inc.", "Family Support Center of Washington County",
  entity_name == "Family Support Center of the Uintah Basin, Inc.", "Family Support Center of Uintah Basin",
  entity_name == "Farmington City Corp", "Farmington City",
  entity_name == "Feyette Town", "Fayette Town",
  entity_name == "Fielding Cemetery maintenance district", "Fielding Cemetery Maintenance District",
  entity_name == "Fielding Town Corp", "Fielding Town",
  entity_name == "Fillmore City Corporation", "Fillmore City",
  # entity_name == "First Step House", "First Step House, Inc.",
  entity_name == "Flaming Gorge Fire and EMS District", "Flaming Gorge Fire and Emergency Medical Services District",
  entity_name == "Four Corners Community Behavioral Health, Inc.", "Four Corners Community Behavioral Health",
  entity_name == "Foundation for the Provo/Jordan River Parkway", "Foundation for the Provo-Jordan River Parkway",
  entity_name == "Franklin Discovery Academy - Vineyard", "Franklin Discovery Academy",
  entity_name == "Friends of Switchpoint Inc.", "Friends of Switchpoint",
  entity_name == "Friends of Territorial Statehouse State Park & Museum", "Friends of Territorial Statehouse Museum",
  entity_name == "Futures Through Choices, Inc", "Futures Through Choices",
  entity_name == "Garden City Cemetery District", "Garden City Cemetery Maintenance District",
  entity_name == "Garden City Fire District", "Garden City Fire Protection District",
  entity_name == "Garland Cemetery District", "Garland Cemetery Maintenance District",
  entity_name == "Gateway at Sand Hollow Public Infrastructure District No.1", "Gateway at Sand Hollow Public Infrastructure District 1",
  entity_name == "Gateway at Sand Hollow Public Infrastructure District No. 2", "Gateway at Sand Hollow Public Infrastructure District 2",
  entity_name == "Gateway at Sand Hollow Public Infrastructure District No.3", "Gateway at Sand Hollow Public Infrastructure District 3",
  entity_name == "Gateway Recreation & Wellness District", "Gateway Recreation and Wellness District",
  entity_name == "Generación Floreciente dba Esperanza Elementary School", "Esperanza Elementary Charter School",
  entity_name == "Glendale Town Corporation", "Glendale Town",
  entity_name == "Grand County Recreation Special Service District No. 1", "Grand County Recreation Special Service District",
  entity_name == "Grand County School District", "Grand School District",
  entity_name == "Grand County Service Area for Castle Valley Fire Protection District", "Grand County Service Area Castle Valley Fire Protection District",
  entity_name == "Grand Water & Sewer Service Agency", "Grand Water and Sewer Service Agency",
  entity_name == "Grantsville City Corporation", "Grantsville City",
  entity_name == "Grapevine Wash Basic Local District", "Grapevine Wash Local District",
  entity_name == "Green River Community Development and Renewal Agency City", "Green River Community Development and Renewal Agency",
  entity_name == "Green River Medical Center, Inc.", "Green River Medical Center",
  entity_name == "Green Hills Water and Sewer District", "Green Hills Water and Sewer Improvement District",
  entity_name == "Guadalupe Charter School", "Guadalupe Schools",
  entity_name == "Gunnison City Corp.", "Gunnison City",
  entity_name == "Hanna Water & Sewer District", "Hanna Water and Sewer District",
  entity_name == "hansel valley watershed", "Hansel Valley Watershed District",
  entity_name == "Hawthorn Academy - South Jordan", "Hawthorn Academy",
  entity_name == "Heart & Soul", "Heart and Soul",
  entity_name == "Heber City Corporation", "Heber City",
  entity_name == "Heber Valley Tourism & Economic Devolpment", "Heber Valley Tourism and Economic Development Agency",
  entity_name == "Heber Light & Power Company", "Heber Light and Power Company",
  entity_name == "Helping Hands, Inc", "Helping Hands",
  entity_name == "Henefer Town Inc.", "Henefer Town",
  entity_name == "Heritage Schools, Inc.", "Heritage Schools",
  entity_name == "Herriman Pipe Line & Development Co", "Herriman Pipe Line and Development",
  entity_name == "Hinckley Deseret Cemetery Special Service District", "Hinckley-Deseret Cemetery Maintenance District",
  entity_name == "Hinckley Town, Inc.", "Hinckley Town",
  entity_name == "Hideout Local District No. 1", "Hideout Local District 1",
  entity_name == "Holden Town Corp", "Holden Town",
  entity_name == "Holy Cross Ministries", "Holy Cross Ministries of Utah",
  entity_name == "HOOPER WATER IMPROVEMENT DISTRICT", "Hooper Water Improvement District",
  entity_name == "House Of Hope", "House of Hope",
  entity_name == "Housing Opportunities, Inc.", "Housing Opportunities",
  entity_name == "Howell Town Corp", "Howell Town",
  entity_name == "Hyrum City Corporation", "Hyrum City",
  entity_name == "Infants Nutrition & Care Associates", "Infants Nutrition and Care Associates",
  entity_name == "Intermountain Power Agency (IPA)", "Intermountain Power Agency",
  entity_name == "Intermountain Specialized Abuse Treatment Center", "Intermountain Specialized Abuse Center",
  entity_name == "International Rescue Committee, Inc", "International Rescue Committee",
  entity_name == "Iron County School District", "Iron School District",
  entity_name == "Iron County Service Area No. 1", "Iron County Service Area 1",
  entity_name == "Iron County Unincorporated Area Services District No. 2", "Iron County Unincorporated Area Services District 2",
  entity_name == "Iron County Special Service District No. 3", "Iron County Special Service District 3",
  entity_name == "IHC Health Services, Inc.", "IHC Health Services",
  entity_name == "Indian Training Education Center", "Indian Training and Education Center",
  entity_name == "Impact Mental Health Dba Polizzi Foundation", "Impact Mental Health",
  entity_name == "Jepson Canyon Public Infrastructure District No. 1", "Jepson Canyon Public Infrastructure District 1",
  entity_name == "Jepson Canyon Public Infrastructure District No. 2", "Jepson Canyon Public Infrastructure District 2",
  entity_name == "Jepson Canyon Public Infrastructure District No. 3", "Jepson Canyon Public Infrastructure District 3",
  entity_name == "Jordanelle Ridge Public Infrastructure District No. 2", "Jordanelle Ridge Public Infrastructure District 2",
  entity_name == "Juab County Special Service District #2", "Juab County Special Service District 2",
  entity_name == "Juab Special Service Fire District", "Juab County Special Service Fire Protection District",
  entity_name == "Kanarraville Town Corporation", "Kanarraville Town",
  entity_name == "Kane County School District", "Kane School District",
  entity_name == "Kane County Recreation and Transportation Special Serivce District", "Kane County Recreation and Transportation Special Service District",
  entity_name == "Kanosh Town Corporation", "Kanosh Town",
  entity_name == "Kids On The Move, Inc", "Kids On The Move",
  entity_name == "Kimball Area Transportation Special Service District (Towncenter Commercial District)", "Kimball Area Special Service District",
  entity_name == "Kostopulos Dream Foundation / Camp Kostopulos", "Kostopulos Dream Foundation",
  entity_name == "LAKE POINT IMPROVEMENT DISTRICT", "Lake Point Improvement District",
  entity_name == "Lake Point Cemetery and Park District", "Lake Point Cemetery and Parks Service Area",
  entity_name == "Laketown", "Laketown Town",
  entity_name == "Laketown Cemetery District", "Laketown Cemetery Maintenance District",
  entity_name == "Lakeview Academy of Arts, Science and Technology", "Lakeview Academy of Science Arts and Technology",
  entity_name == "Law-Related Education Project, Inc.", "Law-Related Education Project",
  entity_name == "Leadership Learning Academy - Ogden", "Leadership Learning Academy",
  entity_name == "Lewiston City Corporation", "Lewiston City",
  entity_name == "Little Lambs Foundation For Kids Inc.", "Little Lambs Foundation for Kids",
  entity_name == "Listener's Community Radio of Utah, Inc.", "Listeners Community Radio of Utah",
  entity_name == "Logan City School District", "Logan School District",
  entity_name == "Logan, Utah City", "Logan City",
  entity_name == "MOSAIC Inter-Faith Ministries dba Lutheran Social Service of UT", "MOSAIC Inter-Faith Ministries",
  entity_name == "Mammoth Creek Special Service District for Fire Protection", "Mammoth Creek Fire District",
  entity_name == "Mana Academy", "Mana Academy",
  entity_name == "Manti City Corporation", "Manti City",
  entity_name == "Main Street Southeastern Utah, Inc.","Main Street Southeastern Utah",
  entity_name == "Mapleton City Corporation", "Mapleton City",
  entity_name == "Meadow Town Corporation", "Meadow Town",
  entity_name == "Medical School Campus PID", "Medical School Campus Public Infrastructure District",
  entity_name == "Metropolitan Water District of Salt Lake & Sandy", "Metropolitan Water District of Salt Lake and Sandy",
  entity_name == "Metropolitan Water District of American Fork", "American Fork Metropolitan Water District",
  entity_name == "Metropolitan Water District of Pleasant Grove", "Pleasant Grove Metropolitan Water District",
  entity_name == "Metropolitan Water District of Provo", "Provo Metropolitan Water District",
  entity_name == "Midvale City Corporation", "Midvale City",
  entity_name == "Milford Area Healthcare Service District #3", "Milford Area Health Care Service District 3",
  entity_name == "Millard  Conservation District", "Millard Conservation District",
  entity_name == "Millard County Drainage District #2", "Millard County Drainage District 2",
  entity_name == "Millard County Drainage District #3", "Millard County Drainage District 3",
  entity_name == "Millard County Drainage District #4", "Millard County Drainage District 4",
  entity_name == "Millard County Drainage District Number One", "Millard County Drainage District 1",
  entity_name == "Millard County Fire Service District", "Millard County Fire District",
  entity_name == "Millard County Special Service District #8", "Millard County Special Service District 8",
  entity_name == "Millard County Water Conservatory District", "Millard County Water Conservancy District",
  entity_name == "Millcreek", "Millcreek City",
  entity_name == "Midtown Community Health Center Inc", "Midtown Community Health Center",
  entity_name == "Mount Ogden Public Infrastructure District No.1", "Mount Ogden Public Infrastructure District 1",
  entity_name == "Mount Ogden Public Infrastructure District No.2", "Mount Ogden Public Infrastructure District 2",
  entity_name == "Mount Ogden Public Infrastructure District No.3", "Mount Ogden Public Infrastructure District 3",
  entity_name == "Mountainland Head Start, Inc.", "Mountainland Head Start",
  entity_name == "Mountainland Dept. of Aging and Family Services", "Mountainland Department of Aging and Family Services",
  entity_name == "Mba #1 North Sevier Community Center/Salina Fire Department", "Municipal Building Authority 1 North Sevier Community Center - Salina Fire Department",
  entity_name == "Mt Nebo Water Agency", "Mount Nebo Water Agency",
  entity_name == "Myton City Corporation", "Myton City",
  entity_name == "Myton City Housing Authority dba Housing Authority of the Uintah Basin", "Myton City Housing Authority",
  entity_name == "Naples 1500 South Redevelopment", "Naples 1500 South Redevelopment Agency",
  entity_name == "National Ability Center", "National Abilities Center",
  entity_name == "National Alliance on Mental Illness-Utah (Nami-Utah)", "Utah Alliance for the Mentally Ill",
  entity_name == "Nebo Consolidated School District", "Nebo School District",
  entity_name == "Neola Community Park", "Neola Community Park District",
  entity_name == "Neola Water & Sewer District", "Neola Water and Sewer District",
  entity_name == "New Hope Crisis Center of Box Elder County", "New Hope Crisis Center",
  entity_name == "New Smiles For Veterans", "New Smiles for Veterans",
  entity_name == "Newton Cemetery District", "Newton Cemetery Maintenance District",
  entity_name == "Nibley Community Reinvestment Agency", "Nibley City Community Reinvestment Agency",
  entity_name == "North Cache Soil Conservation District", "North Cache Conservation District",
  entity_name == "North Central Fire Special Service District Administrative Control Board", "North Central Fire Special Service District",
  entity_name == "North Logan City Corp.", "North Logan City",
  entity_name == "North Summit Fire District", "North Summit Fire Protection District",
  entity_name == "North Salt Lake", "North Salt Lake City",
  entity_name == "NORTH EMERY WATER USERS SSD", "North Emery Water Users Special Service District",
  entity_name == "Northeastern Utah Educational Services (NUES)", "Northeastern Utah Educational Services",
  entity_name == "Ogden City Corporation", "Ogden City",
  entity_name == "Ogden Friends of Acoustic Music (OFOAM)", "Ogden Friends of Acoustic Music",
  entity_name == "Ogden Preparatory Academy Inc.", "Ogden Preparatory Academy",
  entity_name == "Ogden Valley Transmission / Recreational Special Service District", "Ogden Valley Transmission Recreation Special Service District",
  entity_name == "Ogden/Weber Convention & Visitors Bureau dba Visit Ogden", "Ogden-Weber Convention Visitors Bureau",
  entity_name == "Ogden-Weber Community Action Partnership, Inc.", "Ogden-Weber Community Action Partnership",
  entity_name == "Orangeville City or  Orangeville City", "Orangeville City",
  entity_name == "Odyssey House, Inc.", "Odyssey House",
  entity_name == "Olympia Public Infrastructure District No. 1", "Olympia Public Infrastructure District 1",
  entity_name == "Olympia Public Infrastructure District No. 2", "Olympia Public Infrastructure District 2",
  entity_name == "Olympia Public Infrastructure District No. 3", "Olympia Public Infrastructure District 3",
  entity_name == "Olympia Public Infrastructure District No. 4", "Olympia Public Infrastructure District 4",
  entity_name == "Olympia Public Infrastructure District No. 5", "Olympia Public Infrastructure District 5",
  entity_name == "Olympia Public Infrastructure District No. 6", "Olympia Public Infrastructure District 6",
  entity_name == "Olympia Public Infrastructure District No. 7", "Olympia Public Infrastructure District 7",
  entity_name == "Orem Child Care Nutrition Program, Inc.", "Orem Child Care Nutrition Program",
  entity_name == "Owl's Nest Special Service District", "Owls Nest Special Service District",
  entity_name == "P3+", "P3 Plus",
  entity_name == "Paradigm High School dba Paradigm Schools", "Paradigm High School",
  entity_name == "Paradise Town Corp.", "Paradise Town",
  entity_name == "Park City Municipal Corporation", "Park City",
  entity_name == "Park City Tots, Inc.", "Park City Tots",
  entity_name == "Parowan City Corporation", "Parowan City",
  entity_name == "Payson City Corporation", "Payson City",
  entity_name == "Peace House, Inc.", "Peace House",
  entity_name == "Penrose Cemetery District", "Penrose Cemetery Maintenance District",
  entity_name == "Peoples Health Clinic, Inc.", "Peoples Health Clinic",
  entity_name == "Perry City Corporation", "Perry City",
  entity_name == "Perry City Redevelopment Agency", "Perry City RDA",
  entity_name == "Phoenix Services Corporation", "Phoenix Services",
  entity_name == "Pine View Public Infrastructure District No. 1", "Pine View Public Infrastructure District 1",
  entity_name == "Pine View Public Infrastructure District No. 2", "Pine View Public Infrastructure District 2",
  entity_name == "Pine View Public Infrastructure District No. 3", "Pine View Public Infrastructure District 3",
  entity_name == "Pine View Public Infrastructure District No. 4", "Pine View Public Infrastructure District 4",
  entity_name == "Pine View Public Infrastructure District No. 5", "Pine View Public Infrastructure District 5",
  entity_name == "Pine View Public Infrastructure District No. 6", "Pine View Public Infrastructure District 6",
  entity_name == "Piute Conservation District", "Piute County Conservation District",
  entity_name == "Piute County Special Service District #1", "Piute County Special Service District 1",
  entity_name == "Plain City Cemetery District", "Plain City Cemetery Maintenance District",
  entity_name == "Pleasant Grove Redevelopment Agency", "Pleasant Grove City Redevelopment Agency",
  entity_name == "Plymouth Town Corporation", "Plymouth Town",
  entity_name == "Point of the Mountain State Land Authority Board", "Point of the Mountain State Land Authority",
  entity_name == "Prevent Child Abuse Utah, Inc.", "Prevent Child Abuse Utah",
  entity_name == "Preservation Utah (Dba for Utah Heritage Foundation)", "Preservation Utah",
  entity_name == "Price City Urban Renewal and Community Development Agency", "Price City Redevelopment Agency",
  entity_name == "Price Municipal Corporation", "Price City",
  entity_name == "Price River Watershed Conservation District", "Price River Conservation District",
  entity_name == "Promontory Commerce Center Public Infrastructure District", "Promontory Commerce Center Public Infrastructure District 1",
  entity_name == "Provo City Corporation", "Provo City",
  entity_name == "Public Service Commission of Utah", "Public Service Commission",
  entity_name == "RISE, Inc.", "RISE",
  entity_name == "ROAM Public Infrastructure District No. 1", "Roam Public Infrastructure District 1",
  entity_name == "ROAM Public Infrastructure District No. 2", "Roam Public Infrastructure District 2",
  entity_name == "Red Bridge Public Infrastructure District No. 1", "Red Bridge Public Infrastructure District 1",
  entity_name == "Red Bridge Public Infrastructure District No. 2", "Red Bridge Public Infrastructure District 2",
  entity_name == "Red Bridge Public Infrastructure District No.3", "Red Bridge Public Infrastructure District 3",
  entity_name == "Red Rock Center For Independence", "Red Rock Center for Independence",
  entity_name == "Renaissance Academy Charter School", "Renaissance Academy",
  entity_name == "R.I.T.E.S., Inc.", "RITES",
  entity_name == "Riverdale City Corp.", "Riverdale City",
  entity_name == "Rockville/Springdale Fire Protection District", "Rockville Springdale Fire Protection District",
  entity_name == "Roosevelt City Corporation", "Roosevelt City",
  entity_name == "Roosevelt City Housing Authority", "Roosevelt Housing Authority",
  entity_name == "Rural Housing Development Corporation dba Self-Help Homes", "Rural Housing Development Corporation",
  entity_name == "SANTAQUIN SPECIAL SERVICE DISTRICT", "Santaquin Special Service District For Road Maintenance",
  entity_name == "SLC Bike Share DBA GREENbike", "SLC Bike Share",
  entity_name == "SUCCESS Academy", "Success Academy",
  entity_name == "Safe Harbor Crisis Center", "Safe Harbor",
  entity_name == "Saint George Academy", "Saint George Academy",
  entity_name == "Salt Lake County Zoo Arts & Parks", "Salt Lake County Zoo Arts and Parks",
  entity_name == "Salt Lake Art Center dba Utah Museum of Contemporary Art", "Utah Museum of Contemporary Art",
  entity_name == "Salt Lake City Corporation", "Salt Lake City",
  entity_name == "Salt Lake County NMTC, Inc.", "Salt Lake County NMTC",
  entity_name == "Salt Lake County Service Area #3", "Salt Lake County Service Area 3",
  entity_name == "Salt Lake Legal Defender Assoc.", "Salt Lake Legal Defender Association",
  entity_name == "Salt Lake Neighborhood Housing Services Inc.", "Salt Lake Neighborhood Housing Services",
  entity_name == "SALT LAKE VALLEY HABITAT FOR H", "Salt Lake Valley Habitat for Humanity",
  entity_name == "San Juan County Conservation District", "San Juan Conservation District",
  entity_name == "San Juan County School District", "San Juan School District",
  entity_name == "San Juan County Transportation Service District", "San Juan Transportation District",
  entity_name == "San Juan Health Service District", "San Juan Health Care Services District",
  entity_name == "San Juan Mental Health-Substance Abuse SSD", "San Juan Mental Health Substance Abuse Special Service District",
  entity_name == "Sand Hollow Mesa Public Infrastructure District No. 1", "Sand Hollow Mesa Public Infrastructure District 1",
  entity_name == "Sand Hollow Mesa Public Infrastructure District No. 2", "Sand Hollow Mesa Public Infrastructure District 2",
  entity_name == "Sand Hollow Mesa Public Infrastructure District No. 3", "Sand Hollow Mesa Public Infrastructure District 3",
  entity_name == "Sanpete Conservation District", "Sanpete County Conservation District",
  entity_name == "Sanpete County Special Service District No. 1", "Sanpete County Special Service District 1",
  entity_name == "Sanpete County Special Service District No. 2 (Fire District)", "Sanpete County Fire Special Service District",
  entity_name == "Sanpete County Federal Mineral Leasing District #3", "Sanpete County Federal Mineral Lease Special Service District",
  entity_name == "Sanpete Sanitary Landfill Cooperative", "Sanpete Sanitary Landfill COOP",
  entity_name == "Sanpete Water Conservancy District", "Sanpete County Water Conservancy District",
  entity_name == "Santaquin City Incorporated", "Santaquin City",
  entity_name == "Saratoga Springs City", "Saratoga Springs",
  entity_name == "School and Institutional Trust Funds Office", "School and Institutional Trust Fund Office",
  entity_name == "Seekhaven, Inc.", "Seekhaven Family Crisis Center",
  entity_name == "Sego Lily Center for the Abused Deaf (SLCAD)", "Sego Lily Center for the Abused Deaf",
  entity_name == "Sevier Conservation District", "Sevier County Conservation District",
  entity_name == "Sevier County Drainage District NO 1", "Sevier County Drainage District 1",
  entity_name == "Sevier County Special Service District #1", "Sevier County Special Service District 1",
  entity_name == "Sevier Special Service District 2", "Sevier County Special Service District 2",
  entity_name == "Seekhaven, Inc.", "Seekhaven Family Crisis Center",
  entity_name == "Sienna Hills Public Infrastructure District No. 1", "Sienna Hills Public Infrastructure District 1",
  entity_name == "SILVER REEF FOUNDATION, INC.", "Silver Reef Foundation",
  entity_name == "Smithfield City Redevelopment Agency (RDA)", "Smithfield City Redevelopment Agency",
  entity_name == "Snowville Town, Inc.", "Snowville Town",
  entity_name == "Solid Waste Special Service District #1", "Solid Waste Special Service District 1",
  entity_name == "South Utah Valley Animal Special Service District", "South Utah Valley Animal Services Special Service District",
  entity_name == "SOUTH VALLEY TRAINING CO INC.", "South Valley Training Company",
  entity_name == "Southeast Education Service Center", "Southeast Educational Service Center",
  entity_name == "Southeastern Utah Association of Local Governments", "Southeastern Utah Association of Governments",
  entity_name == "Southern Utah Valley Power Systems", "Southern Utah Valley Power",
  entity_name == "Southwest Mosquito Abatement & Control District", "Southwest Mosquito Abatement District",
  entity_name == "Southwest Utah Community Health Center dba Family Healthcare", "Southwest Utah Community Health Center",
  entity_name == "Southwest Utah Public Health Department Foundation", "Southwest Utah Public Health Department",
  entity_name == "Southwestern Special Service District", "Southwestern Special Service District Washington County",
  entity_name == "Special Service Lighting District", "Special Service Lighting District of the City of Orem",
  entity_name == "Spectrum Academy Charter School", "Spectrum Academy",
  entity_name == "Spring City Municipal Corporation", "Spring City",
  entity_name == "Springville Museum of Art Association", "Springville Museum of Art",
  entity_name == "Spring City Planning & Zoning Commission", "Spring City Planning and Zoning Commission",
  entity_name == "St. George City", "Saint George City",
  entity_name == "St. George Housing Authority", "Saint George Housing Authority",
  entity_name == "Summit Academy, Incorporated", "Summit Academy",
  entity_name == "Summit Academy Schools", "Summit Academy High School",
  entity_name == "Summit Conservation District", "Summit County Conservation District",
  entity_name == "Summit County Service Area #3", "Summit County Service Area 3",
  entity_name == "Summit County Service Area #5 (Lake Rockport Estates)", "Summit County Service Area 5",
  entity_name == "Summit County Service Area #6", "Summit County Service Area 6",
  entity_name == "Summit County Service Area #8 - (Chalk Creek)", "Summit County Service Area 8",
  entity_name == "Summit County Special Service District #1 - (Mineral Lease - Roads)", "Summit Special Service District",
  entity_name == "Summit County Wildland Fire Service", "Summit County Wildland Fire Service Area",
  entity_name == "Summit County Health Department", "Summit City-County Health Department",
  entity_name == "Summit County Serv. Area 3", "Summit County Service Area 3",
  entity_name == "Summit Culinary Water Users Company, Inc", "Summit Culinary Water Users Company",
  entity_name == "Sunset City Corporation", "Sunset City",
  entity_name == "Syracuse Arts Academy - North", "Syracuse Arts Academy",
  entity_name == "Syracuse City Corporation", "Syracuse City",
  entity_name == "Taylor West Weber Water Improvement District", "Taylor-West Weber Water Improvement District",
  entity_name == "THE CITY OF WASHINGTON TERRACE", "Washington Terrace",
  entity_name == "Thanksgiving Point Institute, Inc.", "Thanksgiving Point Institute",
  entity_name == "TIMBERLINE SPECIAL SERVICE DISTRICT", "Timberline Special Service District",
  entity_name == "TOWN OF JOSEPH", "Joseph Town",
  entity_name == "TOWN OF MANILA", "Manila Town",
  entity_name == "TOWN OF PORTAGE", "Portage Town",
  entity_name == "TOWN OF STERLING", "Sterling Town",
  entity_name == "TRSSD- Traverse Ridge Special Service District", "Traverse Ridge Special Service District",
  # entity_name == "TWIN CREEKS SPECIAL SERVICE DISTRICT", "",
  entity_name == "The  Huntington City", "Huntington City",
  entity_name == "The  Lehi City", "Lehi City",
  entity_name == "The  South Salt Lake City", "South Salt Lake City",
  entity_name == "The Grand Theatre Foundation, Inc.", "The Grand Theatre Foundation",
  entity_name == "The  South Salt Lake Arts Board, Inc. City", "The South Salt Lake Arts Board",
  entity_name == "Helping Hand Association dba The Haven", "The Haven",
  entity_name == "The Northern Utah Academy for Math, Engineering and Science (NUAMES)", "Northern Utah Academy for Math Engineering and Science",
  entity_name == "The Queen Center Inc.", "The Queen Center",
  entity_name == "The Children's Center", "The Childrens Center",
  entity_name == "The Terra Academy, Inc.", "The Terra Academy",
  entity_name == "Thomas Edison Charter School (South Campus)", "Thomas Edison Charter School",
  entity_name == "Thompson Springs Special Service Fire District", "Thompson Fire Protection District",
  entity_name == "Thompson Springs Special Service Water District", "Thompson Special Service District",
  entity_name == "Timber Lakes Water Special Service District", "Timberlakes Water Special Service District",
  entity_name == "Tooele County School District", "Tooele School District",
  entity_name == "Town of Alta", "Alta Town",
  entity_name == "Town of Ballard", "Ballard Town",
  entity_name == "Town of Bluff", "Bluff Town",
  entity_name == "Town of Brighton", "Brighton Town",
  entity_name == "Town of Castle Valley", "Castle Valley Town",
  entity_name == "Town of Daniel", "Daniel Town",
  entity_name == "Town of Fairfield", "Fairfield Town",
  entity_name == "Town of Fayette", "Feyette Town",
  entity_name == "Town of Genola", "Genola Town",
  entity_name == "Town of Hanksville", "Hanksville Town",
  entity_name == "Town of Hatch", "Hatch Town",
  entity_name == "Town of Hideout, Utah", "Hideout Town",
  entity_name == "Town of Independence", "Independence Town",
  entity_name == "Town of Koosharem", "Koosharem Town",
  entity_name == "Town of Leeds", "Leeds Town",
  entity_name == "Town of Levan", "Levan Town",
  entity_name == "Town of Mantua", "Mantua Town",
  entity_name == "Town of Oak City", "Oak City",
  entity_name == "Town of Randolph", "Randolph Town",
  entity_name == "Town of Rockville", "Rockville Town",
  entity_name == "Town of Rocky Ridge", "Rocky Ridge Town",
  entity_name == "Town of Rush Valley", "Rush Valley Town",
  entity_name == "Town of Springdale", "Springdale Town",
  entity_name == "Town of Trenton", "Trenton Town",
  entity_name == "Town of Vernon", "Vernon Town",
  entity_name == "Town of Woodruff", "Woodruff Town",
  entity_name == "Tridell Lapoint Water Improvement District", "Tridell-Lapoint Water Improvement District",
  entity_name == "Trauma Awareness and Treatment Center, Inc", "Trauma Awareness and Treatment Center",
  entity_name == "TreeUtah of Salt Lake City", "TreeUtah",
  entity_name == "Treehouse Children's Museum", "Treehouse Museum",
  entity_name == "Tremonton City Corporation", "Tremonton City",
  entity_name == "Turn Commity Services, Inc", "Turn Community Services",
  entity_name == "Uintah Basin Area Agency on Aging (Uintah Basin Association of Governments)", "Uintah Basin Association of Governments",
  entity_name == "Uintah Conservation District", "Uintah County Conservation District",
  entity_name == "Uintah County School District", "Uintah School District",
  entity_name == "Uintah Highlands Improvement District", "Uintah Highlands Water and Sewer Improvement District",
  entity_name == "Unified Police Department of Greater Salt Lake", "Unified Police Department",
  entity_name == "United Way of Central and So. Utah/DBA United Way of Utah County", "United Way of Utah County",
  entity_name == "Upper Country Water", "Upper Country Water Improvement District",
  entity_name == "Utah Afterschool Network", "Utah AfterSchool Network",
  entity_name == "Utah Afterschool Network, Inc.", "Utah AfterSchool Network",
  entity_name == "Utah Arts Festival", "Utah Arts Festival Foundation",
  entity_name == "Utah Arts and Cultural Coalition dba Utah Cultural Alliance Foundation", "Utah Arts and Cultural Coalition",
  entity_name == "Utah Athletic Foundation (DBA Utah Olympic Legacy Foundation)", "Utah Athletic Foundation",
  entity_name == "Utah Bicycle Coalition DBA Bike Utah", "Utah Bicycle Coalition",
  entity_name == "Utah Career Path High", "Utah Career Path High School",
  entity_name == "Utah Clean Energy Alliance, Inc.", "Utah Clean Energy Alliance Inc.",
  entity_name == "Utah Clean Energy Alliance Inc.", "Utah Clean Energy Alliance",
  entity_name == "Utah Community Action Partnership Association (DBA Community Action Partnership of Utah)", "Utah Community Action Partnership Association",
  entity_name == "Utah County Government", "Utah County",
  entity_name == "Utah County Service Area #6", "Utah County Service Areas 6 7 8 and 9",
  entity_name == "Utah County Service Area #7", "Utah County Service Areas 6 7 8 and 9",
  entity_name == "Utah County Service Area #8", "Utah County Service Areas 6 7 8 and 9",
  entity_name == "Utah County Service Area #9", "Utah County Service Areas 6 7 8 and 9",
  entity_name == "Utah Dairy Commission - Utah Code Ann 4-22-103(1)", "Utah Dairy Commission",
  entity_name == "The Utah Debate Commission", "Utah Debate Commission",
  entity_name == "Utah Defense Alliance, Inc.", "Utah Defense Alliance",
  entity_name == "Utah Festival Opera Company", "Utah Festival Opera and Musical Theatre",
  entity_name == "Utah Health & Human Rights Project", "Utah Health and Human Rights",
  entity_name == "Utah Inland Port Authority", "Inland Port Authority",
  entity_name == "Utah Open Lands", "Utah Open Lands Conservation Association",
  entity_name == "Utah School & Institutional Trust Lands Administration", "School and Institutional Trust Lands Administration",
  entity_name == "Utah School Boards Risk Management Mutual Insurance Association", "Utah School Boards Risk Management Insurance Association",
  entity_name == "Utah Sports Commission Foundation", "Utah Sports Commission",
  entity_name == "Utah Building & Construction Trades Council Safety Grant", "Utah Building and Construction Trades Council Safety Grant",
  entity_name == "Utah Valley Family Support Center, Inc. dba Family Support & Treatment Center", "Utah Valley Family Support Center",
  entity_name == "Utah Clean Air Partnership, Inc UCAIR", "Utah Clean Air Partnership",
  entity_name == "Utah Legal Clinic Foundation, Inc.", "Utah Legal Clinic Foundation",
  entity_name == "Utah Operation Lifesaver, Inc.", "Utah Operation Lifesaver",
  entity_name == "Utah State University Space Dynamics Laboratory", "Utah State University Research Foundation",
  entity_name == "Utah Telecommunication Open Infrastructure Agency", "UTOPIA",
  entity_name == "Valley Academy Charter School", "Valley Academy",
  entity_name == "Vernon WaterWorks SSD", "Vernon Waterworks Special Service District",
  entity_name == "Vineyard", "Vineyard City",
  entity_name == "Volunteers Of America, Utah", "Volunteers of America Utah",
  entity_name == "WASATCH COUNTY SOLID WASTE SSD", "Wasatch County Solid Waste Special Service District",
  # entity_name == "Warren-West Warren Cemetery Maintenance District", "Warren-West Warren Cemetery Maintenance District",
  entity_name == "Wasatch Behavioral Health Special Service District", "Wasatch Behavioral Health Services Special Service District",
  entity_name == "Wasatch County Fire District", "Wasatch County Fire Protection Special Service District",
  entity_name == "Wasatch County Parks & Recreation SSD #21", "Wasatch County Recreation Special Service District 21",
  entity_name == "Wasatch County Speacial Service Area # 1", "Wasatch County Service Area 1",
  entity_name == "Wasatch County Special Service District # 9 (Mineral Lease)", "Wasatch County Special Service District 9",
  entity_name == "Wasatch County School District", "Wasatch School District",
  entity_name == "Wasatch County Housing Authority", "Wasatch Housing Authority",
  entity_name == "Wasatch Front Regional Council / Wasatch Front Economic Development District", "Wasatch Front Regional Council",
  entity_name == "Wasatch Front Waste & Recycling District", "Wasatch Front Waste and Recycling District",
  entity_name == "Wasatch Homeless Healthcare DBA Fourth Street Clinic", "Wasatch Homeless Health Care",
  entity_name == "Washington County St. George Interlocal Agency", "Washington County St George City Interlocal Agency",
  entity_name == "Washington Terrace City Redevelopment Agency (RDA)", "Washington Terrace City Redevelopment Agency",
  entity_name == "Washington County Special Service District No. 1", "Washington County Special Service District 1",
  entity_name == "Washington Terrace", "Washington Terrace City",
  entity_name == "Wayne County School District", "Wayne School District",
  entity_name == "Wayne County Special Service District #1", "Wayne County Special Service District 1",
  entity_name == "Wayne County Special Service District #3", "Wayne County Special Service District 3",
  entity_name == "Weber Area Dispatch 911 & Emergency Services District", "Weber Area Dispatch 911 and Emergency Services District",
  entity_name == "Weber County Housing Authority", "Weber Housing Authority",
  entity_name == "Weber County Service Area #6", "Weber County Service Area 6",
  entity_name == "Weber Morgan Narcotics Strike Force", "Weber Morgan Strike Force",
  entity_name == "Weber Mosquito Abatement District", "Weber County Mosquito Abatement District",
  entity_name == "Weber Soil Conservation District", "Weber County Conservation District",
  entity_name == "Weber-Morgan Health Department", "Weber Morgan District Health Department",
  entity_name == "Weber Area Dispatch 911 & Emergency Services Local Building Authority", "Weber Area Dispatch 911 and Emergency Services Local Building Authority",
  entity_name == "Wellington City Corporation", "Wellington City",
  entity_name == "Wellington City Redevelopment Agency", "Wellington City Economic Development and Redevelopment Agency",
  entity_name == "Wellsville City Corp.", "Wellsville City",
  entity_name == "Wellsville Mendon Conservation District", "Wellsville-Mendon Conservation District",
  entity_name == "West Jordan Historical Society and Museum", "West Jordan Historical Society",
  entity_name == "West Warren and Warren Water Improvement District", "West Warren-Warren Water District",
  entity_name == "West Weber Taylor Cemetery District", "West Weber-Taylor Cemetery Maintenance District",
  entity_name == "Western Kane County Special Service District No. 1", "Western Kane County Special Service District 1",
  entity_name == "Willard City Corporation", "Willard City",
  entity_name == "Women Of The World", "Women of the World",
  entity_name == "Wohali Public Infrastructure District No. 1", "Wohali Public Infrastructure District 1",
  entity_name == "Woodruff Cemetery District", "Woodruff Cemetery Maintenance District",
  entity_name == "Woods Cross Redevelopment Agency", "Woods Cross City Redevelopment Agency",
  entity_name == "Young Women's Christian Association of Utah (YWCA Utah)", "Young Womens Christian Association of Utah",
  entity_name == "Youth Impact, Inc.", "Youth Impact",
  
  entity_name %>% stri_detect_regex("St\\. George"), entity_name %>% stri_replace_all_regex("St\\. George","Saint George"), 
  entity_name %>% stri_detect_regex("St George"), entity_name %>% stri_replace_all_regex("St George","Saint George"), 
  
  default = NA
  # entity_name == "", "",
  # entity_name == "", "",
  # entity_name == "", "",
  # entity_name == "", "",
  # entity_name == "", "",
  # entity_name == "", "",
  # entity_name == "", "",
  # entity_name == "", "",
  # entity_name == "", "",
  # entity_name == "", "",

  )] %>% 
  .[,`:=`(clean_entity_name = fifelse(is.na(clean_entity_name),entity_name,clean_entity_name) %>% 
            stri_replace_all_regex("\\s+"," ") %>% 
            stri_trim_both() %>% 
            stri_replace_all_regex("\\.","") %>% 
            stri_replace_all_regex(",","") %>% 
            convert_to_osa_titlecase() %>% 
            stri_trim_both())] %>% 
  .[,`:=`(lg_id = .I)]
  

###




# change salesforce entity 
# 
# "Delta-Sutherland-Oasis Cemetery Maint." ==> "Delta-Sutherland-Oasis Cemetery Maintenance District"
# "Flaming Gorge Fire & EMS District" ==> "Flaming Gorge Fire and EMS District"
# "Emery County Fire Protection SSD ==>" "Emery County Fire Protection Special Service District"
# 
# "Gateway at Sand Hollow PID 1" ==> "Gateway at Sand Hollow Public Infrastructure District 1"
# "Gateway at Sand Hollow PID 2" ==> "Gateway at Sand Hollow Public Infrastructure District 2"
# "Gateway at Sand Hollow PID 3" ==> "Gateway at Sand Hollow Public Infrastructure District 3"
# "Green Hills Est. Water and Sewer Imp. District" ==> "Green Hills Water and Sewer District"
# "GUNNISON VALLEY HOSPITAL" ==> "Gunnison Valley Hospital" # there are three pick the right one
# "Helping Hands, Inc." ==> "Helping Hands"
# "High County Special Improvement District" ==> "High Country Special Improvement District"
# "Hinckley-Deseret Cemetery Maint. District" ==> "Hinckley Deseret Cemetery Maintenance District"
# "Indian Training and Education Center, Inc." == > "Indian Training Education Center"
# "International Rescue Committee, Inc." ==> "International Rescue Committee"
# "Kane County Human Resource SSD (Hospital)" ==> "Kane County Human Resource Special Service District"
# "Kane County Recreation and Transportation SSD" ==> "Kane County Recreation and Transportation Special Serivce District"
# "Karl G. Maeser Preparatory Academy" ==> "Karl G Maeser Preparatory Academy"
# "Kids On The Move, Inc." ==> "Kids On The Move"
# "Kids Who Count, Inc." ==> "Kids Who Count"
# "Lakeview Academy of Science, Arts and Technology" ==> "Lakeview Academy of Science Arts and Technology"
# "Mosquito Abatement District- Davis" ==> "Mosquito Abatement District-Davis"
# "Mountain Regional Water Special Service District (Atkinson)" ==> "Mountain Regional Water Special Service District"
# "Mt. Nebo Water Agency" ==> "Mt Nebo Water Agency"
# "North Pointe Solid Waste SSD" ==> "North Pointe Solid Waste Special Service District"
# "North Salt Lake, City of" ==> "North Salt Lake"
# "Northern Utah Academy for Math, Engineering & Science" ==> "Northern Utah Academy for Math Engineering and Science"
# "Ogden Valley Transmission Recreation SSD" ==> "Ogden Valley Transmission Recreation Special Service District"
# "Park City Film Council (dba Park City Film Series)" == > "Park City Film Council"
# "Park City Tots, Inc" ==> "Park City Tots, Inc."
# "RISE, INC" ==> "RISE"
# "Spanish Valley Wtr and Sewer Improv. District" ==> "Spanish Valley Water and Sewer Improvement District"
# "Tabby Valley Parks and Recreation SSD" ==> "Tabby Valley Parks and Recreation Special Service District"
# "Taylor-West Weber Water Improvement Dist" ==> "Taylor West Weber Water Improvement District"
# "Tooele County Recreation SSD" ==> "Tooele County Recreation Special Service District"
# "Tridell-Lapoint Water Improvement District"==> "Tridell Lapoint Water Improvement District"
# "Utah Coalition Against Sexual Assault (UCASA)" ==> "Utah Coalition Against Sexual Assault"
# "Utah County Service Areas 6, 7, 8 & 9" ==> "Utah County Service Areas 6 7 8 and 9"
# "Utah Independent Living Center, Inc." ==> "Utah Independent Living Center"
# "Utah Legal Services, Inc." ==> "Utah Legal Services"
# "Utah Performing Arts Center Agency (Eccles Theatre)" ==> "Utah Performing Arts Center Agency"
# "Utah State University Research Foundation (dba Space Dynamics Laboratory)" ==> "Utah State University Space Dynamics Laboratory"
# "Utah Symphony|Utah Opera" ==> "Utah Symphony and Opera"
# "Utah Valley Family Support Center, Inc." ==> "Utah Valley Family Support Center"
# "Utah Youth Village, Inc." ==> "Utah Youth Village"
# "Volunteers of America, Utah, Inc." ==> "Volunteers of America Utah"
# "Warren-West Warren Cemetery Maint. District" ==> "Warren West Warren Cemetery Maintenance District"
# "Wasatch County Fire Protection SSD" ==> "Wasatch County Fire Protection Special Service District"
# "Wasatch County Recreation SSD 21" ==> "Wasatch County Recreation Special Service District 21"
# "Wasatch County Special Service District 9 (Mineral Lease)" ==> "Wasatch County Special Service District 9 Mineral Lease"
# "Wasatch Homeless Health Care, Inc."==>"Wasatch Homeless Healthcare"
# "Washington County St. George City Interlocal Agency" ==> "Washington County St George City Interlocal Agency"
# "Weber County Serv. Area 6 (W. Warren Park)" ==> "Weber County Service Area 6"
# "Weber-Morgan District Health Department"==> "Weber Morgan District Health Department"
# "Wellsville-Mendon Conservation District" ==> "Wellsville Mendon Conservation District"
# "West Warren-Warren Water Improv. District" ==> "West Warren and Warren Water Improvement District"
# "West Weber-Taylor Cemetery Maint. District" ==> "West Weber Taylor Cemetery District"
# "Western Kane County Spec. Serv. District 1" ==> "Western Kane County Special Service District 1"
# "Young Women's Christian Association of Utah" ==> "Young Womens Christian Association of Utah"







# entity_match_exact <- sf_account2 %>% 
#   merge(entities, 
#         by.x = "new_clean_name",
#         by.y = "clean_entity_name",
#         all.x = T,
#         all.y = T) %>% 
#   .[,.(new_clean_name, sf_id)] %>% 
#   .[,`:=`(lg_entity = is.na(sf_id) )]
  

entity_match_exact <- sf_account2 %>% 
  merge(entities, 
        by.x = "new_clean_name",
        by.y = "clean_entity_name",
        all.x = T,
        all.y = T) %>% 
  .[,.(sf_id, 
       lg_id,
       new_clean_name, 
       osa_govt_lvl = govt_lvl.x, 
       lg_govt_lvl = govt_lvl.y, 
       old_sf_name = sf_name, 
       old_lg_name = entity_name,
       primary_email, 
       primary_first_name,
       primary_last_name,
       primary_title,
       primary_phone
       )] %>% 
  .[,`:=`(new_entity_for_OSA = !is.na(sf_id),
          new_entity_for_LG = !is.na(lg_id)
          )]

















