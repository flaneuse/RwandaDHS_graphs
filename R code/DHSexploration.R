# Exploration of the 2010 DHS Dataset, in prep for Needs Assessment -------
# Laura Hughes, lhughes@usaid.gov; 16 February 2016

# Load pkgs
library(llamar)
loadPkgs()



# Load data ---------------------------------------------------------------
births = read_dta('~/Documents/USAID/Rwanda/data in/RW_2010_DHS/RW_DHS_births_2010/RWBR61FL.DTA')

child = read_dta('~/Documents/USAID/Rwanda/data in/RW_2010_DHS/RW_DHS_child_2010/RWKR61FL.DTA')

couples = read_dta('~/Documents/USAID/Rwanda/data in/RW_2010_DHS/RW_DHS_couples_2010/RWCR61FL.DTA')

hh = read_dta('~/Documents/USAID/Rwanda/data in/RW_2010_DHS/RW_DHS_hh_2010/RWHR61FL.DTA')

hhMem = read_dta('~/Documents/USAID/Rwanda/data in/RW_2010_DHS/RW_DHS_hhMembers_2010/RWPR61FL.DTA')

indiv = read_dta('~/Documents/USAID/Rwanda/data in/RW_2010_DHS/RW_DHS_indiv_2010/RWIR61FL.DTA')

men = read_dta('~/Documents/USAID/Rwanda/data in/RW_2010_DHS/RW_DHS_mens_2010/RWMR61FL.DTA')



geo = read.dbf('~/Documents/USAID/Rwanda/data in/RW_2010_DHS/RW_DHS_GPS_2010/RWGE61FL.dbf')
