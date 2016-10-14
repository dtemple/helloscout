# This script goes through the Synxis files and merges them
# It will try to match on phone number from a googlesheet containing booking info
# It has a separate section that spits out the projected value based on different attributes of a guest
# todo: match on ID rather than phone and get that data from Stripe instead of the googlesheet

# Loop through the csv files in the working directory
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) # imports them all as data frames

# Join them as one monster file
do.matrix <- do.call(rbind, lapply( ls(patt="ArrivalsDeparturesAug"), get) ) # creates one monster data frame from them all
zeppAug <- unique(do.matrix) # remove dupes
zepp<-distinct(zepp) # remove dupes using this instead

# keep just the good columns
zeppdata <- subset(zeppAug, ,c("Hotel_Long_Nm","Which_Dates","Guest_Nm","Cust_Phone1","Cust_Email","Cust_City","Cust_State","Cust_zip","Cust_Country","Arrival_Date","Depart_Date","Nights_Qty","Rm_Typ_Cd","Rate_Typ_Cd","CRS_Confirm_Dt","Adult_Quantity","Child_Quantity","Channel_Cd","Avg_Rate","Mkt_Seg_Leis_Cd","Marketing_Options"))

# open googlesheets library
library(googlesheets)
zeppBookings<-gs_url("https://docs.google.com/spreadsheets/d/1lo1y5_d_be1Ki2WeqWcqjlq7Fxk2mV6446Gl4ufpID4/edit#gid=0")
# find the right sheet
zeppBookings<-gs_read(zeppBookings) # read the sheet into a data frame

# function to remove punctuation and whitespace from phone #s
phone_it_in <- function(phone, invalid = NA)
{
  phone <- gsub("[[:punct:]]", "", phone)          # remove punctuation
  phone <- trimws(phone)                           # remove whitespace
  phone
}

# Use the function to clean up phones in zeppdata and bookings
zeppdata$cleanphone<-lapply(zeppdata$Phone,phone_it_in)
zeppBookings$cleanphone<-lapply(zeppBookings$Phone,phone_it_in)

# convert to data frame
zeppBookings <- as.data.frame(lapply(zeppBookings, function(X) unname(unlist(X))))
zeppdata <- as.data.frame(lapply(zeppdata, function(X) unname(unlist(X))))

# join on phone
phone<-merge(zeppBookings,zeppdata,by="cleanphone")

# Create a Googlesheet object to write the results
zepp_ss<-gs_key('1Ok1id7tRSk0zLL7ImNgvJGcp8ZcaZcmBNniQlWXdaMI')

# A table grouped by night, with the total count, the count of bookings, sum of bookings, and value per guest
n<-zepp %>%
  group_by(Nights_Qty) %>%
  summarize(total.count=n(),
            count.bookings=sum(!is.na(Amount)),
            sum.bookings=sum(Amount,na.rm=TRUE),
            value.guest=sum.bookings/total.count)
View(n)

# Same for country
n<-zepp %>%
  group_by(Country) %>%
  summarize(total.count=n(),
            count.bookings=sum(!is.na(Amount)),
            sum.bookings=sum(Amount,na.rm=TRUE),
            value.guest=sum.bookings/total.count)

# Write data to a new tab
gs_ws_new(zepp_ss,ws_title = "country",input=n)

# Adult Quantity
n<-zepp %>%
  group_by(Adult_Quantity) %>%
  summarize(total.count=n(),
            count.bookings=sum(!is.na(Amount)),
            sum.bookings=sum(Amount,na.rm=TRUE),
            value.guest=sum.bookings/total.count)

# Child Quantity
n<-zepp %>%
  group_by(Child_Quantity) %>%
  summarize(total.count=n(),
            count.bookings=sum(!is.na(Amount)),
            sum.bookings=sum(Amount,na.rm=TRUE),
            value.guest=sum.bookings/total.count)

# Correlation
x<-cor(n,use="pairwise")
gs_edit_cells(zepp_ss,ws = 4,input=x,anchor="A10") # Add to Googlesheet