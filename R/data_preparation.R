# all the data preparation steps
# Import raw data --------------------------
raw_data_surveys <- function(city) {
  stopifnot(city %in% c("Sydney", "Chicago"))
  read_csv(path("Data", "Raw", city, "surveys.csv")) %>%
    rename(id.survey = `_id`)
}

raw_data_people <- function(city) {
  stopifnot(city %in% c("Sydney", "Chicago"))
  read_csv(path("Data", "Raw", city, "people.csv")) %>%
    rename(id.person = `_id`) %>%
    rename(id.survey = survey)
}

raw_data_education <- function(city) {
  stopifnot(city %in% c("Sydney", "Chicago"))
  read_csv(path("Data", "Raw", city, "educations.csv")) %>%
    rename(id.education = `_id`) %>%
    rename(id.survey = survey) %>%
    rename(id.person = person)
}

raw_data_employment <- function(city) {
  stopifnot(city %in% c("Sydney", "Chicago"))
  read_csv(path("Data", "Raw", city, "employments.csv")) %>%
    rename(id.employment = `_id`) %>%
    rename(id.survey = survey) %>%
    rename(id.person = person)
}

raw_data_location <- function(city) {
  stopifnot(city %in% c("Sydney", "Chicago"))
  read_csv(path("Data", "Raw", city, "locations.csv")) %>%
    rename(id.location = `_id`) %>%
    rename(id.survey = survey) %>%
    rename(id.master = master)
}

raw_data_vehicle <- function(city) {
  stopifnot(city %in% c("Sydney", "Chicago"))
  read_csv(path("Data", "Raw", city, "vehicles.csv")) %>%
    rename(id.vehicle = `_id`) %>%
    rename(id.survey = survey) %>%
    rename(id.person = person)
}

# Find submitted rows --------------------------
submitted_surveys <- function(surveys_raw) {
  surveys_raw %>%
    filter(submit == TRUE) %>%
    select(id.survey)
}

submitted_rows <- function(dataset, surveys_submitted) {
  surveys_submitted %>%
    inner_join(dataset, by = c("id.survey" = "id.survey"))
}

find_household_members <- function(location) {
  member_counts_max <-
    max(location %>%
      select(id.location, id.survey, id.master, members) %>%
      group_by(id.location) %>%
      # find the maximum number of members in the dataset
      mutate(member_counts = ifelse(grepl("},{", members, fixed = T) == T, str_count(members, pattern = ","), 0)) %>%
      ungroup() %>%
      select(member_counts))

  people_location <-
    location %>%
    separate(members, into = paste("member", seq(1:(member_counts_max + 1)), sep = ""), ",") %>%
    # modifying the member column strings to have the member id format
    mutate_at(vars(contains("member")), ~ str_remove_all(., "[}{\"$:\\[\\]]")) %>% # remove unneeded characters
    mutate_at(vars(contains("member")), ~ str_sub(., 4, 27)) %>% # remove the first three characters
    mutate_at(vars(contains("member")), ~ str_c("ObjectId(", .)) %>% # add ObjectId( to the begining of the id
    mutate_at(vars(contains("member")), ~ str_c(., ")")) %>% # ) to the end
    mutate_at(vars(contains("member")), ~ ifelse(. == "ObjectId()", NA, .)) %>% # remove non-ids
    # find the number of members in a household
    group_by(id.location) %>%
    mutate(numMembers = sum(!is.na(c_across(contains("member"))))) %>%
    ungroup()

  # create an empty table
  member_location <- data.frame(id.survey = character(0), id.location = character(0), id.master = character(0), id.person = character(0))

  for (i in (1:nrow(people_location))) {
    for (j in (1:as.integer(people_location[i, "numMembers"]))) {
      newrow <- data.frame(
        people_location[i, "id.survey"], people_location[i, "id.location"],
        people_location[i, "id.master"], people_location[i, paste("member", j, sep = "")]
      )
      names(newrow) <- c("id.survey", "id.location", "id.master", "id.person")
      member_location <- rbind(member_location, newrow)
    }
  }

  return(member_location)
}

var_preparation <- function(surveys, people, education, employment, location, vehicle, hh_members, city) {
  # age-check variable is true, then people with age lower than 18 will be replaced with people with mean
  age_check <- T
  income_check <- T

  # to sort database, so that first the previous home comes and the current home
  location <- location %>%
    group_by(id.survey) %>%
    dplyr::arrange(dateMoveIn, .by_group = T) %>%
    ungroup()

  # duration and event dataset
  du.ev <-
    location %>%
    inner_join(surveys, by = c("id.survey" = "id.survey")) %>%
    dplyr::select(id.location, id.survey, id.master, created, dateMoveIn, dateMoveOut) %>% # , screeningEmployment,roomCount, memberCount, homeType, homeOwnership,householdChange ,householdStructureMoveIn) %>%
    mutate(NotCensored = ifelse(is.na(dateMoveOut), 0, 1)) %>% # NotCensored = event
    mutate(duration = ifelse(is.na(dateMoveOut), as.Date(created) - as.Date(dateMoveIn), as.Date(dateMoveOut) - as.Date(dateMoveIn))) %>%
    mutate(city = city)


  # Step1. adding duration and even
  data1 <- du.ev %>%
    dplyr::select(id.location, id.survey, id.master, duration, NotCensored, city) %>%
    dplyr::rename(event = NotCensored)
  rm(du.ev)
  # __________________________________________________________________

  # adding age of oledest to the people dataset ## dataset has a problem-> there are some people in the dataset that have been added to a location as a member but they are not available in the people dataset! Weird!Maybe the reason is that they have been added and then deleted!
  # max age when move in
  maxAge.loc <- hh_members %>%
    inner_join(people, by = c("id.person" = "id.person")) %>%
    dplyr::select(id.survey.x, id.location, id.master, id.person, dob) %>%
    dplyr::rename(id.survey = id.survey.x) %>%
    inner_join(location, by = c("id.location" = "id.location")) %>%
    dplyr::select(id.survey.x, id.location, id.master.x, id.person, dob, dateMoveIn, dateMoveOut) %>%
    dplyr::rename(id.survey = id.survey.x, id.master = id.master.x) %>%
    mutate(dateMoveOut = ifelse(is.na(dateMoveOut), "2020-08-15", dateMoveOut)) %>%
    mutate(age = (as.Date(dateMoveIn) - as.Date(dob)) / 365.25) %>%
    mutate(ageOut = (as.Date(dateMoveOut) - as.Date(dob)) / 365.25) %>%
    group_by(id.location) %>%
    mutate(maxAge = max(age) / 10) %>%
    ungroup() %>%
    dplyr::select(id.location, maxAge) %>%
    unique()

  # Step2 adding max age when move in
  data2 <- maxAge.loc

  rm(maxAge.loc, maxAge.out, outlier_max_age, normal_max_age, normal_max_age_mean)
  # __________________________________________________________________

  # finding the age of the master in each household
  masterAge <-
    location %>%
    inner_join(people, by = c("id.master" = "id.person")) %>%
    dplyr::rename("id.survey" = "id.survey.x") %>%
    dplyr::select(id.survey, id.location, id.master, dateMoveIn, dateMoveOut, dob, gender) %>%
    mutate(masterAge = (as.Date(dateMoveIn) - as.Date(dob)) / 365.25) %>%
    mutate(masterAge = as.numeric(masterAge) / 10) %>%
    dplyr::select(id.location, masterAge, gender) %>%
    mutate(gender = ifelse(gender == "female", 1, 0)) %>%
    dplyr::rename(masterGender = gender)

  # Step3 adding master age when move in
  data3 <- masterAge

  rm(masterAge)
  # ________________________________________________________________

  # Step4 adding home type, room count and homeownership
  data4 <- location %>%
    dplyr::select(id.location, homeOwnership, homeType, roomCount) %>%
    mutate(isOwner = ifelse(homeOwnership == "owner", 1, 0)) %>%
    mutate(isRenter = ifelse(homeOwnership == "renter", 1, 0)) %>%
    mutate(isHouse = ifelse(homeType == "house", 1, 0)) %>%
    mutate(isApartment = ifelse(homeType == "apartment", 1, 0)) %>%
    mutate(isTownhouse = ifelse(homeType == "townhouse", 1, 0)) %>%
    mutate(isStudio = ifelse(roomCount == "studio", 1, 0)) %>%
    mutate(isRooml2 = ifelse(roomCount == 1 | roomCount == 2, 1, 0)) %>%
    mutate(isRooml4m2 = ifelse(roomCount == 3 | roomCount == 4, 1, 0)) %>%
    mutate(isRoomm5 = ifelse(roomCount == "5+", 1, 0)) %>%
    dplyr::select(-homeOwnership, -homeType, -roomCount)

  # ________________________________________________________________
  # Step 5 Number of jobs and income

  # in the employment table -> calculating each job's starting and final income
    employment_ <- employment %>%
      mutate(startingIncome_ = startingIncome) %>%
      mutate(finalIncome_ = finalIncome)

  # Step5-1 adding number of jobs when move in and household income when move in ##Weekly
  data51 <- location %>%
    inner_join(employment_, by = c("id.survey" = "id.survey"), suffix = c(".location", ".job")) %>%
    dplyr::select(id.location, id.survey, id.master, dateMoveIn, dateMoveOut, dateStart, dateLeft, startingIncome_, finalIncome_) %>%
    mutate(jobInStartS = ifelse(as.Date(dateStart) <= as.Date(dateMoveIn), 1, 0)) %>%
    mutate(jobInStartF = ifelse(is.na(dateLeft), 1, ifelse(as.Date(dateLeft) >= as.Date(dateMoveIn), 1, 0))) %>%
    mutate(jobInStart = ifelse((jobInStartS + jobInStartF) == 2, TRUE, FALSE)) %>% # if job has started before move in and have last after move in
    filter(jobInStart == TRUE) %>% # Keeping only jobs that are simultaneous to residing duration

    mutate(incomeInStart = ifelse(jobInStart == T, finalIncome_, 0)) %>% # Hajv!
    group_by(id.location) %>%
    mutate(numJobInStart = sum(jobInStart)) %>%
    mutate(hhIncomeInStart = sum(incomeInStart, na.rm = T) / 1000) %>%
    ungroup() %>%
    dplyr::select(id.location, numJobInStart, hhIncomeInStart) %>%
    unique() %>%
    # Adding other locations with no income
    right_join(location, by = c("id.location" = "id.location")) %>%
    dplyr::select(id.location, numJobInStart, hhIncomeInStart) %>%
    mutate(numJobInStart = ifelse(is.na(numJobInStart), 0, numJobInStart)) %>%
    mutate(hhIncomeInStart = ifelse(is.na(hhIncomeInStart), 0, hhIncomeInStart))

  # data51[is.na(data51)] <- 0  # replacing na with 0 / no need


  # Step5-2 adding nmber of jobs when move out and household income when move out ##Weekly
  data52 <- location %>%
    inner_join(employment_, by = c("id.survey" = "id.survey"), suffix = c(".location", ".job")) %>%
    dplyr::select(id.location, id.survey, id.master, dateMoveIn, dateMoveOut, dateStart, dateLeft, startingIncome_, finalIncome_) %>%
    mutate(jobInEndS = ifelse(is.na(dateMoveOut), 1, ifelse(as.Date(dateStart) <= as.Date(dateMoveOut), 1, 0))) %>%
    mutate(jobInEndF = ifelse(is.na(dateLeft), 1, ifelse(is.na(dateMoveOut), 0, ifelse(as.Date(dateLeft) >= as.Date(dateMoveOut), 1, 0)))) %>%
    mutate(jobInEnd = ifelse((jobInEndS + jobInEndF) == 2, TRUE, FALSE)) %>% # if job has started before move in and have last after move in
    filter(jobInEnd == TRUE) %>%
    mutate(incomeInEnd = ifelse(jobInEnd == 1, finalIncome_, 0)) %>%
    group_by(id.location) %>%
    mutate(numJobInEnd = sum(jobInEnd)) %>%
    mutate(hhIncomeInEnd = sum(incomeInEnd, na.rm = T) / 1000) %>%
    ungroup() %>%
    dplyr::select(id.location, numJobInEnd, hhIncomeInEnd) %>%
    unique() %>%
    # Adding other locations with no income
    right_join(location, by = c("id.location" = "id.location")) %>%
    dplyr::select(id.location, numJobInEnd, hhIncomeInEnd) %>%
    mutate(numJobInEnd = ifelse(is.na(numJobInEnd), 0, numJobInEnd)) %>%
    mutate(hhIncomeInEnd = ifelse(is.na(hhIncomeInEnd), 0, hhIncomeInEnd))

  # data52[is.na(data52)] <- 0  # replacing na with 0 / no need
  rm(income.summary, employment__)

  # ________________________________________________________________

  # Step6 adding number of under 18 and over 18 and member count in the moving out date or survey filling date.
  data6 <- location %>%
    inner_join(hh_members, by = c("id.location" = "id.location"), suffix = c(".location", ".person")) %>%
    dplyr::select(id.location, id.survey.location, id.master.location, id.person, dateMoveIn, dateMoveOut) %>%
    dplyr::rename(id.survey = id.survey.location, id.master = id.master.location) %>%
    inner_join(people, by = c("id.person" = "id.person"), suffix = c(".location", ".people")) %>%
    dplyr::rename(id.survey = id.survey.location) %>%
    dplyr::select(id.location, id.survey, id.master, id.person, dateMoveIn, dateMoveOut, dob, gender) %>%
    mutate(age = round(((as.Date(dateMoveIn) - as.Date(dob)) / 365.25), 1)) %>%
    mutate(over18_ = ifelse(age >= 18, 1, 0)) %>%
    mutate(under18_ = ifelse(age < 18, 1, 0)) %>%
    group_by(id.location) %>%
    mutate(over18 = sum(over18_)) %>%
    mutate(under18 = sum(under18_)) %>%
    dplyr::mutate(memberCount = dplyr::n()) %>%
    mutate(numMale = sum(gender == "male")) %>%
    mutate(numFemale = sum(gender == "female")) %>%
    ungroup() %>%
    dplyr::select(id.location, over18, under18, memberCount, numMale, numFemale) %>%
    unique()

  # ________________________________________________________________

  # Step7 adding number of education when move in
  data7 <- location %>%
    left_join(education, by = c("id.survey" = "id.survey"), suffix = c(".location", ".education")) %>%
    mutate(eduInStart = ifelse(as.Date(dateStart) <= as.Date(dateMoveIn), 1, 0)) %>%
    group_by(id.location) %>%
    mutate(numEduInStart = sum(eduInStart)) %>%
    ungroup() %>%
    dplyr::select(id.location, numEduInStart) %>%
    unique() %>%
    mutate(numEduInStart = ifelse(is.na(numEduInStart), 0, numEduInStart))

  # ________________________________________________________________

  # Step8 adding number of vehicles when move in
  data8 <- location %>%
    left_join(vehicle, by = c("id.survey" = "id.survey"), suffix = c(".location", ".vehicle")) %>%
    mutate(dateAbandon = ifelse(is.na(dateAbandon), "2020-6-15", dateAbandon)) %>%
    mutate(vehInStart = ifelse(as.Date(dateOwn) <= as.Date(dateMoveIn) & as.Date(dateAbandon) >= as.Date(dateMoveIn), 1, 0)) %>%
    group_by(id.location) %>%
    mutate(numVehInStart = sum(vehInStart)) %>%
    ungroup() %>%
    dplyr::select(id.location, numVehInStart) %>%
    unique() %>%
    mutate(numVehInStart = ifelse(is.na(numVehInStart), 0, numVehInStart))

  # ________________________________________________________________

  # Step9 adding household structure and change in household when move in
  data9 <-
    location %>%
    dplyr::select(id.location, householdChange, householdStructureMoveIn) %>%
    # change in the structure of the household with changing home?
    mutate(chgJustChanged = ifelse(householdChange == "justChanged", 1, 0)) %>%
    mutate(chgIndependent = ifelse(householdChange == "alone", 1, 0)) %>%
    mutate(chgOutWithPartner = ifelse(householdChange == "outFromParent", 1, 0)) %>%
    mutate(chgWithPartner = ifelse(householdChange == "withPartner", 1, 0)) %>%
    mutate(chgSeparated = ifelse(householdChange == "separated", 1, 0)) %>%
    # the structure of household when move in
    mutate(strCouple = ifelse(householdStructureMoveIn == "couple", 1, 0)) %>%
    mutate(strCoupleWch = ifelse(householdStructureMoveIn == "coupleWithChildren", 1, 0)) %>%
    mutate(strSingle = ifelse(householdStructureMoveIn == "single", 1, 0)) %>%
    mutate(strSingleParent = ifelse(householdStructureMoveIn == "singleParent", 1, 0)) %>%
    dplyr::select(-householdChange, -householdStructureMoveIn)

  # ________________________________________________________________

  # Step10 adding value of rent for renters or payment for owners "Annually"
  data10 <-
    location %>%
    dplyr::select(
      formattedAddress, id.survey, id.location, masterUsualPayment, masterUsualPaymentRate, masterUsualPaymentPeriod,
      familyUsualPayment, familyUsualPaymentRate, familyUsualPaymentPeriod, homeOwnership
    ) %>%
    mutate(masterPayment_ = ifelse(masterUsualPayment == "yes", masterUsualPaymentRate, 0)) %>%
    mutate(masterpayment = ifelse(is.na(masterUsualPaymentPeriod), 0, ifelse(masterUsualPaymentPeriod == "weekly", (masterPayment_ / 7) * 365.25,
      ifelse(masterUsualPaymentPeriod == "monthly", masterPayment_ * 12,
        masterPayment_
      )
    ))) %>%
    mutate(householdPayment_ = ifelse(familyUsualPayment == "yes", familyUsualPaymentRate, 0)) %>%
    mutate(householdPayment__ = ifelse(is.na(familyUsualPaymentRate), 0, ifelse(familyUsualPaymentPeriod == "weekly", (householdPayment_ / 7) * 365.25,
      ifelse(familyUsualPaymentPeriod == "monthly", householdPayment_ * 12,
        householdPayment_
      )
    ))) %>%
    # mutate(householdPayment = householdPayment__ + masterpayment)%>% # I think summing these two values have not been a corrent thing i did! becase we asked people how much they pay totally! so, i should change this line
    mutate(householdPayment = ifelse(familyUsualPayment == "same" | familyUsualPayment == "no", masterpayment,
      householdPayment__
    )) %>%
    mutate(masterpayment = masterpayment / 10000) %>%
    mutate(householdPayment = householdPayment / 10000) %>%
    mutate(masterpayment_rent = ifelse(homeOwnership == "owner" | homeOwnership == "other", 0, masterpayment)) %>%
    mutate(masterpayment_mortgage = ifelse(homeOwnership == "owner", masterpayment, 0)) %>%
    mutate(householdPayment_rent = ifelse(homeOwnership == "owner" | homeOwnership == "other", 0, householdPayment)) %>%
    mutate(householdPayment_mortgage = ifelse(homeOwnership == "owner", householdPayment, 0)) %>%
    dplyr::select(id.location, masterpayment_rent, masterpayment_mortgage, householdPayment_rent, householdPayment_mortgage)


  # Finding outliers in rent and replace them with the average values
  data10_rent <- data10 %>%
    filter(householdPayment_rent > 0) %>%
    # high and low rent
    mutate(masterpayment_rent = ifelse(masterpayment_rent * 10000 / 52 >= 15000 | masterpayment_rent * 10000 / 52 <= 34, NA, masterpayment_rent)) %>%
    mutate(masterpayment_rent = ifelse(is.na(masterpayment_rent),
      mean(masterpayment_rent, na.rm = T),
      masterpayment_rent
    )) %>%
    mutate(householdPayment_rent = ifelse(householdPayment_rent * 10000 / 52 >= 15000 | householdPayment_rent * 10000 / 52 <= 34, NA, householdPayment_rent)) %>%
    mutate(householdPayment_rent = ifelse(is.na(householdPayment_rent),
      mean(householdPayment_rent, na.rm = T),
      householdPayment_rent
    ))

  data10_mortgage <- data10 %>%
    filter(householdPayment_mortgage > 0) %>%
    # high and low rent
    mutate(masterpayment_mortgage = ifelse(masterpayment_mortgage * 10000 / 12 >= 60000, NA, masterpayment_mortgage)) %>%
    mutate(masterpayment_mortgage = ifelse(is.na(masterpayment_mortgage),
      mean(masterpayment_mortgage, na.rm = T),
      masterpayment_mortgage
    )) %>%
    mutate(householdPayment_mortgage = ifelse(householdPayment_mortgage * 10000 / 12 >= 60000 | householdPayment_mortgage * 10000 / 12 <= 90, NA, householdPayment_mortgage)) %>%
    mutate(householdPayment_mortgage = ifelse(is.na(householdPayment_mortgage),
      mean(householdPayment_mortgage, na.rm = T),
      householdPayment_mortgage
    ))

  data10_owner <- data10 %>%
    filter(householdPayment_mortgage == 0 & householdPayment_rent == 0)

  data10 <-
    rbind(data10_mortgage, data10_owner, data10_rent)

  rm(data10_mortgage, data10_owner, data10_rent)
  # ________________________________________________________________

  # Step11 adding number of children
  data11 <- location %>%
    dplyr::select(id.location) %>%
    inner_join(hh_members, by = c("id.location" = "id.location"), suffix = c(".location", ".person")) %>%
    inner_join(people, by = c("id.person" = "id.person"), suffix = c(".location", ".people")) %>%
    dplyr::select(-id.survey.location) %>%
    dplyr::rename(id.survey = id.survey.people) %>%
    filter(relationshipWithYou == "child") %>%
    group_by(id.location) %>%
    dplyr::mutate(numChild = n()) %>%
    ungroup() %>%
    dplyr::select(id.location, numChild) %>%
    unique() %>%
    right_join(location, by = c("id.location" = "id.location")) %>%
    mutate(numChild = ifelse(is.na(numChild), 0, numChild)) %>%
    dplyr::select(id.location, numChild)


  # ________________________________________________________________


  # Step12 adding decision making variables
  data12 <-
    surveys %>%
    dplyr::select(id.survey, rC1, rC2, rC3, rC4, rC5) %>%
    mutate(Dec1Shared = ifelse(rC1 == 2 | rC1 == 6, 1, 0)) %>% # day to day spending and paying bills
    mutate(Dec1UnShared = 1 - Dec1Shared) %>%
    mutate(Dec2Shared = ifelse(rC2 == 2 | rC1 == 6, 1, 0)) %>% # large household purchases
    mutate(Dec2UnShared = 1 - Dec2Shared) %>%
    mutate(Dec3Shared = ifelse(rC3 == 2 | rC1 == 6, 1, 0)) %>% # number of hours you spend in paid work
    mutate(Dec3UnShared = 1 - Dec3Shared) %>%
    mutate(Dec4Shared = ifelse(rC4 == 2 | rC1 == 6, 1, 0)) %>% # Saving, investment and borrowing
    mutate(Dec4UnShared = 1 - Dec4Shared) %>%
    mutate(Dec5Shared = ifelse(rC5 == 2 | rC1 == 6, 1, 0)) %>% # Social life and leisure activities
    mutate(Dec5UnShared = 1 - Dec5Shared) %>%
    dplyr::select(-rC1, -rC2, -rC3, -rC4, -rC5) %>%
    inner_join(location, by = c("id.survey" = "id.survey")) %>%
    dplyr::select(
      id.location,
      Dec1Shared, Dec1UnShared,
      Dec2Shared, Dec2UnShared,
      Dec3Shared, Dec3UnShared,
      Dec4Shared, Dec4UnShared,
      Dec5Shared, Dec5UnShared
    )

  ### Applying PCA on the data12
  # we need to centerize our data, but no scale is needed because they are dummy variables
  data12.pca <- prcomp(data12[, c("Dec1Shared", "Dec2Shared", "Dec3Shared", "Dec4Shared", "Dec5Shared")], center = T, scale. = F)

  summary(data12.pca) # based on the summary, if we keep 3 principals, we keep 86 of the information

  # We can see the relation of previous variables towards the new principals (as can be seen all of them except 3 are in the first component direction)
  # The library of ggbiplot patches the plyr package with messes with dplyr package!!!
  # library(ggbiplot)
  # ggbiplot(data12.pca)

  # Computing new components for all the observations
  data12_Xpca <- t(t(as.matrix(data12.pca$rotation)) %*% t(as.matrix(data12[, c("Dec1Shared", "Dec2Shared", "Dec3Shared", "Dec4Shared", "Dec5Shared")])))
  colnames(data12_Xpca) <- c("Dec1Shared_pca", "Dec2Shared_pca", "Dec3Shared_pca", "Dec4Shared_pca", "Dec5Shared_pca")

  # To see how much of data will be lost by using k<n number of components we can again compute the approximation of the original values (the projection of the original values on the eigenvectors) like below
  # Obviously, when all principals are selected, all the data approximation will be exactly the original value as no information has been lost. But, when we choose k<n, the values are just an approximation and some information is lost.
  data12_approx <- as.matrix(data12_Xpca[, 1:4]) %*% t(as.matrix(data12.pca$rotation[, 1:4]))

  data12 <- cbind(data12, data12_Xpca)

  # ________________________________________________________________

  # Step13 adding degrees of master
  data13 <- location %>%
    dplyr::select(id.location) %>%
    inner_join(hh_members, by = c("id.location" = "id.location"), suffix = c(".location", ".person")) %>%
    inner_join(education, by = c("id.person" = "id.person"), suffix = c(".location", ".people")) %>%
    dplyr::select(-id.survey.location) %>%
    dplyr::rename(id.survey = id.survey.people) %>%
    mutate(studyLevelPrimary = ifelse(studyType == "primary", 1, 0)) %>%
    mutate(studyLevelSecondary = ifelse(studyType == "secondary", 1, 0)) %>%
    mutate(studyLevelTertiary = ifelse(studyType == "tertiary", 1, 0)) %>%
    mutate(studyLevelOther = ifelse(studyType == "other", 1, 0)) %>%
    filter(id.master == id.person) %>%
    group_by(id.master) %>%
    mutate(studyLevelPrimaryMaster = ifelse(sum(studyLevelPrimary) >= 1, 1, 0)) %>%
    mutate(studyLevelSecondaryMaster = ifelse(sum(studyLevelSecondary) >= 1, 1, 0)) %>%
    mutate(studyLevelTertiaryMaster = ifelse(sum(studyLevelTertiary) >= 1, 1, 0)) %>%
    mutate(studyLevelOtherMaster = ifelse(sum(studyLevelOther) >= 1, 1, 0)) %>%
    ungroup() %>%
    dplyr::select(id.location, studyLevelPrimaryMaster, studyLevelSecondaryMaster, studyLevelTertiaryMaster, studyLevelOtherMaster) %>%
    unique() %>%
    right_join(location, by = c("id.location" = "id.location")) %>%
    mutate(studyLevelPrimaryMaster = ifelse(is.na(studyLevelPrimaryMaster), 0, studyLevelPrimaryMaster)) %>%
    mutate(studyLevelSecondaryMaster = ifelse(is.na(studyLevelSecondaryMaster), 0, studyLevelSecondaryMaster)) %>%
    mutate(studyLevelTertiaryMaster = ifelse(is.na(studyLevelTertiaryMaster), 0, studyLevelTertiaryMaster)) %>%
    mutate(studyLevelOtherMaster = ifelse(is.na(studyLevelOtherMaster), 0, studyLevelOtherMaster)) %>%
    dplyr::select(id.location, studyLevelPrimaryMaster, studyLevelSecondaryMaster, studyLevelTertiaryMaster, studyLevelOtherMaster)


  # ________________________________________________________________

  # Step14 adding number of degree in household
  data14 <- location %>%
    dplyr::select(id.location, dateMoveOut) %>%
    inner_join(hh_members, by = c("id.location" = "id.location"), suffix = c(".location", ".person")) %>%
    inner_join(education, by = c("id.person" = "id.person"), suffix = c(".location", ".people")) %>%
    dplyr::select(-id.survey.location) %>%
    dplyr::rename(id.survey = id.survey.people) %>%
    # to see if this education has started before move out of this home
    mutate(eduBefore = ifelse(is.na(dateGraduate) & is.na(dateMoveOut), 1, ifelse(is.na(dateMoveOut), 1, ifelse(dateStart < dateMoveOut, 1, 0)))) %>%
    filter(eduBefore == 1) %>%
    mutate(studyLevelPrimary = ifelse(studyType == "primary", 1, 0)) %>%
    mutate(studyLevelSecondary = ifelse(studyType == "secondary", 1, 0)) %>%
    mutate(studyLevelTertiary = ifelse(studyType == "tertiary", 1, 0)) %>%
    mutate(studyLevelOther = ifelse(studyType == "other", 1, 0)) %>%
    group_by(id.location) %>%
    mutate(studyLevelPrimaryNum = sum(studyLevelPrimary)) %>%
    mutate(studyLevelSecondaryNum = sum(studyLevelSecondary)) %>%
    mutate(studyLevelTertiaryNum = sum(studyLevelTertiary)) %>%
    mutate(studyLevelOtherNum = sum(studyLevelOther)) %>%
    ungroup() %>%
    dplyr::select(id.location, studyLevelPrimaryNum, studyLevelSecondaryNum, studyLevelTertiaryNum, studyLevelOtherNum) %>%
    unique() %>%
    right_join(location, by = c("id.location" = "id.location")) %>%
    dplyr::select(id.location, studyLevelPrimaryNum, studyLevelSecondaryNum, studyLevelTertiaryNum, studyLevelOtherNum) %>%
    inner_join(data6, by = c("id.location" = "id.location")) %>% # to obtain member count
    ungroup() %>%
    mutate(studyLevelPrimaryNum = ifelse(is.na(studyLevelPrimaryNum), 0, studyLevelPrimaryNum)) %>%
    mutate(studyLevelSecondaryNum = ifelse(is.na(studyLevelSecondaryNum), 0, studyLevelSecondaryNum)) %>%
    mutate(studyLevelTertiaryNum = ifelse(is.na(studyLevelTertiaryNum), 0, studyLevelTertiaryNum)) %>%
    mutate(studyLevelOtherNum = ifelse(is.na(studyLevelOtherNum), 0, studyLevelOtherNum)) %>%
    dplyr::select(id.location, memberCount, studyLevelPrimaryNum, studyLevelSecondaryNum, studyLevelTertiaryNum, studyLevelOtherNum) %>%
    # percent of people in the family that have the level of study
    mutate(studyLevelPrimaryPerc = round(studyLevelPrimaryNum / memberCount, 2)) %>%
    mutate(studyLevelSecondaryPerc = round(studyLevelSecondaryNum / memberCount, 2)) %>%
    mutate(studyLevelTertiaryPerc = round(studyLevelTertiaryNum / memberCount, 2)) %>%
    mutate(studyLevelOtherPerc = round(studyLevelOtherNum / memberCount, 2)) %>%
    # boolean values, is tertiary study available in the family or not
    mutate(studyLevelPrimaryB = ifelse(studyLevelPrimaryNum >= 1, 1, 0)) %>%
    mutate(studyLevelSecondaryB = ifelse(studyLevelSecondaryNum >= 1, 1, 0)) %>%
    mutate(studyLevelTertiaryB = ifelse(studyLevelTertiaryNum >= 1, 1, 0)) %>%
    mutate(studyLevelOtherB = ifelse(studyLevelOtherNum >= 1, 1, 0)) %>%
    dplyr::select(-memberCount)


  # ________________________________________________________________
  # Step15 addinge if a role is in the family when moving out from a home
  # job in the end
  data15 <- location %>%
    dplyr::select(id.location, id.survey, id.master, dateMoveIn, dateMoveOut) %>%
    inner_join(employment, by = c("id.survey" = "id.survey"), suffix = c(".location", ".job")) %>%
    dplyr::select(id.location, id.survey, id.master, id.person, dateMoveIn, dateMoveOut, dateStart, dateLeft, role, workFromHome) %>%
    mutate(jobInEndS = ifelse(is.na(dateMoveOut), 1, ifelse(as.Date(dateStart) <= as.Date(dateMoveOut), 1, 0))) %>%
    mutate(jobInEndF = ifelse(is.na(dateLeft), 1, ifelse(is.na(dateMoveOut), 0, ifelse(as.Date(dateLeft) >= as.Date(dateMoveOut), 1, 0)))) %>%
    mutate(jobInEnd = ifelse((jobInEndS + jobInEndF) == 2, TRUE, FALSE)) %>% # if job has started before move in and have last after move in
    filter(jobInEnd == TRUE) %>%
    mutate(jobIsAdministrative = ifelse(role == "administrative", 1, 0)) %>%
    mutate(jobIsManager = ifelse(role == "manager", 1, 0)) %>%
    mutate(jobIsProfessional = ifelse(role == "professional", 1, 0)) %>%
    mutate(jobIsService = ifelse(role == "service", 1, 0)) %>%
    mutate(jobIsTechnician = ifelse(role == "technician", 1, 0)) %>%
    mutate(jobIsTransportNetwork = ifelse(role == "transportNetwork", 1, 0)) %>%
    mutate(jobIsLabour = ifelse(role == "labour", 1, 0)) %>%
    mutate(jobIsSales = ifelse(role == "sales", 1, 0)) %>%
    mutate(jobIsTaxi = ifelse(role == "taxi", 1, 0)) %>%
    mutate(jobIsTrade = ifelse(role == "trade", 1, 0)) %>%
    mutate(jobIsOther = ifelse(role == "other", 1, 0)) %>%
    mutate(isJobFromHome = ifelse(workFromHome == "yes", 1, 0)) %>%
    group_by(id.location) %>%
    mutate(jobIsAdministrativeSum = ifelse(sum(jobIsAdministrative) >= 1, 1, 0)) %>%
    mutate(jobIsManagerSum = ifelse(sum(jobIsManager) >= 1, 1, 0)) %>%
    mutate(jobIsProfessionalSum = ifelse(sum(jobIsProfessional) >= 1, 1, 0)) %>%
    mutate(jobIsServiceSum = ifelse(sum(jobIsService) >= 1, 1, 0)) %>%
    mutate(jobIsTechnicianSum = ifelse(sum(jobIsTechnician) >= 1, 1, 0)) %>%
    mutate(jobIsTransportNetworkSum = ifelse(sum(jobIsTransportNetwork) >= 1, 1, 0)) %>%
    mutate(jobIsLabourSum = ifelse(sum(jobIsLabour) >= 1, 1, 0)) %>%
    mutate(jobIsSalesSum = ifelse(sum(jobIsSales) >= 1, 1, 0)) %>%
    mutate(jobIsTaxiSum = ifelse(sum(jobIsTaxi) >= 1, 1, 0)) %>%
    mutate(jobIsTradeSum = ifelse(sum(jobIsTrade) >= 1, 1, 0)) %>%
    mutate(jobIsOtherSum = ifelse(sum(jobIsOther) >= 1, 1, 0)) %>%
    mutate(jobFromHomeSum = ifelse(sum(isJobFromHome) >= 1, 1, 0)) %>%
    ungroup() %>%
    dplyr::select(
      id.location,
      jobIsAdministrativeSum,
      jobIsManagerSum,
      jobIsProfessionalSum,
      jobIsServiceSum,
      jobIsTechnicianSum,
      jobIsTransportNetworkSum,
      jobIsLabourSum,
      jobIsSalesSum,
      jobIsTaxiSum,
      jobIsTradeSum,
      jobIsOtherSum,
      jobFromHomeSum
    ) %>%
    unique() %>%
    right_join(location, by = c("id.location" = "id.location")) %>%
    dplyr::select(
      id.location,
      jobIsAdministrativeSum,
      jobIsManagerSum,
      jobIsProfessionalSum,
      jobIsServiceSum,
      jobIsTechnicianSum,
      jobIsTransportNetworkSum,
      jobIsLabourSum,
      jobIsSalesSum,
      jobIsTaxiSum,
      jobIsTradeSum,
      jobIsOtherSum,
      jobFromHomeSum
    )

  data15[is.na(data15)] <- 0 # replacing na with 0 (for those who have had no job experience)

  # ________________________________________________________________
  # Step16 travel behaviour
  data16 <- surveys %>%
    select(id.survey, contains("tBTypical"), contains("tBValidDrivingLicense")) %>%
    mutate(commute_walk = ifelse(tBTypicalTravelModeCommute == "walking", 1, 0)) %>%
    mutate(commute_drive = ifelse(tBTypicalTravelModeCommute == "driveOwn", 1, 0)) %>%
    mutate(commute_public = ifelse(tBTypicalTravelModeCommute == "bus" |
      tBTypicalTravelModeCommute == "train" |
      tBTypicalTravelModeCommute == "lightRail", 1, 0)) %>%
    mutate(shop_walk = ifelse(tBTypicalTravelModeShopping == "walking", 1, 0)) %>%
    mutate(shop_drive = ifelse(tBTypicalTravelModeShopping == "driveOwn", 1, 0)) %>%
    mutate(shop_public = ifelse(tBTypicalTravelModeShopping == "bus" |
      tBTypicalTravelModeShopping == "train" |
      tBTypicalTravelModeShopping == "lightRail", 1, 0)) %>%
    mutate(recreation_walk = ifelse(tBTypicalTravelModeRecreation == "walking", 1, 0)) %>%
    mutate(recreation_drive = ifelse(tBTypicalTravelModeRecreation == "driveOwn", 1, 0)) %>%
    mutate(recreation_public = ifelse(tBTypicalTravelModeRecreation == "bus" |
      tBTypicalTravelModeRecreation == "train" |
      tBTypicalTravelModeRecreation == "lightRail", 1, 0)) %>%
    mutate(study_walk = ifelse(tBTypicalTravelModeStudy == "walking", 1, 0)) %>%
    mutate(study_drive = ifelse(tBTypicalTravelModeStudy == "driveOwn", 1, 0)) %>%
    mutate(study_public = ifelse(tBTypicalTravelModeStudy == "bus" |
      tBTypicalTravelModeStudy == "train" |
      tBTypicalTravelModeStudy == "lightRail", 1, 0)) %>%
    mutate(driveLicense = ifelse(is.na(tBValidDrivingLicense), 0, ifelse(tBValidDrivingLicense == "yes", 1, 0))) %>%
    inner_join(location, by = c("id.survey" = "id.survey")) %>%
    mutate(sumtravel = (commute_walk + commute_drive + commute_public +
      shop_walk + shop_drive + shop_public +
      recreation_walk + recreation_drive + recreation_public +
      study_walk + study_drive + study_public)) %>%
    mutate(walking_main = ifelse(sumtravel == 0, 0,
      ifelse(((commute_walk + shop_walk + recreation_walk + study_walk) / sumtravel) == 1, 1, 0)
    )) %>%
    mutate(driving_main = ifelse(sumtravel == 0, 0,
      ifelse(((commute_drive + shop_drive + recreation_drive + study_drive) / sumtravel) == 1, 1, 0)
    )) %>%
    mutate(public_main = ifelse(sumtravel == 0, 0,
      ifelse(((commute_public + shop_public + recreation_public + study_public) / sumtravel) == 1, 1, 0)
    ))


  data16 <- data16[, c(7:20, 57:59)]
  # rm(travelBehaviourColumns)

  # ________________________________________________________________
  # Step17 reason for relocation
  data17 <-
    surveys %>%
    select(id.survey, starts_with("rB")) %>%
    mutate(Att_personal = rB1 / 5) %>% # personal attitude (closeness to family and friends)
    mutate(Att_situ = (rB2 + rB3 + rB4 + rB6 + rB7 + rB10 + rB11 + rB12 + rB13 + rB14 + rB15 + rB16) / (12 * 5)) %>% # Situation of new location attitudes
    mutate(Att_work = rB5 / 5) %>% # being near to the work location
    mutate(Att_home = (rB8 + rB9) / (2 * 5)) %>%
    dplyr::select(1, 18:21) %>%
    inner_join(location, by = c("id.survey" = "id.survey")) %>%
    dplyr::select(6, 2:5)


  # last 1/2 years job start/leave
  data18 <- location %>%
    inner_join(employment, by = c("id.survey" = "id.survey"), suffix = c(".location", ".job")) %>%
    dplyr::select(id.location, id.survey, id.master, dateMoveIn, dateMoveOut, dateStart, dateLeft) %>%
    inner_join(surveys, by = c("id.survey" = "id.survey"), suffix = c(".location", ".job")) %>%
    dplyr::select(id.location, id.survey, id.master, dateMoveIn, dateMoveOut, dateStart, dateLeft, created) %>%
    mutate(dateMoveOut = if_else(is.na(dateMoveOut), as.Date(created), as.Date(dateMoveOut))) %>%
    mutate(lastYearStartJob_ = ifelse(as.Date(dateStart) <= as.Date(dateMoveOut) & as.Date(dateStart) >= (as.Date(dateMoveOut) - 365.25), 1, 0)) %>%
    mutate(lastYearLeftJob_ = ifelse(is.na(dateLeft), 0,
      ifelse(as.Date(dateLeft) <= as.Date(dateMoveOut) & as.Date(dateLeft) >= (as.Date(dateMoveOut) - 365.25), 1, 0)
    )) %>%
    mutate(last2YearStartJob_ = ifelse(as.Date(dateStart) <= as.Date(dateMoveOut) & as.Date(dateStart) >= (as.Date(dateMoveOut) - 730.5), 1, 0)) %>%
    mutate(last2YearLeftJob_ = ifelse(is.na(dateLeft), 0,
      ifelse(as.Date(dateLeft) <= as.Date(dateMoveOut) & as.Date(dateLeft) >= (as.Date(dateMoveOut) - 730.5), 1, 0)
    )) %>%
    group_by(id.location) %>%
    mutate(lastYearStartJob = ifelse(sum(lastYearStartJob_) >= 1, 1, 0)) %>%
    mutate(lastYearLeftJob = ifelse(sum(lastYearLeftJob_) >= 1, 1, 0)) %>%
    mutate(last2YearStartJob = ifelse(sum(last2YearStartJob_) >= 1, 1, 0)) %>%
    mutate(last2YearLeftJob = ifelse(sum(last2YearLeftJob_) >= 1, 1, 0)) %>%
    ungroup() %>%
    dplyr::select(id.location, lastYearStartJob, lastYearLeftJob, last2YearStartJob, last2YearLeftJob) %>%
    unique() %>%
    # Adding other locations with no income
    right_join(location, by = c("id.location" = "id.location")) %>%
    dplyr::select(id.location, lastYearStartJob, lastYearLeftJob, last2YearStartJob, last2YearLeftJob) %>%
    mutate(lastYearStartJob = ifelse(is.na(lastYearStartJob), 0, lastYearStartJob)) %>%
    mutate(lastYearLeftJob = ifelse(is.na(lastYearLeftJob), 0, lastYearLeftJob)) %>%
    mutate(last2YearStartJob = ifelse(is.na(last2YearStartJob), 0, last2YearStartJob)) %>%
    mutate(last2YearLeftJob = ifelse(is.na(last2YearLeftJob), 0, last2YearLeftJob))


  # last 1/2 years education start/leave
  data19 <- location %>%
    inner_join(education, by = c("id.survey" = "id.survey"), suffix = c(".location", ".edu")) %>%
    dplyr::select(id.location, id.survey, id.master, dateMoveIn, dateMoveOut, dateStart, dateGraduate) %>%
    inner_join(surveys, by = c("id.survey" = "id.survey"), suffix = c(".location", ".edu")) %>%
    dplyr::select(id.location, id.survey, id.master, dateMoveIn, dateMoveOut, dateStart, dateGraduate, created) %>%
    mutate(dateGraduate = if_else(is.na(dateGraduate), as.Date(created), as.Date(dateGraduate))) %>%
    mutate(lastYearStartEdu_ = ifelse(as.Date(dateStart) <= as.Date(dateMoveOut) & as.Date(dateStart) >= (as.Date(dateMoveOut) - 365.25), 1, 0)) %>%
    mutate(lastYearLeftEdu_ = ifelse(is.na(dateGraduate), 0,
      ifelse(as.Date(dateGraduate) <= as.Date(dateMoveOut) & as.Date(dateGraduate) >= (as.Date(dateMoveOut) - 365.25), 1, 0)
    )) %>%
    mutate(last2YearStartEdu_ = ifelse(as.Date(dateStart) <= as.Date(dateMoveOut) & as.Date(dateStart) >= (as.Date(dateMoveOut) - 730.5), 1, 0)) %>%
    mutate(last2YearLeftEdu_ = ifelse(is.na(dateGraduate), 0,
      ifelse(as.Date(dateGraduate) <= as.Date(dateMoveOut) & as.Date(dateGraduate) >= (as.Date(dateMoveOut) - 730.5), 1, 0)
    )) %>%
    group_by(id.location) %>%
    mutate(lastYearStartEdu = ifelse(sum(lastYearStartEdu_) >= 1, 1, 0)) %>%
    mutate(lastYearLeftEdu = ifelse(sum(lastYearLeftEdu_) >= 1, 1, 0)) %>%
    mutate(last2YearStartEdu = ifelse(sum(last2YearStartEdu_) >= 1, 1, 0)) %>%
    mutate(last2YearLeftEdu = ifelse(sum(last2YearLeftEdu_) >= 1, 1, 0)) %>%
    ungroup() %>%
    dplyr::select(id.location, lastYearStartEdu, lastYearLeftEdu, last2YearStartEdu, last2YearLeftEdu) %>%
    unique() %>%
    # Adding other locations with no income
    right_join(location, by = c("id.location" = "id.location")) %>%
    dplyr::select(id.location, lastYearStartEdu, lastYearLeftEdu, last2YearStartEdu, last2YearLeftEdu) %>%
    mutate(lastYearStartEdu = ifelse(is.na(lastYearStartEdu), 0, lastYearStartEdu)) %>%
    mutate(lastYearLeftEdu = ifelse(is.na(lastYearLeftEdu), 0, lastYearLeftEdu)) %>%
    mutate(last2YearStartEdu = ifelse(is.na(last2YearStartEdu), 0, last2YearStartEdu)) %>%
    mutate(last2YearLeftEdu = ifelse(is.na(last2YearLeftEdu), 0, last2YearLeftEdu))


  # last 1/2 years vehicle start/leave
  data20 <- location %>%
    inner_join(vehicle, by = c("id.survey" = "id.survey"), suffix = c(".location", ".Veh")) %>%
    dplyr::select(id.location, id.survey, id.master, dateMoveIn, dateMoveOut, dateOwn, dateAbandon) %>%
    inner_join(surveys, by = c("id.survey" = "id.survey"), suffix = c(".location", ".Veh")) %>%
    dplyr::select(id.location, id.survey, id.master, dateMoveIn, dateMoveOut, dateOwn, dateAbandon, created) %>%
    mutate(dateAbandon = if_else(is.na(dateAbandon), as.Date(created), as.Date(dateAbandon))) %>%
    mutate(lastYearStartVeh_ = ifelse(as.Date(dateOwn) <= as.Date(dateMoveOut) & as.Date(dateOwn) >= (as.Date(dateMoveOut) - 365.25), 1, 0)) %>%
    mutate(lastYearLeftVeh_ = ifelse(is.na(dateAbandon), 0,
      ifelse(as.Date(dateAbandon) <= as.Date(dateMoveOut) & as.Date(dateAbandon) >= (as.Date(dateMoveOut) - 365.25), 1, 0)
    )) %>%
    mutate(last2YearStartVeh_ = ifelse(as.Date(dateOwn) <= as.Date(dateMoveOut) & as.Date(dateOwn) >= (as.Date(dateMoveOut) - 730.5), 1, 0)) %>%
    mutate(last2YearLeftVeh_ = ifelse(is.na(dateAbandon), 0,
      ifelse(as.Date(dateAbandon) <= as.Date(dateMoveOut) & as.Date(dateAbandon) >= (as.Date(dateMoveOut) - 730.5), 1, 0)
    )) %>%
    group_by(id.location) %>%
    mutate(lastYearStartVeh = ifelse(sum(lastYearStartVeh_) >= 1, 1, 0)) %>%
    mutate(lastYearLeftVeh = ifelse(sum(lastYearLeftVeh_) >= 1, 1, 0)) %>%
    mutate(last2YearStartVeh = ifelse(sum(last2YearStartVeh_) >= 1, 1, 0)) %>%
    mutate(last2YearLeftVeh = ifelse(sum(last2YearLeftVeh_) >= 1, 1, 0)) %>%
    ungroup() %>%
    dplyr::select(id.location, lastYearStartVeh, lastYearLeftVeh, last2YearStartVeh, last2YearLeftVeh) %>%
    unique() %>%
    # Adding other locations with no income
    right_join(location, by = c("id.location" = "id.location")) %>%
    dplyr::select(id.location, lastYearStartVeh, lastYearLeftVeh, last2YearStartVeh, last2YearLeftVeh) %>%
    mutate(lastYearStartVeh = ifelse(is.na(lastYearStartVeh), 0, lastYearStartVeh)) %>%
    mutate(lastYearLeftVeh = ifelse(is.na(lastYearLeftVeh), 0, lastYearLeftVeh)) %>%
    mutate(last2YearStartVeh = ifelse(is.na(last2YearStartVeh), 0, last2YearStartVeh)) %>%
    mutate(last2YearLeftVeh = ifelse(is.na(last2YearLeftVeh), 0, last2YearLeftVeh))


  # last 1/2 years child
  data21 <- location %>%
    inner_join(hh_members, by = c("id.location" = "id.location"), suffix = c(".location", ".person")) %>%
    dplyr::select(id.location, id.survey.location, id.master.location, id.person, dateMoveIn, dateMoveOut) %>%
    dplyr::rename(id.survey = id.survey.location, id.master = id.master.location) %>%
    left_join(people, by = c("id.person" = "id.person")) %>%
    dplyr::select(id.location, id.survey.x, id.master, id.person, dateMoveIn, dateMoveOut, dob) %>%
    dplyr::rename(id.survey = id.survey.x) %>%
    inner_join(surveys, by = c("id.survey" = "id.survey"), suffix = c(".location", ".Veh")) %>%
    dplyr::select(id.location, id.survey, id.master, id.person, dateMoveIn, dateMoveOut, dob, created) %>%
    mutate(dateMoveOut = if_else(is.na(dateMoveOut), as.Date(created), as.Date(dateMoveOut))) %>%
    mutate(lastYearNewChild = ifelse((dob >= (dateMoveOut - 365.25)) & dob <= dateMoveOut, 1, 0)) %>%
    mutate(last2YearNewChild = ifelse((dob >= (dateMoveOut - 730.5)) & dob <= dateMoveOut, 1, 0)) %>%
    group_by(id.location) %>%
    mutate(lastYearNewChild = ifelse(sum(lastYearNewChild) >= 1, 1, 0)) %>%
    mutate(last2YearNewChild = ifelse(sum(last2YearNewChild) >= 1, 1, 0)) %>%
    ungroup() %>%
    mutate(lastYearNewChild = ifelse(is.na(lastYearNewChild), 0, lastYearNewChild)) %>% # for the persons that their code are not available in people database
    mutate(last2YearNewChild = ifelse(is.na(last2YearNewChild), 0, last2YearNewChild)) %>% # for the persons that their code are not available in people database

    dplyr::select(id.location, lastYearNewChild, last2YearNewChild) %>%
    unique()
    
  # home attitudes
  data22_ <-
    surveys %>%
    select(id.survey, starts_with("rB")) %>%
    mutate(across(starts_with("rB"), ~ ifelse(. == "na", 0, .), .name = "{col}")) %>% 
    inner_join(location, by = c("id.survey" = "id.survey")) %>% 
    select(c(id.location, starts_with("rB"))) %>% 
    mutate(across(starts_with("rB"), ~ ifelse(. == c(4,5), 1, 0), .names = "imp_{col}")) %>%
    mutate(across(starts_with("rB"), ~ ifelse(. == c(1,2), 1, 0), .names = "unimp_{col}")) %>%
    mutate(across(starts_with("rB"), ~ as.factor(.), .names = "{col}_f"))
  
  
  # x <- as.data.frame(sapply(x, as.numeric))
  
  data22_f <- model.matrix(~ rB1_f + rB2_f + rB3_f + rB4_f + rB5_f +
                             rB6_f + rB7_f + rB8_f + rB9_f + rB10_f +
                             rB11_f + rB12_f + rB13_f + rB14_f + rB15_f +
                             rB16_f - 1, data = data22_) %>%
    as.data.frame() %>% 
    select(-rB1_f1)
  
  data22 = cbind(data22_, data22_f) %>% 
    select(-ends_with("_f"))
    
  
  
  # ________________________________________________________________
  # Final Step Combining all the datasets

  data.comp <- data1 %>%
    inner_join(data2, by = c("id.location" = "id.location")) %>%
    inner_join(data3, by = c("id.location" = "id.location")) %>%
    inner_join(data4, by = c("id.location" = "id.location")) %>%
    inner_join(data51, by = c("id.location" = "id.location")) %>%
    inner_join(data52, by = c("id.location" = "id.location")) %>%
    inner_join(data6, by = c("id.location" = "id.location")) %>%
    inner_join(data7, by = c("id.location" = "id.location")) %>%
    inner_join(data8, by = c("id.location" = "id.location")) %>%
    inner_join(data9, by = c("id.location" = "id.location")) %>%
    inner_join(data10, by = c("id.location" = "id.location")) %>%
    inner_join(data11, by = c("id.location" = "id.location")) %>%
    inner_join(data12, by = c("id.location" = "id.location")) %>%
    inner_join(data13, by = c("id.location" = "id.location")) %>%
    inner_join(data14, by = c("id.location" = "id.location")) %>%
    inner_join(data15, by = c("id.location" = "id.location")) %>%
    inner_join(data16, by = c("id.location" = "id.location")) %>%
    inner_join(data17, by = c("id.location" = "id.location")) %>%
    mutate(hhIncomeChange = hhIncomeInEnd - hhIncomeInStart) %>%
    inner_join(data18, by = c("id.location" = "id.location")) %>%
    inner_join(data19, by = c("id.location" = "id.location")) %>%
    inner_join(data20, by = c("id.location" = "id.location")) %>%
    inner_join(data21, by = c("id.location" = "id.location")) %>% 
    inner_join(data22, by = c("id.location" = "id.location"))

  rm(data1, data2, data3, data4, data51, data52, data6, data7, data8, data9, data10, data11, data12, data13, data14, data15, data16, data17, data18, data19, data20, data21)


  p2 <- function(x) (x**2)
  # p3 <- function(x) (x ** 3)

  #
  data.comp_ <- data.comp %>%
    mutate(across(c(7, 8, 19:29, 39:43, 63:66, 103:107), p2, .names = "{col}_p2")) # some selected columns - should be modified

  data.comp___ <- cbind(data.comp, data.comp_[, (ncol(data.comp) + 1):ncol(data.comp_)])

  data.comp___ <- data.comp___ %>%
    mutate(strCoupleACoupleChil = strCouple + strCoupleWch)

  dat_v9 <- data.comp___
  dat_v10 <- dat_v9 %>%
    mutate(numJobInEndmStart = numJobInEnd - numJobInStart) %>%
    select(
      -c(
        "city",
        "masterAge",
        "masterGender",
        "isRenter",
        "isApartment",
        "isTownhouse",
        "isStudio",
        "isRooml4m2",
        "isRoomm5",
        "numJobInEnd",
        "numJobInStart",
        "hhIncomeInEnd",
        # "hhIncomeInStart",
        "numMale",
        "memberCount",
        "chgOutWithPartner",
        "chgSeparated",
        "strSingleParent",
        "strSingle",
        "masterpayment_rent",
        "masterpayment_mortgage",
        "numChild",
        ends_with("UnShared"),
        "Dec4Shared_pca",
        "Dec5Shared_pca",
        intersect(starts_with("study"), ends_with("Master")),
        intersect(starts_with("study"), ends_with("Perc")),
        intersect(starts_with("study"), ends_with("B")),
        "jobIsServiceSum",
        "jobIsTechnicianSum",
        "jobIsTransportNetworkSum",
        "jobIsLabourSum",
        "jobIsSalesSum",
        "jobIsTaxiSum",
        "jobIsTradeSum",
        "jobIsOtherSum",
        ends_with("_walk"),
        "walking_main",
        "masterAge_p2",
        "numJobInStart_p2",
        "hhIncomeInStart_p2",
        "numJobInEnd_p2",
        "hhIncomeInEnd_p2",
        "memberCount_p2",
        "numMale_p2",
        "masterpayment_rent_p2",
        "masterpayment_mortgage_p2",
        "numChild_p2"
      )
    )
  dat_v10$duration <- dat_v10$duration / 365.25 # duration into years
  return(dat_v10)
}

loc_data_home <- function(surveys, location) {
  location %>%
    select(id.survey, id.location, home_move_in = dateMoveIn, home_move_out = dateMoveOut, home.loc = formattedAddress)
}

loc_data <- function(surveys, home_locs, education, employment) {
  edu_locs <- education %>%
    select(id.survey, id.education, edu_start = dateStart, edu_end = dateGraduate, edu.loc = formattedAddress)

  emp_locs <- employment %>%
    select(id.survey, id.employment, emp_start = dateStart, emp_end = dateLeft, emp.loc = formattedAddress)

  edu_locs_joined <- surveys %>%
    select(id.survey) %>%
    left_join(home_locs, by = "id.survey") %>%
    left_join(edu_locs, by = "id.survey") %>%
    mutate(across(ends_with(c("end", "move_out")), ~ fifelse(is.na(.), as.Date(Sys.Date()), as.Date(.)), .names = "{col}")) %>%
    mutate(edu_overlap = lubridate::int_overlaps(
      interval(home_move_in, home_move_out),
      interval(edu_start, edu_end)
    )) %>%
    filter(edu_overlap == T)

  emp_locs_joined <- surveys %>%
    select(id.survey) %>%
    left_join(home_locs, by = "id.survey") %>%
    left_join(emp_locs, by = "id.survey") %>%
    mutate(across(ends_with(c("end", "move_out")), ~ fifelse(is.na(.), as.Date(Sys.Date()), as.Date(.)), .names = "{col}")) %>%
    mutate(emp_overlap = lubridate::int_overlaps(
      interval(home_move_in, home_move_out),
      interval(emp_start, emp_end)
    )) %>%
    filter(emp_overlap == T)

  # home locations that are not related to any home or edu
  home_locs_missed <- home_locs %>%
    anti_join(emp_locs_joined, by = "id.location") %>%
    anti_join(edu_locs_joined, by = "id.location") %>%
    mutate(home_move_out = as.Date(home_move_out))

  bind_rows(edu_locs_joined, emp_locs_joined, home_locs_missed)
}

find_unavailable_locs <- function(home_edu_emp_locs, car_ttm) {
  unav_ <- home_edu_emp_locs %>%
    mutate(across(ends_with(".loc"), ~ as.character(.), .names = "{col}")) %>%
    mutate(across(ends_with(".loc"), ~ gsub(",.*", "", .), .names = "{col}")) %>%
    left_join(car_ttm, by = c("home.loc" = "fromId", "emp.loc" = "toId")) %>%
    left_join(car_ttm, by = c("home.loc" = "fromId", "edu.loc" = "toId")) %>%
    rename(travel_time.emp = travel_time.x, travel_time.edu = travel_time.y) %>%
    select(home.loc, emp.loc, edu.loc, travel_time.emp, travel_time.edu)

  unav_emp <- unav_ %>%
    filter((!is.na(emp.loc)) & is.na(travel_time.emp)) %>%
    select(home.loc, emp.loc) %>%
    unlist() %>%
    unique()

  unav_edu <- unav_ %>%
    filter((!is.na(edu.loc)) & is.na(travel_time.edu)) %>%
    select(home.loc, edu.loc) %>%
    unlist() %>%
    unique()

  unav_possible <- c(unav_emp, unav_edu) %>% unique()
  unav <- unav_possible[!(unav_possible %in% car_ttm$fromId)]

  out_path <- path("Data", "Processed", "Accessibility", "unavailableLocations.csv")
  message("Saving unavailable locations to: ", out_path)
  fwrite(as.data.table(unav), file = out_path)
  unav
}

home_edu_emp_locs_geocoded_func <- function(home_edu_emp_locs, geocoded_suburbs_in_survey, read_from) {
  geocoded_suburbs_in_survey_sub <- cbind(
    data.frame(refined_address = geocoded_suburbs_in_survey$results$formatted_address),
    survey_address = geocoded_suburbs_in_survey$survey_location_name
  )

  fixed_locations <- data.table::fread(read_from)

  fixed_home_edu_emp_locs <- home_edu_emp_locs %>%
    left_join(fixed_locations, by = c("home.loc" = "all_suburbs_in_survey")) %>%
    mutate(home.loc = ifelse(is.na(all_suburbs_in_survey_fixed), home.loc, all_suburbs_in_survey_fixed)) %>%
    select(-all_suburbs_in_survey_fixed) %>%
    left_join(fixed_locations, by = c("edu.loc" = "all_suburbs_in_survey")) %>%
    mutate(edu.loc = ifelse(is.na(all_suburbs_in_survey_fixed), edu.loc, all_suburbs_in_survey_fixed)) %>%
    select(-all_suburbs_in_survey_fixed) %>%
    left_join(fixed_locations, by = c("emp.loc" = "all_suburbs_in_survey")) %>%
    mutate(emp.loc = ifelse(is.na(all_suburbs_in_survey_fixed), emp.loc, all_suburbs_in_survey_fixed)) %>%
    select(-all_suburbs_in_survey_fixed)

  fixed_home_edu_emp_locs %>%
    mutate(across(ends_with(".loc"), ~ as.character(.), .names = "{col}")) %>%
    left_join(geocoded_suburbs_in_survey_sub, by = c("home.loc" = "survey_address")) %>%
    rename(home.loc.geo = refined_address) %>%
    left_join(geocoded_suburbs_in_survey_sub, by = c("emp.loc" = "survey_address")) %>%
    rename(emp.loc.geo = refined_address) %>%
    left_join(geocoded_suburbs_in_survey_sub, by = c("edu.loc" = "survey_address")) %>%
    rename(edu.loc.geo = refined_address) %>%
    filter(!(is.na(id.education) & is.na(id.employment))) %>%
    mutate(edu.loc.geo = ifelse(!is.na(id.education) & is.na(edu.loc.geo), "Sydney NSW, Australia", edu.loc.geo)) %>%
    mutate(emp.loc.geo = ifelse(!is.na(id.employment) & is.na(emp.loc.geo), "Sydney NSW, Australia", emp.loc.geo))
}

home_locs_geocoded_func <- function(home_locs, geocoded_suburbs_in_survey) {
  geocoded_suburbs_in_survey_sub <- cbind(
    data.frame(refined_address = geocoded_suburbs_in_survey$results$formatted_address),
    survey_address = geocoded_suburbs_in_survey$survey_location_name
  )

  home_locs %>%
    mutate(across(ends_with(".loc"), ~ as.character(.), .names = "{col}")) %>%
    left_join(geocoded_suburbs_in_survey_sub, by = c("home.loc" = "survey_address")) %>%
    rename(home.loc.geo = refined_address) %>%
    group_by(id.location) %>%
    filter(row_number() == 1)
}

find_tt_func <- function(home_edu_emp_locs_geocoded, car_ttm_extra, pt_ttm) {
  aa = home_edu_emp_locs_geocoded %>%
    mutate(across(ends_with(".loc"), ~ as.character(.), .names = "{col}")) %>%
    # mutate(across(ends_with(".loc"), ~gsub(",.*", "", .), .names = "{col}")) %>%
    left_join(car_ttm_extra, by = c("home.loc.geo" = "fromId", "emp.loc.geo" = "toId")) %>%
    rename(travel_time_to_work_car = travel_time) %>%
    left_join(car_ttm_extra, by = c("emp.loc.geo" = "fromId", "home.loc.geo" = "toId")) %>%
    rename(travel_time_from_work_car = travel_time) %>%
    left_join(car_ttm_extra, by = c("home.loc.geo" = "fromId", "edu.loc.geo" = "toId")) %>%
    rename(travel_time_to_edu_car = travel_time) %>%
    left_join(car_ttm_extra, by = c("edu.loc.geo" = "fromId", "home.loc.geo" = "toId")) %>%
    rename(travel_time_from_edu_car = travel_time) %>%
    left_join(pt_ttm, by = c("home.loc.geo" = "fromId", "emp.loc.geo" = "toId")) %>%
    rename(travel_time_to_work_pt = travel_time) %>%
    left_join(pt_ttm, by = c("emp.loc.geo" = "fromId", "home.loc.geo" = "toId")) %>%
    rename(travel_time_from_work_pt = travel_time) %>%
    left_join(pt_ttm, by = c("home.loc.geo" = "fromId", "edu.loc.geo" = "toId")) %>%
    rename(travel_time_to_edu_pt = travel_time) %>%
    left_join(pt_ttm, by = c("edu.loc.geo" = "fromId", "home.loc.geo" = "toId")) %>%
    rename(travel_time_from_edu_pt = travel_time) %>%

  # for now, lets replace NA with the average
  mutate(across(starts_with("travel_time"), ~case_when(is.na(.) ~ mean(., na.rm=TRUE),
                                                       TRUE ~ as.numeric(.))))
}

extra_tt_func <- function(ttm, read_from) {
  extra_tt <- data.table::fread(read_from)
  rbind(ttm, extra_tt)
}


add_tt_data <- function(dat_vars, travel_time) {
  travel_time_work_car <- travel_time %>%
    filter(!is.na(id.employment)) %>%
    group_by(id.location) %>%
    mutate(tt_work_car_mean = mean(travel_time_to_work_car + travel_time_from_work_car)) %>%
    select(id.location, tt_work_car_mean) %>%
    unique()

  travel_time_edu_car <- travel_time %>%
    filter(!is.na(id.education)) %>%
    group_by(id.location) %>%
    mutate(tt_edu_car_mean = mean(travel_time_to_edu_car + travel_time_from_edu_car)) %>%
    select(id.location, tt_edu_car_mean) %>%
    unique()

  travel_time_work_pt <- travel_time %>%
    filter(!is.na(id.employment)) %>%
    group_by(id.location) %>%
    mutate(tt_work_pt_mean = mean(travel_time_to_work_pt + travel_time_from_work_pt)) %>%
    select(id.location, tt_work_pt_mean) %>%
    unique()

  travel_time_edu_pt <- travel_time %>%
    filter(!is.na(id.education)) %>%
    group_by(id.location) %>%
    mutate(tt_edu_pt_mean = mean(travel_time_to_edu_pt + travel_time_from_edu_pt)) %>%
    select(id.location, tt_edu_pt_mean) %>%
    unique()

  dat_vars %>%
    left_join(travel_time_work_car, by = c("id.location")) %>%
    left_join(travel_time_edu_car, by = c("id.location")) %>%
    left_join(travel_time_work_pt, by = c("id.location")) %>%
    left_join(travel_time_edu_pt, by = c("id.location")) %>%
    # for people without work or edu record
    mutate(tt_work_car_mean = ifelse(is.na(tt_work_car_mean), 0, tt_work_car_mean)) %>%
    mutate(tt_edu_car_mean = ifelse(is.na(tt_edu_car_mean), 0, tt_edu_car_mean)) %>%
    mutate(tt_work_pt_mean = ifelse(is.na(tt_work_pt_mean), 0, tt_work_pt_mean)) %>%
    mutate(tt_edu_pt_mean = ifelse(is.na(tt_edu_pt_mean), 0, tt_edu_pt_mean))
}

add_landuse_data <- function(dat_tt, home_locs_geocoded, suburb_landuse_sf) {
  # remove the parantheses and what is inside
  suburb_landuse_sf_ <- suburb_landuse_sf %>%
    mutate(across(ends_with("16"), ~ gsub("\\s*\\([^\\)]+\\)", "", as.character(.)), .names = "{col}")) %>%
    mutate(ssc_name16 = tolower(ssc_name16))


  library(stringr)
  land_use_locs <- home_locs_geocoded %>%
    # resolve the one NA manually
    mutate(home.loc.geo = ifelse(is.na(home.loc.geo) | home.loc.geo == "", "Kensington NSW 2033, Australia", home.loc.geo)) %>%
    unique() %>%
    select(id.location, home.loc.geo) %>%
    mutate(across(ends_with(".geo"), ~ as.character(.), .names = "{col}")) %>%
    mutate(across(ends_with(".geo"), ~ gsub(", Australia", "", .), .names = "{col}")) %>%
    mutate(across(ends_with(".geo"), ~ gsub("[0-9]", "", .), .names = "{col}")) %>%
    mutate(across(ends_with(".geo"), ~ gsub("NSW", "", .), .names = "{col}")) %>%
    mutate(across(ends_with(".geo"), ~ gsub(", New South Wales", "", .), .names = "{col}")) %>%
    mutate(across(ends_with(".geo"), ~ trimws(., "right"), .names = "{col}")) %>%
    mutate(home.loc.geo = tolower(home.loc.geo)) %>%
    left_join(suburb_landuse_sf_, by = c("home.loc.geo" = "ssc_name16"))
  
  fuzzy_lu = land_use_locs %>% 
    filter(is.na(commercial)) %>%
    select(-c(commercial:primary_production_per)) %>% 
    mutate(home.loc.geo = tolower(home.loc.geo)) %>%
    fuzzyjoin::fuzzy_left_join(suburb_landuse_sf_, by = c("home.loc.geo" = "ssc_name16"), match_fun = str_detect) %>% 
    group_by(id.location) %>% 
    mutate(maxnchar = max(nchar(ssc_name16))) %>% 
    filter(nchar(ssc_name16) == maxnchar) %>% 
    select(-c(home.loc.geo, ssc_name16, total_area, maxnchar))
  # for now, lets replace NA with the average
  # mutate(across(commercial:primary_production_per, ~case_when(is.na(.) ~ mean(., na.rm=TRUE),
  #                                                      TRUE ~ as.numeric(.))))
 normal_lu = land_use_locs %>% 
      filter(!is.na(commercial)) %>% 
      select(-c(home.loc.geo, total_area))
    
 land_use_locs_all = rbind(normal_lu, fuzzy_lu)
        
 dat_tt %>%
    left_join(land_use_locs_all, by = c("id.location"))
}

add_accessibility_data <- function(dat, home_locs_geocoded, accessibility_data, mode){
  # remove the parantheses and what is inside
  accessibility_data_ <- accessibility_data %>%
    mutate(from_id = gsub("\\s*\\([^\\)]+\\)", "", as.character(from_id)),
           from_id = tolower(from_id))
  
  acc_locs <- home_locs_geocoded %>%
    # resolve the one NA manually :|
    mutate(home.loc.geo = ifelse(is.na(home.loc.geo) | home.loc.geo == "", "Kensington NSW 2033, Australia", home.loc.geo)) %>%
    unique() %>%
    select(id.location, home.loc.geo) %>%
    mutate(across(ends_with(".geo"), ~ as.character(.), .names = "{col}")) %>%
    mutate(across(ends_with(".geo"), ~ gsub(", Australia", "", .), .names = "{col}")) %>%
    mutate(across(ends_with(".geo"), ~ gsub("[0-9]", "", .), .names = "{col}")) %>%
    mutate(across(ends_with(".geo"), ~ gsub("NSW", "", .), .names = "{col}")) %>%
    mutate(across(ends_with(".geo"), ~ gsub(", New South Wales", "", .), .names = "{col}")) %>%
    mutate(across(ends_with(".geo"), ~ trimws(., "right"), .names = "{col}")) %>%
    mutate(home.loc.geo = tolower(home.loc.geo)) %>%
    left_join(accessibility_data_, by = c("home.loc.geo" = "from_id"))
  
  fuzzy_acc = acc_locs %>% 
    filter(is.na(accessibility)) %>%
    select(-c(percentile:accessibility)) %>% 
    fuzzyjoin::fuzzy_left_join(accessibility_data_, by = c("home.loc.geo" = "from_id"), match_fun = str_detect) %>% 
    group_by(id.location) %>% 
    mutate(maxnchar = max(nchar(from_id))) %>% 
    filter(nchar(from_id) == maxnchar) %>% 
    select(-c(home.loc.geo, from_id, maxnchar)) %>% 
    filter(accessibility == max(accessibility)) %>% 
    filter(row_number() == 1)
  # for now, lets replace NA with the average
  # mutate(across(commercial:primary_production_per, ~case_when(is.na(.) ~ mean(., na.rm=TRUE),
  #                                                      TRUE ~ as.numeric(.))))
  normal_acc = acc_locs %>% 
    filter(!is.na(accessibility)) %>% 
    select(-c(home.loc.geo)) %>% 
    group_by(id.location) %>% 
    filter(accessibility == max(accessibility)) %>% 
    filter(row_number() == 1)
  
  acc_locs_all = rbind(normal_acc, fuzzy_acc) %>% 
    select(-c(percentile, cutoff)) %>% 
    rename_at(vars(contains("accessibility")), list( ~paste0(., "_", mode)))
  
  dat %>%
    left_join(acc_locs_all, by = c("id.location"))
  
  
}

geocode_locations <- function(location_names) {
  components <- data.frame(component = c("country"), value = c("AU"))
  purrr::map_dfr(location_names, ~ {
    res <- googleway::google_geocode(
      address = .x,
      components = components,
      region = "AU",
      key = Sys.getenv("GOOGLE_MAP_API"),
      simplify = TRUE
    )
    res$survey_location_name <- .x
    res
  })
}

extract_centroids_from_geocodes <- function(geocoded_suburbs_in_survey) {
  cbind(
    data.frame(address = geocoded_suburbs_in_survey$results$formatted_address),
    geocoded_suburbs_in_survey$results$geometry$location
  ) %>%
    data.table::setnames(c("id", "lat", "lon")) %>%
    dplyr::distinct(id, .keep_all = TRUE)
}

# This part is added for adding landuse variables to the home locations
# there are some suburbs that are correctly detected by geocode - But their names cannot
# be found in ss_name16 because the ss_name16 has some extra explanation on the name
# in order to find the correct ss_name16 based on the found geo, I have created a dataset for
# the equivalent ssc_name_16 of not-found geos and they are replaced as shown below
fix_geocoded_suburbs <- function(geocoded_suburbs_in_survey, read_from) {
  equivalent_locs <- data.table::fread(read_from)
  geocoded_suburbs_in_survey$results %>%
    left_join(equivalent_locs, by = c("formatted_address" = "home.loc.geo.survey")) %>%
    mutate(formatted_address = ifelse(is.na(ssc_name16_equivalent), formatted_address, ssc_name16_equivalent)) %>%
    select(-ssc_name16_equivalent)
}

# In the dataset, some of the locations are not standard (e.g. university of Sydney).
# These make it hard when we want to calculate accessibility variables
# They are fixed using an external csv file
fix_all_suburbs <- function(all_suburbs_in_survey, read_from) {
  fixed_locations <- data.table::fread(read_from)

  as.data.frame(all_suburbs_in_survey) %>%
    left_join(fixed_locations, by = c("all_suburbs_in_survey" = "all_suburbs_in_survey")) %>%
    mutate(all_suburbs_in_survey = ifelse(is.na(all_suburbs_in_survey_fixed), all_suburbs_in_survey, all_suburbs_in_survey_fixed)) %>%
    select(-all_suburbs_in_survey_fixed) %>%
    unlist()
}
