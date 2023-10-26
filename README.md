# entity-registry-etl

This repo helps pull data from the Lieutenant Governor's Entity Registry Salesforce Export and cleans each name to match a standardized name in our system. 

The LG office is the official area for all local governments, however for a variety of reasons we do not want our entities to always have the exact same names as their list. This script helps convert their local government names into a better naming convention so we can flag any new entities and add them to our compliance system.

I also cleaned up a lot of our own names to follow the new conventions.

# Motivation:
- Need a standardized name that follows expected conventions. 
- Remove characters that can have unintended consequences on databases, computer scripts, and other programming tasks. Many special characters are reserved for specific computer related operators and tasks. For example a "," is an often used separator for CSVs, or "\","()", and "[]" are commonly used in Regular Expressions and should not be used. "/" is common fore file name paths, and can break file writes/reads by an entity name. 
- A standard name also makes it easier to find a certain entity in a database or search.

# OSA Naming Conventions:

1. Alphanumeric characters and "-" are allowed. All other characters must be omitted.
   - If you are combining two words then there should not be any space between the "-". For example: "Marriott-Slaterville City"
   - If you are separating or adding hierarchy, then there should be a space between the "-" For Example "Big Brothers - Big Sisters of Utah" or "American Preparatory Academy - Utah Charter Academies"
2. No Abbreviations/Acronyms unless it is apart of the entity's name. This is most common for Non profits and CPA Firms.
3. Title Case except for common minor words like "and", "for", etc. Accepted Acronyms and Abbreviations do not need to follow title case, and should be uppercase. 
  - Note, we have filters for English words, but Spanish words must be hard-coded. For example "Centro De La Familia De Utah" becomes "Centro de la Familia de Utah".
4. No "Inc.", "LLC", "Incorporated", or other private industry suffixes are necessary.
5. No Roman Numerals, use an Arabic numeral instead.
6. If the government type is a "health provider" or "CPA Firm" then acronyms are ok (MD, PC, RN, CPA, etc)
7. When possible, the parent entity name of a component unit should come first. 
  - For example, "Salt Lake City Housing Authority" instead of "Housing Authority of Salt Lake City"
  - Another example, "Redevelopment Agency of Salt Lake City" should be "Salt Lake City Redevelopment Agency".
8. Commas should not be added after the "core" name. 
  - For example, "North Salt Lake, City of" should be "North Salt Lake City"
9. Do not use DBA or something like that. Change the entity to the new legal name, or add the DBA name with a "-" to the end. For example: "ABC Company (DBA Example Holdings)" would be "ABC Company" or "ABC Company - Example Holdings"
