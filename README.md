# Haskell - Google sheets Example!
A simple example program written in Haskell to update a sheet on Google Sheets!
I wrote this because it can be difficult to find working examples of code to do very specific things in uncommon languages, especially if you are a beginner. 
Having said that, should this example not work for you, feel free to post an issue with as much info as possible. 
There are many ways to write this program in Haskell, this is just one to get you started.

##  Requirements 
- Stack build tool (My version is 2.1.3, other versions will likely work)
 You can get the build tool from the link below 
 [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)

##  Google Sheets Setup
1. Make a project on the google Developer Console. https://console.developers.google.com/
 1.1. Give this project google Sheets API access

2. Add a Service Account for your project on the google Developer Console.
 2.1. Your Service Account needs the role of Project Editor (Edit Access To All Resources).
 2.2. Download your Service Account Key File, JSON format. Do not lose this file. 

Should look something like this 
```
{
  "type": "service_account",
  "project_id": "serviceWorkerID",
  "private_key_id": "<40-character-key-here>",
  "private_key": "<1732-character-key-here>",
  "client_email": "projectName@serviceWorkerID.iam.gserviceaccount.com",
  "client_id": "<clientIDkey>",
  "auth_uri": "https://accounts.google.com/o/oauth2/auth",
  "token_uri": "https://oauth2.googleapis.com/token",
  "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
  "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/projectName%40serviceWorkerID.iam.gserviceaccount.com"
}
```

3. Put the key.json in the /config/ folder of the project
4.  Create a Google sheet (using the same Google Account that you created the Google Developer Project) and get the SheetID from it.
    https://docs.google.com/spreadsheets/d/<44-character-sheet-id>/

5. Add your Service Account (service worker) to your sheet 
(i.e. share the sheet with projectName@serviceWorkerID.iam.gserviceaccount.com).

6.  Create a **config.json**, place the sheetID of your google sheet in the config, and the range you want to edit. 
more info on ranges can be found on the google sheets API website.  Put this **config.json** into your /config/ folder of your project
```
{ "sheetID":"<44-character-sheet-id>"
, "range": "sheet1"
}
```

## Running the Project
- **stack build**
- **stack exec haskell-googlesheets-example-exe**
