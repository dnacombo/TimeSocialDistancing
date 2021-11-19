# TimeSocialDistancing

Here we deal with data collected in the project TimeSocialDistancing.

## Working with this repository

- NOT using Git: Download the .zip behind the green "Code" button on top of this page and unzip it to a new directory.

- Using Git: Clone the repository to your favorite location.

If using Rstudio (v 1.4.1717 or above), then *load the TimeSocialDistancing project* (double-click on TimeSocialDistancing.Rproj). This will start Rstudio and suggest to install a number of packages. It can take a while to download and install all of them, but they are useful in the long run.

In the following, we document how data exported from the [Gorilla.sc](https://gorilla.sc/) projects used to run this experiment are processed and stored to be used in the [Blursday shinyapp](https://dnacombo.shinyapps.io/Blursday/).

## Gorilla nomenclature

These are the terms defined in the Gorilla.sc environment used here:

- **Experiment ID, Country and Session**: a generic and unique number is attributed to each Project run on Gorilla.sc. In our case, it corresponds to a given Country and Session. Some countries have used several projects to account for slight changes in settings, so **Task 1** of this program is to maintain the correspondence between Experiment ID, Country, and Session. This is done in [this table](https://docs.google.com/spreadsheets/d/1p6_WHQXNGFw2EJGny1jb5qivMy2pJ_VRRYoDGRLxgbY).
- **NodeKeys and Unique Names**: a generic 4 letters string (a.k.a. NodeKey) is attributed to each task in a Project in Gorilla.sc. In our case, these NodeKeys correspond to individual tasks and questionnaires called with a "Unique Name". The mapping between each NodeKey and what Unique Name is ensured via [this table](https://docs.google.com/spreadsheets/d/1Mwy2aGCJ6vSpp4a32NOs83e2H73MQRFUOL_193yb8sQ).
- **Participant identification: Participant Private ID, Participant Public ID, and PID**: Gorilla assigns a Participant Private ID to each participant. This number is unique to each participant, and differs across sessions. We used the following strategy to track participants across sessions. First, we asked participants to choose a Public ID in order to identify themselves in the system and collected this information, along with an email address in a specific questionnaire whose Unique Name is "S1_Welcome", "S2_Welcome" and so on for each session a given participant ran. In session 4, this questionnaire was called "S4_ParticipantTracker". These questionnaires' data is never made available in the public database. Note that the participant public ID can be retrieved alongside the data in Gorilla, but we chose against this alternative and used our specific Welcome questionnaire for safety, to avoid having to export the data in unblinded mode.
- **Experiment Versions**: Changes to an experimental protocol *while participants are running* a project experiment creates a new numbered version of the experiment. Different versions of an experiment are subsequently merged in a single data file (though version numbers are tracked for tracability).

## Data preprocessing

Minimal preprocessing was done on the data using scripts from this repository.

### `DataUnzip.R`

1. Data was downloaded from Gorilla.sc using instructions [here](https://support.gorilla.sc/support/reference/faq/metrics) with the following options: File type: "csv (Comma)", Blinding: "Blind", TimeFrame: "All", Form: "Long Form".
2. The collected zip files were unzipped.
3. Files were renamed to use Unique Names instead of KeyNodes.
4. Experiment versions were merged by concatenating files across versions.

### `DataPIDmatch_XX.Rmd`

Where XX represents a Country code used in Column A of [this table](https://docs.google.com/spreadsheets/d/1p6_WHQXNGFw2EJGny1jb5qivMy2pJ_VRRYoDGRLxgbY).

1. For a given Country (i.e. a row in [this table](https://docs.google.com/spreadsheets/d/1p6_WHQXNGFw2EJGny1jb5qivMy2pJ_VRRYoDGRLxgbY))
2. Read all "Welcome" questionnaires across sessions, retrieve all Participant Private ID, and corresponding public ID, and email address.
3. Consider that the same person was running the experiment wherever email or public ID match across sessions.
4. Create an extra PID column in the data equal to the first Participant Private ID a given participant appeared under.
5. NB: To catch typos in public ID and emails, we chose to assign participants to the same PID whenever email _or_ public ID matched. In the rare event that this procedure led to obvious misattributions (e.g. different persons using the same email...), we fixed the data by hand.

The table below shows how the automated PID attribution worked: Two participants came to session 1, 2, 3, and 4. They sometimes misspelled their public ID or their email, but because they never misspelled both at the same time in a given session, they were assigned with a constant PID across sessions.

| Participant Private ID         | Session     | publid ID | email | PID |
|--------------|-----------|------------|---|---|
| 123456 | S1   | KingOfTheBongo        | King@bongobong.com |123456|
| 929292 | S1   | JohnnyJoe        | johnny@joe.com |929292|
| 850463 | S2   | JohnnyJoe        | KingJohnny@joe.com |929292|
| 783498 | S2   | KingOfBongo        | King@bongobong.com |123456|
| 378302 | S3   | KingOfTheBongo        | Queen@bongobong.com |123456|
| 017454 | S3   | JohnyJoe        | johnny@joe.com |929292|
| 920271 | S4   | JohnnyJoe        | johnny@joe.com |929292|
| 383838 | S4   | KingOfTheBongo        | King@bongobong.com |123456|

This participant tracking algorithm was used for each country separately.

### `DataTranslate.R`: preparing for response translation

Some responses were provided as free text by participants. To ensure our ability to map responses such as "00 horas 30 minutos 00 segundos", "30 dakika 15 saniye", "00 ωρες 18 λεπτα", or "j'étais très concentrée et un peu perdu la notion de l'heure, je dirais 30 à 35 mn" ("I was very focused and lost track of time, I would say 30 to 35 mn") to their respective durations, this script collects all unique responses to all questions in all countries. We then copy-paste the content of these tables to online spreadsheets and ask native speakers to translate to the best of their knowledge what was meant by the participants in [this table](https://docs.google.com/spreadsheets/d/1YOZ_3MMdo7ghgdIyhxgWqE8WYsB_wFihlEAVhODkz7Q) for questionnaires, and [this table](https://docs.google.com/spreadsheets/d/16pewaHuHCu8YStxHvF9Nis_RZOB3hT4utYLm5T9DSS4) for tasks. 

Following this, the `QTranslate()` and `TTranslate()` functions in `helpers.R` automatically translate all Questionnaire and Task responses, respectively for a given dataset passed as argument.

### `db_update.R`: saving data in RData format

The data was then saved from the .csv files processed above to .RData files in a data directory subsequently accessible by the _Blursday_ server (see below).

### `Anonymize.R`: discarding any identifiable data

Demographics questionnaires and Welcome questionnaires contained sensitive information (emails, public ID, date of birth), which were removed from any publicly available data. Emails and public IDs were simply removed, date of birth was translated to the Age (in years with one decimal) at the time the Demographics questionnaire was passed by the participant.

### `Daily_login_times.R`: extracting login times

We extracted the task performed by each participant each day in order to measure the time spent between this first task and any given later point in time. This is especially useful to compute Clock Duration in the `Questionnaire_RetroDur.Rmd` script (see below).

### `SubjectiveConfinementIndices.R`

We extracted responses to the UCLA questionnaire and computed the subjective confinement index at the time the questionnaire was run, in order to be able to associate this measure as a covariate in any task run by a given participant. This association was performed later at each instant of any event in a dataset with the `add_SubjectiveConfinementIndices()` function of the `add_covariates.R` script.

### `SubjectiveConfinementDuration.R`

We extracted responses to the question "As of today, for how long have you been confined (in days)?" (`ConfDuration`) of the Confinement Tracker questionnaire in each session. We computed the subjective time elapsed between each date (`DeltaConfDuration` the difference in `ConfDuration`) as the difference in response to the question on each occurrence of the questionnaire. We then associated this duration with the date at which the questionnaire was filled (`TimeQuestionnaire` and associated `DeltaTimeQuestionnaire`), and computed the slope `DeltaConfDuration / DeltaTimeQuestionnaire` to determine how many days one actual day passed was subjectively reported by participants. This slope was stored, and later applied to each instant of any event in a dataset to compute the subjective confinement duration at that time `add_SubjectiveConfinementDuration()` function in the `add_covariates.R` script.

## Download data

Download the data from the _Blursday_ data server online at https://timesocialdistancing.shinyapps.io/Blursday/

