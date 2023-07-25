# kthcorpus

## kthcorpus 0.3.2

* add fcns for guessing author identifiers when importing MODS to DiVA
* add fcns for getting conference info from Scopus and using it in MODS generation
* various minor fixes
* change checks report to include non quality controlled items (fresh/old)

## kthcorpus 0.3.1

* add fcn for retrieving and uploading project data to object storage

## kthcorpus 0.3.0

* add fcns for generating MODS for DiVA
* add fcns to transform scopus data to DiVA MODS
* add report for ambiguous ORCiDs
* add report for DiVA MODS generation

## kthcorpus 0.2.9

* fix bad links for title column in check_invalid_use_ISBN() #37
* fix warning message related to diva_orcid_kth_upload fcn and its usage of dplyr::summarise() #36
* add a check for kthid-orcid relations with a cardinality of >1 #35 
* Windows-friendly upload functions visavi object storage #34
* possibility to not scope DiVA download of authors to KTHs orgid #33

## kthcorpus 0.2.8

* add check for invalid use of ISBNs (certain publication types should not have ISBNs)
* add function to retrieve detailed data from Scopus Extended Abstract API

## kthcorpus 0.2.7

* add check for manuscripts with identifiers
* add Year to various output tables

## kthcorpus 0.2.6

* modify check for missing ScopusID to check publications (not authors)
* add check for missing ORCID where these identifiers can be backfilled

## kthcorpus 0.2.5

* add scopus ratelimit quota function
* add some updates to meilisearch API calls when uploading data to the search index
* fix issue getting S3 data from specific buckets
* temporarily disabled priority queueing for DiVA downloads

## kthcorpus 0.2.4

* add scopus client for making calls against the Elsevier Scopus API when monitoring publications from KTH - Royal Institute of Technology - by default with load dates from the previous fortnight

## kthcorpus 0.2.3

* add a check for identifiers that contain unicode (for example invisible blankspace characters) and fix some warnings for link_*-functions for vector inputs
* only include publications which have "QC" in Notes field
* add user agent to parallel curl download call
* no longer read FridaLevel as double due to "deprecated".
* by default show swepub issues from the current year only
* change check for missing journal identifiers to exclude records with "No ISSN" in Notes field, and add some more exceptions in the check for multiplettes title
* fix link to terms in swepub checks

## kthcorpus 0.2.2

* Introduced diva_config() to be able to use non-default org for DiVA client
* Fix to accommodate changed response format from SwePub

## kthcorpus 0.2.1

* Changed check_multiplettes_title to catch more variations of "not duplicate with"

## kthcorpus 0.2.0

* Added function to scrape DiVA portal for units/organisations (en/sv)
* Dockerfile and Makefile moved to inst/docker and GHA adjusted for this
* Download of persons data from DiVA to remove need for time consuming name parsing
* Functions to upload and download from S3 buckets
* Bootstrap v 5 w latest DT and vertical scrolling fix
* Added checks for missing kthid, invalid ISSN, ISBN, orcid

## kthcorpus 0.1.0

* Refactored functions to remove non-ascii variable names due to Windows build issue, see https://developer.r-project.org/Blog/public/2020/05/02/utf-8-support-on-windows/
* Added GitHub Actions CI workflow
* Added function to retrieve SwePub checks data in tsv format
* Renamed the package to "kthcorpus" and put it on GitHub
* Added function to allow retrieval of data from S3-enabled object storage backend, given appropriate credentials present in local .Renviron
* Added neo4j bulk extract function to allow export of data for further processing using neo4j
* Added UKÄ reference data

## kthcorpus 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
