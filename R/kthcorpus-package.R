#' KTH APIs
#'
#' DiVA is a Swedish research publication database with national and
#' institutional portals available. This R package downloads and makes
#' data available to use from R.
#'
#' There is a national DiVA database portal and institutional database portals:
#'
#'  - [National DiVA portal](https://www.diva-portal.org/smash/searchad.jsf)
#'  - [Institutional DiVA portal for KTH Royal Institute of Technology](https://kth.diva-portal.org/smash/search.jsf)
#' @name kthcorpus
#' @docType package
#' @keywords internal
"_PACKAGE"

# needed for use of . in magrittr pipelines
utils::globalVariables(
  c(
    ".", "description.en", "Name", "PID", "extorg",
    "is_external", "is_remappable_extorg", "kthid", "n2", "n_commas",
    "n_pid", "name", "orcid", "orcid.x", "orcid.y", "orgids", "pids", "pubs",
    "DOI", "ISI", "JournalEISSN", "JournalISSN", "Notes", "PublicationDate",
    "PublicationType", "Status", "Title", "has_notes", "is_QSorNQC", "is_not_published",
    "emp_beg", "emp_degree", "emp_end", "emp_lastmod", "LastModified",
    "mid", "Key", "check_key", "initials", "last_name"
  )
)
NULL

#print_global_vars <- function(undef_global_vars)
#  cat(paste(collapse = ", ", sprintf("\"%s\"", unlist(strsplit(undef_global_vars, " ")))))

#' @title Research Subject Areas from https://uka.se
#' @description Research Subject Areas have codes, descriptions and are arranged
#' in three levels, 6 at level 1, 42 at level 2, 260 at level 3 (5 digit codes)
#' See \url{https://www.uka.se/statistik--analys/information-om-statistiken/amneslistor-och-huvudomraden/2017-02-14-forskningsamnen.html}
#' @format A data frame with 308 rows and 4 variables:
#' \describe{
#'   \item{\code{id}}{integer subject area code}
#'   \item{\code{level}}{double a level, granularity for code (1..3)}
#'   \item{\code{swe}}{character description in Swedish}
#'   \item{\code{eng}}{character description in English}
#'}
#' @details Data from \url{https://www.uka.se/download/18.7391c377159bc0155b81ef8/1487841861615/forskningsamnen-standard-2011.xlsx}
"research_areas"

#' @title Employment Titles from Statistics Sweden
#' @description Employment Titles have codes and descriptions
#' @format A data frame with 8234 rows and 4 variables:
#' \describe{
#'   \item{\code{id}}{character code for employment title}
#'   \item{\code{desc_swe}}{character description in Swedish}
#'   \item{\code{cat_desc}}{character staff category}
#'   \item{\code{is_uf_ta}}{character UF denotes educational or research title, TA denotes technical or administrative}
#'}
#' @details See \url{https://anstallningsbenamningar.scb.se/}
"ss_employment_title"

#' @title Employment Titles from KTH
#' @description Employment Titles have codes and descriptions in English and Swedish
#' @format A data frame with 61 rows and 5 variables:
#' \describe{
#'   \item{\code{emp_code_hr}}{character code for employment title}
#'   \item{\code{emp_desc}}{character description in uppercase Swedish}
#'   \item{\code{title_sv}}{character title in mixed case Swedish}
#'   \item{\code{title_en}}{character title in mixed case English}
#'   \item{\code{title_comment}}{character comment about this category of employees}
#'}
"kth_employment_title"

#' @title Employment Titles from KTH according to BESTA
#' @description Employment Titles have BESTA codes and descriptions in English and Swedish
#' @format A data frame with 53 rows and 3 variables:
#' \describe{
#'   \item{\code{emp_code}}{character code for employment title}
#'   \item{\code{emp_title_sv}}{character title in mixed case Swedish}
#'   \item{\code{emp_title_en}}{character title in mixed case English}
#'}
"kth_besta"

#' @title DiVA unit identifiers for KTH with Swedish and English translations
#' @description In DiVA, organizational units have identifiers. This mapping table
#' maps these units in DiVA to abbreviated unit codes used within KTH and provides
#' translations in English and Swedish, as well as organization level with higher
#' numbers representing a deeper nesting within the organizational structure (0 = root)
#' @format A data frame with 160 rows and 6 variables:
#' \describe{
#'   \item{\code{orgid}}{integer representing DiVA organizational unit id}
#'   \item{\code{unit_abbr}}{character KTH organizational abbreviation code}
#'   \item{\code{level}}{integer representing node level where 0 is the root node}
#'   \item{\code{unit_en}}{character title in mixed case English}
#'   \item{\code{unit_sv}}{character title in mixed case Swedish}
#'   \item{\code{is_closed}}{logical denoting if mapping is no longer in use}
#'}
"unit_names"

#' @title HR mapping
#' @description Field mappings used in HR data extract
#' @format A data frame with 14 rows and 5 variables:
#' \describe{
#'   \item{\code{colname}}{character lowersnakecase column name}
#'   \item{\code{export}}{character export table non-ascii name}
#'   \item{\code{name}}{character Name of column}
#'   \item{\code{table}}{character Source table}
#'   \item{\code{field}}{character Source field}
#'}
"hr_mapping"


#' @title Mappings for check data
#' @description Field mappings for the check data used for column names etc
#' \describe{
#'   \item{\code{colname_en}}{character lowersnakecase column name}
#'   \item{\code{desc_swe}}{character Swedish language explanation for field}
#'}
"check_mapping"

#' @title Web of Science tags and field names
#' @description A curated set of WoS tags and suggested field names, explanations.
#' @format A data frame with 101 rows and 4 variables:
#' \describe{
#'   \item{\code{field}}{character suggested field name using snake case}
#'   \item{\code{tag}}{character short tag or field abbreviation used in WoS}
#'   \item{\code{tag_long}}{character a slightly longer explanation}
#'   \item{\code{tag_set}}{character the kind of tag, a grouping}
#'}
"wos_tag_fields"

#' @title HR24 (Visma) lookup table
#' @description A lookup table for employee codes and corresponding employee descriptions
#' and titles
#' @format A data frame with 65 rows and 4 variables:
#' \describe{
#'   \item{\code{emp_code}}{character the 5-digit employee code}
#'   \item{\code{emp_desc}}{character role description in Swedish}
#'   \item{\code{title_sv}}{character title in Swedish}
#'   \item{\code{title_en}}{character title in English}
#'   \item{\code{cat_desc}}{character category description in Swedish}
#'   \item{\code{is_uf_ta}}{character notes if role is UF or TA}
#'}
"hr24_employment_title"

#' @title SCB/UKÄ research topics
#' @description Research topics from UKÄ/SCB, 2025 version
#' @format A data frame with 345 rows and 6 variables:
#' \describe{
#'   \item{\code{id}}{integer An id with 1, 3 or 5 digits}
#'   \item{\code{Swedish}}{character Swedish name of topic}
#'   \item{\code{English}}{character English name of topic}
#'   \item{\code{level}}{integer Level of topic 1-3}
#'   \item{\code{Swe_comment}}{character Extra information in Swedish if available}
#'   \item{\code{Eng_comment}}{character Extra information in English if available}
#'}
"scb_topics"
