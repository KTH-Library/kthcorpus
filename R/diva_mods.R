#' Read a MODS file from DiVA given a DiVA publication identifier
#' @param pid DiVA publication identifier
#' @export
#' @importFrom httr GET status_code content
read_diva_mods <- function(pid) {

  mods_url <- function(x) sprintf(paste0(
    "https://kth.diva-portal.org/smash/references",
    "?referenceFormat=MODS&pids=[diva2:%s]&fileName=export.xml"
  ), x)

  res <- httr::GET(mods_url(pid))

  if (httr::status_code(res) != 200)
    return (NA_character_)

  httr::content(res, type = "text/xml", encoding = "UTF-8") |>
    as.character()
}

#' @importFrom xml2relational toRelational
to_relational <- function(mods) {
  mods %>% xml2relational::toRelational(keys.dim = 10)
}

# functions to get DiVA MODS formatted records from the DiVA-portal
# and flat "CSVALL2" format

read_diva_mods_old <- function(pid) {

  mods_url <- function(x) sprintf(paste0(
    "https://kth.diva-portal.org/smash/references",
    "?referenceFormat=MODS&pids=[diva2:%s]&fileName=export.xml"
  ), x)

  httr::content(httr::GET(mods_url(pid))) %>% xml2::as_list()
}

#' Read rectangular data from DiVA given a specific identifier
#' @param pid the DiVA publication identifier
#' @export
#' @importFrom httr GET
read_diva_csvall2 <- function(pid) {

  csv_url <- function(x) sprintf(paste0(
    "https://kth.diva-portal.org/smash/references",
    "?referenceFormat=CSVALL2&pids=[diva2:%s]&fileName=export.csv"
  ), x)

  httr::content(httr::GET(csv_url(pid)), show_col_types = FALSE)

}

# retrieve lists with some of the enums/lookups used in DiVA MODS format
read_diva_mods_lists <- function() {

  urls <-
    sprintf("https://wiki.epc.ub.uu.se/download/attachments/86082771/%s", c(
      "Funder.csv?version=4&modificationDate=1635168949009&api=v2",
      "H%C3%B6gskolepo%C3%A4ng.csv?version=1&modificationDate=1627030653157&api=v2",
      "Uppsatsniv%C3%A5.csv?version=2&modificationDate=1631543033423&api=v2",
      "Utf%C3%A4rdande%20l%C3%A4ros%C3%A4te.csv?version=1&modificationDate=1626958182874&api=v2"
    ))

  res <-
    urls %>%
    purrr:: map(function(x) httr::content(httr::GET(x)))

  return(res)

}

printxml <- function(xml)
  readr::write_lines(file = stdout(), as.character(xml))

lu_wos_pubtype <- function() {

    wos_pubtype <- list()

    wos_pubtype['Article'] = 'article';
    wos_pubtype['Proceedings Paper'] = 'conferencePaper';
    wos_pubtype['Conference Paper'] = 'conferencePaper';
    wos_pubtype['Editorial Material'] = 'article';
    wos_pubtype['Meeting Abstract'] = 'article';
    wos_pubtype['Book Review'] = 'bookReview';
    wos_pubtype['Letter'] = 'articleLetter';
    wos_pubtype['Review'] = 'article';
    wos_pubtype['Correction'] = 'articleLetter';
    wos_pubtype['Erratum'] = 'articleErratum';
    wos_pubtype['News Item'] = 'other';
    wos_pubtype['Biographical-Item'] = 'articleOther';
    wos_pubtype['Discussion'] = 'other';
    wos_pubtype['Note'] = 'editorialMaterial';
    wos_pubtype['Item About an Individual'] = 'other';
    wos_pubtype['Abstract of Published Item'] = 'other';
    wos_pubtype['Art Exhibit Review'] = 'other';
    wos_pubtype['Bibliography'] = 'other';
    wos_pubtype['Book'] = 'book';
    wos_pubtype['Book Chapter'] = 'chapter';
    wos_pubtype['Book Review'] = 'bookReview';
    wos_pubtype['Chronology'] = 'other';
    wos_pubtype['Correction'] = 'article';
    wos_pubtype['Correction, Addition'] = 'article';
    wos_pubtype['Dance Performance Review'] = 'other';
    wos_pubtype['Data Paper'] = 'article';
    wos_pubtype['Database Review'] = 'review';
    wos_pubtype['Discussion'] = 'article';
    wos_pubtype['Early Access (Web of Science Core Collection only)'] = 'other';
    wos_pubtype['Editorial Material'] = 'editorialMaterial';
    wos_pubtype['Editorial'] = 'editorialMaterial';
    wos_pubtype['Excerpt'] = 'other';
    wos_pubtype['Fiction, Creative Prose'] = 'other';
    wos_pubtype['Film Review'] = 'other';
    wos_pubtype['Hardware Review'] = 'other';
    wos_pubtype['Item About an Individual'] = 'other';
    wos_pubtype['Meeting Abstract'] = 'meetingAbstract';
    wos_pubtype['Meeting Summary'] = 'other';
    wos_pubtype['Music Performance Review'] = 'other';
    wos_pubtype['Music Score'] = 'other';
    wos_pubtype['Music Score Review'] = 'other';
    wos_pubtype['Poetry'] = 'other';
    wos_pubtype['Record Review'] = 'other';
    wos_pubtype['Reprint'] = 'other';
    wos_pubtype['Retracted Publication'] = 'article';
    wos_pubtype['Retraction'] = 'article';
    wos_pubtype['Script'] = 'other';
    wos_pubtype['Software Review'] = 'review';
    wos_pubtype['TV Review, Radio Review'] = 'review';
    wos_pubtype['TV Review, Radio Review, Video'] = 'review';
    wos_pubtype['Theater Review'] = 'review';

  wos_pubtype |> as.vector()

}

lu_genre <- function() {

    genre <- list()

    genre$publicationType['article'] <- '
        <genre authority="diva" type="contentTypeCode">referee</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['articleOther'] <- '
        <genre authority="diva" type="contentTypeCode">other</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['articleLetter'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>

        <genre authority="diva" type="publicationSubTypeCode">letter</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['articleErratum'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['editorialMaterial'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="diva" type="publicationSubTypeCode">editorialMaterial</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['meetingAbstract'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="diva" type="publicationSubTypeCode">meetingAbstract</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    # NB: contentTypeCode changed from science to refereed
    genre$publicationType['conferencePaper'] <- '
        <genre authority="diva" type="contentTypeCode">refereed</genre>
        <genre authority="diva" type="publicationTypeCode">conferencePaper</genre>
        <genre authority="svep" type="publicationType">kon</genre>
        <genre authority="diva" type="publicationType" lang="eng">Conference paper</genre>
        <genre authority="kev" type="publicationType" lang="eng">proceeding</genre>'

    genre$publicationType['chapter'] <- '
       <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">chapter</genre>
        <genre authority="svep" type="publicationType">kap</genre>
        <genre authority="diva" type="publicationType" lang="eng">Chapter in book</genre>
        <genre authority="kev" type="publicationType" lang="eng">bookitem</genre>'

    genre$publicationType['review'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">review</genre>
        <genre authority="svep" type="publicationType">for</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article, review/survey</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['report'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">report</genre>
        <genre authority="svep" type="publicationType">rap</genre>
        <genre authority="diva" type="publicationType" lang="eng">Report</genre>
        <genre authority="kev" type="publicationType" lang="eng">book</genre>'

    genre$publicationType['patent'] <- '
        <genre authority="diva" type="publicationTypeCode">patent</genre>
        <genre authority="svep" type="publicationType">pat</genre>
        <genre authority="diva" type="publicationType" lang="eng">Patent</genre>
        <genre authority="kev" type="publicationType" lang="eng">patent</genre>'

    genre$publicationType['other'] <- '
        <genre authority="diva" type="contentTypeCode">other</genre>
        <genre authority="diva" type="publicationTypeCode">other</genre>
        <genre authority="svep" type="publicationType">ovr</genre>
        <genre authority="diva" type="publicationType" lang="eng">Other</genre>'

    genre$publicationType['manuscript'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">manuscript</genre>
        <genre authority="svep" type="publicationType">ovr</genre>
        <genre authority="diva" type="publicationType" lang="eng">Manuscript (preprint)</genre>
        <genre authority="kev" type="publicationType" lang="eng">preprint</genre>'

    genre$publicationType['dataset'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">dataset</genre>
        <genre authority="diva" type="publicationType" lang="eng">Data set</genre>
        <genre authority="kev" type="publicationType" lang="eng"/>'

    genre$publicationType['conferenceProceedings'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">conferenceProceedings</genre>
        <genre authority="svep" type="publicationType">pro</genre>
        <genre authority="diva" type="publicationType" lang="eng">Conference proceedings (editor)</genre>
        <genre authority="kev" type="publicationType" lang="eng">conference</genre>'

    genre$publicationType['collection'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">collection</genre>
        <genre authority="svep" type="publicationType">sam</genre>
        <genre authority="diva" type="publicationType" lang="eng">Collection (editor)</genre>
        <genre authority="kev" type="publicationType" lang="eng">book</genre>'

    genre$publicationType['bookReview'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">bookReview</genre>
        <genre authority="svep" type="publicationType">rec</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article, book review</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['book'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">book</genre>
        <genre authority="svep" type="publicationType">bok</genre>
        <genre authority="diva" type="publicationType" lang="eng">Book</genre>
        <genre authority="kev" type="publicationType" lang="eng">book</genre>'

    genre$publicationType['artisticOutput'] <- '
        <genre authority="diva" type="publicationTypeCode">artisticOutput</genre>
        <genre authority="diva" type="publicationType" lang="eng">Artistic output</genre>
        <genre authority="kev" type="publicationType" lang="eng"/>
        <genre authority="svep" type="publicationType">kfa</genre>'

    genre |> as.vector()
}

lu_keywords <- function(keywords) {
  subjects <- sprintf("<subject lang=\"eng\"><topic>%s</topic></subject", keywords)
  paste0(collapse = "\n", subjects)
}

lu_diva_mods_xml <- function(params) {

    #$pubType, $authors, $titleEng, $publisher, $publication_year, $doi, $isi, $pmid,
    #$scopus, $abstract, $journalTitle, $issn, $eissn, $isbn, $volume, $issue,
    #$starting_page, $ending_page, $keyword, $art_no, $note, $ResearchSubject, $is_early_access

    if (params$is_early_access) {
        noteString <- "<note type=\"publicationStatus\" lang=\"eng\">Epub ahead of print</note>";
    }

    params$publicationTypeCode <- unname(lu_genre()$publicationType[unlist(unname(lu_wos_pubtype()[params$pubType]))])

    params$authorsString <- params$authors
    params$ResearchSubjectString <- params$ResearchSubject
    params$keywordString <- ""
    params$noteString <- ""

    template <- with(params, paste0("<mods xmlns=\"http://www.loc.gov/mods/v3\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
                  xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"3.7\"
                  xsi:schemaLocation=\"http://www.loc.gov/mods/v3 http://www.loc.gov/standards/mods/v3/mods-3-7.xsd\">
                ", publicationTypeCode, "
                ", authorsString, "
                <titleInfo lang=\"eng\">
                    <title>", titleEng, "</title>
                </titleInfo>
                <language>
                    <languageTerm type=\"code\" authority=\"iso639-2b\">eng</languageTerm>
                </language>
                <originInfo>
                    <publisher>", publisher, "</publisher>
                    <dateIssued>", publication_year, "</dateIssued>
                </originInfo>
                <physicalDescription>
                    <form authority=\"marcform\">print</form>
                </physicalDescription>
                <identifier type=\"doi\">", doi, "</identifier>
                <identifier type=\"pmid\">", pmid, "</identifier>
                <identifier type=\"isi\">", isi, "</identifier>
                <identifier type=\"scopus\">", scopus, "</identifier>
                <identifier type=\"articleId\">", art_no, "</identifier>
                <typeOfResource>text</typeOfResource>", keywordString, ResearchSubjectString,
        "<abstract lang=\"eng\">", "'", "&apos;", abstract, "</abstract>
            <note>", note, "</note>", noteString, "
                <relatedItem type=\"host\">
                    <titleInfo>
                        <title>", journalTitle, "</title>
                    </titleInfo>
                    <identifier type=\"issn\">", issn, "</identifier>
                    <identifier type=\"eissn\">", eissn, "</identifier>
                    <identifier type=\"isbn\">", isbn, "</identifier>
                    <part>
                        <detail type=\"volume\">
                            <number>", volume, "</number>
                        </detail>
                        <detail type=\"issue\">
                            <number>", issue, "</number>
                        </detail>
                        <extent>
                            <start>", starting_page, "</start>
                            <end>", ending_page, "</end>
                        </extent>
                    </part>
                </relatedItem>
            </mods>"))

    return(template)
}

# xml fragments

diva_publication_types <- readr::read_lines(trimws("
article
review
bookReview
book
dataset
dissertation
monographDoctoralThesis
comprehensiveDoctoralThesis
chapter
conferencePaper
monographLicentiateThesis
comprehensiveLicentiateThesis
manuscript
patent
conferenceProceedings
report
collection
studentThesis
other"))

frag_genre_orig <- function(publication_type) {
  if (!publication_type %in% diva_publication_types)
    stop("Publication type not in diva_publication_types")
  glue::glue('<genre authority="diva" type="publicationTypeCode">{publication_type}</genre>')
}

#' @importFrom tibble tibble
frag_genre <- function(pubtype) {

  genre <- genre_id <- NULL

  g <- lu_genre()$publicationType

  tibble::tibble(genre_id = names(g), genre = unlist(unname(g))) %>%
    dplyr::filter(genre_id == pubtype) %>%
    dplyr::pull(genre)

}

kth_genre <- function() {

    genre <- list()

    genre$publicationType['article'] <- '
        <genre authority="diva" type="contentTypeCode">referee</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['articleOther'] <- '
        <genre authority="diva" type="contentTypeCode">other</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['articleLetter'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="diva" type="publicationSubTypeCode">letter</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    # NB: used for errata and combo "Journal", "Note"
    genre$publicationType['articleErratum'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    # NB: added to cover combo "Journal", "Conference Paper"
    genre$publicationType['articleConferencePaper'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="diva" type="publicationSubTypeCode">meetingAbstract</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['editorialMaterial'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="diva" type="publicationSubTypeCode">editorialMaterial</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['meetingAbstract'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">article</genre>
        <genre authority="diva" type="publicationSubTypeCode">meetingAbstract</genre>
        <genre authority="svep" type="publicationType">art</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article in journal</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    # NB: contentTypeCode changed from science to refereed
    genre$publicationType['conferencePaper'] <- '
        <genre authority="diva" type="contentTypeCode">refereed</genre>
        <genre authority="diva" type="publicationTypeCode">conferencePaper</genre>
        <genre authority="svep" type="publicationType">kon</genre>
        <genre authority="diva" type="publicationType" lang="eng">Conference paper</genre>
        <genre authority="kev" type="publicationType" lang="eng">proceeding</genre>'

    # NB: added for Scopus combo "Conference Proceedings", "Conference Paper"
    genre$publicationType['conferencePaperPublished'] <- '
        <genre authority="diva" type="contentTypeCode">refereed</genre>
        <genre authority="diva" type="publicationTypeCode">conferencePaper</genre>
        <genre authority="diva" type="publicationSubTypeCode">publishedPaper</genre>
        <genre authority="svep" type="publicationType">kon</genre>
        <genre authority="diva" type="publicationType" lang="eng">Conference paper</genre>
        <genre authority="kev" type="publicationType" lang="eng">proceeding</genre>'

    genre$publicationType['chapter'] <- '
       <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">chapter</genre>
        <genre authority="svep" type="publicationType">kap</genre>
        <genre authority="diva" type="publicationType" lang="eng">Chapter in book</genre>
        <genre authority="kev" type="publicationType" lang="eng">bookitem</genre>'

    genre$publicationType['review'] <- '
        <genre authority="diva" type="contentTypeCode">refereed</genre>
        <genre authority="diva" type="publicationTypeCode">review</genre>
        <genre authority="svep" type="publicationType">for</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article, review/survey</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['report'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">report</genre>
        <genre authority="svep" type="publicationType">rap</genre>
        <genre authority="diva" type="publicationType" lang="eng">Report</genre>
        <genre authority="kev" type="publicationType" lang="eng">book</genre>'

    genre$publicationType['patent'] <- '
        <genre authority="diva" type="publicationTypeCode">patent</genre>
        <genre authority="svep" type="publicationType">pat</genre>
        <genre authority="diva" type="publicationType" lang="eng">Patent</genre>
        <genre authority="kev" type="publicationType" lang="eng">patent</genre>'

    genre$publicationType['other'] <- '
        <genre authority="diva" type="contentTypeCode">other</genre>
        <genre authority="diva" type="publicationTypeCode">other</genre>
        <genre authority="svep" type="publicationType">ovr</genre>
        <genre authority="diva" type="publicationType" lang="eng">Other</genre>'

    genre$publicationType['manuscript'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">manuscript</genre>
        <genre authority="svep" type="publicationType">ovr</genre>
        <genre authority="diva" type="publicationType" lang="eng">Manuscript (preprint)</genre>
        <genre authority="kev" type="publicationType" lang="eng">preprint</genre>'

    genre$publicationType['dataset'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">dataset</genre>
        <genre authority="diva" type="publicationType" lang="eng">Data set</genre>
        <genre authority="kev" type="publicationType" lang="eng"/>'

    genre$publicationType['conferenceProceedings'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">conferenceProceedings</genre>
        <genre authority="svep" type="publicationType">pro</genre>
        <genre authority="diva" type="publicationType" lang="eng">Conference proceedings (editor)</genre>
        <genre authority="kev" type="publicationType" lang="eng">conference</genre>'

    genre$publicationType['collection'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">collection</genre>
        <genre authority="svep" type="publicationType">sam</genre>
        <genre authority="diva" type="publicationType" lang="eng">Collection (editor)</genre>
        <genre authority="kev" type="publicationType" lang="eng">book</genre>'

    genre$publicationType['bookReview'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">bookReview</genre>
        <genre authority="svep" type="publicationType">rec</genre>
        <genre authority="diva" type="publicationType" lang="eng">Article, book review</genre>
        <genre authority="kev" type="publicationType" lang="eng">article</genre>'

    genre$publicationType['book'] <- '
        <genre authority="diva" type="contentTypeCode">science</genre>
        <genre authority="diva" type="publicationTypeCode">book</genre>
        <genre authority="svep" type="publicationType">bok</genre>
        <genre authority="diva" type="publicationType" lang="eng">Book</genre>
        <genre authority="kev" type="publicationType" lang="eng">book</genre>'

    genre$publicationType['artisticOutput'] <- '
        <genre authority="diva" type="publicationTypeCode">artisticOutput</genre>
        <genre authority="diva" type="publicationType" lang="eng">Artistic output</genre>
        <genre authority="kev" type="publicationType" lang="eng"/>
        <genre authority="svep" type="publicationType">kfa</genre>'

    genre |> as.vector()
}

genre_scopus_diva <- function() {

  #TODO: add Conference Proceeding\tBook Chapter\t????

  combos <- "prism:aggregationType	subtype	key
Journal	Article	article
Conference Proceeding	Conference Paper	conferencePaperPublished
Journal	Review	review
Book	Book Chapter	chapter
Journal	Conference Paper	articleConferencePaper
Book Series	Book Chapter	chapter
Book Series\tEditorial\tchapter
Conference Proceeding	Editorial	conferenceProceedings
Journal	Note	article
Book	Book	book
Book	Editorial	chapter
Journal	Editorial	editorialMaterial
Journal	Letter	articleLetter
Trade Journal	Article	articleOther
Book Series\tConference Paper\tconferencePaperPublished
Journal\tData Paper\tarticle
Journal\tErratum\tarticleErratum
Journal\tShort Survey\tarticle
Conference Proceeding\tBook Chapter\tconferencePaperPublished
Book Series\tNote\tchapter
" |> readr::read_tsv(show_col_types = FALSE)

  return (combos)

}

frag_genre2 <- function(aggregationType, subtypeDescription) {

  `prism:aggregationType` <- subtype <- NULL

  combos <- genre_scopus_diva()

  key <-
    combos |>
    filter(`prism:aggregationType` == aggregationType, subtype == subtypeDescription) |>
    pull(key)

  res <- kth_genre()$publicationType[key]

  if (length(res) == 0)
    kth_genre()$publicationType["other"]

  return (res)

}

frag_affiliation <- function(affiliation) {
  glue::glue("<affiliation>{affiliation}</affiliation>")
}

frag_description <- function(description) {
  glue::glue("<description>{description}</description>")
}

frag_name_personal <- function(kthid, source, family, given, role, affiliations, descriptions) {

  footer <- ""

  if (!missing(affiliations) && !is.null(affiliations) && !is.na(affiliations)) {
    f_aff <- affiliations %>% purrr::map_chr(frag_affiliation)  %>% paste0(collapse = "\n")
    footer <- paste0(footer, "\n{f_aff}")
  }

  if (!(missing(descriptions)) && !is.null(descriptions) && !is.na(descriptions)) {
    f_desc <- descriptions %>% purrr::map_chr(frag_description) %>% paste0(collapse = "\n")
    footer <- paste0(footer, "\n{f_desc}\n")
  }

  glue::glue('<name type="personal" authority="{source}" xlink:href="{kthid}">
  <namePart type="family">{family}</namePart>
  <namePart type="given">{given}</namePart>
  <role>
    <roleTerm type="code" authority="marcrelator">{role}</roleTerm>
  </role>',
  footer,
  '</name>', .na = "")
}

frag_namepart <- function(namepart) {
  glue::glue("<namePart>{namepart}</namePart>")
}

frag_name_organization <- function(source = "kth", orgid, nameparts) {

  np <- ""

  if (!missing(nameparts)) {
    np <- nameparts %>% purrr::map_chr(frag_namepart) %>% paste0(collapse = "\n")
    footer <- "{np}"
  }

  glue::glue(paste0('<name type="corporate" authority="{source}" xlink:href="{orgid}">
    ', footer, '
    <role>
      <roleTerm type="code" authority="marcrelator">pbl</roleTerm>
    </role>
    </name>'))
}


frag_name_researchgroup <- function(researcher, researchgroup = "Research Group") {
  glue::glue('<name type="corporate">
    <namePart>{researcher}</namePart>
    <role>
      <roleTerm type="code" authority="marcrelator">oth</roleTerm>
    </role>
    <description>{researchgroup}</description>
    </name>')
}

frag_abstract <- function(abstract, lang) {
  glue::glue('<abstract lang="lang">{abstract}</abstract>')
}

frag_titleInfo <- function(title, subtitle, lang = "eng", alternative = FALSE) {

  if (!(lang %in% diva_languages))
    stop("Not valid ISO3 code for the language")

  header <- '<titleInfo lang="{lang}">
  <title>{title}</title>'

  if (!missing(subtitle))
    header <- paste0(header, "<subTitle>{subtitle}</subTitle>")

  if (alternative)
    header <- '<titleInfo type="alternative", lang="{lang}">'

  glue::glue(paste0(header, "
  </titleInfo>"))
}

frag_note <- function(note) {
  glue::glue('<note>{note}</note>')
}

frag_location <- function(url) {
  glue::glue('<location><url>{url}</url></location>')
}

frag_recordInfo <- function(kthid, source, creation, change, pid) {
  # TODO, what if missing values in params?
  glue::glue("<recordInfo>
    <recordOrigin>{kthid}</recordOrigin>
    <recordContentSource>{source}</recordContentSource>
    <recordCreationDate>{creation}</recordCreationDate>
    <recordChangeDate>{change}</recordChangeDate>
    <recordIdentifier>{pid}</recordIdentifier>
  </recordInfo>")

}

#' @importFrom rvest read_html html_table
#' @importFrom dplyr select mutate
#' @importFrom tidyr unnest_longer
read_diva_langs <- function() {

  iso <- NULL
  # this site may sometimes be down:
  # "https://www.loc.gov/standards/iso639-2/ISO-639-2_8859-1.txt"
  # so use wikipedia instead
  langs <-
    "https://en.wikipedia.org/wiki/List_of_ISO_639-2_codes" |>
    rvest::read_html() |> rvest::html_table() |> getElement(1)

  langs |> dplyr::select("iso" = 1, "desc" = 5) |>
    mutate(iso = strsplit(iso, " / ")) |>
    tidyr::unnest_longer(iso, values_to = "iso", transform = function(x) gsub("[*]$", "", x))
}

read_diva_langs_loc <- function() {
  # this site may sometimes be down:
  url <- "https://www.loc.gov/standards/iso639-2/ISO-639-2_8859-1.txt"
  # so use wikipedia instead

  langs <-
    readr::read_delim(url,
      col_names = c("iso3", "iso3_term", "iso2", "desc_en", "desc_fr"),
      delim = "|",
      locale = readr::locale(encoding = "ISO-8859-1"),
      show_col_types = FALSE
    )
  tibble::tibble(iso = langs$iso3, desc = langs$desc_en)
}

diva_languages <-
"aar abk ace ach ada ady afa afh afr ain aka akk ale alg alt amh ang anp apa ara arc arg arn arp art arw asm ast ath aus ava ave awa aym aze bad bai bak bal bam ban bas bat bej bel bem ben ber bho bih bik bin bis bla bnt bod tib bos bra bre btk bua bug bul byn cad cai car cat cau ceb cel ces cze cha chb che chg chk chm chn cho chp chr chu chv chy cmc cnr cop cor cos cpe cpf cpp cre crh crp csb cus cym wel dak dan dar day del den deu ger dgr din div doi dra dsb dua dum dyu dzo efi egy eka ell gre elx eng enm epo est eus baq ewe ewo fan fao fas per fat fij fil fin fiu fon fra fre frm fro frr frs fry ful fur gaa gay gba gem gez gil gla gle glg glv gmh goh gon gor got grb grc grn gsw guj gwi hai hat hau haw heb her hil him hin hit hmn hmo hrv hsb hun hup hye arm iba ibo ido iii ijo iku ile ilo ina inc ind ine inh ipk ira iro isl ice ita jav jbo jpn jpr jrb kaa kab kac kal kam kan kar kas kat geo kau kaw kaz kbd kha khi khm kho kik kin kir kmb kok kom kon kor kos kpe krc krl kro kru kua kum kur kut lad lah lam lao lat lav lez lim lin lit lol loz ltz lua lub lug lui lun luo lus mad mag mah mai mak mal man map mar mas mdf mdr men mga mic min mis mkd mac mkh mlg mlt mnc mni mno moh mon mos mri mao msa may mul mun mus mwl mwr mya bur myn myv nah nai nap nau nav nbl nde ndo nds nep new nia nic niu nld dut nno nob nog non nor nqo nso nub nwc nya nym nyn nyo nzi oci oji ori orm osa oss ota oto paa pag pal pam pan pap pau peo phi phn pli pol pon por pra pro pus qaa-qtz que raj rap rar roa roh rom ron rum run rup rus sad sag sah sai sal sam san sas sat scn sco sel sem sga sgn shn sid sin sio sit sla slk slo slv sma sme smi smj smn smo sms sna snd snk sog som son sot spa sqi alb srd srn srp srr ssa ssw suk sun sus sux swa swe syc syr tah tai tam tat tel tem ter tet tgk tgl tha tig tir tiv tkl tlh tli tmh tog ton tpi tsi tsn tso tuk tum tup tur tut tvl twi tyv udm uga uig ukr umb und urd uzb vai ven vie vol vot wak wal war was wen wln wol xal xho yao yap yid yor ypk zap zbl zen zgh zha zho chi znd zul zun zxx zza" |>
  strsplit(" ") |> unlist()

frag_subject <- function(lang = "eng", source, href, topic, genre) {

  footer <- '<subject lang="{lang}"'
  infix <- ""

  if (!missing(source))
    footer <- paste(footer, 'authority="{source}"')

  if (!missing(href))
    footer <- paste(footer, 'xlink:href="{href}"')

  if (!missing(genre))
    infix <-  '\n<genre>{genre}</genre>\n'

  topix <- paste0(collapse = "\n", sprintf("  <topic>%s</topic>", topic))

  glue::glue(paste0(footer, '>{topix}', infix, '</subject>'))
}

frag_physicalDescription <- function(desc = "text") {
    glue::glue('<physicalDescription>
      <form authority="marcform">{desc}</form>
    </physicalDescription>')
}

frag_identifier <- function(type, identifier, displayLabel) {
  template <- '<identifier type="{type}">{identifier}</identifier>'
  if (type == "isbn")
    template <- '<identifier type="{type}" displayLabel="{displayLabel}">{identifier}</identifier>'
  glue::glue(template, .na = "")
}

frag_typeOfResource <- function(resourcetype = "text") {
  glue::glue('<typeOfResource>{resourcetype}</typeOfResource>')
}

frag_abstract <- function(abstract, lang = "eng") {
  glue::glue('<abstract lang="{lang}">{abstract}</abstract>')
}

# conf_title <-> "sourcetitle"  #
# conf_ext_start <-> abstract[["abstracts-retrieval-response"]][["coredata"]][["prism:startingPage"]]
# conf_ext_end <-> abstract[["abstracts-retrieval-response"]][["coredata"]][["prism:endingPage"]]
# conf_details <-> "" # composite of confname, location and duration
frag_relatedItem_conference <- function(conf_title, conf_subtitle, conf_ext_start, conf_ext_end, conf_details) {
  glue::glue(.na = "", '<relatedItem type="host">
      <titleInfo>
        <title>{conf_title}</title>
        <subTitle>{conf_subtitle}</subTitle>
      </titleInfo>
      <part>
        <extent>
          <start>{conf_ext_start}</start>
          <end>{conf_ext_end}</end>
        </extent>
      </part>
    </relatedItem>
    <name type="conference">
      <namePart>{conf_details}</namePart>
    </name>')
}

blank <- function(x) ifelse(is_empty(x) || is.na(x), "", x)

frag_originInfo <- function(place = NULL, publisher = NULL, yearIssued = NULL,
  availableFrom = NULL, edition = NULL) {

  f_publisher <- glue("<publisher>{publisher}</publisher>") |> blank()
  f_place <- glue("<place><placeTerm>{place}</placeTerm></place>") |> blank()
  f_edition <- glue("<edition>{edition}</edition>") |> blank()
  f_yi <- glue("<dateIssued>{yearIssued}</dateIssued>") |> blank()
  f_af <- glue('<dateOther type="availableFrom">{availableFrom}</dateOther>') |> blank()

  glue::glue('    <originInfo>
      {f_publisher}
      {f_place}
      {f_yi}
      {f_af}
      {f_edition}
    </originInfo>')
}

frag_relatedItem_series <- function(title, issn, eissn, issue) {
  glue::glue('  		<relatedItem type="series">
			<titleInfo>
				<title>{title}</title>
			</titleInfo>
			<identifier type="issn">{issn}</identifier>
			<identifier type="eissn">{eissn}</identifier>
			<identifier type="issue number">{issue}</identifier>
		</relatedItem>')
}

frag_publication_status <- function(
    status = c("Submitted", "Accepted", "In press", "Published")) {
    glue::glue("<note type='publicationStatus' lang='eng'>{status}</note>")
}

# part of a journal or other magazine
# TODO: Artikel-ID för e-publikationer som saknar sidnumrering anges i samma fält som första sida
frag_relatedItem_host_journal <- function(
    pub_title, pub_issn, pub_eissn,
    pub_volume, pub_issue,
    pub_extent_beg, pub_extent_end) {
  glue::glue('<relatedItem type="host">
    <titleInfo>
      <title>{pub_title}</title>
    </titleInfo>
    <identifier type="issn">{pub_issn}</identifier>
    <identifier type="eissn">{pub_eissn}</identifier>
    <part>
      <detail type="volume">
        <number>{pub_volume}</number>
      </detail>
      <detail type="issue">
        <number>{pub_issue}</number>
      </detail>
      <extent>
        <start>{pub_extent_beg}</start>
        <end>{pub_extent_end}</end>
      </extent>
    </part>
  </relatedItem>', .na = "")
}


# part of a book
frag_relatedItem_host_book <- function(
    book_title, book_subtitle,
    book_editors,
    book_extent_beg, book_extent_end) {
  editors <- paste0(collapse = ", ", book_editors)
  glue::glue('<relatedItem type="host">
      <titleInfo>
        <title>{book_title}</title>
        <subTitle>{book_subtitle}</subTitle>
      </titleInfo>
      <note type="statement of responsibility">editors</note>
      <extent>
        <start>{book_extent_beg}</start>
        <end>{book_extent_end}</end>
      </extent>
    </relatedItem>')
}

frag_keyword <- function(keywords) {
  kw <- strsplit(keywords, "\\W+") %>% unlist()
  ks <- function(x) glue::glue(.na = "", '<subject lang="eng">
    <topic>{x}</topic>
  </subject>')
  kw %>% map_chr(ks) %>% paste0(collapse = "\n")
}

# part of proceeding
frag_relatedItem_host_proceeding <- function(
    proceeding_title, proceeding_subtitle,
    proceeding_editors,
    proceeding_extent_beg, proceeding_extent_end) {
  title <- proceeding_title
  subtitle <- proceeding_subtitle
  editors <- proceeding_editors
  extent_beg <- proceeding_extent_beg
  extent_end <- proceeding_extent_end
  frag_relatedItem_host_book(title, subtitle, editors, extent_beg, extent_end)
}

# part of conference
frag_relatedItem_host_conference <- function(
  conference_name
) {
  glue::glue('<name type="conference">
    <namePart>conference_name</namePart>
  </name>')
}

#' @importFrom whisker iteratelist
diva_mods_params <- function(genres = NULL, recordinfos = NULL,
  identifiers = NULL, persons = NULL, subjects = NULL, relateditems = NULL,
  title = NULL, abstract = NULL, notes = NULL, origins = NULL,
  resourcetypes = NULL, desc = NULL, location = NULL, series = NULL) {

  args_defined <- ls()
  args_passed <- names(as.list(match.call())[-1])

  if (!any(args_defined %in% args_passed)) {
    warning(paste("No arguments  for", paste(setdiff(args_defined, args_passed), collapse=", ")))
    warning("Will use some defaults...")

    genres <- frag_genre_orig("conferencePaper")

    recordinfos <- frag_recordInfo(
      "u1o2txrh",
      source = "kth",
      creation = "2014-02-05",
      change = "2022-06-23",
      pid = "diva2:694034"
    )

    identifiers <- rep(c(frag_identifier(
      "id_#_name", "id_#_value", "id#displayLabel"
    )), 2)

    persons <- c(frag_name_personal(
      kthid = "u1sockan", source = "source_#1", family = "Family Name",
      given = "Name given", role = "Role", affiliations = "affiliations",
      descriptions = "descriptions"))

    subjects <- c(
      frag_subject(topic = c(
        "Antibacterial", "Antioxidant",
        "Bioplastics", "Nanoparticles")),
      frag_subject(
        lang = "eng", source = "hsv",
        href = "10403", topic = "Materials Chemistry"),
      frag_subject(
        lang = "swe", source = "hsv",
        href = "10403", topic = "Materialkemi")
    )

    relateditems <- frag_relatedItem_conference(
      conf_title = "conf_title", conf_subtitle = "conf_subtitle",
      conf_ext_start = "conf_ext_start", conf_ext_end = "conf_ext_end",
      conf_details = "conf_details")

    title <- frag_titleInfo(
      title = "Some title", subtitle = "Some subtitle",
      lang = "eng")

    origins <- frag_originInfo(yearIssued = "2022", availableFrom = "2023")

    desc <- frag_physicalDescription(desc = "Physical Description")

    resourcetypes <- frag_typeOfResource(resourcetype = "text")

    notes <- c(
      frag_note("Some note goes here. VERIFY."),
      frag_note("Another. @Funder@ [@project_number_from_funder@")
    )

    abstract <- frag_abstract("Some abstract here. In english.")

    location <- frag_location("https://example.com/somepath?q=where")

    series <- frag_relatedItem_series(title = "relseries",
      issn = "my_issn", eissn = "my_eissn", issue = "1")

  }

  values <- list(
    frag_genre = whisker::iteratelist(genres, value = "genre"),
    frag_name_personal = whisker::iteratelist(persons, value = "person"),
    frag_titleInfo = whisker::iteratelist(title, value = "title"),
    frag_originInfo = whisker::iteratelist(origins, value = "origin"),
    frag_physicalDescription = whisker::iteratelist(desc, value = "desc"),
    frag_typeOfResource = whisker::iteratelist(resourcetypes, value = "resourcetype"),
    frag_relatedItem_series = whisker::iteratelist(series, value = "series"),
    frag_location = whisker::iteratelist(location, value = "loc"),
    frag_note = whisker::iteratelist(notes, value = "note"),
    frag_abstract = whisker::iteratelist(abstract, value = "abstract"),
    frag_subject = whisker::iteratelist(subjects, value = "subj"),
    frag_recordInfo = whisker::iteratelist(recordinfos, value = "ri"),
    frag_relatedItem = whisker::iteratelist(relateditems, value = "rel"),
    frag_identifier = whisker::iteratelist(identifiers, value = "ids")
  )

  return(values)

}

create_diva_mods <- function(params = diva_mods_params()) {

  whiskers <- readLines(system.file(package = "kthcorpus",
    "extdata", "mods_template.whisker"))

  values <- params

  #message("Values are: ")
  #print(values)

  txt <-
    whisker::whisker.render(whiskers, values, debug = TRUE)

  return(txt)

}

create_diva_modscollection <- function(mods) {

  wrapper <- readLines(system.file(package = "kthcorpus",
    "extdata", "modsCollection_template.whisker"))

  mods <- list(mods = mods)

  collection <-
    whisker::whisker.render(wrapper, mods, debug = TRUE)

  # we don't mind that/if an xml declaration is added here so we can compact
  # some whitespace by passing this through xml2::xml_read

  collection |> xml2::read_xml() |> as.character()

}

