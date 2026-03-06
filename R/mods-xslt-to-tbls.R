mods_to_json <- function(xml_string) {

  xml <- 
    xml_string |> xml2::read_xml() |> xml_ns_strip() |> xml_find_all("//mods") |> 
    as.character() |> xml2::read_xml()

  xsl <- system.file(package = "kthcorpus", "extdata", "xml2json.xsl") |> xml2::read_xml()
  #xsl <- system.file(package = "kthcorpus", "extdata", "xml-to-json.xsl") |> xml2::read_xml()
  #xsl <- system.file(package = "kthcorpus", "extdata", "xml-to-jsonml.xsl") |> xml2::read_xml()
  
  xslt::xml_xslt(xml, xsl) 
  
}

mods_to_xjsonld <- function(xml_string) {

  xml <- 
    xml_string |> xml2::read_xml() 

  xsl <- system.file(package = "kthcorpus", "extdata", "mods_to_xjsonld.xsl") |> xml2::read_xml()  
  xslt::xml_xslt(xml, xsl) 
  
}


mods_json_to_object <- function(json) {
  obj <- json |> fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
  x <- obj$mods
  return (x)
}

parse_mods_object <- function(x) {

  type <- aff <- id <- NULL

  genre <- 
    x$genre |> as_tibble() |> 
    rename(any_of(c(desc = "#", aut = "@authority", type = "@type", lang = "@lang")))
   
  np <- 
    x$name |>
    rename(any_of(c(
      desc = "#", aut = "@authority", type = "@type", lang = "@lang", href = "@href",
      role = "role.#", role_term = "role.roleTerm.#", role_type = "role.roleTerm.@type", 
      role_authority = "role.roleTerm.@authority",
      affiliation = "affiliation.#", description = "description.#"
    ))) |> 
    as_tibble()
  
  orgs_names <- 
    np |> filter(type == "corporate") |> 
    pull("namePart") |> rrapply(how = "melt") |> unnest(any_of("value")) |> 
    select(org = "value", id = "L1") |>
    mutate(id = as.integer(id))

  orgs_affs <- 
    np |> filter(type == "corporate") |> 
    pull("affiliation") 
  
  if (all(is.na(orgs_affs))) {
    orgs_affs <- data.frame(id = integer(0), aff = character(0)) |> as_tibble()
  } else {
    orgs_affs <- orgs_affs |> 
      rrapply(how = "melt") |> unnest(any_of("value")) |> 
        select(aff = "value", id = "L1") |> 
        mutate(id = as.integer(id), aff = as.character(aff))
  } 
 
  orgs_descs <- data.frame(id = integer(0), desc = character(0)) |> as_tibble()

  if ("description" %in% colnames(np)) {

    descs <- np |> filter(type == "corporate") |> pull("description")

    if ("character" %in% class(descs) && !all(is.na(descs))) {
      orgs_descs <- 
        data.frame(id = seq_along(descs), desc = descs) |> as_tibble()
    } else if (is.list(descs) || is.data.frame(descs)) {
      org_descs <- 
        descs |> 
        rrapply(how = "melt") |> 
        unnest(any_of("value")) |> 
        select(desc = "value", id = "L1") |>
        mutate(id = as.integer(id))  
    }
  }
  
  orgs <- 
    np |> filter(type == "corporate") |> 
    select(-any_of(c("namePart", "affiliation", "description", "desc"))) |> 
    tibble::rowid_to_column("id") |> 
    left_join(by = "id", orgs_names, relationship = "many-to-many") |> 
    left_join(by = "id", orgs_affs, relationship = "many-to-many") |> 
    left_join(by = "id", orgs_descs, relationship = "many-to-many")
  
  persons <- 
    np |> filter(type == "personal") 

  authors_aff <- 
    persons |> pull("affiliation") |> 
    map_df(\(x) as_tibble(x), .id = "id") |> 
    rename(any_of(c(
      affiliation = "#"
    )))
  
  if (is.null(persons |> getElement("description"))) {
    authors_desc <- data.frame(id = character(0), description = character(0))
  } else {
    authors_desc <- 
      persons |> pull("description") |> 
      map_df(\(x) as_tibble(x), .id = "id") |>
      # persons |> 
      # mutate(across(matches("description"), \(x) map_df(as_tibble, .id = "id"))) |> 
      # select(any_of("description")) |> unnest(any_of("description")) |> 
      rename(any_of(c(
        description = "#"
      )))  
  }
  
  authors_names <- 
    persons |> pull("namePart") |> 
    map_df(\(x) as.data.frame(x), .id = "id") |> 
    rename(any_of(c(
      value = "#",
      field = "@type",
      kind = "X.type",
      etal = "X."
    ))) |> pivot_wider(
      names_from = "field",
      values_from = "value"
    )
  
  authors <- 
    persons |> 
    select(-any_of(c("namePart", "affiliation", "description", "desc", "role"))) |> 
    tibble::rowid_to_column("id") |> 
    mutate(id = as.character(id)) |> 
    left_join(by = "id", authors_names, relationship = "many-to-many") |> 
    left_join(by = "id", authors_desc, relationship = "many-to-many") |> 
    left_join(by = "id", authors_aff, relationship = "many-to-many")

  titleInfo <- 
    x$titleInfo |> rrapply(how = "melt") |> pivot_wider(names_from = "L1") |> 
    rename(any_of(c(
      lang = "@lang", 
      titleInfo = "#"
    )))
  
  language <- 
    x$language |> rrapply(how = "bind") |> rename(any_of(c(
      lang = "#",
      object_part = "@objectPart",
      language_term = "languageTerm.#",
      language_type = "languageTerm.@type",
      language_authority = "languageTerm.@authority"
    ))) |> as_tibble()
    
  originInfo <- 
    x$originInfo |> rrapply(how = "bind") |> as_tibble() |> 
    rename(any_of(c(
      "originInfo" = "#",
      publisher = "publisher.#",
      date_issued = "dateIssued.#",
      date_other = "dateOther.#",
      date_other_type = "dateOther.@type"
    ))) |> 
    unnest(any_of(c("date_other", "date_other_type")))
  
  physicalDescription <- 
    x$physicalDescription |> 
    rrapply(how = "bind") |> as_tibble() |>
    rename(any_of(c(
      "physicalDescription" = "#",
      form = "form.#",
      form_authority = "form.@authority",
      extent = "#"
    )))
  
  identifier <- 
    x$identifier |> as_tibble() |> 
    rename(any_of(c(
    id_name = "@type",
    id_value = "#",
    id_label = "@displayLabel"
  ))) |> as_tibble()
  
  typeOfResource <- 
    x$typeOfResource |> bind_cols() |> 
    rename(any_of(c(
      resource_type = "#"
    ))) |> as_tibble()
  
  if (is.data.frame(x$relatedItem)) {
    reli <- x$relatedItem |> as_tibble()
  } else {
    reli <- x$relatedItem |> rrapply(how = "bind") |> as_tibble()
  }

  ri <- 
    reli |>
    rename(any_of(c(
      relateditem = "#",
      relateditem_type = "@type",
      title_info = "titleInfo.#",
      title = "titleInfo.title.#",
      subtitle = "titleInfo.subTitle.#",
      note = "note.#",
      note_type = "note.@type",
      identifier = "identifier.#",
      identifier_type = "identifier.@type",
      part = "part.#",
      part_detail = "part.detail.#",
      part_detail_type = "part.detail.@type",
      part_detail_nr = "part.detail.number.#",
      part_extent = "part.extent.#",
      part_extent_end = "part.extent.end.#",
      part_extent_start = "part.extent.start.#"
    ))) |> 
    # unnest(any_of(c("relateditem", "relateditem_type", "title_info", "title"))) |> 
    # unnest_longer(any_of(c("identifier", "identifier_type"))) |> 
    unnest(cols = any_of(c(
      "part_detail", "part_detail_type", "part_detail_nr", 
      "identifier", "identifier_type"
    ))) |> 
    rename(any_of(c(
      series_id = "#",
      series_type = "@type",
      genre = "genre.#"
    )))  

  # if (all(c("identifier_type", "identifier") %in% colnames(ri))) {
  #   ri <- 
  #     ri |> 
  #     pivot_wider(
  #       names_from = any_of("identifier_type"), 
  #       values_from = any_of("identifier"), 
  #       names_prefix = "identifier_") |> 
  #     rename(any_of(c(
  #       "identifier_issue_nr" = "identifier_issue number"
  #     )))  
  # }
  
  subj <- 
    x$subject |> as_tibble() |> 
    mutate(across(any_of("topic"), \(x) map(x, as_tibble))) |> 
    unnest(any_of("topic"), names_sep = "_") |> 
    rename(any_of(c(
      topic = "topic_#",
      topic = "topic.#",
      subject = "#",
      lang = "@lang",
      authority = "@authority",
      href = "@href",
      genre = "genre.#"
    )))
  
  abstract <- 
    x$abstract |> bind_cols() |> 
    rename(any_of(c(
      abstract = "#",
      lang = "@lang"
    )))
  
  note <- 
    x$note |> as_tibble() |> 
    rename(any_of(c(
      note = "#",
      note_type = "@type",
      note_lang = "@lang"
    )))
  
  if (is.null(x$location)) {
    location <- NULL
  } else {
    location <- 
      x$location |> rrapply(how = "bind") |> as_tibble() |> 
      rename(any_of(c(
        location = "#",
        url = "url.#",
        url_display = "url.@displayLabel",
        url_note = "url.@note",
        url_access = "url.@access"
      ))) |> 
      unnest_longer(col = any_of(c("location", "url", "url_display", "url_note", "url_access")))
  }
  
  recordInfo <- 
    x$recordInfo |> rrapply(how = "bind") |> as_tibble() |> 
    rename(c(
      recordInfo = "#",
      record_origin = "recordOrigin.#",
      record_source = "recordContentSource.#",
      record_created = "recordCreationDate.#",
      record_changed = "recordChangeDate.#",
      record_id = "recordIdentifier.#"
    )) 

  
  list(genre = genre, orgs = orgs, authors = authors, titleInfo = titleInfo, 
    language = language, originInfo = originInfo, physicalDescription = physicalDescription, 
    identifier = identifier, typeOfResource = typeOfResource, relatedItem = ri, 
    subjects = subj, abstract = abstract, note = note, location = location,
    recordInfo = recordInfo
  ) |> 
  compact() |> map(\(x) x |> bind_cols(
      pid = gsub("diva2:(.*)", "\\1", recordInfo$record_id) |> as.integer()
    ) |> 
    select(c("pid", everything()))
  )

}

parse_mods_objects <- function(records) {
  to_obj <- function(record) record |> mods_to_json() |> mods_json_to_object() 
  objs <- records |> map(to_obj)
  objs |> map(parse_mods_object, .progress = TRUE)  
}

mods_records_to_tbls <- function(l) {
  list(
    genre = l |> map_dfr("genre"),
    orgs = l |> map_dfr("orgs"),
    authors = l |> map_dfr("authors"), 
    titleInfo = l |> map_dfr("titleInfo"),
    language = l |> map_dfr("language"),
    typeOfResource = l |> map_dfr("typeOfResource"),
    relatedItem = l |> map_dfr("relatedItem"),
    subjects = l |> map_dfr("subjects"),
    abstract = l |> map_dfr("abstract"),
    note = l |> map_dfr("note"),
    recordInfo = l |> map_dfr("recordInfo"),
    location = l |> map_dfr("location")
  )  |> 
    map(\(x) suppressMessages(type_convert(x)))
}
