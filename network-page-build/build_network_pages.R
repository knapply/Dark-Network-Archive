build_network_pages <- function(proto_net_names) {
  on.exit(options(warn = 0))
  options(warn = 2) # FAIL ON ANY WARNINGS

  stopifnot(length(proto_net_names) && is.character(proto_net_names))

  valid_names <- COREnets::list_data_sources()

  if (!all(proto_net_names %in% valid_names)) {
    invalid_names <- setdiff(proto_net_names, valid_names)
    stop("The following `net_names` are invalid:",
         paste0("\n\t- ", invalid_names),
         call. = FALSE)
  }

  target_nets <- intersect(proto_net_names, valid_names)
  if (length(target_nets) == 0L) {
    stop("No valid nets found.", call. = FALSE)
  }

  for (i in seq_along(target_nets)) {
    out_dir <- paste0("content/post/", target_nets[[i]])
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE, force = TRUE)
    }
    dir.create(out_dir)

    image_url <- sprintf(
      "https://raw.githubusercontent.com/NPSCORELAB/dna-pics/master/networks/%s.%s",
      target_nets[[i]],
      "jpg"
    )

    if (RCurl::url.exists(image_url)) {
      image_target_path <- paste0(out_dir, "/featured.jpg")
      download.file(image_url, destfile = image_target_path, mode = "wb")
    }

    out_path <- paste0(out_dir, "/", "index.Rmd")

    message("Rendering:\t", target_nets[[i]])

    this_net <- COREnets::get_data(target_nets[[i]])

    rmd <- readr::read_file(
      "network-page-build/network-page-template.Rmd"
    )

    .gsub <- function(string, pattern, replacement) {
      gsub(pattern, replacement, string, fixed = TRUE)
    }

    rmd <- .gsub(rmd, "{{{TITLE}}}", this_net$reference$title)
    rmd <- .gsub(rmd, "{{{SUMMARY}}}", this_net$reference$abstract)
    rmd <- .gsub(rmd, "{{{PROTO_NET_NAME}}}", this_net$reference$name)
    rmd <- .gsub(
      rmd, '"{{{TAGS}}}"', 
      paste0("\n- ", this_net$reference$tags, collapse = "")
    )

    readr::write_file(rmd, out_path)
  }
}

build_network_pages(
  c("drugnet", "anabaptists", "cocaine_smuggling_acero",
    "cocaine_smuggling_jake", "cocaine_smuggling_juanes",
    "cocaine_smuggling_mambo", "montreal_street_gangs",
    "november17", "siren", "harry_potter_death_eaters",
    "harry_potter_dumbledores_army")
)


