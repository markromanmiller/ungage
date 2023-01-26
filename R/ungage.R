# So the interface ought to be:
# Give BIDS info, and it extracts the ENGAGE .myrec into that BIDS info.
# Check in on the BIDs validator (low priority)
#
# engager uses dddr AND bids.
#
#

#' @importFrom withr with_tempdir
#' @importFrom jsonlite read_json
#' @importFrom logger log_info log_debug
NULL

check_file <- function(myrec_path) {
  if (!file.exists(myrec_path)) {
    stop(paste0("Could not find file `", myrec_path, "` from working directory `", getwd(), "`."))
  }
  normalizePath(myrec_path)
}

check_single_character <- function(x) {
  xname <- deparse(substitute(x))
  if (!is.character(x)) {
    stop(paste0("`", xname, "` is not a character vector, it is `", mode(x), "`"))
  }
  if (length(x) != 1) {
    stop(paste0("`", xname, "` is expected to be length 1, it is length ", length(x)))
  }
}

unzip_myrec <- function(absolute_path_to_myrec, unzip_dest) {
  log_info("Unzipping myrec file...")
  unzip(absolute_path_to_myrec, exdir = unzip_dest)

  # file list
  file_list <- list.files(unzip_dest)

  log_info(paste0("Unzipping ", sum(startsWith(file_list, "stream")), " stream files..."))
  for (file in file_list) {
    if (!file %in% c("count.txt")) {
      unzip(file.path(unzip_dest, file), exdir = file.path(unzip_dest, paste0(file, "_dir")))
    }
  }
  log_info("Streams unzipped.")
}

get_player_names <- function(master_json) {
  if ("avatar_variables_v1" %in% names(master_json)) {
    player_names <- vapply(
      master_json$avatar_variables_v1,
      function(x) {x$name},
      character(1)
    )
  } else {
    player_names <- vapply(
      master_json$avatar_variables,
      function(x) {x$avatarVars$name},
      character(1)
    )
  }
  return (player_names)
}

#' Pull Player Names present within a recording
#'
#' @param myrec_path Path to the myrec file
#'
#' @export
myrec_player_names <- function(myrec_path) {
  absolute_path_to_myrec <- check_file(myrec_path)

  unzip_dest <- "unzip_dest"

  with_tempdir({
    log_debug(paste0("Working at ", getwd()))

    unzip_myrec(absolute_path_to_myrec, unzip_dest)

    master_json <- read_json(file.path(unzip_dest, "master_dir", "master.txt"))
    player_names <- get_player_names(master_json)
  })

  return(player_names)
}

#' Convert myrec file into BIDS format
#'
#' @param myrec_path Path to the .myrec file
#' @param bids_dataset_object BIDS object created by `rbids::bids()` to which this data will be written
#' @param session_id BIDS session id for this recording, one alphanumeric string
#' @param task_id BIDS task id for this recording, also an alphanumeric string
#' @param participant_id_map Named vector with names as player names and values as participant IDs
#'
#' @importFrom dddr semantics_angles_unity semantics_axes_unity set_dddr_semantics get_dddr_semantics
#' @importFrom stringr str_match str_split str_sub str_subset
#' @importFrom tidyr unnest fill separate pivot_wider pivot_longer unite
#' @importFrom dplyr mutate ungroup group_by select n filter arrange group_nest
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom rbids bids_write_motion_files
#' @importFrom brio read_file
#' @export
myrec_to_bids <- function (myrec_path, bids_dataset_object, session_id, task_id, participant_id_map) {

  # Check whether the relevant files exists and are in a reasonable state.
  absolute_path_to_myrec <- check_file(myrec_path)
  check_single_character(session_id)
  check_single_character(task_id)

  unzip_dest <- "unzip_dest"

  with_tempdir({
    log_debug(paste0("Working at ", getwd()))

    unzip_myrec(absolute_path_to_myrec, unzip_dest)

    master_json <- read_json(file.path(unzip_dest, "master_dir", "master.txt"))
    player_names <- get_player_names(master_json)


    ## TODO: remove brio dependency
    stream_count <- as.numeric(str_match(read_file(file.path(unzip_dest, "count.txt")), "streamCount\\|(\\d+);")[2])

    # I can't parse all of them! Write after a certain number of blocks... about 1000 if we include people.

    batches <- split(seq_len(stream_count)-1, ceiling(seq_len(stream_count) * length(player_names) / 1000))

    # TODO: setup progress bar here.
    #pb <- progress::progress_bar$new(total = 446)
    #pb$tick(0)

    old_dddr_semantics <- get_dddr_semantics()
    set_dddr_semantics(angles = semantics_angles_unity, axes = semantics_axes_unity)

    # MIDDLE OF DOING: convert wirtten_playernames to a set, at include append as an entry in the table for write_file
    written_playernames <- c()

    for (batch_index in seq_along(batches)) {
      batch <- batches[[batch_index]]
      log_debug("Making batch...")

      # First, expand out the file into a table where each row represents
      # a single piece of written information from the stream
      #
      # # A tibble: 10 x 5
      #     stream_no playername header               entry full_frame_no
      #         <dbl> <chr>      <chr>                <chr>         <dbl>
      #   1         0 vhil2      LipSyncAverageflox   0                 0
      # 101         5 Cyan       LeftFootRotationsv3x <<              762

      read_data <- tibble(stream_no = batch) %>%
        mutate(
          data = map(stream_no, function(stream_no) {
            log_debug("mapping")
            stream <- read_file(file.path(unzip_dest, paste0("stream", stream_no, "_dir"), "stream.txt"))

            result <- tibble(player_split = str_split(stream, pattern = ">") %>% unlist()) %>%
              mutate(
                player_index = (1:n()) - 1,
              ) %>%
              filter(player_split != "") %>%
              mutate(
                playername = player_names[as.character(player_index)],
                entry_fulltext = map(player_split, function(ft) {ft %>% str_split(";") %>% unlist()})
              ) %>%
              select(-player_split) %>%
              unnest(entry_fulltext) %>%
              separate(entry_fulltext, into = c("header", "data"), sep = "\\|", extra = "merge", fill="left") %>%
              mutate(
                playername = player_names[as.character(player_index)],
                entry = map(data, function(en) {en %>% str_split("\\|") %>% unlist()})
              ) %>%
              select(-data, -player_index) %>%
              unnest(entry) %>%
              group_by(header, playername) %>%
              mutate(
                full_frame_no = stream_no * 140 + 0:(n()-1)
              ) %>%
              ungroup() %>%
              mutate(
                entry = ifelse(entry == "", NA, entry)
              ) %>%
              fill(entry)

            #pb$tick()

            result
          })
        ) %>%
        unnest(data)

      log_debug("making sep and fill data")

      selected_tracked_data_types <- c(
        # Positions and orientations
        "AvaRootPositionsv3x", "AvaRootRotationsv3x",
        "HeadPositionsv3x", "HeadRotationsv3x",
        "RightHandPositionsv3x", "RightHandRotationsv3x",
        "LeftHandPositionsv3x", "LeftHandRotationsv3x",
        "HipPositionsv3x", "HipRotationsv3x",
        "LeftFootPositionsv3x", "LeftFootRotationsv3x",
        "RightFootPositionsv3x", "RightFootRotationsv3x",

        # Status
        "IsAwayintx",
        "IsInSitTriggerintx",
        "TabletOutintx",
        # "UseSitTriggerOverridesintx",

        # Interactions
        "LeftTriggerPressedintx",
        "RightTriggerPressedintx",
        "LipSyncAverageflox",
        "IFXScaleflox", "IfxPositionsv3x", "IfxRotationsv3x"
        # "LeftHandLaserPointerintx", "LeftHandPointingintx",
        # "RightHandLaserPointerintx", "RightHandPointingintx"
      )

      parsed_v3x <- read_data %>%
        filter(header %in% str_subset(selected_tracked_data_types, "v3x")) %>%
        mutate(
          header = str_sub(header, 1, -5), # remove last four characters with -5
        ) %>%
        separate(
          entry, into = c("x", "y", "z"), sep = "<"
        ) %>%
        mutate(across(-c(stream_no, playername, full_frame_no), function(x) {ifelse(x == "", NA, x)})) %>%
        fill(x, y, z) %>%
        pivot_longer(c(x, y, z), names_to = "dimension", values_to = "entry") %>%
        unite("header", header, dimension)
      # do whatever to unite the columns

      parsed_single_streams <- read_data %>%
        filter(header %in% str_subset(selected_tracked_data_types, "v3x", negate = T))

      sep_and_fill <- rbind(parsed_v3x, parsed_single_streams) %>%
        pivot_wider(names_from = "header", values_from = entry)

      data_files <- sep_and_fill %>%
        select(-stream_no) %>%
        group_nest(playername, .key = "data") %>%
        mutate(
          participant_id = participant_id_map[playername],
          session_id = session_id,
          task_id = task_id,
        )

      bids_write_motion_files(data_files, bids_dataset_object, append = T)

    }
  })
}
