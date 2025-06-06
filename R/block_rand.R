#' Generate Block Randomization for Clinical Trial
#'
#' This function generates a block randomization sequence for clinical trials
#' using variable block sizes to prevent prediction while maintaining balance.
#'
#' @param n_participants Number of participants to randomize
#' @param n_arms Number of treatment arms (default: 2)
#' @param arm_labels Character vector of arm labels (default: c("A", "B"))
#' @param block_sizes Vector of possible block sizes (default: c(2, 4, 6, 8) for 2 arms)
#' @param allocation_ratio Vector of allocation ratios (default: c(1, 1) for 1:1)
#' @param seed Random seed for reproducibility
#'
#' @return A data frame with participant assignments
#'
#' @export
#'
#' @examples
#' # Simple 1:1 randomization for 62 participants
#' assignments <- generate_block_randomization(62)
#'
#' # Custom block sizes and seed
#' assignments <- generate_block_randomization(
#'   n_participants = 62,
#'   block_sizes = c(4, 6, 8),
#'   seed = 12345
#' )
generate_block_randomization <- function(n_participants,
                                         n_arms = 2,
                                         arm_labels = c("A", "B"),
                                         block_sizes = c(2, 4, 6, 8),
                                         allocation_ratio = c(1, 1),
                                         seed = NULL) {

  # Input validation
  if (length(arm_labels) != n_arms) {
    stop("Length of arm_labels must equal n_arms")
  }

  if (length(allocation_ratio) != n_arms) {
    stop("Length of allocation_ratio must equal n_arms")
  }

  if (any(block_sizes %% sum(allocation_ratio) != 0)) {
    stop("All block sizes must be multiples of sum(allocation_ratio)")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Initialize variables
  assignments <- character(0)
  participants_assigned <- 0
  block_number <- 1

  # Generate assignments until we have enough participants
  while (participants_assigned < n_participants) {
    # Randomly select block size
    current_block_size <- sample(block_sizes, 1)

    # Create block with proper allocation ratio
    block_assignments <- rep(arm_labels, times = allocation_ratio * (current_block_size / sum(allocation_ratio)))

    # Randomly permute the block
    block_assignments <- sample(block_assignments)

    # Add to overall assignments
    assignments <- c(assignments, block_assignments)
    participants_assigned <- length(assignments)
    block_number <- block_number + 1
  }

  # Trim to exact number needed
  assignments <- assignments[1:n_participants]

  # Create data frame with additional information
  result <- data.frame(
    participant_id = 1:n_participants,
    assignment = assignments,
    stringsAsFactors = FALSE
  )

  # Add block information
  result$block_number <- NA
  current_block <- 1
  current_position <- 1

  for (i in 1:length(assignments)) {
    if (current_position == 1) {
      # Determine current block size by looking ahead
      remaining_assignments <- assignments[i:length(assignments)]
      for (block_size in block_sizes) {
        if (length(remaining_assignments) >= block_size) {
          # Check if this could be a valid block
          block_subset <- remaining_assignments[1:block_size]
          expected_counts <- allocation_ratio * (block_size / sum(allocation_ratio))
          actual_counts <- table(factor(block_subset, levels = arm_labels))

          if (all(actual_counts == expected_counts)) {
            current_block_size <- block_size
            break
          }
        }
      }
    }

    result$block_number[i] <- current_block
    current_position <- current_position + 1

    if (current_position > current_block_size) {
      current_block <- current_block + 1
      current_position <- 1
    }
  }

  return(result)
}

#' Generate Encrypted Assignment Strings
#'
#' Creates encrypted arm identifiers for blinded randomization
#'
#' @param arm_labels Character vector of actual arm labels
#' @param encryption_key Raw vector containing encryption key
#'
#' @return A data frame with arm labels and encrypted arm IDs
#'
#' @export
generate_encrypted_arms <- function(arm_labels, encryption_key = NULL) {

  if (is.null(encryption_key)) {
    encryption_key <- sodium::random(32)  # Generate 256-bit key
  }

  # Generate random strings for each arm
  arm_ids <- replicate(length(arm_labels), {
    paste0(sample(c(LETTERS, letters, 0:9), 8, replace = TRUE), collapse = "")
  })

  # Create the assignment mask (this gets encrypted)
  assignment_mask <- data.frame(
    arm_label = arm_labels,
    arm_id = arm_ids,
    stringsAsFactors = FALSE
  )

  # Encrypt the assignment mask
  encrypted_mask <- sodium::data_encrypt(
    serialize(assignment_mask, NULL),
    encryption_key
  )

  # Create assignment strings table (visible to randomizing statistician)
  assignment_strings <- data.frame(
    arm_id = arm_ids,
    stringsAsFactors = FALSE
  )

  return(list(
    assignment_mask = assignment_mask,
    encrypted_mask = encrypted_mask,
    assignment_strings = assignment_strings,
    encryption_key = encryption_key
  ))
}

#' Decrypt Assignment Mask
#'
#' Decrypts the assignment mask to reveal arm labels
#'
#' @param encrypted_mask Encrypted assignment mask
#' @param encryption_key Raw vector containing encryption key
#'
#' @return Data frame with decrypted arm labels and IDs
#'
#' @export
decrypt_assignment_mask <- function(encrypted_mask, encryption_key) {
  decrypted_data <- sodium::data_decrypt(encrypted_mask, encryption_key)
  assignment_mask <- unserialize(decrypted_data)
  return(assignment_mask)
}

#' Generate User IDs
#'
#' Creates unique participant IDs for the study
#'
#' @param n_participants Number of participants
#' @param prefix Character prefix for IDs (default: "EHS")
#' @param id_length Length of random component (default: 6)
#'
#' @return Character vector of unique participant IDs
#'
#' @export
generate_user_ids <- function(n_participants, prefix = "EHS", id_length = 6) {

  # Generate unique random IDs
  ids <- character(n_participants)
  existing_ids <- character(0)

  for (i in 1:n_participants) {
    repeat {
      random_part <- paste0(sample(c(LETTERS, 0:9), id_length, replace = TRUE), collapse = "")
      new_id <- paste0(prefix, random_part)

      if (!new_id %in% existing_ids) {
        ids[i] <- new_id
        existing_ids <- c(existing_ids, new_id)
        break
      }
    }
  }

  return(ids)
}

#' Create Complete Randomization Package
#'
#' Generates all tables needed for the blinded randomization system
#'
#' @param n_participants Number of participants to randomize
#' @param arm_labels Character vector of treatment arm labels
#' @param block_sizes Vector of possible block sizes
#' @param allocation_ratio Vector of allocation ratios
#' @param study_id Character string identifying the study
#' @param seed Random seed for reproducibility
#'
#' @return List containing all randomization tables and metadata
#'
#' @export
create_randomization_package <- function(n_participants,
                                         arm_labels = c("Treatment", "Control"),
                                         block_sizes = c(2, 4, 6, 8),
                                         allocation_ratio = c(1, 1),
                                         study_id = "EHS_PILOT",
                                         seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate user IDs
  user_ids <- generate_user_ids(n_participants, prefix = paste0(study_id, "_"))

  # Generate encrypted arms
  arm_system <- generate_encrypted_arms(arm_labels)

  # Generate block randomization using encrypted arm IDs
  randomization <- generate_block_randomization(
    n_participants = n_participants,
    n_arms = length(arm_labels),
    arm_labels = arm_system$assignment_strings$arm_id,
    block_sizes = block_sizes,
    allocation_ratio = allocation_ratio
  )

  # Create all required tables

  # 1. User-IDs Table (available to all)
  user_ids_table <- data.frame(
    participant_id = user_ids,
    stringsAsFactors = FALSE
  )

  # 2. Assignment Strings Table (available to all)
  assignment_strings_table <- arm_system$assignment_strings

  # 3. Assignment Allocations Table (controlled access)
  assignment_allocations_table <- data.frame(
    participant_id = user_ids,
    arm_id = randomization$assignment,
    block_number = randomization$block_number,
    stringsAsFactors = FALSE
  )

  # 4. Assignment Mask Table (encrypted until unblinding)
  assignment_mask_table <- arm_system$assignment_mask
  encrypted_assignment_mask <- arm_system$encrypted_mask

  # Create metadata
  metadata <- list(
    study_id = study_id,
    n_participants = n_participants,
    arm_labels = arm_labels,
    block_sizes = block_sizes,
    allocation_ratio = allocation_ratio,
    seed = seed,
    created_date = Sys.time(),
    encryption_key = arm_system$encryption_key,
    randomization_algorithm = "block_randomization_variable_blocks"
  )

  return(list(
    user_ids_table = user_ids_table,
    assignment_strings_table = assignment_strings_table,
    assignment_allocations_table = assignment_allocations_table,
    assignment_mask_table = assignment_mask_table,
    encrypted_assignment_mask = encrypted_assignment_mask,
    metadata = metadata
  ))
}
