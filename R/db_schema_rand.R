#' Setup Randomization Database Schema
#'
#' Creates all necessary tables for the blinded randomization system
#'
#' @param postgres_module A postgresModule instance
#' @param study_id Character string identifying the study
#'
#' @return TRUE if successful
#'
#' @export
setup_randomization_database <- function(postgres_module, study_id = "EHS_PILOT") {

  # 1. Name-ID Link Table (restricted access)
  name_id_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_name_id_link (
      id SERIAL PRIMARY KEY,
      participant_name TEXT NOT NULL,
      participant_id TEXT UNIQUE NOT NULL,
      created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      created_by TEXT,
      CONSTRAINT unique_name_id UNIQUE(participant_name, participant_id)
    )", study_id)

  # 2. User-IDs Table (available to all study personnel)
  user_ids_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_user_ids (
      id SERIAL PRIMARY KEY,
      participant_id TEXT UNIQUE NOT NULL,
      created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )", study_id)

  # 3. Assignment Mask Table (restricted until unblinding)
  assignment_mask_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_assignment_mask (
      id SERIAL PRIMARY KEY,
      arm_label TEXT NOT NULL,
      arm_id TEXT UNIQUE NOT NULL,
      created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      is_decrypted BOOLEAN DEFAULT FALSE,
      decrypted_date TIMESTAMP,
      decrypted_by TEXT
    )", study_id)

  # 4. Assignment Strings Table (for blinded randomization)
  assignment_strings_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_assignment_strings (
      id SERIAL PRIMARY KEY,
      arm_id TEXT UNIQUE NOT NULL,
      created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )", study_id)

  # 5. Assignment Allocations Table (controlled access)
  assignment_allocations_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_assignment_allocations (
      id SERIAL PRIMARY KEY,
      participant_id TEXT NOT NULL,
      arm_id TEXT NOT NULL,
      block_number INTEGER,
      allocation_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      allocated_by TEXT,
      FOREIGN KEY (participant_id) REFERENCES %s_user_ids(participant_id),
      FOREIGN KEY (arm_id) REFERENCES %s_assignment_strings(arm_id),
      CONSTRAINT unique_participant_allocation UNIQUE(participant_id)
    )", study_id, study_id, study_id)

  # 6. Clinical Data Tables
  clinical_data_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_clinical_data (
      id SERIAL PRIMARY KEY,
      participant_id TEXT NOT NULL,
      data_type TEXT NOT NULL,
      data_value TEXT,
      collection_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      collected_by TEXT,
      FOREIGN KEY (participant_id) REFERENCES %s_user_ids(participant_id)
    )", study_id, study_id)

  # 7. EHS Data Tables (treatment group only)
  ehs_data_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_ehs_data (
      id SERIAL PRIMARY KEY,
      participant_id TEXT NOT NULL,
      app_usage_date DATE,
      session_duration_minutes NUMERIC,
      modules_completed TEXT[],
      data_recorded_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (participant_id) REFERENCES %s_user_ids(participant_id)
    )", study_id, study_id)

  # 8. Encryption Keys Table (highly restricted)
  encryption_keys_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_encryption_keys (
      id SERIAL PRIMARY KEY,
      key_name TEXT UNIQUE NOT NULL,
      encrypted_key BYTEA NOT NULL,
      key_salt BYTEA NOT NULL,
      created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      created_by TEXT,
      is_active BOOLEAN DEFAULT TRUE
    )", study_id)

  # 9. Audit Log Table
  audit_log_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_audit_log (
      id SERIAL PRIMARY KEY,
      user_id TEXT NOT NULL,
      action TEXT NOT NULL,
      table_name TEXT,
      record_id TEXT,
      old_values JSONB,
      new_values JSONB,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      ip_address INET,
      user_agent TEXT
    )", study_id)

  # 10. User Permissions Table
  user_permissions_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_user_permissions (
      id SERIAL PRIMARY KEY,
      user_id TEXT NOT NULL,
      permission_type TEXT NOT NULL,
      table_access TEXT[],
      can_view BOOLEAN DEFAULT FALSE,
      can_edit BOOLEAN DEFAULT FALSE,
      can_delete BOOLEAN DEFAULT FALSE,
      can_decrypt BOOLEAN DEFAULT FALSE,
      valid_from TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      valid_until TIMESTAMP,
      granted_by TEXT,
      CONSTRAINT unique_user_permission UNIQUE(user_id, permission_type)
    )", study_id)

  # 11. Study Metadata Table
  study_metadata_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s_study_metadata (
      id SERIAL PRIMARY KEY,
      metadata_key TEXT UNIQUE NOT NULL,
      metadata_value TEXT,
      is_encrypted BOOLEAN DEFAULT FALSE,
      created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_by TEXT
    )", study_id)

  # Execute all SQL statements
  sql_statements <- list(
    name_id_sql,
    user_ids_sql,
    assignment_mask_sql,
    assignment_strings_sql,
    assignment_allocations_sql,
    clinical_data_sql,
    ehs_data_sql,
    encryption_keys_sql,
    audit_log_sql,
    user_permissions_sql,
    study_metadata_sql
  )

  tryCatch({
    for (sql in sql_statements) {
      postgres_module$executeQuery(sql)
    }

    # Insert initial study metadata
    metadata_insert <- sprintf("
      INSERT INTO %s_study_metadata (metadata_key, metadata_value)
      VALUES
        ('study_name', 'EHS Pilot Study'),
        ('study_phase', 'setup'),
        ('blinding_status', 'blinded'),
        ('randomization_complete', 'false')
      ON CONFLICT (metadata_key) DO NOTHING
    ", study_id)

    postgres_module$executeQuery(metadata_insert)

    return(TRUE)

  }, error = function(e) {
    stop(paste("Error setting up database schema:", e$message))
  })
}

#' Log User Action
#'
#' Records user actions in the audit log
#'
#' @param postgres_module A postgresModule instance
#' @param study_id Character string identifying the study
#' @param user_id User identifier
#' @param action Description of action taken
#' @param table_name Table affected (optional)
#' @param record_id Record ID affected (optional)
#' @param old_values Previous values (optional)
#' @param new_values New values (optional)
#' @param session Shiny session object for IP/user agent
#'
#' @export
log_user_action <- function(postgres_module, study_id, user_id, action,
                            table_name = NULL, record_id = NULL,
                            old_values = NULL, new_values = NULL,
                            session = NULL) {

  # Get session information if available
  ip_address <- if (!is.null(session)) session$clientData$url_hostname else "unknown"
  user_agent <- if (!is.null(session)) session$request$HTTP_USER_AGENT else "unknown"

  # Convert values to JSON if provided
  old_json <- if (!is.null(old_values)) jsonlite::toJSON(old_values, auto_unbox = TRUE) else NULL
  new_json <- if (!is.null(new_values)) jsonlite::toJSON(new_values, auto_unbox = TRUE) else NULL

  # Create insert statement
  log_data <- data.frame(
    user_id = user_id,
    action = action,
    table_name = ifelse(is.null(table_name), NA, table_name),
    record_id = ifelse(is.null(record_id), NA, as.character(record_id)),
    old_values = ifelse(is.null(old_json), NA, old_json),
    new_values = ifelse(is.null(new_json), NA, new_json),
    ip_address = ip_address,
    user_agent = user_agent,
    stringsAsFactors = FALSE
  )

  # Replace table name in saveData call
  original_datatable <- postgres_module$datatable
  postgres_module$datatable <- paste0(study_id, "_audit_log")

  tryCatch({
    postgres_module$saveData(log_data)
  }, finally = {
    postgres_module$datatable <- original_datatable
  })
}

#' Check User Permissions
#'
#' Verifies if a user has specific permissions
#'
#' @param postgres_module A postgresModule instance
#' @param study_id Character string identifying the study
#' @param user_id User identifier
#' @param permission_type Type of permission to check
#' @param action Specific action (view, edit, delete, decrypt)
#'
#' @return Boolean indicating if user has permission
#'
#' @export
check_user_permission <- function(postgres_module, study_id, user_id,
                                  permission_type, action = "view") {

  # Query user permissions
  query <- sprintf("
    SELECT can_view, can_edit, can_delete, can_decrypt
    FROM %s_user_permissions
    WHERE user_id = '%s' AND permission_type = '%s'
    AND (valid_until IS NULL OR valid_until > NOW())
  ", study_id, user_id, permission_type)

  tryCatch({
    result <- postgres_module$executeQuery(query)

    if (nrow(result) == 0) {
      return(FALSE)  # No permissions found
    }

    # Check specific action
    switch(action,
           "view" = result$can_view[1],
           "edit" = result$can_edit[1],
           "delete" = result$can_delete[1],
           "decrypt" = result$can_decrypt[1],
           FALSE
    )

  }, error = function(e) {
    return(FALSE)  # Default to no permission on error
  })
}

#' Store Encrypted Key
#'
#' Securely stores encryption keys in the database
#'
#' @param postgres_module A postgresModule instance
#' @param study_id Character string identifying the study
#' @param key_name Name/identifier for the key
#' @param encryption_key Raw vector containing the key
#' @param master_password Character string for key encryption
#' @param user_id User storing the key
#'
#' @export
store_encrypted_key <- function(postgres_module, study_id, key_name,
                                encryption_key, master_password, user_id) {

  # Generate salt
  salt <- sodium::random(32)

  # Derive key from master password
  derived_key <- sodium::scrypt(charToRaw(master_password), salt, size = 32)

  # Encrypt the encryption key
  encrypted_key <- sodium::data_encrypt(encryption_key, derived_key)

  # Store in database
  key_data <- data.frame(
    key_name = key_name,
    encrypted_key = list(encrypted_key),  # Store as bytea
    key_salt = list(salt),
    created_by = user_id,
    stringsAsFactors = FALSE
  )

  # Temporarily change table
  original_datatable <- postgres_module$datatable
  postgres_module$datatable <- paste0(study_id, "_encryption_keys")

  tryCatch({
    postgres_module$saveData(key_data)

    # Log the action
    log_user_action(
      postgres_module, study_id, user_id,
      paste("Stored encryption key:", key_name),
      table_name = paste0(study_id, "_encryption_keys")
    )

  }, finally = {
    postgres_module$datatable <- original_datatable
  })
}

#' Retrieve Encrypted Key
#'
#' Retrieves and decrypts encryption keys from the database
#'
#' @param postgres_module A postgresModule instance
#' @param study_id Character string identifying the study
#' @param key_name Name/identifier for the key
#' @param master_password Character string for key decryption
#' @param user_id User retrieving the key
#'
#' @return Raw vector containing the decrypted key
#'
#' @export
retrieve_encrypted_key <- function(postgres_module, study_id, key_name,
                                   master_password, user_id) {

  # Query for the key
  query <- sprintf("
    SELECT encrypted_key, key_salt
    FROM %s_encryption_keys
    WHERE key_name = '%s' AND is_active = TRUE
  ", study_id, key_name)

  result <- postgres_module$executeQuery(query)

  if (nrow(result) == 0) {
    stop("Encryption key not found")
  }

  # Extract encrypted key and salt
  encrypted_key <- result$encrypted_key[[1]]
  salt <- result$key_salt[[1]]

  # Derive key from master password
  derived_key <- sodium::scrypt(charToRaw(master_password), salt, size = 32)

  # Decrypt the key
  tryCatch({
    decrypted_key <- sodium::data_decrypt(encrypted_key, derived_key)

    # Log the action
    log_user_action(
      postgres_module, study_id, user_id,
      paste("Retrieved encryption key:", key_name),
      table_name = paste0(study_id, "_encryption_keys")
    )

    return(decrypted_key)

  }, error = function(e) {
    # Log failed attempt
    log_user_action(
      postgres_module, study_id, user_id,
      paste("FAILED key retrieval attempt:", key_name),
      table_name = paste0(study_id, "_encryption_keys")
    )

    stop("Invalid master password or corrupted key")
  })
}
