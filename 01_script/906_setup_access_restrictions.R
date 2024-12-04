saros::setup_access_restrictions(
  remote_basepath = paths$remote_basepath,
  local_basepath = paths$local_basepath,
  rel_path_base_to_parent_of_user_restricted_folder = params$reports,
  local_main_password_path = paths$local_main_password_path,
  universal_usernames = c("admin", "nifu", "udir"))