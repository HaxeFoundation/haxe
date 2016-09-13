let version = 3300
let version_major = version / 1000
let version_minor = (version mod 1000) / 100
let version_revision = (version mod 100)
let version_is_stable = version_minor land 1 = 0

let is_windows = Sys.os_type = "Win32" || Sys.os_type = "Cygwin"

let s_version =
	Printf.sprintf "%d.%d.%d%s" version_major version_minor version_revision (match Version.version_extra with None -> "" | Some v -> " " ^ v)