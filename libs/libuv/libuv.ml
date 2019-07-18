(* ------------- TYPES ---------------------------------------------- *)

(* Handle types *)

type uv_loop_t
type uv_handle_t
type uv_dir_t
type uv_stream_t
type uv_tcp_t
type uv_udp_t
type uv_pipe_t
type uv_tty_t
type uv_poll_t
type uv_timer_t
type uv_prepare_t
type uv_check_t
type uv_idle_t
type uv_async_t
type uv_process_t
type uv_fs_event_t
type uv_fs_poll_t
type uv_signal_t

(* Request types *)

type uv_req_t
type uv_getaddrinfo_t
type uv_getnameinfo_t
type uv_shutdown_t
type uv_write_t
type uv_connect_t
type uv_udp_send_t
type uv_fs_t
type uv_work_t

(* Other types *)

type uv_cpu_info_t
type uv_interface_address_t
type uv_dirent_t
type uv_passwd_t
type uv_utsname_t
type uv_file
(* type uv_stat_t *)
type uv_buf_t

(* Non-abstract type definitions  *)

type uv_stat_t = {
  dev: int;
  kind: int;
  perm: int;
  nlink: int;
  uid: int;
  gid: int;
  rdev: int;
  ino: int;
  size: int64;
  blksize: int;
  blocks: int;
  flags: int;
  gen: int;
  atime: int64;
  atime_nsec: int;
  mtime: int64;
  mtime_nsec: int;
  ctime: int64;
  ctime_nsec: int;
  birthtime: int64;
  birthtime_nsec: int;
}

(* ------------- LOOP ----------------------------------------------- *)

external loop_init : unit -> uv_loop_t = "w_loop_init"
external loop_close : uv_loop_t -> unit = "w_loop_close"
external run : uv_loop_t -> int -> bool = "w_run"
external loop_alive : uv_loop_t -> bool = "w_loop_alive"

(* ------------- FILESYSTEM ----------------------------------------- *)

type fs_cb = unit -> unit
type fs_cb_bytes = string -> unit
type fs_cb_path = string -> unit
type fs_cb_file = uv_file -> unit
type fs_cb_int = int -> unit
type fs_cb_stat= uv_stat_t -> unit
type fs_cb_scandir = (string * int) list -> unit

external fs_close : uv_loop_t -> uv_file -> fs_cb -> unit = "w_fs_close"
external fs_open : uv_loop_t -> string -> int -> int -> fs_cb_file -> unit = "w_fs_open"
external fs_unlink : uv_loop_t -> string -> fs_cb -> unit = "w_fs_unlink"
external fs_mkdir : uv_loop_t -> string -> int -> fs_cb -> unit = "w_fs_mkdir"
external fs_mkdtemp : uv_loop_t -> string -> fs_cb_path -> unit = "w_fs_mkdtemp"
external fs_rmdir : uv_loop_t -> string -> fs_cb -> unit = "w_fs_rmdir"
external fs_scandir : uv_loop_t -> string -> int -> fs_cb_scandir -> unit = "w_fs_scandir"
external fs_stat : uv_loop_t -> string -> fs_cb_stat -> unit = "w_fs_stat"
external fs_fstat : uv_loop_t -> uv_file -> fs_cb_stat -> unit = "w_fs_fstat"
external fs_lstat : uv_loop_t -> string -> fs_cb_stat -> unit = "w_fs_lstat"
external fs_rename : uv_loop_t -> string -> string -> fs_cb -> unit = "w_fs_rename"
external fs_fsync : uv_loop_t -> uv_file -> fs_cb -> unit = "w_fs_fsync"
external fs_fdatasync : uv_loop_t -> uv_file -> fs_cb -> unit = "w_fs_fdatasync"
external fs_ftruncate : uv_loop_t -> uv_file -> int64 -> fs_cb -> unit = "w_fs_ftruncate"
external fs_access : uv_loop_t -> string -> int -> fs_cb -> unit = "w_fs_access"
external fs_chmod : uv_loop_t -> string -> int -> fs_cb -> unit = "w_fs_chmod"
external fs_fchmod : uv_loop_t -> uv_file -> int -> fs_cb -> unit = "w_fs_fchmod"
external fs_utime : uv_loop_t -> string -> float -> float -> fs_cb -> unit = "w_fs_utime"
external fs_futime : uv_loop_t -> uv_file -> float -> float -> fs_cb -> unit = "w_fs_futime"
external fs_link : uv_loop_t -> string -> string -> fs_cb -> unit = "w_fs_link"
external fs_symlink : uv_loop_t -> string -> string -> int -> fs_cb -> unit = "w_fs_symlink"
external fs_readlink : uv_loop_t -> string -> fs_cb_bytes -> unit = "w_fs_readlink"
external fs_realpath : uv_loop_t -> string -> fs_cb_bytes -> unit = "w_fs_realpath"
external fs_chown : uv_loop_t -> string -> int -> int -> fs_cb -> unit = "w_fs_chown"
external fs_fchown : uv_loop_t -> uv_file -> int -> int -> fs_cb -> unit = "w_fs_fchown"

external fs_close_sync : uv_loop_t -> uv_file -> unit = "w_fs_close_sync"
external fs_open_sync : uv_loop_t -> string -> int -> int -> uv_file = "w_fs_open_sync"
external fs_unlink_sync : uv_loop_t -> string -> unit = "w_fs_unlink_sync"
external fs_mkdir_sync : uv_loop_t -> string -> int -> unit = "w_fs_mkdir_sync"
external fs_mkdtemp_sync : uv_loop_t -> string -> string = "w_fs_mkdtemp_sync"
external fs_rmdir_sync : uv_loop_t -> string -> unit = "w_fs_rmdir_sync"
external fs_scandir_sync : uv_loop_t -> string -> int -> (string * int) list = "w_fs_scandir_sync"
external fs_stat_sync : uv_loop_t -> string -> uv_stat_t = "w_fs_stat_sync"
external fs_fstat_sync : uv_loop_t -> uv_file -> uv_stat_t = "w_fs_fstat_sync"
external fs_lstat_sync : uv_loop_t -> string -> uv_stat_t = "w_fs_lstat_sync"
external fs_rename_sync : uv_loop_t -> string -> string -> unit = "w_fs_rename_sync"
external fs_fsync_sync : uv_loop_t -> uv_file -> unit = "w_fs_fsync_sync"
external fs_fdatasync_sync : uv_loop_t -> uv_file -> unit = "w_fs_fdatasync_sync"
external fs_ftruncate_sync : uv_loop_t -> uv_file -> int64 -> unit = "w_fs_ftruncate_sync"
external fs_access_sync : uv_loop_t -> string -> int -> unit = "w_fs_access_sync"
external fs_chmod_sync : uv_loop_t -> string -> int -> unit = "w_fs_chmod_sync"
external fs_fchmod_sync : uv_loop_t -> uv_file -> int -> unit = "w_fs_fchmod_sync"
external fs_utime_sync : uv_loop_t -> string -> float -> float -> unit = "w_fs_utime_sync"
external fs_futime_sync : uv_loop_t -> uv_file -> float -> float -> unit = "w_fs_futime_sync"
external fs_link_sync : uv_loop_t -> string -> string -> unit = "w_fs_link_sync"
external fs_symlink_sync : uv_loop_t -> string -> string -> int -> unit = "w_fs_symlink_sync"
external fs_readlink_sync : uv_loop_t -> string -> string = "w_fs_readlink_sync"
external fs_realpath_sync : uv_loop_t -> string -> string = "w_fs_realpath_sync"
external fs_chown_sync : uv_loop_t -> string -> int -> int -> unit = "w_fs_chown_sync"
external fs_fchown_sync : uv_loop_t -> uv_file -> int -> int -> unit = "w_fs_fchown_sync"
