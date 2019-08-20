(* ------------- TYPES ---------------------------------------------- *)

(* Handle types *)

type t_loop
type t_handle
type t_dir
type t_stream
type t_tcp
type t_udp
type t_pipe
type t_tty
type t_poll
type t_timer
type t_prepare
type t_check
type t_idle
type t_async
type t_process
type t_fs_event
type t_fs_poll
type t_signal

(* Request types *)

type t_req
type t_getaddrinfo
type t_getnameinfo
type t_shutdown
type t_write
type t_connect
type t_udp_send
type t_fs
type t_work

(* Other types *)

type t_cpu_info
type t_interface_address
type t_dirent
type t_passwd
type t_utsname
type t_file
(* type t_stat *)
type t_buf

(* Non-abstract type definitions  *)

type t_stat = {
	dev: int;
	mode: int;
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

type 'a uv_result =
	| UvError of int (* error number *)
	| UvSuccess of 'a

type unit_cb = unit uv_result -> unit

(* ------------- LOOP ----------------------------------------------- *)

external loop_init : unit -> t_loop uv_result = "w_loop_init"
external loop_close : t_loop -> unit uv_result = "w_loop_close"
external run : t_loop -> int -> bool uv_result = "w_run"
external stop : t_loop -> unit uv_result = "w_stop"
external loop_alive : t_loop -> bool uv_result = "w_loop_alive"

(* ------------- FILESYSTEM ----------------------------------------- *)

type fs_cb_bytes = string uv_result -> unit
type fs_cb_path = string uv_result -> unit
type fs_cb_file = t_file uv_result -> unit
type fs_cb_int = int uv_result -> unit
type fs_cb_stat= t_stat uv_result -> unit
type fs_cb_scandir = (string * int) list uv_result -> unit

external fs_access : t_loop -> string -> int -> unit_cb -> unit uv_result = "w_fs_access"
external fs_chmod : t_loop -> string -> int -> unit_cb -> unit uv_result = "w_fs_chmod"
external fs_chown : t_loop -> string -> int -> int -> unit_cb -> unit uv_result = "w_fs_chown"
external fs_close : t_loop -> t_file -> unit_cb -> unit uv_result = "w_fs_close"
external fs_fchmod : t_loop -> t_file -> int -> unit_cb -> unit uv_result = "w_fs_fchmod"
external fs_fchown : t_loop -> t_file -> int -> int -> unit_cb -> unit uv_result = "w_fs_fchown"
external fs_fdatasync : t_loop -> t_file -> unit_cb -> unit uv_result = "w_fs_fdatasync"
external fs_fstat : t_loop -> t_file -> fs_cb_stat -> unit uv_result = "w_fs_fstat"
external fs_fsync : t_loop -> t_file -> unit_cb -> unit uv_result = "w_fs_fsync"
external fs_ftruncate : t_loop -> t_file -> int64 -> unit_cb -> unit uv_result = "w_fs_ftruncate"
external fs_futime : t_loop -> t_file -> float -> float -> unit_cb -> unit uv_result = "w_fs_futime"
external fs_link : t_loop -> string -> string -> unit_cb -> unit uv_result = "w_fs_link"
external fs_lstat : t_loop -> string -> fs_cb_stat -> unit uv_result = "w_fs_lstat"
external fs_mkdir : t_loop -> string -> int -> unit_cb -> unit uv_result = "w_fs_mkdir"
external fs_mkdtemp : t_loop -> string -> fs_cb_path -> unit uv_result = "w_fs_mkdtemp"
external fs_open : t_loop -> string -> int -> int -> fs_cb_file -> unit uv_result = "w_fs_open"
external fs_read : t_loop -> t_file -> bytes -> int -> int -> int -> fs_cb_int -> unit uv_result = "w_fs_read_bytecode" "w_fs_read"
external fs_readlink : t_loop -> string -> fs_cb_bytes -> unit uv_result = "w_fs_readlink"
external fs_realpath : t_loop -> string -> fs_cb_bytes -> unit uv_result = "w_fs_realpath"
external fs_rename : t_loop -> string -> string -> unit_cb -> unit uv_result = "w_fs_rename"
external fs_rmdir : t_loop -> string -> unit_cb -> unit uv_result = "w_fs_rmdir"
external fs_scandir : t_loop -> string -> int -> fs_cb_scandir -> unit uv_result = "w_fs_scandir"
external fs_sendfile : t_loop -> t_file -> t_file -> int -> int -> unit_cb -> unit uv_result = "w_fs_sendfile_bytecode" "w_fs_sendfile"
external fs_stat : t_loop -> string -> fs_cb_stat -> unit uv_result = "w_fs_stat"
external fs_symlink : t_loop -> string -> string -> int -> unit_cb -> unit uv_result = "w_fs_symlink"
external fs_unlink : t_loop -> string -> unit_cb -> unit uv_result = "w_fs_unlink"
external fs_utime : t_loop -> string -> float -> float -> unit_cb -> unit uv_result = "w_fs_utime"
external fs_write : t_loop -> t_file -> bytes -> int -> int -> int -> fs_cb_int -> unit uv_result = "w_fs_write_bytecode" "w_fs_write"

external fs_access_sync : t_loop -> string -> int -> unit uv_result = "w_fs_access_sync"
external fs_chmod_sync : t_loop -> string -> int -> unit uv_result = "w_fs_chmod_sync"
external fs_chown_sync : t_loop -> string -> int -> int -> unit uv_result = "w_fs_chown_sync"
external fs_close_sync : t_loop -> t_file -> unit uv_result = "w_fs_close_sync"
external fs_fchmod_sync : t_loop -> t_file -> int -> unit uv_result = "w_fs_fchmod_sync"
external fs_fchown_sync : t_loop -> t_file -> int -> int -> unit uv_result = "w_fs_fchown_sync"
external fs_fdatasync_sync : t_loop -> t_file -> unit uv_result = "w_fs_fdatasync_sync"
external fs_fstat_sync : t_loop -> t_file -> t_stat uv_result = "w_fs_fstat_sync"
external fs_fsync_sync : t_loop -> t_file -> unit uv_result = "w_fs_fsync_sync"
external fs_ftruncate_sync : t_loop -> t_file -> int64 -> unit uv_result = "w_fs_ftruncate_sync"
external fs_futime_sync : t_loop -> t_file -> float -> float -> unit uv_result = "w_fs_futime_sync"
external fs_link_sync : t_loop -> string -> string -> unit uv_result = "w_fs_link_sync"
external fs_lstat_sync : t_loop -> string -> t_stat uv_result = "w_fs_lstat_sync"
external fs_mkdir_sync : t_loop -> string -> int -> unit uv_result = "w_fs_mkdir_sync"
external fs_mkdtemp_sync : t_loop -> string -> string uv_result = "w_fs_mkdtemp_sync"
external fs_open_sync : t_loop -> string -> int -> int -> t_file uv_result = "w_fs_open_sync"
external fs_read_sync : t_loop -> t_file -> bytes -> int -> int -> int -> int uv_result = "w_fs_read_sync_bytecode" "w_fs_read_sync"
external fs_readlink_sync : t_loop -> string -> string uv_result = "w_fs_readlink_sync"
external fs_realpath_sync : t_loop -> string -> string uv_result = "w_fs_realpath_sync"
external fs_rename_sync : t_loop -> string -> string -> unit uv_result = "w_fs_rename_sync"
external fs_rmdir_sync : t_loop -> string -> unit uv_result = "w_fs_rmdir_sync"
external fs_scandir_sync : t_loop -> string -> int -> (string * int) list uv_result = "w_fs_scandir_sync"
external fs_sendfile_sync : t_loop -> t_file -> t_file -> int -> int -> unit uv_result = "w_fs_sendfile_sync_bytecode" "w_fs_sendfile_sync"
external fs_stat_sync : t_loop -> string -> t_stat uv_result = "w_fs_stat_sync"
external fs_symlink_sync : t_loop -> string -> string -> int -> unit uv_result = "w_fs_symlink_sync"
external fs_unlink_sync : t_loop -> string -> unit uv_result = "w_fs_unlink_sync"
external fs_utime_sync : t_loop -> string -> float -> float -> unit uv_result = "w_fs_utime_sync"
external fs_write_sync : t_loop -> t_file -> bytes -> int -> int -> int -> int uv_result = "w_fs_write_sync_bytecode" "w_fs_write_sync"

(* ------------- HANDLE --------------------------------------------- *)

(* 'a should be a subtype of t_handle (uv_handle_t) *)
external close : 'a -> unit_cb -> unit uv_result = "w_close"
external ref_ : 'a -> unit = "w_ref"
external unref : 'a -> unit = "w_unref"

(* ------------- FILESYSTEM EVENTS ---------------------------------- *)

type fs_event_cb = (string * int) uv_result -> unit

external fs_event_start : t_loop -> string -> bool -> fs_event_cb -> t_fs_event uv_result = "w_fs_event_start"
external fs_event_stop : t_fs_event -> unit_cb -> unit uv_result = "w_fs_event_stop"

(* ------------- STREAM --------------------------------------------- *)

type stream_bytes_cb = bytes uv_result -> unit

(* 'a should be a subtype of t_stream (uv_stream_t) *)
external shutdown : 'a -> unit_cb -> unit uv_result = "w_shutdown"
external listen : 'a -> int -> unit_cb -> unit uv_result = "w_listen"
external write : 'a -> bytes -> unit_cb -> unit uv_result = "w_write"
external read_start : 'a -> stream_bytes_cb -> unit uv_result = "w_read_start"
external read_stop : 'a -> unit uv_result = "w_read_stop"
external stream_of_handle : 'a -> t_stream = "w_stream_of_handle"

(* ------------- TCP ------------------------------------------------ *)

type uv_ip_address =
	| UvIpv4 of int32
	| UvIpv6 of bytes

type uv_ip_address_port = {
	address: uv_ip_address;
	port: int;
}

external tcp_init : t_loop -> t_tcp uv_result = "w_tcp_init"
external tcp_nodelay : t_tcp -> bool -> unit uv_result = "w_tcp_nodelay"
external tcp_keepalive : t_tcp -> bool -> int -> unit uv_result = "w_tcp_keepalive"
external tcp_accept : t_loop -> t_tcp -> t_tcp uv_result = "w_tcp_accept"
external tcp_bind_ipv4 : t_tcp -> int -> int -> unit uv_result = "w_tcp_bind_ipv4"
external tcp_bind_ipv6 : t_tcp -> bytes -> int -> bool -> unit uv_result = "w_tcp_bind_ipv6"
external tcp_connect_ipv4 : t_tcp -> int -> int -> unit_cb -> unit uv_result = "w_tcp_connect_ipv4"
external tcp_connect_ipv6 : t_tcp -> bytes -> int -> unit_cb -> unit uv_result = "w_tcp_connect_ipv6"
external tcp_getsockname : t_tcp -> uv_ip_address_port uv_result = "w_tcp_getsockname"
external tcp_getpeername : t_tcp -> uv_ip_address_port uv_result = "w_tcp_getpeername"

(* ------------- UDP ------------------------------------------------ *)

type udp_message = {
	data: bytes;
	address: uv_ip_address;
	port: int;
}

type udp_read_cb = udp_message uv_result -> unit

external udp_init : t_loop -> t_udp uv_result = "w_udp_init"
external udp_bind_ipv4 : t_udp -> int -> int -> unit uv_result = "w_udp_bind_ipv4"
external udp_bind_ipv6 : t_udp -> bytes -> int -> bool -> unit uv_result = "w_udp_bind_ipv6"
external udp_send_ipv4 : t_udp -> bytes -> int -> int -> int -> int -> unit_cb -> unit uv_result = "w_udp_send_ipv4_bytecode" "w_udp_send_ipv4"
external udp_send_ipv6 : t_udp -> bytes -> int -> int -> bytes -> int -> unit_cb -> unit uv_result = "w_udp_send_ipv6_bytecode" "w_udp_send_ipv6"
external udp_recv_start : t_udp -> udp_read_cb -> unit uv_result = "w_udp_recv_start"
external udp_recv_stop : t_udp -> unit uv_result = "w_udp_recv_stop"
external udp_set_membership : t_udp -> string -> string -> bool -> unit uv_result = "w_udp_set_membership"
external udp_close : t_udp -> unit_cb -> unit uv_result = "w_udp_close"
external udp_getsockname : t_udp -> uv_ip_address_port uv_result = "w_udp_getsockname"
external udp_set_broadcast : t_udp -> bool -> unit uv_result = "w_udp_set_broadcast"
external udp_set_multicast_interface : t_udp -> string -> unit uv_result = "w_udp_set_multicast_interface"
external udp_set_multicast_loopback : t_udp -> bool -> unit uv_result = "w_udp_set_multicast_loopback"
external udp_set_multicast_ttl : t_udp -> int -> unit uv_result = "w_udp_set_multicast_ttl"
external udp_set_ttl : t_udp -> int -> unit uv_result = "w_udp_set_ttl"
external udp_get_recv_buffer_size : t_udp -> int = "w_udp_get_recv_buffer_size"
external udp_get_send_buffer_size : t_udp -> int = "w_udp_get_send_buffer_size"
external udp_set_recv_buffer_size : t_udp -> int -> int = "w_udp_set_recv_buffer_size"
external udp_set_send_buffer_size : t_udp -> int -> int = "w_udp_set_send_buffer_size"

(* ------------- DNS ------------------------------------------------ *)

type dns_gai_cb = (uv_ip_address list) uv_result -> unit

external dns_getaddrinfo : t_loop -> string -> bool -> bool -> int -> dns_gai_cb -> unit uv_result = "w_dns_getaddrinfo_bytecode" "w_dns_getaddrinfo"

(* ------------- TIMERS --------------------------------------------- *)

type timer_cb = unit -> unit

external timer_start : t_loop -> int -> timer_cb -> t_timer uv_result = "w_timer_start"
external timer_stop : t_timer -> unit_cb -> unit uv_result = "w_timer_stop"

(* ------------- PROCESS -------------------------------------------- *)

type process_cb = (int * int) uv_result -> unit

type process_io =
	| UvIoPipe of bool * bool * t_stream
	| UvIoIgnore
	| UvIoInherit

external spawn : t_loop -> process_cb -> string -> string array -> string array -> string -> int -> process_io array -> int -> int -> t_process uv_result = "w_spawn_bytecode" "w_spawn"
external process_kill : t_process -> int -> unit uv_result = "w_process_kill"
external process_get_pid : t_process -> int = "w_process_get_pid"

(* ------------- PIPES ---------------------------------------------- *)

external pipe_init : t_loop -> bool -> t_pipe uv_result = "w_pipe_init"
external pipe_accept : t_loop -> t_pipe -> t_pipe uv_result = "w_pipe_accept"
external pipe_bind_ipc : t_pipe -> string -> unit uv_result = "w_pipe_bind_ipc"
external pipe_connect_ipc : t_pipe -> string -> unit_cb -> unit uv_result = "w_pipe_connect_ipc"
