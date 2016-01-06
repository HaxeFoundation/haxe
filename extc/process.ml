(*
 *  Extc : C common OCaml bindings
 *  Copyright (c)2004-2015 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *)

type process

external run : string -> string array option -> process = "process_run"
external read_stdout : process -> string -> int -> int -> int = "process_stdout_read"
external read_stderr : process -> string -> int -> int -> int = "process_stderr_read"
external write_stdin : process -> string -> int -> int -> int = "process_stdin_write"
external close_stdin : process -> unit = "process_stdin_close"
external exit : process -> int = "process_exit"
external pid : process -> int = "process_pid"
external close : process -> unit = "process_close"
external kill : process -> unit = "process_kill"

