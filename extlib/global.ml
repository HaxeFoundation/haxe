(*
 * Global - Mutable global variable
 * Copyright (C) 2003 Nicolas Cannasse
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
 
exception Global_not_initialized of string

type 'a t = ('a option ref * string)

let empty name = ref None,name

let name = snd

let set (r,_) v = r := Some v

let get (r,name) =
	match !r with
	| None -> raise (Global_not_initialized name)
	| Some v -> v

let undef (r,_) = r := None

let isdef (r,_) = !r <> None

let opt (r,_) = !r
