(*
 *  This file is part of SwfLib
 *  Copyright (c)2004 Nicolas Cannasse
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

let inflate i =
	IO.input_string (Extc.unzip (IO.read_all i))

let deflate o =
	let buf = Buffer.create 0 in
	let flush() =
		let data = Buffer.contents buf in
		IO.nwrite o (Extc.zip data);
		IO.flush o;
		Buffer.reset buf;
	in
	IO.create_out 
		~write:(Buffer.add_char buf)
		~output:(fun s p l -> Buffer.add_substring buf s p l; l)
		~flush
		~close:(fun () -> flush(); IO.close_out o)

;;
Swf.__inflate := inflate;
Swf.__deflate := deflate;