(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open EvalBytes_E_L79
open EvalBytes_U79
open EvalBytes_U1021

let _UPPER = Array.of_list [_E;_U1;_U2;_U3;_U4;_U5;_U6;_U7;_U8;_U9;_U10;_E;_E;_U13;_U14;_U15;_U16;_U17;_U18;_U19;_U20;_U21;_U22;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_U79;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_U117;_E;_E;_U120;_U121;_U122;_U123;_U124;_U125;_U126;_U127;_E;_E;_E;_E;_E;_U133;_U134;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_U147;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_U176;_U177;_U178;_U179;_U180;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_U665;_U666;_E;_U668;_U669;_U670;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_U685;_U686;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_E;_U1021]