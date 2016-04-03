/*
 * Copyright (C)2005-2016 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package lua;
/**
  Externs for the "debug" class for haxe lua  
**/
import haxe.Constraints.Function;

import lua.Table.AnyTable;

@:native("debug")
extern class Debug {
	public static function getlocal(stackLevel : Int, varName : String) : Dynamic;
	public static function setlocal(stackLevel : Int, varName: String, value: Dynamic) : Void;
	public static function getinfo(stackLevel  : Int) : DebugInfo;
	public static function sethook(?fun : Function, ?monitor : String) : Void;
	public static function debug() : Void;
	public static function gethook(thread : Coroutine) : Function;
	public static function getregistry() : AnyTable;
	public static function getmetatable(value : AnyTable) : AnyTable;
	public static function setmetatable(value : AnyTable, table : AnyTable) : Void;
	public static function getupvalue(f : Function, up : Int) : Dynamic;
	public static function setupvalue(f : Function, up : Int, val : Dynamic) : Void;
	public static function getuservalue(val : Dynamic) : Dynamic;
	public static function setuservalue(udata : Dynamic, val : Dynamic) : Void;
	public static function traceback(?thread : Coroutine, ?message : String, ?level : Int) : Void;
	public static function upvalueid(f : Function, n : Int) : Dynamic;
	public static function upvaluejoin(f1 : Function, n1 : Int, f2 : Function, n2 : Int) : Void;
}

typedef DebugInfo = { 
	currentline     : Int,
	func            : Function,
	istailcall      : Bool,
	isvararg        : Bool,
	lastlinedefined : Int,
	linedefined     : Int,
	name            : String,
	namewhat        : String,
	nparams         : Int,
	nups            : Int,
	short_src       : String,
	source          : String,
	what            : String
}

