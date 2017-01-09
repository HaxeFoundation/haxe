/*
 * Copyright (C)2005-2017 Haxe Foundation
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
  Externs for the "debug" class for Haxe lua  
**/
import haxe.Constraints.Function;

import lua.Table.AnyTable;

@:native("debug")
extern class Debug {
	/**
		This function returns the name and the value of the local variable with 
		index local of the function at level level of the stack.
	**/
	public static function getlocal(stackLevel : Int, idx : Int) : Dynamic;

	/**
		This function assigns the value value to the local variable with index 
		local of the function at level level of the stack. 
		Call `getinfo` to check whether the level is valid. 
	**/
	public static function setlocal(stackLevel : Int, varName: String, value: Dynamic) : Void;

	/**
		Returns a table with information about a function. 
	**/
	public static function getinfo(stackLevel  : Int) : DebugInfo;

	/**
		Sets the given function as a hook. 
		When called without arguments, `Debug.sethook` turns off the hook.
	**/
	public static function sethook(?fun : Function, ?monitor : String) : Void;

	/**
		Enters an interactive mode with the user, running each string that the user enters. 
		Using simple commands and other debug facilities, the user can inspect 
		global and local variables, change their values, evaluate expressions, 
		and so on. A line containing only the word `cont` finishes this function, 
		so that the caller continues its execution.

		Note that commands for `Debug.debug` are not lexically nested within any 
		function, and so have no direct access to local variables.
	**/
	public static function debug() : Void;

	/**
		Returns the current hook settings of the thread, as three values: 
		the current hook function, the current hook mask, and the current hook count 
		(as set by the `Debug.sethook` function).
	**/
	public static function gethook(thread : Thread) : Function;

	/**
		Returns the registry table.
	**/
	public static function getregistry() : AnyTable;

	/**
		Returns the metatable of the given `value` or `null` if it does not have a metatable.
	**/
	public static function getmetatable(value : AnyTable) : AnyTable;

	/**
		Sets the metatable for the given `value` to the given `table` (can be `null`).
	**/
	public static function setmetatable(value : AnyTable, table : AnyTable) : Void;

	/**
		This function returns the name and the value of the upvalue with index `up` 
		of the function `f`. The function returns `null` if there is no upvalue with 
		the given index.
	**/
	public static function getupvalue(f : Function, up : Int) : Dynamic;

	/**
		This function assigns the value value to the upvalue with index up of
		the function `f`. The function returns `null` if there is no upvalue with
		the given index. Otherwise, it returns the name of the upvalue.
	**/
	public static function setupvalue(f : Function, up : Int, val : Dynamic) : Void;

	/**
		Returns the Lua value associated to `val`. 
		If `val` is not a `UserData`, returns `null`.
	**/
	public static function getuservalue(val : Dynamic) : Dynamic;

	/**
		Sets the given value as the Lua value associated to the given udata. 
		`udata` must be a full `UserData`.
	**/
	public static function setuservalue(udata : Dynamic, val : Dynamic) : Void;

	/**
		Returns a string with a traceback of the call stack. 
		@param message (optional) is appended at the beginning of the traceback.
		@param level (optional) tells at which level to start the traceback. 
		       default is `1`, the function calling traceback.
	**/
	public static function traceback(?thread : Thread, ?message : String, ?level : Int) : String;

	/**
		Returns a unique identifier (as a light userdata) for the upvalue numbered 
		`n` from the given function `f`.
	**/
	public static function upvalueid(f : Function, n : Int) : Dynamic;

	/**
		Make the `n1`-th upvalue of the Lua closure `f1` refer to the `n2`-th 
		upvalue of the Lua closure `f2`.
	**/
	public static function upvaluejoin(f1 : Function, n1 : Int, f2 : Function, n2 : Int) : Void;
}

/**
	A enumerator that describes the output of `Debug.getinfo()`.
**/
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

