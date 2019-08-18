/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package php.db;

import haxe.Constraints;
import php.*;
import haxe.extern.EitherType;

@:native('SQLite3')
extern class SQLite3 {
	static function version():NativeArray;
	static function escapeString(value:String):String;
	function busyTimeout(msecs:Int):Bool;
	function changes():Int;
	function close():Bool;
	function new(filename:String, ?flags:Int, encryption_key:String = null):Void;
	function createAggregate(name:String, step_callback:Function, final_callback:Function, argument_count:Int = -1):Bool;
	function createCollation(name:String, callback:Function):Bool;
	function createFunction(name:String, callback:Function, argument_count:Int = -1):Bool;
	function enableExceptions(enableExceptions:Bool = false):Bool;
	function exec(query:String):Bool;
	function lastErrorCode():Int;
	function lastErrorMsg():String;
	function lastInsertRowID():Int;
	function loadExtension(shared_library:String):Bool;
	function open(filename:String, ?flags:Int, encryption_key:String = null):Void;
	function prepare(query:String):SQLite3Stmt;
	function query(query:String):EitherType<Bool, SQLite3Result>;
	function querySingle(query:String, entire_row:Bool = false):Dynamic;
}
