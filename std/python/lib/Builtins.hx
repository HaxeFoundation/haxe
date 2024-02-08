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

package python.lib;

import haxe.extern.Rest;
import python.lib.io.FileIO;
import python.Dict;
import python.lib.io.IOBase;
import python.NativeIterable;
import python.NativeIterator;

@:pythonImport("builtins")
extern class Builtins {
	@:overload(function(f:Int):Int {})
	static function abs(x:Float):Float;
	static function all(i:Iterable<Bool>):Bool;
	static function any(i:Iterable<Bool>):Bool;

	static function bool(x:Dynamic):Bool;

	static function issubclass(x:Class<Dynamic>, from:Class<Dynamic>):Bool;
	static function callable(x:Dynamic):Bool;

	@:overload(function(obj:Dynamic, f:Tuple<Dynamic>):Bool {})
	static function isinstance(obj:Dynamic, cl:Dynamic):Bool;

	static function hasattr(obj:Dynamic, attr:String):Bool;
	static function getattr(obj:Dynamic, attr:String):Dynamic;

	@:overload(function(f:Set<Dynamic>):Int {})
	@:overload(function(f:StringBuf):Int {})
	@:overload(function(f:Array<Dynamic>):Int {})
	@:overload(function(f:Dict<Dynamic, Dynamic>):Int {})
	@:overload(function(f:Bytes):Int {})
	@:overload(function(f:DictView<Dynamic>):Int {})
	@:overload(function(f:Bytearray):Int {})
	@:overload(function(f:Tuple<Dynamic>):Int {})
	static function len(x:String):Int;

	static function open(file:String, mode:String, ?buffering:Int = -1, ?encoding:String = null, ?errors:String, ?newline:String, ?closefd:Bool,
		?opener:String->Int->FileDescriptor):IOBase;

	// static function divmod():Void;
	// static function input():Void;
	// static function staticmethod():Void;
	// static function enumerate():Void;
	@:overload(function(x:Dynamic, base:Int):Int {})
	static function int(x:Dynamic):Int;
	static function ord(s:String):Int;
	static function str(o:Dynamic):String;

	// static function eval():Void;
	// static function pow():Void;
	// static function sum():Void;
	// static function basestring():Void;
	// static function execfile():Void;
	static function print(o:Dynamic):Void;

	// static function super():Void;
	// static function bin():Void;
	// static function file():Void;
	static function iter<X>(d:DictView<X>):NativeIterator<X>;

	// static function property():Void;
	/*
		@:overload(function <X>():Tuple<X> {})
		static function tuple<X>(a:Array<X>):Tuple<X>;
	 */
	// static function range():Void;
	static function type():Void;
	/*
		@:overload(function (it:Array<Int>):python.Bytearray {})
		@:overload(function (it:NativeIterable<Int>):python.Bytearray {})
		@:overload(function (size:Int):python.Bytearray {})
		static function bytearray(source:String,encoding:String,?errors:Dynamic):python.Bytearray;
	 */
	static function float(x:Dynamic):Float;

	@:overload(function<T>(f:Array<T>):Array<T> {})
	@:overload(function<T>(f:Tuple<T>):Array<T> {})
	@:overload(function<T>(f:Dict.DictView<T>):Array<T> {})
	@:overload(function(f:String):Array<String> {})
	static function list<T>(i:NativeIterable<T>):Array<T>;

	@:overload(function<A>(f:A->Bool, i:NativeIterable<A>):NativeIterator<A> {})
	static function filter<A>(f:A->Bool, i:Array<A>):NativeIterator<A>;

	// static function raw_input():Void;
	// static function unichr():Void;
	// static function format():Void;
	// static function locals():Void;
	// static function reduce():Void;
	// static function unicode():Void;
	static function chr(c:Int):String;

	// static function frozenset():Void;
	// static function long():Void;
	// static function reload():Void;
	// static function vars():Void;
	// static function classmethod():Void;
	static function map<A, B>(fn:A->B, it:NativeIterable<A>):NativeIterator<B>;
	static function repr(o:Dynamic):String;
	// static function xrange():Void;
	// static function cmp():Void;
	// static function globals():Void;
	@:overload(function(a1:Float, a2:Float, rest:Rest<Float>):Float {})
	static function max(a1:Int, a2:Int, rest:Rest<Int>):Int;

	// static function reversed():Void;
	// static function zip():Void;
	// static function compile():Void;
	// static function memoryview():Void;
	static function round(f:Float):Int;
	// static function __import__():Void;
	// static function complex():Void;
	// static function hash():Void;
	@:overload(function(a1:Float, a2:Float, rest:Rest<Float>):Float {})
	static function min(a1:Int, a2:Int, rest:Rest<Int>):Int;
	// static function set():Void;
	// static function apply():Void;
	static function delattr(o:Dynamic, attr:String):Void;
	// static function help():Void;
	// static function next():Void;
	static function setattr(o:Dynamic, attr:String, val:Dynamic):Void;
	// static function buffer():Void;
	// static function dict():Void;
	// static function hex():Void;
	// static function object():Void;
	// static function slice():Void;
	// static function coerce():Void;
	// static function dir():Void;
	static function id(x:{}):Int;
	// static function oct():Void;
	// static function sorted():Void;
	// static function intern():Void;
}
