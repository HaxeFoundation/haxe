/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package java.lang;
import haxe.Int64;

extern class System 
{
	static var err(default, null):java.io.PrintStream;
	static var out(default, null):java.io.PrintStream;
	//static var in(default, null):java.io.InputStream; //collision
	
	static function arraycopy(src:Dynamic, srcPos:Int, dest:Dynamic, destPos:Int, length:Int):Void;
	static function clearProperty(key:String):String;
	static function currentTimeMillis():Int64;
	static function exit(status:Int):Void;
	@:overload(function():java.util.Map<String,String> {})
	static function getenv(name:String):String;
	static function getProperty(key:String, ?def:String):String;
	static function identityHashCode(x:Dynamic):Int;
	static function load(filename:String):Void;
	static function loadLibrary(libname:String):Void;
	static function mapLibraryName(libname:String):String;
	static function nanoTime():Int64;
	static function gc():Void;
	static function runFinalization():Void;
	static function setErr(err:java.io.PrintStream):Void;
	static function setOut(out:java.io.PrintStream):Void;
	static function setIn(v:java.io.InputStream):Void;
	static function setProperty(key:String, value:String):String;
	
}