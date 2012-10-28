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

extern class Thread 
{
	static function activeCount():Int;
	static function currentThread():Thread;
	static function dumpStack():Void;
	static function enumerate(tarray:java.NativeArray<Thread>):Int;
	static function interrupted():Bool;
	@:overload(function(ms:Int64, nano:Int):Void {})
	static function sleep(ms:Int64):Void;
	static function yield():Void;
	
	function new(target:Runnable):Void;
	
	function destroy():Void;
	function getName():String;
	function getPriority():Int;
	function interrupt():Void;
	function isAlive():Bool;
	function isDaemon():Bool;
	function isInterrupted():Bool;
	
	@:overload(function(ms:Int64):Void {})
	@:overload(function(ms:Int64, nanos:Int):Void {})
	function join():Void;
	
	function run():Void; //should call start(), not run()
	function setPriority(newPriority:Int):Void;
	function start():Void;
}