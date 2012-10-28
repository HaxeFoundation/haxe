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
package cs.system.threading;
import cs.system.LocalDataStoreSlot;

@:native("System.Threading.ThreadStart") @:delegate typedef ThreadStart = Void->Void;

@:native("System.Threading.Thread") extern class Thread
{
	static function AllocateDataStoreSlot():LocalDataStoreSlot;
	static function GetData(slot:LocalDataStoreSlot):Dynamic;
	static function SetData(slot:LocalDataStoreSlot, data:Dynamic):Void;
	static function Sleep(ms:Int):Void;
	
	@:overload(function(s:ThreadStart, maxStack:Int):Void {})
	function new(s:ThreadStart):Void;
	
	@:overload(function(obj:Dynamic):Void {})
	function Abort():Void;
	
	@:overload(function(msTimeout:Int):Void {})
	function Join():Void;
	
	function Start():Void;
}