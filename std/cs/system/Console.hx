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
package cs.system;

/**
	Warning: This class definition is incomplete.
	In order to get most current extern definitions, install/update hxcs library with:
		haxelib install hxcs
	Please refer to http://lib.haxe.org/p/hxcs for more information.
**/
@:native('System.Console') extern class Console 
{
	static var Error(default, null):cs.system.io.StreamWriter;
	static var In(default, null):cs.system.io.StreamReader;
	static var Out(default, null):cs.system.io.StreamWriter;
	
	static function Write(obj:Dynamic):Void;
	static function WriteLine(obj:Dynamic):Void;
	
	#if !(Xbox || CF || MF)
	static function OpenStandardOutput():cs.system.io.Stream;
	static function OpenStandardInput():cs.system.io.Stream;
	static function OpenStandardError():cs.system.io.Stream;
	#end
}