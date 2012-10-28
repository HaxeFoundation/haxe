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
package cs.system.text.regularExpressions;
import cs.NativeArray;

@:native('System.Text.RegularExpressions.Regex') extern class Regex 
{
	function new(pattern:String, options:RegexOptions):Void;
	function Match(input:String):Match;
	function Split(input:String):NativeArray<String>;
	function Replace(input:String, replacement:String):String;
}

@:native("System.Text.RegularExpressions.RegexOptions") extern enum RegexOptions
{
	None;
	IgnoreCase;
	Multiline;
	ExplicitCapture;
	Compiled;
	Singleline;
	IgnorePatternWhitespace;
	RightToLeft;
	ECMAScript;
	CultureInvariant;
}

@:native("System.Text.RegularExpressions.Capture") extern class Capture
{
	var Index(default, null):Int;
	var Length(default, null):Int;
	var Value(default, null):String;
}

@:native("System.Text.RegularExpressions.Group") extern class Group extends Capture
{
	var Success(default, null):Bool;
}

@:native("System.Text.RegularExpressions.Match") extern class Match extends Group
{
	var Captures(default, null):CaptureCollection;
	var Groups(default, null):GroupCollection;
}

@:native("System.Text.RegularExpressions.CaptureCollection") extern class CaptureCollection implements ArrayAccess<Capture>
{
	var Count(default, null):Int;
}

@:native("System.Text.RegularExpressions.GroupCollection") extern class GroupCollection implements ArrayAccess<Group>
{
	var Count(default, null):Int;
}