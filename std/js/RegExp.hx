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
 package js;

/**
    Native JavaScript regular expressions.

    For cross-platform regular expressions, use Haxe `EReg` class or 
    [regexp literals](https://haxe.org/manual/std-regex.html).
**/
@:native("RegExp")
extern class RegExp {
    var global(default,null):Bool;
    var ignoreCase(default,null):Bool;
    var multiline(default,null):Bool;
    var source(default,null):String;
    var lastIndex:Int;
    function new(pattern:String, ?flags:String);
    function exec(str:String):Null<RegExpMatch>;
    function test(str:String):Bool;
    function toString():String;
}

extern class RegExpMatch extends Array<String> {
    var index:Int;
    var input:String;
}
