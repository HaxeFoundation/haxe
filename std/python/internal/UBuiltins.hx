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
package python.internal;

/**
    This class provides unqualified access to python builtins that are safe to use in haxe/python code.
    Fields listed here must be synchronized with genpy's KeywordHandler.kwds2 list to be properly escaped.
**/
extern class UBuiltins {
    static function len(x:Dynamic):Int;
    static function isinstance(o:Dynamic, c:Dynamic):Bool;
    static function str(o:Dynamic):String;
    static function bool(o:Dynamic):Bool;
    static function float(o:Dynamic):Float;
    static function int(o:Dynamic, ?base:Int):Int;
    static function list<T>(o:Dynamic):Array<T>;
    static function min<T>(a:T, b:T):T;
    static function max<T>(a:T, b:T):T;
    static function hasattr(o:Dynamic, a:String):Bool;
    static function getattr(o:Dynamic, a:String, ?def:Dynamic):Dynamic;
    static function setattr(o:Dynamic, a:String, v:Dynamic):Void;
    static function delattr(o:Dynamic, attr:String):Void;
    static function callable(x:Dynamic):Bool;
    static function type(o:Dynamic):Dynamic;
    static function ord(s:String):Int;
    static function chr(c:Int):String;
    static function map<T,S>(f:T->S, a:Array<T>):Array<S>;
    static function filter<T>(f:T->Bool, a:Array<T>):Array<T>;
    static function iter<T>(o:python.NativeIterable<T>):python.NativeIterator<T>;
}
