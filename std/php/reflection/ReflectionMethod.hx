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

package php.reflection;

import haxe.Constraints;
import haxe.extern.Rest;

@:native('ReflectionMethod')
extern class ReflectionMethod extends ReflectionFunctionAbstract {
	@:phpClassConst static final IS_STATIC:Int;
	@:phpClassConst static final IS_PUBLIC:Int;
	@:phpClassConst static final IS_PROTECTED:Int;
	@:phpClassConst static final IS_PRIVATE:Int;
	@:phpClassConst static final IS_ABSTRACT:Int;
	@:phpClassConst static final IS_FINAL:Int;

	// var class : String;
	static function export(className:String, name:String, ?returnValue:Bool):String;

	function new(cls:Dynamic, name:String):Void;
	function getClosure(object:{}):Function;
	function getDeclaringClass():ReflectionClass;
	function getModifiers():Int;
	function getPrototype():ReflectionMethod;
	function invoke(object:{}, args:Rest<Dynamic>):Dynamic;
	function invokeArgs(object:{}, args:NativeIndexedArray<Dynamic>):Dynamic;
	function isAbstract():Bool;
	function isConstructor():Bool;
	function isDestructor():Bool;
	function isFinal():Bool;
	function isPrivate():Bool;
	function isProtected():Bool;
	function isPublic():Bool;
	function isStatic():Bool;
	function setAccessible(accessible:Bool):Void;
}
