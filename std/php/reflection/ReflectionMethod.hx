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
	@:phpClassConst static var IS_STATIC:Int;
	@:phpClassConst static var IS_PUBLIC:Int;
	@:phpClassConst static var IS_PROTECTED:Int;
	@:phpClassConst static var IS_PRIVATE:Int;
	@:phpClassConst static var IS_ABSTRACT:Int;
	@:phpClassConst static var IS_FINAL:Int;

	// public var class : String;
	public static function export(className:String, name:String, ?returnValue:Bool):String;

	public function new(cls:Dynamic, name:String):Void;
	public function getClosure(object:{}):Function;
	public function getDeclaringClass():ReflectionClass;
	public function getModifiers():Int;
	public function getPrototype():ReflectionMethod;
	public function invoke(object:{}, args:Rest<Dynamic>):Dynamic;
	public function invokeArgs(object:{}, args:NativeIndexedArray<Dynamic>):Dynamic;
	public function isAbstract():Bool;
	public function isConstructor():Bool;
	public function isDestructor():Bool;
	public function isFinal():Bool;
	public function isPrivate():Bool;
	public function isProtected():Bool;
	public function isPublic():Bool;
	public function isStatic():Bool;
	public function setAccessible(accessible:Bool):Void;
}
