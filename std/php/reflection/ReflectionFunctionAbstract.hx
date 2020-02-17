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

@:native('ReflectionFunctionAbstract')
extern class ReflectionFunctionAbstract implements Reflector {
	var name:String;

	function getClosureScopeClass():ReflectionClass;
	function getClosureThis():Dynamic;
	function getDocComment():String;
	function getEndLine():Int;
	// function getExtension() : ReflectionExtension;
	function getExtensionName():String;
	function getFileName():String;
	function getName():String;
	function getNamespaceName():String;
	function getNumberOfParameters():Int;
	function getNumberOfRequiredParameters():Int;
	// function getParameters() : NativeIndexedArray<ReflectionParameter>;
	// function getReturnType() : ReflectionType;
	function getShortName():String;
	function getStartLine():Int;
	function getStaticVariables():NativeAssocArray<Dynamic>;
	function hasReturnType():Bool;
	function inNamespace():Bool;
	function isClosure():Bool;
	function isDeprecated():Bool;
	function isGenerator():Bool;
	function isInternal():Bool;
	function isUserDefined():Bool;
	function isVariadic():Bool;
	function returnsReference():Bool;
	@:phpMagic function __toString():String;
}
