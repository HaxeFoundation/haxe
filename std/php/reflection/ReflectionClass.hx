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

import haxe.extern.Rest;

@:native('ReflectionClass')
extern class ReflectionClass implements Reflector {
	@:phpClassConst static final IS_IMPLICIT_ABSTRACT:Int;
	@:phpClassConst static final IS_EXPLICIT_ABSTRACT:Int;
	@:phpClassConst static final IS_FINAL:Int;

	static function export(argument:Dynamic, returnValue:Bool = false):String;

	var name:String;

	function new(argument:Dynamic):Void;
	function getConstant(name:String):Dynamic;
	function getConstants():NativeAssocArray<Dynamic>;
	function getConstructor():ReflectionMethod;
	function getDefaultProperties():NativeAssocArray<Dynamic>;
	function getDocComment():String;
	function getEndLine():Int;
	// function getExtension() : ReflectionExtension;
	function getExtensionName():String;
	function getFileName():String;
	function getInterfaceNames():NativeIndexedArray<String>;
	function getInterfaces():NativeIndexedArray<ReflectionClass>;
	function getMethod(name:String):ReflectionMethod;
	function getMethods(?filter:Int):NativeIndexedArray<ReflectionMethod>;
	function getModifiers():Int;
	function getName():String;
	function getNamespaceName():String;
	function getParentClass():Null<ReflectionClass>;
	function getProperties(?filter:Int):NativeIndexedArray<ReflectionProperty>;
	function getProperty(name:String):ReflectionProperty;
	function getShortName():String;
	function getStartLine():Int;
	function getStaticProperties():NativeAssocArray<Dynamic>;
	function getStaticPropertyValue(name:String, ?def_value:Ref<Dynamic>):Dynamic;
	function getTraitAliases():NativeAssocArray<String>;
	function getTraitNames():NativeIndexedArray<String>;
	function getTraits():NativeIndexedArray<ReflectionClass>;
	function hasConstant(name:String):Bool;
	function hasMethod(name:String):Bool;
	function hasProperty(name:String):Bool;
	function implementsInterface(interfaceName:String):Bool;
	function inNamespace():Bool;
	function isAbstract():Bool;
	function isAnonymous():Bool;
	function isCloneable():Bool;
	function isFinal():Bool;
	function isInstance(object:{}):Bool;
	function isInstantiable():Bool;
	function isInterface():Bool;
	function isInternal():Bool;
	function isIterateable():Bool;
	function isSubclassOf(className:String):Bool;
	function isTrait():Bool;
	function isUserDefined():Bool;
	function newInstance(args:Rest<Dynamic>):Dynamic;
	function newInstanceArgs(?args:NativeIndexedArray<Dynamic>):Dynamic;
	function newInstanceWithoutConstructor():Dynamic;
	function setStaticPropertyValue(name:String, value:String):Void;
	@:phpMagic function __toString():String;
}
