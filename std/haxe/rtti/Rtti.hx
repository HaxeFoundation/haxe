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

package haxe.rtti;
import haxe.rtti.CType;

/**
	Rtti is a helper class which supplements the `@:rtti` metadata.
	
	@see <https://haxe.org/manual/cr-rtti.html>
**/
class Rtti {

	/**
		Returns the `haxe.rtti.CType.Classdef` corresponding to class `c`.

		If `c` has no runtime type information, e.g. because no `@:rtti@` was
		added, `null` is returned.

		If `c` is null, the result is unspecified.
	**/
	static public function getRtti<T>(c:Class<T>):Classdef {
		var rtti = Reflect.field(c, "__rtti");
		if (rtti == null) {
			throw 'Class ${Type.getClassName(c)} has no RTTI information, consider adding @:rtti';
		}
		var x = Xml.parse(rtti).firstElement();
		var infos = new haxe.rtti.XmlParser().processElement(x);
		switch (infos) {
			case TClassdecl(c): return c;
			case t: throw 'Enum mismatch: expected TClassDecl but found $t';
		}
	}

	/**
		Tells if `c` has runtime type information.

		If `c` is null, the result is unspecified.
	**/
	static public function hasRtti<T>(c:Class<T>):Bool {
		return Lambda.has(Type.getClassFields(c), "__rtti");
	}
}