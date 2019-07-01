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

package js.lib.webassembly;

/**
	A WebAssembly `Global` object represents a global variable instance, accessible from
	both JavaScript and importable/exportable across one or more WebAssembly `Module` instances.
	This allows dynamic linking of multiple modules.

	Documentation [Global](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Global) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("WebAssembly.Global")
extern class Global {
	/**
		The value contained inside the global variable — this can be used to directly set
		and get the global's value.
	**/
	var value:Any;

	@:pure function new(descriptor:GlobalDescriptor, value:Any):Void;

	/**
		Old-style method that returns the value contained inside the global variable.
	 */
	@:pure function valueOf():Any;
}

typedef GlobalDescriptor = {
	var value:ValueType;

	/**
		By default, this is false.
	 */
	var ?mutable:Bool;
}

enum abstract ValueType(String) {
	var I32 = "i32";
	var I64 = "i64";
	var F32 = "f32";
	var F64 = "f64";
}
