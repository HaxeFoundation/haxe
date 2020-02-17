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

import haxe.Constraints.Function;

/**
	A Table object of the given size and element type.

	This is a JavaScript wrapper object — an array-like structure representing a WebAssembly Table,
	which stores function references. A table created by JavaScript or in WebAssembly code will be
	accessible and mutable from both JavaScript and WebAssembly.

	Documentation [Table](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Table) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Table$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("WebAssembly.Table")
extern class Table {
	/**
		Returns the length of the table, i.e. the number of elements.
	**/
	var length:Int;

	@:pure function new(tableDescriptor:TableDescriptor):Void;

	/**
		Accessor function — gets the element stored at a given index.
	**/
	@:pure function get(index:Int):Function;

	/**
		Increases the size of the Table instance by a specified number of elements.
	**/
	function grow(number:Int):Int;

	/**
		Sets an element stored at a given index to a given value.
	**/
	function set(index:Int, value:Function):Void;
}

typedef TableDescriptor = {
	/**
		A string representing the type of value to be stored in the table.
		At the moment this can only have a value of `Anyfunc` (functions).
	**/
	var element:TableKind;

	/**
		The initial number of elements of the WebAssembly Table.
	**/
	var initial:Int;

	/**
		The maximum number of elements the WebAssembly Table is allowed to grow to.
	**/
	var ?maximum:Int;
}

enum abstract TableKind(String) {
	var Anyfunc = "anyfunc";
}
