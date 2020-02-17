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
	A new WebAssembly `Memory` object which is a resizable ArrayBuffer that holds the raw bytes of memory
	accessed by a WebAssembly WebAssembly `Instance`.

	A memory created by JavaScript or in WebAssembly code will be accessible and mutable from
	both JavaScript and WebAssembly.

	Documentation [Memory](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Memory) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Memory$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("WebAssembly.Memory")
extern class Memory {
	/**
		An accessor property that returns the buffer contained in the memory.
	**/
	final buffer:js.lib.ArrayBuffer;

	@:pure function new(memoryDescriptor:MemoryDescriptor):Void;

	/**
		Increases the size of the memory instance by a specified number of WebAssembly pages
		(each one is 64KB in size).
	**/
	function grow(number:Int):Int;
}

typedef MemoryDescriptor = {
	/**
		The initial size of the WebAssembly Memory, in units of WebAssembly pages.
	**/
	var initial:Int;

	/**
		The maximum size the WebAssembly Memory is allowed to grow to, in units of WebAssembly pages.
		When present, the `maximum` parameter acts as a hint to the engine to reserve memory up front.
		However, the engine may ignore or clamp this reservation request.
		In general, most WebAssembly modules shouldn't need to set a `maximum`.
	 */
	var ?maximum:Int;
}
