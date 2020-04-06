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

package js.lib;

import js.lib.webassembly.Module;
import js.lib.webassembly.Instance;
import js.html.Response;

/**
	The WebAssembly JavaScript object acts as the namespace for all WebAssembly-related functionality.

	Documentation [WebAssembly](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("WebAssembly")
extern class WebAssembly {
	/**
		The `WebAssembly.instantiate()` function allows you to compile and instantiate WebAssembly code.
		This function has two overloads:

		- The primary overload takes the WebAssembly binary code, in the form of a typed array or ArrayBuffer,
		  and performs both compilation and instantiation in one step. The returned Promise resolves to both
		  a compiled WebAssembly.Module and its first WebAssembly.Instance.

		- The secondary overload takes an already-compiled WebAssembly.Module and returns a Promise that resolves
		  to an Instance of that Module. This overload is useful if the Module has already been compiled.
	**/
	@:overload(function(module:Module, importObject:{}):Promise<Instance> {})
	@:pure static function instantiate(bufferSource:BufferSource, importObject:{}):Promise<WebAssemblyInstantiatedSource>;

	/**
		The `WebAssembly.instantiateStreaming()` function compiles and instantiates a WebAssembly module
		directly from a streamed underlying source. This is the most efficient, optimized way to load wasm code.
	**/
	@:pure static function instantiateStreaming(source:Response, importObject:{}):Promise<WebAssemblyInstantiatedSource>;

	/**
		The `WebAssembly.compile()` function compiles a WebAssembly `Module` from WebAssembly binary code.
		This function is useful if it is necessary to a compile a module before it can be instantiated
		(otherwise, the `WebAssembly.instantiate()` function should be used).
	**/
	@:pure static function compile(bufferSource:BufferSource):Promise<Module>;

	/**
		The `WebAssembly.compileStreaming()` function compiles a WebAssembly `Module` directly from a streamed
		underlying source. This function is useful if it is necessary to a compile a module before it can
		be instantiated (otherwise, the `WebAssembly.instantiateStreaming()` function should be used).
	**/
	@:pure static function compileStreaming(source:Response):Promise<Module>;

	/**
		The `WebAssembly.validate()` function validates a given typed array of WebAssembly binary code,
		returning whether the bytes form a valid wasm module (`true`) or not (`false`).
	**/
	@:pure static function validate(bufferSource:BufferSource):Bool;
}

typedef WebAssemblyInstantiatedSource = {
	final module:Module;
	final instance:Instance;
}
