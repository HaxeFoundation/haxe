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

/**
	`ArrayBufferView` is a helper type representing any of the following JavaScript `TypedArray` types:

	Documentation [ArrayBufferView](https://developer.mozilla.org/en-US/docs/Web/API/ArrayBufferView) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ArrayBufferView$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ArrayBufferView>
**/
@:forward
abstract ArrayBufferView(_ArrayBufferView)
	from Int8Array
	from Uint8Array
	from Uint8ClampedArray
	from Int16Array
	from Uint16Array
	from Int32Array
	from Uint32Array
	from Float32Array
	from Float64Array
	from DataView
{}

private typedef _ArrayBufferView = {
	final buffer : ArrayBuffer;
	final byteOffset : Int;
	final byteLength : Int;
}