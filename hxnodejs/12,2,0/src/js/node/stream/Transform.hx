/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node.stream;

#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	A `Transform` stream is a `Duplex` stream where the output is computed in some way from the input.
	Examples include `zlib` streams or `crypto` streams that compress, encrypt, or decrypt data.

	@see https://nodejs.org/api/stream.html#stream_implementing_a_transform_stream
**/
@:jsRequire("stream", "Transform")
extern class Transform<TSelf:Transform<TSelf>> extends Duplex<TSelf> implements ITransform {
	function new(?options:TransformNewOptions);

	/**
		This function **MUST NOT** be called by application code directly.
		It should be implemented by child classes, and called by the internal `Readable` class methods only.

		@see https://nodejs.org/api/stream.html#stream_transform_flush_callback
	**/
	private function _flush(callback:Null<Error>->Void):Void;

	/**
		This function **MUST NOT** be called by application code directly.
		It should be implemented by child classes, and called by the internal `Readable` class methods only.

		@see https://nodejs.org/api/stream.html#stream_transform_transform_chunk_encoding_callback
	**/
	#if haxe4
	private function _transform(chunk:Dynamic, encoding:String, callback:(error:Null<Error>, data:Dynamic) -> Void):Void;
	#else
	private function _transform(chunk:Dynamic, encoding:String, callback:Null<Error>->Dynamic->Void):Void;
	#end
}

/**
	@see https://nodejs.org/api/stream.html#stream_new_stream_transform_options
**/
typedef TransformNewOptions = {
	> Duplex.DuplexNewOptions,

	/**
		Implementation for the `stream._transform()` method.
	**/
	#if haxe4
	@:optional var transform:(chunk:Dynamic, encoding:String, callback:(error:Null<Error>, data:Dynamic) -> Void) -> Void;
	#else
	@:optional var transform:Dynamic->String->(Null<Error>->Dynamic->Void)->Void;
	#end

	/**
		Implementation for the `stream._flush()` method.
	**/
	@:optional var flush:Null<Error>->Void;
}

@:remove
extern interface ITransform extends Duplex.IDuplex {}
