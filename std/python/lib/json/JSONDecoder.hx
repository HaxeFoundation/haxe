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
package python.lib.json;

import python.Tuple;

typedef JSONDecoderOptions = {
	@:optional var object_hook : Dict<String, Dynamic>->Dynamic;
	@:optional var parse_float : String->Float;
	@:optional var parse_int : String->Int;
	@:optional var parse_constant : String->Dynamic;
	@:optional var strict:Bool;
	@:optional var object_pairs_hook : Array<Tuple2<String,String>>->Dict<String,Dynamic>;
}

@:pythonImport("json", "JSONDecoder")
extern class JSONDecoder {
	public function new (?options:KwArgs<JSONDecoderOptions>):Void;

	public function decode (o:String):Dynamic;
	public function raw_decode (o:String):Tuple2<Dynamic, Int>;
}
