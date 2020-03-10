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

package sys.ssl;

extern class Certificate {
	static function loadFile(file:String):Certificate;

	static function loadPath(path:String):Certificate;

	static function fromString(str:String):Certificate;

	static function loadDefaults():Certificate;

	var commonName(get, null):Null<String>;

	var altNames(get, null):Array<String>;

	var notBefore(get, null):Date;

	var notAfter(get, null):Date;

	function subject(field:String):Null<String>;

	function issuer(field:String):Null<String>;

	function next():Null<Certificate>;

	function add(pem:String):Void;

	function addDER(der:haxe.io.Bytes):Void;

	private function get_commonName():Null<String>;

	private function get_altNames():Array<String>;

	private function get_notBefore():Date;

	private function get_notAfter():Date;
}
