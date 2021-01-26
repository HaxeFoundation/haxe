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

package python.lib.xml.etree;

import python.Tuple.Tuple2;
import python.NativeIterable;
import python.NativeIterator;
import python.Dict;

extern class XMLParser {}

@:pythonImport("xml.etree.ElementTree", "Element")
extern class Element {
	function getroot():ElementTree;
	var tag:String;
	var attrib:Dict<String, String>;
	var text:Null<String>;

	function get<T>(key:String, def:T = null):T;
	function set(key:String, val:String):Void;

	function copy():Element;

	function keys():Array<String>;
	function items():Array<Tuple2<String, String>>;

	function iter(tag:String):NativeIterable<Element>;
	function iterfind(tag:String, namespaces:Dict<String, String> = null):NativeIterator<Element>;
	function find(match:String, namespaces:Dict<String, String> = null):Null<Element>;
	function findall(match:String, namespaces:Dict<String, String> = null):Array<Element>;
}

@:pythonImport("xml.etree.ElementTree")
extern class ElementTree {
	static function XML(text:String, ?parser:XMLParser):Element;
	static function parse(xml:String):ElementTree;

	function iter(tag:String):NativeIterable<Element>;
	function find(match:String, namespaces:Dict<String, String> = null):Null<Element>;
	function getroot():Element;
}
