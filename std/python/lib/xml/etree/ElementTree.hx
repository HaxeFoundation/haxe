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
package python.lib.xml.etree;

import python.Tuple.Tuple2;

import python.NativeIterable;
import python.NativeIterator;
import python.Dict;

extern class XMLParser {

}

@:pythonImport("xml.etree.ElementTree", "Element")
extern class Element {
	public function getroot ():ElementTree;
	public var tag:String;
	public var attrib : Dict<String, String>;
	public var text:Null<String>;


	public function get <T>(key:String, def:T = null):T;
	public function set (key:String, val:String):Void;

	public function copy ():Element;

	public function keys ():Array<String>;
	public function items ():Array<Tuple2<String, String>>;

	public function iter (tag:String):NativeIterable<Element>;
	public function iterfind (tag:String, namespaces:Dict<String,String> = null):NativeIterator<Element>;
	public function find (match:String, namespaces:Dict<String,String> = null):Null<Element>;
	public function findall (match:String, namespaces:Dict<String,String> = null):Array<Element>;
}

@:pythonImport("xml.etree.ElementTree")
extern class ElementTree {



	public static function XML(text:String, ?parser:XMLParser):Element;
	public static function parse(xml:String):ElementTree;

	public function iter (tag:String):NativeIterable<Element>;
	public function find (match:String, namespaces:Dict<String,String> = null):Null<Element>;
	public function getroot ():Element;
}
