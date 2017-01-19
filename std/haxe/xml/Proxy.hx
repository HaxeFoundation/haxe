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
package haxe.xml;

/**
	This proxy can be inherited with an XML file name parameter.
	It will	only allow access to fields which corresponds to an "id" attribute
	value in the XML file :

	```haxe
	class MyXml extends haxe.xml.Proxy<"my.xml", MyStructure> {
	}
	
	var h = new haxe.ds.StringMap<MyStructure>();
	// ... fill h with "my.xml" content
	var m = new MyXml(h.get);
	trace(m.myNode.structField);
	// Access to "myNode" is only possible if you have an id="myNode" attribute
	// in your XML, and completion works as well.
	```
**/
class Proxy<Const,T> {

	var __f : String -> T;

	public function new(f) {
		this.__f = f;
	}

	public function resolve(k) {
		return __f(k);
	}

}
