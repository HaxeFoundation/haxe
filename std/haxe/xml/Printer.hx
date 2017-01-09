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
	This class provides utility methods to convert Xml instances to 
	String representation.
**/
class Printer {
	/**
		Convert `Xml` to string representation.
		
		Set `pretty` to `true` to prettify the result.
	**/
	static public function print(xml:Xml, ?pretty = false) {
		var printer = new Printer(pretty);
		printer.writeNode(xml, "");
		return printer.output.toString();
	}

	var output:StringBuf;
	var pretty:Bool;

	function new(pretty) {
		output = new StringBuf();
		this.pretty = pretty;
	}

	function writeNode(value:Xml, tabs:String) {
		switch (value.nodeType) {
			case CData:
				write(tabs + "<![CDATA[");
				write(StringTools.trim(value.nodeValue));
				write("]]>");
				newline();
			case Comment:
				var commentContent:String = value.nodeValue;
				commentContent = ~/[\n\r\t]+/g.replace(commentContent, "");
				commentContent = "<!--" + commentContent + "-->";
				write(tabs);
				write(StringTools.trim(commentContent));
				newline();
			case Document:
				for (child in value) {
					writeNode(child, tabs);
				}
			case Element:
				write(tabs + "<");
				write(value.nodeName);
				for (attribute in value.attributes()) {
					write(" " + attribute + "=\"");
					write(StringTools.htmlEscape(value.get(attribute), true));
					write("\"");
				}
				if (hasChildren(value)) {
					write(">");
					newline();
					for (child in value) {
						writeNode(child, pretty ? tabs + "\t" : tabs);
					}
					write(tabs + "</");
					write(value.nodeName);
					write(">");
					newline();
				} else {
					write("/>");
					newline();
				}
			case PCData:
				var nodeValue:String = value.nodeValue;
				if (nodeValue.length != 0) {
					write(tabs + StringTools.htmlEscape(nodeValue));
					newline();
				}
			case ProcessingInstruction:
				write("<?" + value.nodeValue + "?>");
				newline();
			case DocType:
				write("<!DOCTYPE " + value.nodeValue + ">");
				newline();
		}
	}

	inline function write(input:String) {
		output.add(input);
	}

	inline function newline() {
		if (pretty) {
			output.add("\n");
		}
	}

	function hasChildren(value:Xml):Bool {
		for (child in value) {
			switch (child.nodeType) {
				case Element, PCData:
					return true;
				case CData, Comment:
					if (StringTools.ltrim(child.nodeValue).length != 0) {
						return true;
					}
				case _:
			}
		}
		return false;
	}
}
