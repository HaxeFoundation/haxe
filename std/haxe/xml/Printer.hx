/*
 * Copyright (c)2005-2013, Haxe Foundation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * Neither the name of the Haxe Foundation nor the names of its contributors
 * may be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package haxe.xml;

class Printer {

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
			case DocType:
				write("<!DOCTYPE " + value.nodeValue + ">");
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
				case Xml.Element, Xml.PCData:
					return true;
				case Xml.CData, Xml.Comment:
					if (StringTools.ltrim(child.nodeValue).length != 0) {
						return true;
					}
				case _:
			}
		}
		return false;
	}
}
