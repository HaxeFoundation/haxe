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

 /*
  * Adapted from mlc.haxe.PrettyPrinter, from here:
  * https://code.google.com/p/hxmlc/source/browse/trunk/source/mlc/haxe/Console.hx
  * 
  * The original code was licensed under The BSD-3 Clause:
  * http://opensource.org/licenses/BSD-3-Clause
  * so that's what I used above. I hope this is okay?
  * 
  * -Lars Doucet
  */
 
package haxe.xml;

class Printer {
	
	public function new() {
		
	}
	
	public function print(xml:Xml):String {
		_output = "";			//start with fresh string buffer
		writeNode(xml, 0);		//recursively pretty-print the xml
		return _output;			//return the string buffer
	}
	
	/****PRIVATE****/
	
	private var _output:String;	//output buffer we write to
	
	private function writeNode(value:Xml, depth:Int):Void {
		switch (value.nodeType) {
			case Xml.CData:
				write(createTabs(depth) + "<", false);
				write("![CDATA[", false);
				write(StringTools.trim(value.nodeValue), false);
				write("]]", false);
				write(">", true);
			case Xml.Comment:
				var commentContent:String = value.nodeValue;
				commentContent = ~/[\n\r\t]+/g.replace(commentContent, "");
				write(createTabs(depth), false);
				write(StringTools.trim(commentContent), false);
				write("", true);
			case Xml.Document:
				for (child in value) {
					writeNode(child, depth);
				}
			case Xml.Element:
				write(createTabs(depth) + "<", false);
				write(value.nodeName, false);
				for (attribute in value.attributes()) {
					write(" " + attribute + "=\"", false);
					write(value.get(attribute), false);
					write("\"", false);
				}
				if (determineHasChildren(value)) {
					write(">", true);
				for (child in value) {
					writeNode(child, depth + 1);
				}
				write(createTabs(depth) + "<", false);
				write("/", false);
				write(value.nodeName, false);
				write(">", true);
				} else {
					write(" /", false);
					write(">", true);
				}
		case Xml.PCData:
			var nodeValue:String = StringTools.trim(value.nodeValue);
				if (nodeValue.length != 0) {
					write(createTabs(depth) + nodeValue, true);
				}
			}
	}
	
	private inline function write(input:String, breakline:Bool):Void{
		_output += input;
		if (breakline) {
			_output += "\n";
		}
	}
	
	private inline function createTabs(numberOfTabs:Int):String {
		return
			switch (numberOfTabs) {
				case 0:
					"";
				case 1:
					"\t";
				case 2:
					"\t\t";
				case 3:	
					"\t\t\t";
				default:
					var result:String = "";
					for (i in 0...numberOfTabs) {
						result += "\t";
					}
					result;
			}
	}
	
	private function determineHasChildren(value:Xml):Bool {
		for (child in value) {
			switch (child.nodeType) {
				case Xml.Element:
					return true;
				case Xml.CData, Xml.Comment, Xml.PCData:
					if (StringTools.ltrim(child.nodeValue).length != 0) {
						return true;
					}
			}
		}
		return false;
	}
}