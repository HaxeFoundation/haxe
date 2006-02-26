/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

class XmlParser {

	public static function parse( xmlData : String ) : Node {
		#if flash
		untyped {
			var x = __new__(_global["XML"]);
			x.parseXML(xmlData);
			if( x.status != 0 )
				throw ("Xml parse error #"+x.status);
			return x;
		}
		#else neko
			var x = new Node();
			x.childNodes = new Array<Node>();
			var hierarchy = function(x : Node) {
				x.firstChild = x.childNodes[0];
				var i = 0;
				var prev = null;
				var l = x.childNodes.length;
				while( i < l ) {
					var c = x.childNodes[i++];
					c.previousSibling = prev;
					if( prev != null ) prev.nextSibling = c;
					prev = c;
				}
				x.lastChild = prev;
			}
			var parser = {
				cur : x,
				xml : function(name,att) {
					var x = new Node();
					x.nodeType = Node.element_node;
					x.nodeName = new String(name);
					x.childNodes = new Array<Node>();
					x.parentNode = untyped this.cur;
					x.attributes = att;
					untyped {
						var f = __dollar__objfields(att);
						var i = 0;
						var l = __dollar__asize(f);
						while( i < l ) {
							__dollar__objset(att,f[i], new String(__dollar__objget(att,f[i])) );
							i++;
						}
					}
					untyped this.cur.childNodes.push(x);
					untyped this.cur = x;
				},
				cdata : function(text) {
					var x = new Node();
					x.nodeType = Node.text_node;
					x.nodeValue = new String(text);
					x.parentNode = untyped this.cur;
					untyped this.cur.childNodes.push(x);
				},
				pcdata : function(text) {
					var x = new Node();
					x.nodeType = Node.text_node;
					x.nodeValue = new String(text);
					x.parentNode = untyped this.cur;
					untyped this.cur.childNodes.push(x);
				},
				comment : function(text) {
				},
				doctype : function(text) {
				},
				done : function() {
					var x = untyped this.cur;
					hierarchy(x);
					x = x.parentNode;
					untyped this.cur = x;
				}
			};
			untyped _parse(xmlData.__s,parser);
			hierarchy(x);
			return x;
		#else js
		if( js.Lib.isIE ) untyped {
			var x = __new__("ActiveXObject","Microsoft.XMLDOM");
			x.async="false";
			x.loadXML(xmlData);
			return x;
		} else
			return untyped __new__("DOMParser").parseFromString(xmlData, "text/xml");
		#else error
		#end
	}

	#if neko
	private static var _parse = neko.Lib.load("std","parse_xml",2);
	#end

}