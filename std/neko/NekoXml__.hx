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
package neko;
import Xml;

class NekoXml__ {

	private var _children : Array<Xml>;
	public property nodeType(default,null) : XmlType;

	private var _nodeName : String;
	private var _nodeValue : String;
	private var _attributes : Dynamic<String>;

	private function new() {
		_attributes = untyped Reflect.empty();
		_children = new Array();
	}

	private var _parentNode : Xml;

	private static var _parse = neko.Lib.load("std","parse_xml",2);

	private static function unescape( s : String ) : String {
		return s.split("&lt;").join("<").split("&gt;").join(">").split("&quot;").join("\"").split("&amp;").join("&");
	}

	private static function escape( s : String ) : String {
		return s.split("&").join("&amp;").split("\"").join("&quot;").split("<").join("&lt;").split(">").join("&gt;");
	}

	static function parse( xmlData : String ) : Xml {
		var x = new NekoXml__();

		var parser = {
			cur : x,
			xml : function(name,att) {
				var x : Dynamic = new NekoXml__();
				x._parentNode = untyped this.cur;
				x.nodeType = Xml.Element;
				x._nodeName = new String(name);
				x._attributes = att;
				untyped {
					var f = __dollar__objfields(att);
					var i = 0;
					var l = __dollar__asize(f);
					while( i < l ) {
						__dollar__objset(att,f[i], unescape(new String(__dollar__objget(att,f[i]))) );
						i++;
					}
					this.cur.addChild(x);
					this.cur = x;
				}
			},
			cdata : function(text) {
				var x : Dynamic = new NekoXml__();
				x._parentNode = untyped this.cur;
				x.nodeType = Xml.CData;
				x._nodeValue = new String(text);
				untyped this.cur.addChild(x);
			},
			pcdata : function(text) {
				var x : Dynamic = new NekoXml__();
				x._parentNode = untyped this.cur;
				x.nodeType = Xml.PCData;
				x._nodeValue = unescape(new String(text));
				untyped this.cur.addChild(x);
			},
			comment : function(text) {
				var x : Dynamic = new NekoXml__();
				x._parentNode = untyped this.cur;
				if( untyped __dollar__sget(text,1) == 63 )
					x.nodeType = Xml.Prolog;
				else
					x.nodeType = Xml.Comment;
				x._nodeValue = new String(text);
				untyped this.cur.addChild(x);
			},
			doctype : function(text) {
				var x : Dynamic = new NekoXml__();
				x._parentNode = untyped this.cur;
				x.nodeType = Xml.DocType;
				x._nodeValue = new String(text);
				untyped this.cur.addChild(x);
			},
			done : function() {
				untyped this.cur = this.cur._parentNode;
			}
		};
		untyped _parse(xmlData.__s,parser);
		x.nodeType = Xml.Document;
		return untyped x;
	}


	static function createElement( name : String ) : Xml {
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.Element;
			r._nodeName = name;
			return r;
		}
	}
	static function createPCData( data : String ) : Xml {
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.PCData;
			r._nodeValue = data;
			return r;
		}
	}
	static function createCData( data : String ) : Xml{
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.CData;
			r._nodeValue = data;
			return r;
		}
	}
	static function createComment( data : String ) : Xml{
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.Comment;
			r._nodeValue = data;
			return r;
		}
	}
	static function createDocType( data : String ) : Xml{
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.DocType;
			r._nodeValue = data;
			return r;
		}
	}
	static function createProlog( data : String ) : Xml{
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.Prolog;
			r._nodeValue = data;
			return r;
		}
	}
	static function createDocument() : Xml{
		var r = new NekoXml__();
		r.nodeType = Xml.Document;
		return untyped r;
	}


	public property nodeName(getNodeName,setNodeName) : String;
	private function getNodeName() : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _nodeName;
	}
	private function setNodeName( n : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _nodeName = n;
	}

	public property nodeValue(getNodeValue,setNodeValue) : String;
	private function getNodeValue() : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return _nodeValue;
	}
	private function setNodeValue( v : String ) : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return _nodeValue = v;
	}

	public function get( att : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.field( _attributes, att );
	}

	public function set( att : String, value : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		Reflect.setField (_attributes, att, value );
	}

	public function remove( att : String ) : Void{
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		Reflect.deleteField( _attributes, att );
	}

	public function exists( att : String ) : Bool {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.hasField( _attributes, att );
	}

	public function attributes() : Iterator<String> {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.fields( _attributes ).iterator();
	}


	public function iterator(){
		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function(){
				return this.cur < this.x.length;
			},
			next : function(){
				return this.x[this.cur++];
			}
		}
	}


	public function elements(){
		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function() {
				var k = this.cur;
				var l = this.x.length;
				while( k < l ) {
					if( this.x[k].nodeType == Xml.Element )
						break;
					k += 1;
				}
				this.cur = k;
				return k < l;
			},
			next : function() {
				var k = this.cur;
				var l = this.x.length;
				while( k < l ) {
					var n = this.x[k];
					k += 1;
					if( n.nodeType == Xml.Element ) {
						this.cur = k;
						return n;
					}
				}
				return null;
			}
		}
	}

	public function elementsNamed( name : String ) {
		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function() {
				var k = this.cur;
				var l = this.x.length;
				while( k < l ) {
					var n = this.x[k];
					if( n.nodeType == Xml.Element && n._nodeName == name )
						break;
					k++;
				}
				this.cur = k;
				return k < l;
			},
			next : function() {
				var k = this.cur;
				var l = this.x.length;
				while( k < l ) {
					var n = this.x[k];
					k++;
					if( n.nodeType == Xml.Element && n._nodeName == name ) {
						this.cur = k;
						return n;
					}
				}
				return null;
			}
		}
	}

	public function firstChild() : Xml {
		return _children[0];
	}

	public function firstElement() : Xml {
		for( cur in 0..._children.length ) {
			var n = _children[cur];
			if( n.nodeType == Xml.Element )
				return n;
		}
		return null;
	}

	public function addChild( x : Xml ) : Void {
		_children.push( x );
	}

	public function removeChild( x : Xml ) : Bool {
		return _children.remove( x );
	}

	public function insertChild( x : Xml, pos : Int ) : Void {
		_children.insert( pos, x );
	}

	public function toString() {
		if( nodeType == Xml.PCData )
			return escape(_nodeValue);
		if( nodeType == Xml.CData )
			return "<![CDATA["+_nodeValue+"]]>";
		if( nodeType == Xml.Comment || nodeType == Xml.DocType || nodeType == Xml.Prolog )
			return _nodeValue;

		var s = new StringBuf();

		if( nodeType == Xml.Element ) {
			s.add("<");
			s.add(_nodeName);
			for( k in Reflect.fields(_attributes) ) {
				s.add(" ");
				s.add(k);
				s.add("=\"");
				s.add(escape(Reflect.field(_attributes,k)));
				s.add("\"");
			}
			if( _children.length == 0 ) {
				s.add("/>");
				return s.toString();
			}
			s.add(">");
		}

		for( x in iterator() )
			s.add(x);

		if( nodeType == Xml.Element ) {
			s.add("</");
			s.add(_nodeName);
			s.add(">");
		}
		return s.toString();
	}


}
