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

	static var __name__ = ["Xml"];

	public var nodeName(getNodeName,setNodeName) : String;
	public var nodeValue(getNodeValue,setNodeValue) : String;
	public var parent(getParent,null) : NekoXml__;
	public var nodeType(default,null) : XmlType;

	private var _nodeName : String;
	private var _nodeValue : String;
	private var _attributes : Dynamic<String>;
	private var _children : Array<NekoXml__>;
	private var _parent : NekoXml__;

	private function new() {
	}

	private static var _parse = neko.Lib.load("std","parse_xml",2);

	static function parse( xmlData : String ) : NekoXml__ {
		var x = new NekoXml__();
		x._children = new Array();
		var parser = {
			cur : x,
			xml : function(name,att) {
				var x : Dynamic = new NekoXml__();
				x._parent = untyped this.cur;
				x.nodeType = Xml.Element;
				x._nodeName = new String(name);
				x._attributes = att;
				x._children = new Array();
				untyped {
					var f = __dollar__objfields(att);
					var i = 0;
					var l = __dollar__asize(f);
					while( i < l ) {
						__dollar__objset(att,f[i], new String(__dollar__objget(att,f[i]))) ;
						i++;
					}
					this.cur.addChild(x);
					this.cur = x;
				}
			},
			cdata : function(text) {
				var x : Dynamic = new NekoXml__();
				x._parent = untyped this.cur;
				x.nodeType = Xml.CData;
				x._nodeValue = new String(text);
				untyped this.cur.addChild(x);
			},
			pcdata : function(text) {
				var x : Dynamic = new NekoXml__();
				x._parent = untyped this.cur;
				x.nodeType = Xml.PCData;
				x._nodeValue = new String(text);
				untyped this.cur.addChild(x);
			},
			comment : function(text) {
				var x : Dynamic = new NekoXml__();
				x._parent = untyped this.cur;
				if( untyped __dollar__sget(text,0) == 63 ) {
					x.nodeType = Xml.Prolog;
					text = new String(text);
					text = text.substr(1,text.length - 2);
				} else {
					x.nodeType = Xml.Comment;
					text = new String(text);
				}
				x._nodeValue = text;
				untyped this.cur.addChild(x);
			},
			doctype : function(text) {
				var x : Dynamic = new NekoXml__();
				x._parent = untyped this.cur;
				x.nodeType = Xml.DocType;
				x._nodeValue = new String(text).substr(1);
				untyped this.cur.addChild(x);
			},
			done : function() {
				untyped this.cur = this.cur._parent;
			}
		};
		untyped _parse(xmlData.__s,parser);
		x.nodeType = Xml.Document;
		return x;
	}


	static function createElement( name : String ) : NekoXml__ {
		var r = new NekoXml__();
		r.nodeType = Xml.Element;
		r._nodeName = name;
		r._attributes = untyped __dollar__new(null);
		r._children = new Array();
		return r;
	}

	static function createPCData( data : String ) : NekoXml__ {
		var r = new NekoXml__();
		r.nodeType = Xml.PCData;
		r._nodeValue = data;
		return r;
	}

	static function createCData( data : String ) : NekoXml__ {
		var r = new NekoXml__();
		r.nodeType = Xml.CData;
		r._nodeValue = data;
		return r;
	}

	static function createComment( data : String ) : NekoXml__ {
		var r = new NekoXml__();
		r.nodeType = Xml.Comment;
		r._nodeValue = data;
		return r;
	}

	static function createDocType( data : String ) : NekoXml__ {
		var r = new NekoXml__();
		r.nodeType = Xml.DocType;
		r._nodeValue = data;
		return r;
	}

	static function createProlog( data : String ) : NekoXml__ {
		var r = new NekoXml__();
		r.nodeType = Xml.Prolog;
		r._nodeValue = data;
		return r;
	}

	static function createDocument() : NekoXml__ {
		var r = new NekoXml__();
		r.nodeType = Xml.Document;
		r._children = new Array();
		return r;
	}

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

	private function getParent() : NekoXml__ {
		return _parent;
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

	public function iterator() : Iterator<NekoXml__> {
		if( _children == null )
			throw "bad nodetype";
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


	public function elements() {
		if( _children == null )
			throw "bad nodetype";
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
		if( _children == null )
			throw "bad nodetype";
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

	public function firstChild() : NekoXml__ {
		if( _children == null )
			throw "bad nodetype";
		return _children[0];
	}

	public function firstElement() : NekoXml__ {
		if( _children == null )
			throw "bad nodetype";
		for( cur in 0..._children.length ) {
			var n = _children[cur];
			if( n.nodeType == Xml.Element )
				return n;
		}
		return null;
	}

	public function addChild( x : NekoXml__ ) : Void {
		if( _children == null )
			throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.push( x );
	}

	public function removeChild( x : NekoXml__ ) : Bool {
		if( _children == null )
			throw "bad nodetype";
		var b = _children.remove( x );
		if( b ) x._parent = null;
		return b;
	}

	public function insertChild( x : NekoXml__, pos : Int ) : Void {
		if( _children == null )
			throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.insert( pos, x );
	}

	public function toString() {
		var s = new StringBuf();
		toStringRec(s);
		return s.toString();
	}

	public function toStringRec(s: StringBuf) {
		switch( nodeType ) {
		case Xml.Document:
			for( x in _children )
				x.toStringRec(s);
		case Xml.Element:
			s.addChar("<".code);
			s.add(_nodeName);
			for( k in Reflect.fields(_attributes) ) {
				s.addChar(" ".code);
				s.add(k);
				s.addChar("=".code);
				s.addChar("\"".code);
				s.add(Reflect.field(_attributes,k));
				s.addChar("\"".code);
			}
			if( _children.length == 0 ) {
				s.addChar("/".code);
				s.addChar(">".code);
				return;
			}
			s.addChar(">".code);
			for( x in _children )
				x.toStringRec(s);
			s.addChar("<".code);
			s.addChar("/".code);
			s.add(_nodeName);
			s.addChar(">".code);
		case Xml.PCData:
			s.add(_nodeValue);
		case Xml.CData:
			s.add("<![CDATA[");
			s.add(_nodeValue);
			s.add("]]>");
		case Xml.Comment:
			s.add("<!--");
			s.add(_nodeValue);
			s.add("-->");
		case Xml.DocType:
			s.add("<!DOCTYPE ");
			s.add(_nodeValue);
			s.add(">");
		case Xml.Prolog:
			s.add("<?");
			s.add(_nodeValue);
			s.add("?>");
		}
	}


}
