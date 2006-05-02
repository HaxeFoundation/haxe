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

	static function parse( xmlData : String ) : Xml {
		var x = new NekoXml__();

		var parser = {
			cur : x,
			xml : function(name,att) {
				var x : Dynamic = new NekoXml__();
				untyped x._parentNode = this.cur;
				x.nodeType = Xml.Element;
				x._nodeName = new String(name);
				x._attributes = att;
				untyped {
					var f = __dollar__objfields(att);
					var i = 0;
					var l = __dollar__asize(f);
					while( i < l ) {
						__dollar__objset(att,f[i], new String(__dollar__objget(att,f[i])) );
						i++;
					}
				}
				untyped this.cur.addChild(x);
				untyped this.cur = x;
			},
			cdata : function(text) {
				var x : Dynamic = new NekoXml__();
				untyped x._parentNode = this.cur;
				x.nodeType = Xml.CData;
				x._nodeValue = new String(text);
				untyped this.cur.addChild(x);
			},
			pcdata : function(text) {
				var x : Dynamic = new NekoXml__();
				untyped x._parentNode = this.cur;
				x.nodeType = Xml.PCData;
				x._nodeValue = new String(text);
				untyped this.cur.addChild(x);
			},
			comment : function(text:String) {
				var x : Dynamic = new NekoXml__();
				untyped x._parentNode = this.cur;
				if( text.charCodeAt(1) == 63 ){
					x.nodeType = Xml.Prolog;
				}else{
					x.nodeType = Xml.Comment;
				}
				x._nodeValue = new String(text);
				untyped this.cur.addChild(x);
			},
			doctype : function(text) {
				var x : Dynamic = new NekoXml__();
				untyped x._parentNode = this.cur;
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
		untyped return x;
	}


	static function createElement( name : String ) : Xml {
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.Element;
			r.setNodeName( name );
			return r;
		}
	}
	static function createPCData( data : String ) : Xml {
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.PCData;
			r.setNodeValue( data );
			return r;
		}
	}
	static function createCData( data : String ) : Xml{
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.CData;
			r.setNodeValue( data );
			return r;
		}
	}
	static function createComment( data : String ) : Xml{
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.Comment;
			r.setNodeValue( data );
			return r;
		}
	}
	static function createDocType( data : String ) : Xml{
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.DocType;
			r.setNodeValue( data );
			return r;
		}
	}
	static function createProlog( data : String ) : Xml{
		var r = new NekoXml__();
		untyped {
			r.nodeType = Xml.Prolog;
			r.setNodeValue( data );
			return r;
		}
	}
	static function createDocument() : Xml{
		var r = new NekoXml__();
		r.nodeType = Xml.Document;
		untyped return r;
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
				return this.x[this.cur] != null;
			},
			next : function(){
				return this.x[this.cur++];
			}
		}
	}

	public function elements(){
		var nextElement = untyped function( cur, x ) {
			while( x[cur] != null && x[cur].nodeType != Xml.Element ){
				cur++;
			}
			return cur;
		};

		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function(){
				this.cur = nextElement(this.cur,this.x);
				return this.x[this.cur] != null;
			},
			next : function(){
				var r = nextElement(this.cur,this.x);
				this.cur = nextElement(this.cur+1,this.x);
				return this.x[r];
			}
		}
	}

	public function elementsNamed( name : String ){
		var nextElement = untyped function( cur, x ) {
			var t = x[cur];
			while( t != null && (t.nodeType != Xml.Element || t.nodeName != name) ){
				cur++;
			}
			return cur;
		};

		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function(){
				this.cur = nextElement(this.cur,this.x);
				return this.x[this.cur] != null;
			},
			next : function(){
				var r = nextElement(this.cur,this.x);
				this.cur = nextElement(this.cur+1,this.x);
				return this.x[r];
			}
		}
	}

	public function firstChild() : Xml {
		return _children[0];
	}

	public function firstElement() : Xml {
		var cur = 0;
		while( _children[cur] != null && _children[cur].nodeType != Xml.Element ){
			cur++;
		}

		return _children[cur];
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
		if( nodeType == Xml.PCData || nodeType == Xml.CData || nodeType == Xml.Comment || nodeType == Xml.DocType || nodeType == Xml.Prolog )
			return nodeValue;

		var s = new StringBuf();

		if( nodeType == Xml.Element ) {
			s.add("<");
			s.add(nodeName);
			for( k in Reflect.fields(_attributes) ) {
				s.add(" ");
				s.add(k);
				s.add("=\"");
				s.add(Reflect.field(_attributes,k));
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
			s.add(nodeName);
			s.add(">");
		}
		return s.toString();
	}


}
