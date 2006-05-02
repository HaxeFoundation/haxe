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
package js;
import Xml;

class JsXml__ {

	static var enode = ~/^<([a-zA-Z0-9:-]+)/;
	static var ecdata = ~/^<!\[CDATA\[/i;
	static var edoctype = ~/^<!DOCTYPE/i;
	static var eend = ~/^<\/([a-zA-Z0-9:-]+)>/;
	static var epcdata = ~/^[^<]+/;
	static var ecomment = ~/^<!--/;
	static var eprolog = ~/^<\?[^\?]+\?>/;

	static var eattribute = ~/^[ \r\n\t]*([a-zA-Z0-9:-]+)[ \r\n\t]*=[ \r\n\t]*"([^"\n\r]+)"/;
	static var eclose = ~/^[ \r\n\t]*(>|(\/>))/;
	static var ecdata_end = ~/\]\]>/;
	static var edoctype_elt = ~/[\[|\]>]/;
	static var ecomment_end = ~/-->/;

	public property nodeType(default,null) : XmlType;
	public var _nodeName : String;
	public var _nodeValue : String;
	public var _attributes : Hash<String>;
	public var _children : Array<Xml>;

	public static function parse( str : String ) : Xml {
		var rules = [enode,epcdata,ecdata,edoctype,eend,ecomment,eprolog];
		var nrules = rules.length;
		var current = Xml.createDocument();

		var stack = new List();
		while( str.length > 0 ) {
			var i = 0;
			while( i < nrules ) {
				var r = rules[i];
				if( r.match(str) ) {
					switch( i ) {
					case 0: // Node
						var x = Xml.createElement(r.matched(1));
						current.addChild(x);
						str = r.matchedRight();
						while( eattribute.match(str) ) {
							x.set(eattribute.matched(1),eattribute.matched(2));
							str = eattribute.matchedRight();
						}
						if( !eclose.match(str) ) {
							i = nrules;
							break;
						}
						if( eclose.matched(1) == ">" ) {
							stack.push(current);
							current = x;
						}
						str = eclose.matchedRight();
					case 1: // PCData
						var x = Xml.createPCData(r.matched(0));
						current.addChild(x);
						str = r.matchedRight();
					case 2: // CData
						str = r.matchedRight();
						if( !ecdata_end.match(str) )
							throw "End of CDATA section not found";
						var x = Xml.createCData(ecdata_end.matchedLeft());
						current.addChild(x);
						str = ecdata_end.matchedRight();
					case 3: // DocType
						var pos = 0;
						var count = 0;
						var old = str;
						while( true ) {
							if( !edoctype_elt.match(str) )
								throw "End of DOCTYPE section not found";
							var p = edoctype_elt.matchedPos();
							pos += p.pos + p.len;
							str = edoctype_elt.matchedRight();
							switch( edoctype_elt.matched(0) ) {
							case "[": count++;
							case "]": count--; if( count < 0 ) throw "Invalid ] found in DOCTYPE declaration";
							default:
								if( count == 0 )
									break;
							}
						}
						var x = Xml.createDocType(old.substr(0,pos));
						current.addChild(x);
					case 4: // End Node
						untyped if( current._children != null && current._children.length == 0 ) {
							var e = Xml.createPCData("");
							current.addChild(e);
						}
						untyped if( r.matched(1) != current._nodeName || stack.isEmpty() ) {
							i = nrules;
							break;
						}
						current = stack.pop();
						str = r.matchedRight();
					case 5: // Comment
						if( !ecomment_end.match(str) )
							throw "Unclosed Comment";
						var p = ecomment_end.matchedPos();
						var x = Xml.createComment(str.substr(0,p.pos+p.len));
						current.addChild(x);
						str = ecomment_end.matchedRight();
					case 6: // Prolog
						var x = Xml.createProlog(r.matched(0));
						current.addChild(x);
						str = r.matchedRight();
					}
					break;
				}
				i += 1;
			}
			if( i == nrules ) {
				if( str.length > 10 )
					throw ("Xml parse error : Unexpected "+str.substr(0,10)+"...");
				else
					throw ("Xml parse error : Unexpected "+str);
			}
		}
		untyped return current;
	}

	private function new(){
	}

	static function createElement( name : String ) : Xml {
		var r = new JsXml__();
		untyped {
			r.nodeType = Xml.Element;
			r._children = new Array();
			r._attributes = new Hash();
			r.setNodeName( name );
			return r;
		}
	}
	static function createPCData( data : String ) : Xml {
		var r = new JsXml__();
		untyped {
			r.nodeType = Xml.PCData;
			r.setNodeValue( data );
			return r;
		}
	}
	static function createCData( data : String ) : Xml{
		var r = new JsXml__();
		untyped {
			r.nodeType = Xml.CData;
			r.setNodeValue( data );
			return r;
		}
	}
	static function createComment( data : String ) : Xml{
		var r = new JsXml__();
		untyped {
			r.nodeType = Xml.Comment;
			r.setNodeValue( data );
			return r;
		}
	}
	static function createDocType( data : String ) : Xml{
		var r = new JsXml__();
		untyped {
			r.nodeType = Xml.DocType;
			r.setNodeValue( data );
			return r;
		}
	}
	static function createProlog( data : String ) : Xml{
		var r = new JsXml__();
		untyped {
			r.nodeType = Xml.Prolog;
			r.setNodeValue( data );
			return r;
		}
	}
	static function createDocument() : Xml{
		var r = new JsXml__();
		r.nodeType = Xml.Document;
		r._children = new Array();
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
		return _attributes.get( att );
	}

	public function set( att : String, value : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		_attributes.set( att, value );
	}

	public function remove( att : String ) : Void{
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		_attributes.remove( att );
	}

	public function exists( att : String ) : Bool {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _attributes.exists( att );
	}

	public function attributes() : Iterator<String> {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _attributes.keys();
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
		while( _children[cur] != null && _children[cur].nodeType != Xml.Element )
			cur++;
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
			for( k in _attributes.keys() ){
				s.add(" ");
				s.add(k);
				s.add("=\"");
				s.add(_attributes.get(k));
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
