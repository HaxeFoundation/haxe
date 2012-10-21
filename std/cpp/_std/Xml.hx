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

enum XmlType {
}

@:coreApi class Xml {
	public static var Element(default,null) : XmlType;
	public static var PCData(default,null) : XmlType;
	public static var CData(default,null) : XmlType;
	public static var Comment(default,null) : XmlType;
	public static var DocType(default,null) : XmlType;
	public static var Prolog(default,null) : XmlType;
	public static var Document(default,null) : XmlType;


	private var _nodeName : String;
	private var _nodeValue : String;
	private var _attributes : Dynamic<String>;
	private var _children : Array<Xml>;
	private var _parent : Xml;

	function new() : Void {
	}

	private static var _parse = cpp.Lib.load("std","parse_xml",2);

	public static function parse( str : String ) : Xml {
		var x = new Xml();
		x._children = new Array();
		var parser = {
			cur : x,
			xml : function(name,att) {
				var x = new Xml();
				x._parent = untyped __this__.cur;
				x.nodeType = Xml.Element;
				x._nodeName = new String(name);
				x._attributes = att;
				x._children = new Array();
				untyped {
					var i = 0;
					__this__.cur.addChild(x);
					__this__.cur = x;
				}
			},
			cdata : function(text) {
				var x = new Xml();
				x._parent = untyped __this__.cur;
				x.nodeType = Xml.CData;
				x._nodeValue = new String(text);
				untyped __this__.cur.addChild(x);
			},
			pcdata : function(text) {
				var x = new Xml();
				x._parent = untyped __this__.cur;
				x.nodeType = Xml.PCData;
				x._nodeValue = new String(text);
				untyped __this__.cur.addChild(x);
			},
			comment : function(text:String) {
				var x = new Xml();
				x._parent = untyped __this__.cur;
				if( untyped text.cca(0) == 63 ) {
					x.nodeType = Xml.Prolog;
					text = new String(text);
					text = text.substr(1, text.length - 2);
				} else {
					x.nodeType = Xml.Comment;
					text = new String(text);
				}
				x._nodeValue = text;
				untyped __this__.cur.addChild(x);
			},
			doctype : function(text) {
				var x = new Xml();
				x._parent = untyped __this__.cur;
				x.nodeType = Xml.DocType;
				x._nodeValue = (new String(text)).substr(1);
				var p : Xml = untyped __this__.cur;
				p.addChild(x);
			},
			done : function() {
				untyped __this__.cur = __this__.cur._parent;
			}
		};
		untyped _parse(str,parser);
		x.nodeType = Xml.Document;
		return x;
	}


	public static function createElement( name : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.Element;
		r._nodeName = name;
		r._attributes = null;
		r._children = new Array();
		return r;
	}

	public static function createPCData( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.PCData;
		r._nodeValue = data;
		return r;
	}

	public static function createCData( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.CData;
		r._nodeValue = data;
		return r;
	}

	public static function createComment( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.Comment;
		r._nodeValue = data;
		return r;
	}

	public static function createDocType( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.DocType;
		r._nodeValue = data;
		return r;
	}

	public static function createProlog( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.Prolog;
		r._nodeValue = data;
		return r;
	}

	public static function createDocument() : Xml {
		var r = new Xml();
		r.nodeType = Xml.Document;
		r._children = new Array();
		return r;
	}

	public var nodeType(default,null) : XmlType;

	public var nodeName(get_nodeName,set_nodeName) : String;

	public var nodeValue(get_nodeValue,set_nodeValue) : String;


	private function get_nodeName() : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _nodeName;
	}

	private function set_nodeName( n : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _nodeName = n;
	}

	private function get_nodeValue() : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return _nodeValue;
	}

	private function set_nodeValue( v : String ) : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return _nodeValue = v;
	}

	public var parent(getParent,null) : Xml;
	private function getParent() : Xml {
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
		if (_attributes==null)
			_attributes = {};
		Reflect.setField (_attributes, att, value );
		return null;
	}

	public function remove( att : String ) : Void{
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		Reflect.deleteField( _attributes, att );
		return null;
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

	public function iterator() : Iterator<Xml> {
		if( _children == null )
			throw "bad nodetype";
      return untyped _children.iterator();
	}


	public function elements(): Iterator<Xml> {
		if( _children == null )
			throw "bad nodetype";
      var children = _children;
		return untyped {
			cur: 0,
			hasNext : function() {
				var k:Int = __this__.cur;
				var l = children.length;
				while( k < l ) {
					if( children[k].nodeType == Xml.Element )
						break;
					k += 1;
				}
				__this__.cur = k;
				return k < l;
			},
			next : function() {
				var k = __this__.cur;
				var l = children.length;
				while( k < l ) {
					var n = children[k];
					k += 1;
					if( n.nodeType == Xml.Element ) {
						__this__.cur = k;
						return n;
					}
				}
				return null;
			}
		}
	}

	public function elementsNamed( name : String ) : Iterator<Xml> {
		if( _children == null )
			throw "bad nodetype";
      var children = _children;
		return untyped {
			cur: 0,
			hasNext : function() {
				var k = __this__.cur;
				var l = children.length;
				while( k < l ) {
					var n = children[k];
					if( n.nodeType == Xml.Element && n._nodeName == name )
						break;
					k++;
				}
				__this__.cur = k;
				return k < l;
			},
			next : function() {
				var k = __this__.cur;
				var l = children.length;
				while( k < l ) {
					var n = children[k];
					k++;
					if( n.nodeType == Xml.Element && n._nodeName == name ) {
						__this__.cur = k;
						return n;
					}
				}
				return null;
			}
		}
	}

	public function firstChild() : Xml {
		if( _children == null )
			throw "bad nodetype";
		return _children[0];
	}

	public function firstElement() : Xml {
		if( _children == null )
			throw "bad nodetype";
		for( cur in 0..._children.length ) {
			var n:Xml = _children[cur];
			if( n.nodeType == Xml.Element )
				return n;
		}
		return null;
	}

   public function addChild( x : Xml ) : Void {
		if( _children == null )
			throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.push( x );
		return null;
	}

   public function removeChild( x : Xml ) : Bool {
		if( _children == null )
			throw "bad nodetype";
		var b = _children.remove( x );
		if( b ) x._parent = null;
		return b;
	}

	public function insertChild( x : Xml, pos : Int ) : Void {
		if( _children == null )
			throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.insert( pos, x );
		return null;
	}

	public function toString() : String {
		var s = new StringBuf();
		toStringRec(s);
		return s.toString();
	}

	private function toStringRec(s: StringBuf) : Void {
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

	static function __init__() : Void untyped {
		PCData = Type.createEnum(XmlType,"__");
		Element = Type.createEnum(XmlType,"__");
		CData =  Type.createEnum(XmlType,"__");
		Comment = Type.createEnum(XmlType,"__");
		DocType = Type.createEnum(XmlType,"__");
		Prolog =  Type.createEnum(XmlType,"__");
		Document = Type.createEnum(XmlType,"__");
		__global__.__hxcpp_enum_force(PCData , "pcdata", 0);
		__global__.__hxcpp_enum_force(Element , "element", 1);
		__global__.__hxcpp_enum_force(CData , "cdata", 2);
		__global__.__hxcpp_enum_force(Comment , "comment", 3);
		__global__.__hxcpp_enum_force(DocType , "doctype", 4);
		__global__.__hxcpp_enum_force(Prolog , "prolog", 5);
		__global__.__hxcpp_enum_force(Document , "document", 6);
	}

}

