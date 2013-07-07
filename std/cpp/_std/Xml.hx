/*
 * Copyright (C)2005-2012 Haxe Foundation
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
enum XmlType {
}

@:coreApi class Xml {
	public static var Element(default,null) : XmlType;
	public static var PCData(default,null) : XmlType;
	public static var CData(default,null) : XmlType;
	public static var Comment(default,null) : XmlType;
	public static var DocType(default,null) : XmlType;
	public static var ProcessingInstruction(default,null) : XmlType;
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
					x.nodeType = Xml.ProcessingInstruction;
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

	public static function createProcessingInstruction( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.ProcessingInstruction;
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

	public var nodeName(get,set) : String;

	public var nodeValue(get,set) : String;


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

	public var parent(get,null) : Xml;
	private function get_parent() : Xml {
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
			s.add(StringTools.htmlEscape(_nodeValue));
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
		case Xml.ProcessingInstruction:
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
		ProcessingInstruction =  Type.createEnum(XmlType,"__");
		Document = Type.createEnum(XmlType,"__");
		__global__.__hxcpp_enum_force(PCData , "pcdata", 0);
		__global__.__hxcpp_enum_force(Element , "element", 1);
		__global__.__hxcpp_enum_force(CData , "cdata", 2);
		__global__.__hxcpp_enum_force(Comment , "comment", 3);
		__global__.__hxcpp_enum_force(DocType , "doctype", 4);
		__global__.__hxcpp_enum_force(ProcessingInstruction , "processingInstruction", 5);
		__global__.__hxcpp_enum_force(Document , "document", 6);
	}

}

