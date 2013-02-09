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

	public var nodeType(default,null) : XmlType;
	public var nodeName(get,set) : String;
	public var nodeValue(get,set) : String;
	public var parent(get,null) : Xml;

	var _nodeName : String;
	var _nodeValue : String;
	var _attributes : haxe.ds.StringMap<String>;
	var _children : Array<Xml>;
	var _parent : Xml;

	public static function parse( str : String ) : Xml {
		return haxe.xml.Parser.parse(str);
	}

	private function new() : Void {
	}

	public static function createElement( name : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.Element;
		r._children = new Array();
		r._attributes = new haxe.ds.StringMap();
		r.set_nodeName( name );
		return r;
	}

	public static function createPCData( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.PCData;
		r.set_nodeValue( data );
		return r;
	}

	public static function createCData( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.CData;
		r.set_nodeValue( data );
		return r;
	}

	public static function createComment( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.Comment;
		r.set_nodeValue( data );
		return r;
	}

	public static function createDocType( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.DocType;
		r.set_nodeValue( data );
		return r;
	}

	public static function createProcessingInstruction( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.ProcessingInstruction;
		r.set_nodeValue( data );
		return r;
	}

	public static function createDocument() : Xml {
		var r = new Xml();
		r.nodeType = Xml.Document;
		r._children = new Array();
		return r;
	}

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

	private function get_parent() : Xml {
		return _parent;
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

	public function iterator() : Iterator<Xml> {
		if( _children == null ) throw "bad nodetype";
		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function(){
				return __this__.cur < __this__.x.length;
			},
			next : function(){
				return __this__.x[__this__.cur++];
			}
		}
	}

	public function elements() : Iterator<Xml> {
		if( _children == null ) throw "bad nodetype";
		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function() {
				var k = __this__.cur;
				var l = __this__.x.length;
				while( k < l ) {
					if( __this__.x[k].nodeType == Xml.Element )
						break;
					k += 1;
				}
				__this__.cur = k;
				return k < l;
			},
			next : function() {
				var k = __this__.cur;
				var l = __this__.x.length;
				while( k < l ) {
					var n = __this__.x[k];
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
		if( _children == null ) throw "bad nodetype";
		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function() {
				var k = __this__.cur;
				var l = __this__.x.length;
				while( k < l ) {
					var n = __this__.x[k];
					if( n.nodeType == Xml.Element && n._nodeName == name )
						break;
					k++;
				}
				__this__.cur = k;
				return k < l;
			},
			next : function() {
				var k = __this__.cur;
				var l = __this__.x.length;
				while( k < l ) {
					var n = __this__.x[k];
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
		if( _children == null ) throw "bad nodetype";
		return _children[0];
	}

	public function firstElement() : Xml {
		if( _children == null ) throw "bad nodetype";
		var cur = 0;
		var l = _children.length;
		while( cur < l ) {
			var n = _children[cur];
			if( n.nodeType == Xml.Element )
				return n;
			cur++;
		}
		return null;
	}

	public function addChild( x : Xml ) : Void {
		if( _children == null ) throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.push( x );
	}

	public function removeChild( x : Xml ) : Bool {
		if( _children == null ) throw "bad nodetype";
		var b = _children.remove( x );
		if( b )
			x._parent = null;
		return b;
	}

	public function insertChild( x : Xml, pos : Int ) : Void {
		if( _children == null ) throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.insert( pos, x );
	}

	public function toString() : String {
		if( nodeType == Xml.PCData )
			return StringTools.htmlEscape(_nodeValue);
		if( nodeType == Xml.CData )
			return "<![CDATA["+_nodeValue+"]]>";
		if( nodeType == Xml.Comment )
			return "<!--"+_nodeValue+"-->";
		if( nodeType == Xml.DocType )
			return "<!DOCTYPE "+_nodeValue+">";
		if( nodeType == Xml.ProcessingInstruction )
			return "<?"+_nodeValue+"?>";
		var s = new StringBuf();

		if( nodeType == Xml.Element ) {
			s.add("<");
			s.add(_nodeName);
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
			s.add(x.toString());

		if( nodeType == Xml.Element ) {
			s.add("</");
			s.add(_nodeName);
			s.add(">");
		}
		return s.toString();
	}

	static function __init__() : Void untyped {
		Xml.Element = "element";
		Xml.PCData = "pcdata";
		Xml.CData = "cdata";
		Xml.Comment = "comment";
		Xml.DocType = "doctype";
		Xml.ProcessingInstruction = "processingInstruction";
		Xml.Document = "document";
	}

}
