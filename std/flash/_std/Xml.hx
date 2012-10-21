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

import flash.xml.XML;
import flash.xml.XMLList;

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

	public var nodeType(default,null) : XmlType;
	public var nodeName(get_nodeName,set_nodeName) : String;
	public var nodeValue(get_nodeValue,set_nodeValue) : String;
	public var parent(getParent,null) : Xml;

	var _node : flash.xml.XML;

	public static function parse( str : String ) : Xml {
		XML.ignoreWhitespace = false;
		XML.ignoreProcessingInstructions = false;
		XML.ignoreComments = false;
		var prefix = "<__document";
		var root = null;
		while( root == null ) {
			try {
				root = new flash.xml.XML(prefix+">" + str + "</__document>");
			} catch( e : flash.errors.TypeError ) {
				// if we miss a namespace, let's add it !
				var r = ~/"([^"]+)"/; //"
				if( e.errorID == 1083 && r.match(e.message) ) {
					var ns = r.matched(1);
					prefix += " xmlns:" + ns + '="@' + ns + '"';
				} else
					throw e;
			}
		}
		return wrap( root, Xml.Document );
	}

	@:keep #if as3 @:hack public #end static function compare( a : Xml, b : Xml ) : Bool {
		return a == null ? b == null : (b == null ? false : a._node == b._node);
	}

	private function new() : Void {}

	public static function createElement( name : String ) : Xml {
		return wrap( new flash.xml.XML("<"+name+"/>"), Xml.Element );
	}

	public static function createPCData( data : String ) : Xml {
		XML.ignoreWhitespace = false;
		return wrap( new flash.xml.XML(data), Xml.PCData );
	}

	public static function createCData( data : String ) : Xml {
		return wrap( new flash.xml.XML("<![CDATA["+data+"]]>"), Xml.CData );
	}

	public static function createComment( data : String ) : Xml {
		XML.ignoreComments = false;
		return wrap( new flash.xml.XML("<!--"+data+"-->"), Xml.Comment );
	}

	public static function createDocType( data : String ) : Xml {
		return wrap( new flash.xml.XML("<!DOCTYPE "+data+">"), Xml.DocType );
	}

	public static function createProlog( data : String ) : Xml {
		XML.ignoreProcessingInstructions = false;
		return wrap( new flash.xml.XML("<?"+data+"?>"), Xml.Prolog );
	}

	public static function createDocument() : Xml {
		return wrap( new flash.xml.XML("<__document/>"), Xml.Document );
	}

	private static function getNodeType( node : flash.xml.XML ) : XmlType {
		switch( node.nodeKind() ) {
		case "element":
			return Xml.Element;
		case "text":
			return Xml.PCData;
		case "processing-instruction":
			return Xml.Prolog;
		case "comment":
			return Xml.Comment;
		default :
			throw "unimplemented node type: " + node.nodeType;
		}
		return null;
	}

	private function get_nodeName() : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var ns = _node.namespace();
		return (ns.prefix == "") ? _node.localName() : ns.prefix+":"+_node.localName();
	}

	private function set_nodeName( n : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var ns = n.split(":");
		if( ns.length == 1 )
			_node.setLocalName(n);
		else {
			_node.setLocalName(ns[1]);
			_node.setNamespace(_node.namespace(ns[0]));
		}
		return n;
	}

	private function get_nodeValue() : String {
		var nodeType = nodeType;
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		if( nodeType == Xml.Comment )
			return _node.toString().substr(4,-7);
		return _node.toString();
	}

	private function set_nodeValue( v : String ) : String {
		var nodeType = nodeType;
		var x = null;
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		else if( nodeType == Xml.PCData )
			x = createPCData(v);
		else if( nodeType == Xml.CData )
			x = createCData(v);
		else if( nodeType == Xml.Comment )
			x = createComment(v);
		else if( nodeType == Xml.DocType )
			x = createDocType(v);
		else
			x = createProlog(v);
		var p = _node.parent();
		if( p != null ) {
			p.insertChildAfter(_node, x._node);
			var i = _node.childIndex();
			var children = p.children();
			untyped __delete__(children, Reflect.fields(children)[i]);
		}
		_node = x._node;
		return v;
	}

	private function getParent() :Xml {
		var p = _node.parent();
		return p == null ? null : wrap( p );
	}

	private static function wrap( node : XML, ?type : XmlType ) : Xml {
		var x = new Xml();
		x._node = node;
		x.nodeType = (type != null) ? type : getNodeType( node );
		return x;
	}

	private function wraps( xList : XMLList ) : Array<Xml> {
		var out = new Array<Xml>();
		for( i in 0...xList.length() )
			out.push( wrap(xList[i]) );
		return out;
	}

	function getAttribNS( cur : XML, ns : Array<String> ) : XMLList {
		var n = cur.namespace(ns[0]);
		if( n == null ) {
			var parent = cur.parent();
			if( parent == null ) {
				n = new flash.utils.Namespace(ns[0], "@"+ns[0]);
				cur.addNamespace(n);
			} else
				return getAttribNS(parent, ns);
		}
		return _node.attribute(new flash.utils.QName(n,ns[1]));
	}

	public function get( att : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var ns = att.split(":");
		if( ns[0] == "xmlns" ) {
			var n = _node.namespace((ns[1] == null) ? "" : ns[1]);
			return (n == null) ? null : n.uri;
		}
		if( ns.length == 1 ) {
			if( !Reflect.hasField(_node,"@"+att) )
				return null;
			return Reflect.field(_node, "@"+att);
		}
		var a = getAttribNS(_node,ns);
		return (a.length() == 0) ? null : a.toString();
	}

	public function set( att : String, value : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var ns = att.split(":");
		if( ns[0] == "xmlns" ) {
			var n = _node.namespace((ns[1] == null) ? "" : ns[1]);
			if( n != null )
				throw "Can't modify namespace";
			if( ns[1] == null )
				throw "Can't set default namespace";
			_node.addNamespace(new flash.utils.Namespace(ns[1], value));
			return;
		}
		if( ns.length == 1 )
			Reflect.setField(_node, "@"+att, value);
		else {
			var a = getAttribNS(_node,ns);
			untyped a[0] = value;
		}
	}

	public function remove( att : String ) : Void{
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var ns = att.split(":");
		if( ns.length == 1 )
			Reflect.deleteField(_node, "@"+att);
		else
			untyped __delete__(getAttribNS(_node,ns),0);
	}

	public function exists( att : String ) : Bool {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var ns = att.split(":");
		if( ns[0] == "xmlns" )
			return _node.namespace((ns[1] == null) ? "" : ns[1]) != null;
		if( ns.length == 1 )
			return Reflect.hasField(_node, "@"+att);
		return getAttribNS(_node,ns).length() > 0;
	}

	public function attributes() : Iterator<String> {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var attributes :XMLList = _node.attributes();
		var names = Reflect.fields(attributes);
		var cur = 0;
		return {
			hasNext : function(){
				return cur < names.length;
			},
			next : function(){
				return attributes[Std.parseInt(names[cur++])].name();
			}
		}
	}

	public function iterator() : Iterator<Xml> {
		if( nodeType != Xml.Element && nodeType != Xml.Document )
			throw "bad nodeType";
		var children:XMLList = _node.children();
		var wrappers :Array<Xml> = wraps(children);
		var cur = 0;
		return {
			hasNext : function(){
				return cur < wrappers.length;
			},
			next : function(){
				return wrappers[cur++];
			}
		};
	}

	public function elements() : Iterator<Xml> {
		if( nodeType != Xml.Element && nodeType != Xml.Document )
			throw "bad nodeType";
		var elements:XMLList = _node.elements();
		var wrappers :Array<Xml> = wraps(elements);
		var cur = 0;
		return {
			hasNext : function(){
				return cur < wrappers.length;
			},
			next : function(){
				return wrappers[cur++];
			}
		};
	}

	public function elementsNamed( name : String ) : Iterator<Xml> {
		if( nodeType != Xml.Element && nodeType != Xml.Document )
			throw "bad nodeType";
		var ns = name.split(":");
		var elements:XMLList;
		if( ns.length == 1 )
			elements = _node.elements(name);
		else
			elements = _node.elements();
		var wrappers :Array<Xml> = wraps(elements);
		if( ns.length != 1 )
			for( w in wrappers.copy() )
				if( w._node.localName() != ns[1] || w._node.namespace().prefix != ns[0] )
					wrappers.remove(w);
		var cur = 0;
		return {
			hasNext : function(){
				return cur < wrappers.length;
			},
			next : function(){
				return wrappers[cur++];
			}
		};
	}

	public function firstChild() : Xml {
		if( nodeType != Xml.Element && nodeType != Xml.Document )
			throw "bad nodeType";
		var children:XMLList = _node.children();
		if( children.length() == 0 )
			return null;
		return wrap( children[0] );
	}

	public function firstElement() : Xml {
		if( nodeType != Xml.Element && nodeType != Xml.Document )
			throw "bad nodeType";
		var elements:XMLList = _node.elements();
		if( elements.length() == 0 )
			return null;
		return wrap( elements[0] );
	}

	public function addChild( x : Xml ) : Void {
		if( nodeType != Xml.Element && nodeType != Xml.Document )
			throw "bad nodeType";
		var children:XMLList = _node.children();
		_node.appendChild(x._node);
	}

	public function removeChild( x : Xml ) : Bool {
		if( nodeType != Xml.Element && nodeType != Xml.Document )
			throw "bad nodeType";
		var children:XMLList = _node.children();
		if( _node != x._node.parent() )
			return false;
		var i = x._node.childIndex();
		untyped __delete__(children, Reflect.fields(children)[i]);
		return true;
	}

	public function insertChild( x : Xml, pos : Int ) : Void {
		if( nodeType != Xml.Element && nodeType != Xml.Document )
			throw "bad nodeType";
		var children:XMLList = _node.children();
		if( pos < children.length() )
			_node.insertChildBefore(children[pos], x._node);
		else
			_node.appendChild(x._node);
	}

	public function toString() : String {
		XML.prettyPrinting = false;
		if( nodeType == Xml.Document ) {
			var str = _node.toXMLString();
			// remove <__document xmlns....>STR</__document> wrapper
			str = str.substr(str.indexOf(">") + 1);
			str = str.substr(0, str.length - 13);
			return str;
		}
		return _node.toXMLString();
	}

	static function __init__() : Void untyped {
		Element = "element";
		PCData = "pcdata";
		CData = "cdata";
		Comment = "comment";
		DocType = "doctype";
		Prolog = "prolog";
		Document = "document";
	}


}
