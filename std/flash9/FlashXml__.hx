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
package flash;

import Xml;
import flash.xml.XML;
import flash.xml.XMLList;

class FlashXml__ {

	public static var Element : String;
	public static var PCData : String;
	public static var CData : String;
	public static var Comment : String;
	public static var DocType : String;
	public static var Prolog : String;
	public static var Document : String;

	public var nodeType(default,null) : XmlType;
	public var nodeName(getNodeName,setNodeName) : String;
	public var nodeValue(getNodeValue,setNodeValue) : String;
	public var parent(getParent,null) : FlashXml__;

	public static var _map : flash.utils.Dictionary;
	var _node : flash.xml.XML;

	public static function parse( str : String ) : FlashXml__ {
		XML.ignoreWhitespace = false;
		XML.ignoreProcessingInstructions = false;
		XML.ignoreComments = false;
		var root = new flash.xml.XML("<__document>" + str + "</__document>");
		return wrap( root, Xml.Document );
	}

	private function new() {}

	public static function createElement( name : String ) : FlashXml__ {
		return wrap( new flash.xml.XML("<"+name+"/>"), Xml.Element );
	}

	public static function createPCData( data : String ) : FlashXml__ {
		XML.ignoreWhitespace = false;
		return wrap( new flash.xml.XML(data), Xml.PCData );
	}

	public static function createCData( data : String ) : FlashXml__ {
		return wrap( new flash.xml.XML("<![CDATA[ "+data+" ]]>"), Xml.CData );
	}

	public static function createComment( data : String ) : FlashXml__ {
		XML.ignoreComments = false;
		return wrap( new flash.xml.XML("<!-- "+data+" -->"), Xml.Comment );
	}

	public static function createDocType( data : String ) : FlashXml__ {
		return wrap( new flash.xml.XML("<!DOCTYPE "+data+">"), Xml.DocType );
	}

	public static function createProlog( data : String ) : FlashXml__ {
		XML.ignoreProcessingInstructions = false;
		return wrap( new flash.xml.XML("<?"+data+"?>"), Xml.Prolog );
	}

	public static function createDocument() : FlashXml__ {
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

	private function getNodeName() : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var ns = _node.namespace();
		return (ns.prefix == "") ? _node.localName() : ns.prefix+":"+_node.localName();
	}

	private function setNodeName( n : String ) : String {
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

	private function getNodeValue() : String {
		if( _node.hasComplexContent() )
			throw "bad nodeType";
		return _node.toString();
	}

	private function setNodeValue( v : String ) : String {
		if( _node.hasComplexContent() || _node.children() == null )
			throw "bad nodeType";
		var children = _node.children();
		untyped __delete__(children, Reflect.fields(children)[0]);
		_node.appendChild(new XML(v));
		return v;
	}

	private function getParent() :FlashXml__ {
		return wrap( _node.parent() );
	}

	private static function wrap( node : XML, ?type : XmlType ) : FlashXml__ {
		var map : Dynamic = _map;
		if( map == null ) {
			map = new flash.utils.Dictionary(true);
			_map = map;
		}
		var x = untyped map[node];
		if( x == null ) {
			x = new FlashXml__();
			x._node = node;
			x.nodeType = (type != null) ? type : getNodeType( node );
			untyped map[node] = x;
		}
		return x;
	}

	private function wraps( xList : XMLList ) : Array<FlashXml__> {
		var out = new Array<FlashXml__>();
		for( i in 0...xList.length() )
			out.push( wrap(xList[i]) );
		return out;
	}

	function getAttribNS( ns : Array<String> ) {
		return _node.attribute(new flash.utils.QName(_node.namespace(ns[0]).uri,ns[1]));
	}

	public function get( att : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var ns = att.split(":");
		if( ns.length == 1 ) {
			if( !Reflect.hasField(_node,"@"+att) )
				return null;
			return Reflect.field(_node, "@"+att);
		}
		var a = getAttribNS(ns);
		return (a.length() == 0) ? null : a.toString();
	}

	public function set( att : String, value : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var ns = att.split(":");
		if( ns.length == 1 )
			Reflect.setField(_node, "@"+att, value);
		else {
			var a = getAttribNS(ns);
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
			untyped __delete__(getAttribNS(ns),0);
	}

	public function exists( att : String ) : Bool {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		var ns = att.split(":");
		if( ns.length == 1 )
			return Reflect.hasField(_node, "@"+att);
		return getAttribNS(ns).length() > 0;
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

	public function iterator() {
		var children:XMLList = _node.children();
		if( children == null )
			throw "bad nodetype";
		var wrappers :Array<FlashXml__> = wraps(children);
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

	public function elements() {
		var elements:XMLList = _node.elements();
		if( elements == null )
			throw "bad nodetype";
		var wrappers :Array<FlashXml__> = wraps(elements);
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

	public function elementsNamed( name : String ) {
		var ns = name.split(":");
		var elements:XMLList;
		if( ns.length == 1 )
			elements = _node.elements(name);
		else
			elements = _node.elements();
		if( elements == null )
			throw "bad nodetype";
		var wrappers :Array<FlashXml__> = wraps(elements);
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

	public function firstChild() : FlashXml__ {
		var children:XMLList = _node.children();
		if( children == null )
			throw "bad nodetype";
		if( children.length() == 0 )
			return null;
		return wrap( children[0] );
	}

	public function firstElement() : FlashXml__ {
		var elements:XMLList = _node.elements();
		if( elements == null )
			throw "bad nodetype";
		if( elements.length() == 0 )
			return null;
		return wrap( elements[0] );
	}

	public function addChild( x : FlashXml__ ) : Void {
		var children:XMLList = _node.children();
		if( children == null )
			throw "bad nodetype";
		_node.appendChild(x._node);
	}

	public function removeChild( x : FlashXml__ ) : Bool {
		var children:XMLList = _node.children();
		if( children == null )
			throw "bad nodetype";
		if( _node != x._node.parent() )
			return false;
		var i = x._node.childIndex();
		untyped __delete__(children, Reflect.fields(children)[i]);
		return true;
	}

	public function insertChild( x : FlashXml__, pos : Int ) : Void {
		var children:XMLList = _node.children();
		if( children == null )
			throw "bad nodetype";
		if( pos < children.length() )
			_node.insertChildBefore(children[pos], x._node);
		else
			_node.appendChild(x._node);
	}

	public function toString() {
		XML.prettyPrinting = false;
		if( nodeType == Xml.Document ) {
			var str = "";
			var c = _node.children();
			for( i in 0...c.length() )
				str += c[i].toXMLString();
			return str;
		}
		return _node.toXMLString();
	}

}
