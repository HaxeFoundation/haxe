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
package php;
import Xml;

class PhpXml__ {

	public static var Element(default,null) : XmlType;
	public static var PCData(default,null) : XmlType;
	public static var CData(default,null) : XmlType;
	public static var Comment(default,null) : XmlType;
	public static var DocType(default,null) : XmlType;
	public static var Prolog(default,null) : XmlType;
	public static var Document(default,null) : XmlType;

	public var nodeType(default,null) : XmlType;
	public var nodeName(getNodeName,setNodeName) : String;
	public var nodeValue(getNodeValue,setNodeValue) : String;
	public var parent(getParent,null) : PhpXml__;

	public var _nodeName : String;
	public var _nodeValue : String;
	public var _attributes : Hash<String>;
	public var _children : Array<PhpXml__>;
	public var _parent : PhpXml__;

	private static var build : PhpXml__;
	private static function __start_element_handler(parser : Dynamic, name : String, attribs : Array<String>) {
		var node = createElement(name);
		untyped __php__("foreach($attribs as $key => $value) $node->set($key, $value)");
		build.addChild(node);
		build = node;
	}
	
	private static function __end_element_handler(parser : Dynamic, name : String) {
//		if(untyped __call__("count", build._children) == 0)
//			build.addChild(createPCData(""));
		build = build.getParent();
	}
	
	private static function __character_data_handler(parser : Dynamic, data : String) {
		// TODO: this function can probably be simplified
		var lc : PhpXml__ = (build._children == null || untyped __call__("count", build._children) == 0) ? null : build._children[untyped __call__("count", build._children) -1];
		if(lc != null && Xml.PCData == lc.nodeType) {
			lc.nodeValue = lc.nodeValue + untyped __call__("htmlentities", data);
		} else if((untyped __call__("strlen", data) == 1 && __call__("htmlentities", data) != data) || untyped __call__("htmlentities", data) == data) {
			build.addChild(createPCData(untyped __call__("htmlentities", data)));
		} else
			build.addChild(createCData(data));
	}
	
	private static function __default_handler(parser : Dynamic, data : String) {
		build.addChild(createPCData(data));
	}

	public static function parse( str : String ) : PhpXml__ {
		build = createDocument();
		var xml_parser = untyped __call__("xml_parser_create");
		untyped __call__("xml_set_element_handler", xml_parser, __start_element_handler, __end_element_handler);
		untyped __call__("xml_set_character_data_handler", xml_parser, __character_data_handler);
		untyped __call__("xml_set_default_handler", xml_parser, __default_handler);
		untyped __call__("xml_parser_set_option", xml_parser, __php__("XML_OPTION_CASE_FOLDING"), 0);
		untyped __call__("xml_parser_set_option", xml_parser, __php__("XML_OPTION_SKIP_WHITE"), 0);

		if(untyped __call__("xml_parse", xml_parser, str, true) != 1) {
			throw untyped __call__("xml_error_string", xml_parser) + ", at line #" + __call__("xml_get_current_line_number", xml_parser);
		}
		untyped __call__("xml_parser_free", xml_parser);
		return build;
	}

	private function new(){
	}

	public static function createElement( name : String ) : PhpXml__ {
		var r = new PhpXml__();
		r.nodeType = Xml.Element;
		r._children = new Array();
		r._attributes = new Hash();
		r.setNodeName( name );
		return r;
	}

	public static function createPCData( data : String ) : PhpXml__ {
		var r = new PhpXml__();
		r.nodeType = Xml.PCData;
		r.setNodeValue( data );
		return r;
	}

	public static function createCData( data : String ) : PhpXml__ {
		var r = new PhpXml__();
		r.nodeType = Xml.CData;
		r.setNodeValue( data );
		return r;
	}

	public static function createComment( data : String ) : PhpXml__ {
		var r = new PhpXml__();
		r.nodeType = Xml.Comment;
		r.setNodeValue( data );
		return r;
	}

	public static function createDocType( data : String ) : PhpXml__ {
		var r = new PhpXml__();
		r.nodeType = Xml.DocType;
		r.setNodeValue( data );
		return r;
	}

	public static function createProlog( data : String ) : PhpXml__ {
		var r = new PhpXml__();
		r.nodeType = Xml.Prolog;
		r.setNodeValue( data );
		return r;
	}

	public static function createDocument() : PhpXml__ {
		var r = new PhpXml__();
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

	private function getParent() {
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
		_attributes.set( att, untyped __call__("htmlspecialchars", value, __php__('ENT_COMPAT'), 'UTF-8', true));
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

	public function iterator() : Iterator<PhpXml__> {
		if( _children == null ) throw "bad nodetype";
		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function(){
				return this.cur < __call__("count", this.x);
			},
			next : function(){
				return this.x[this.cur++];
			}
		}
	}

	public function elements(){
		if( _children == null ) throw "bad nodetype";
		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function() {
				var k = this.cur;
				var l = __call__("count", this.x);
				while( k < l ) {
				
					if( this.x[k].nodeType == Xml.Element )
						untyped __php__("break");
					k += 1;
				}
				this.cur = k;
				return k < l;
			},
			next : function() {
				var k = this.cur;
				var l = __call__("count", this.x);
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
		if( _children == null ) throw "bad nodetype";
		return untyped {
			cur: 0,
			x: this._children,
			hasNext : function() {
				var k = this.cur;
				var l = __call__("count", this.x);
				while( k < l ) {
					var n = this.x[k];
					if( n.nodeType == Xml.Element && n._nodeName == name )
						untyped __php__("break");
					k++;
				}
				this.cur = k;
				return k < l;
			},
			next : function() {
				var k = this.cur;
				var l = __call__("count", this.x);
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

	public function firstChild() : PhpXml__ {
		if( _children == null ) throw "bad nodetype";
		return _children[0];
	}

	public function firstElement() : PhpXml__ {
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

	public function addChild( x : PhpXml__ ) : Void {
		if( _children == null ) throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.push( x );
	}

	public function removeChild( x : PhpXml__ ) : Bool {
		if( _children == null ) throw "bad nodetype";
		var b = _children.remove( x );
		if( b )
			x._parent = null;
		return b;
	}

	public function insertChild( x : PhpXml__, pos : Int ) : Void {
		if( _children == null ) throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.insert( pos, x );
	}

	public function toString() {
		if( nodeType == Xml.PCData )
			return _nodeValue;
		if( nodeType == Xml.CData )
			return "<![CDATA["+_nodeValue+"]]>";
		if( nodeType == Xml.Comment || nodeType == Xml.DocType || nodeType == Xml.Prolog )
			return _nodeValue;

		var s = "";

		if( nodeType == Xml.Element ) {
			s += "<";
			s += _nodeName;
			for( k in _attributes.keys() ){
				s += " ";
				s += k;
				s += "=\""; // \"
				s += _attributes.get(k);
				s += "\""; // \"
			}
			if( _children.length == 0 ) {
				s += "/>";
				return s;
			}
			s += ">";
		}

		for( x in iterator() )
			s += x.toString();

		if( nodeType == Xml.Element ) {
			s += "</";
			s += _nodeName;
			s += ">";
		}
		return s;
	}
	
	static function __init__() : Void untyped {
		PhpXml__.Element = "element";
		PhpXml__.PCData = "pcdata";
		PhpXml__.CData = "cdata";
		PhpXml__.Comment = "comment";
		PhpXml__.DocType = "doctype";
		PhpXml__.Prolog = "prolog";
		PhpXml__.Document = "document";
	}

}
