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

class NekoNode__ implements Node {

	public static var element_node : Int = 1;
	public static var text_node : Int = 3;

	public var nodeName : String;
	public var nodeValue : String;
	public var nodeType : Int;

	public var attributes : Dynamic<String>;

	public var parentNode : Node;
	public var childNodes : Array<Node>;
	public var firstChild : Node;
	public var lastChild : Node;
	public var previousSibling : Node;
	public var nextSibling : Node;

	private function new() {
	}

	public function insertBefore(newChild,refChild) {
		throw "not implemented";
	}

	public function removeChild(child) {
		throw "not implemented";
	}

	public function appendChild(child) {
		throw "not implemented";
	}

	public function hasChildNodes() {
		return throw "not implemented";
	}

	public function cloneNode(deep) {
		return throw "not implemented";
	}

	public function toString() {
		if( nodeType == text_node )
			return nodeValue;
		var s = new StringBuf();
		if( nodeName != null ) {
			s.add("<");
			s.add(nodeName);
			for( k in Reflect.fields(attributes) ) {
				s.add(" ");
				s.add(k);
				s.add("=\"");
				s.add(Reflect.field(attributes,k));
				s.add("\"");
			}
			if( childNodes.length == 0 ) {
				s.add("/>");
				return s.toString();
			}
			s.add(">");
		}
		for( x in childNodes )
			s.add(x);
		if( nodeName != null ) {
			s.add("</");
			s.add(nodeName);
			s.add(">");
		}
		return s.toString();
	}

	public function replaceChild(newChild,oldChild) {
		throw "not implemented";
	}

	public function nodes() {
		return untyped {
			p : 0,
			a : childNodes,
			next : function() {
				while true {
					var x = this.a[this.p];
					if( x == null )
						return null;
					this.p++;
					if( x.nodeType == 1 )
						return x;
				}
				return null;
			},
			hasNext : function() {
				var x = this.next();
				if( x != null ) {
					this.p--;
					return true;
				}
				return false;
			}
		};
	}

}