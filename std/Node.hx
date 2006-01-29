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

extern class Node {

	static var element_node : Int = 1;
	static var text_node : Int = 3;

	var nodeName : String;
	var nodeValue : String;
	var nodeType : Int;

	// working in JS ?
	var attributes : Dynamic<String>;

	var parentNode : Node;
	var childNodes : Array<Node>;
	var firstChild : Node;
	var lastChild : Node;
	var previousSibling : Node;
	var nextSibling : Node;
	// var ownerDocument : Document; => Not in Flash

	function new() : Void;

	function insertBefore( newChild : Node, refChild : Node ) : Void;

	/* renamed from removeNode in Flash */
	function removeChild( child : Node ) : Void;

	function appendChild( child : Node ) : Void;
	function hasChildNodes() : Bool;
	function cloneNode( deep : Bool ) : Node;
	function toString() : String;

	/* added in Flash */
	function replaceChild( newChild : Node, oldChild : Node ) : Void;

}