/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/Document.webidl line 18:0. Do not edit!

package js.html;

@:native("Document")
extern class Document extends Node
{
	var implementation(default,null) : DOMImplementation;
	var URL(default,null) : String;
	var documentURI(default,null) : String;
	var compatMode(default,null) : String;
	var characterSet(default,null) : String;
	var contentType(default,null) : String;
	var doctype(default,null) : DocumentType;
	var documentElement(default,null) : Element;
	var inputEncoding(default,null) : String;
	var fullscreenEnabled(default,null) : Bool;
	var fullscreenElement(default,null) : Element;
	var onfullscreenchange : haxe.Constraints.Function;
	var onfullscreenerror : haxe.Constraints.Function;
	var location(default,null) : Location;
	var referrer(default,null) : String;
	var lastModified(default,null) : String;
	var readyState(default,null) : String;
	var title : String;
	var dir : String;
	var defaultView(default,null) : Window;
	var activeElement(default,null) : Element;
	var onreadystatechange : haxe.Constraints.Function;
	var onwheel : haxe.Constraints.Function;
	var oncopy : haxe.Constraints.Function;
	var oncut : haxe.Constraints.Function;
	var onpaste : haxe.Constraints.Function;
	var onbeforescriptexecute : haxe.Constraints.Function;
	var onafterscriptexecute : haxe.Constraints.Function;
	var currentScript(default,null) : Element;
	var pointerLockElement(default,null) : Element;
	var hidden(default,null) : Bool;
	var visibilityState(default,null) : VisibilityState;
	var styleSheets(default,null) : StyleSheetList;
	var selectedStyleSheetSet : String;
	var lastStyleSheetSet(default,null) : String;
	var preferredStyleSheetSet(default,null) : String;
	var styleSheetSets(default,null) : DOMStringList;
	var timeline(default,null) : AnimationTimeline;
	var fonts(default,null) : FontFaceSet;
	var onabort : haxe.Constraints.Function;
	var onblur : haxe.Constraints.Function;
	var onfocus : haxe.Constraints.Function;
	var oncanplay : haxe.Constraints.Function;
	var oncanplaythrough : haxe.Constraints.Function;
	var onchange : haxe.Constraints.Function;
	var onclick : haxe.Constraints.Function;
	var oncontextmenu : haxe.Constraints.Function;
	var ondblclick : haxe.Constraints.Function;
	var ondrag : haxe.Constraints.Function;
	var ondragend : haxe.Constraints.Function;
	var ondragenter : haxe.Constraints.Function;
	var ondragleave : haxe.Constraints.Function;
	var ondragover : haxe.Constraints.Function;
	var ondragstart : haxe.Constraints.Function;
	var ondrop : haxe.Constraints.Function;
	var ondurationchange : haxe.Constraints.Function;
	var onemptied : haxe.Constraints.Function;
	var onended : haxe.Constraints.Function;
	var oninput : haxe.Constraints.Function;
	var oninvalid : haxe.Constraints.Function;
	var onkeydown : haxe.Constraints.Function;
	var onkeypress : haxe.Constraints.Function;
	var onkeyup : haxe.Constraints.Function;
	var onload : haxe.Constraints.Function;
	var onloadeddata : haxe.Constraints.Function;
	var onloadedmetadata : haxe.Constraints.Function;
	var onloadstart : haxe.Constraints.Function;
	var onmousedown : haxe.Constraints.Function;
	var onmouseenter : haxe.Constraints.Function;
	var onmouseleave : haxe.Constraints.Function;
	var onmousemove : haxe.Constraints.Function;
	var onmouseout : haxe.Constraints.Function;
	var onmouseover : haxe.Constraints.Function;
	var onmouseup : haxe.Constraints.Function;
	var onpause : haxe.Constraints.Function;
	var onplay : haxe.Constraints.Function;
	var onplaying : haxe.Constraints.Function;
	var onprogress : haxe.Constraints.Function;
	var onratechange : haxe.Constraints.Function;
	var onreset : haxe.Constraints.Function;
	var onscroll : haxe.Constraints.Function;
	var onseeked : haxe.Constraints.Function;
	var onseeking : haxe.Constraints.Function;
	var onselect : haxe.Constraints.Function;
	var onshow : haxe.Constraints.Function;
	var onstalled : haxe.Constraints.Function;
	var onsubmit : haxe.Constraints.Function;
	var onsuspend : haxe.Constraints.Function;
	var ontimeupdate : haxe.Constraints.Function;
	var onvolumechange : haxe.Constraints.Function;
	var onwaiting : haxe.Constraints.Function;
	var onpointercancel : haxe.Constraints.Function;
	var onpointerdown : haxe.Constraints.Function;
	var onpointerup : haxe.Constraints.Function;
	var onpointermove : haxe.Constraints.Function;
	var onpointerout : haxe.Constraints.Function;
	var onpointerover : haxe.Constraints.Function;
	var onpointerenter : haxe.Constraints.Function;
	var onpointerleave : haxe.Constraints.Function;
	var ongotpointercapture : haxe.Constraints.Function;
	var onlostpointercapture : haxe.Constraints.Function;
	var onpointerlockchange : haxe.Constraints.Function;
	var onpointerlockerror : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	var children(default,null) : HTMLCollection;
	var firstElementChild(default,null) : Element;
	var lastElementChild(default,null) : Element;
	var childElementCount(default,null) : Int;
	var ontouchstart : haxe.Constraints.Function;
	var ontouchend : haxe.Constraints.Function;
	var ontouchmove : haxe.Constraints.Function;
	var ontouchcancel : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new() : Void;
	function getElementsByTagName( localName : String ) : HTMLCollection;
	/** @throws DOMError */
	function getElementsByTagNameNS( namespace_ : String, localName : String ) : HTMLCollection;
	function getElementsByClassName( classNames : String ) : HTMLCollection;
	function getElementById( elementId : String ) : Element;
	/** @throws DOMError */
	@:overload( function( localName : String ) : Element {} )
	function createElement( localName : String, typeExtension : String ) : Element;
	/** @throws DOMError */
	@:overload( function( namespace_ : String, qualifiedName : String ) : Element {} )
	function createElementNS( namespace_ : String, qualifiedName : String, typeExtension : String ) : Element;
	function createDocumentFragment() : DocumentFragment;
	function createTextNode( data : String ) : Text;
	function createComment( data : String ) : Comment;
	/** @throws DOMError */
	function createProcessingInstruction( target : String, data : String ) : ProcessingInstruction;
	/** @throws DOMError */
	function importNode( node : Node, ?deep : Bool = false ) : Node;
	/** @throws DOMError */
	function adoptNode( node : Node ) : Node;
	/** @throws DOMError */
	function createEvent( interface_ : String ) : Event;
	/** @throws DOMError */
	function createRange() : Range;
	/** @throws DOMError */
	function createNodeIterator( root : Node, ?whatToShow : Int = cast 4294967295, ?filter : NodeFilter ) : NodeIterator;
	/** @throws DOMError */
	function createTreeWalker( root : Node, ?whatToShow : Int = cast 4294967295, ?filter : NodeFilter ) : TreeWalker;
	/** @throws DOMError */
	function createCDATASection( data : String ) : CDATASection;
	/** @throws DOMError */
	function createAttribute( name : String ) : Attr;
	/** @throws DOMError */
	function createAttributeNS( namespace_ : String, name : String ) : Attr;
	function exitFullscreen() : Void;
	/** @throws DOMError */
	function hasFocus() : Bool;
	function releaseCapture() : Void;
	function exitPointerLock() : Void;
	/** @throws DOMError */
	function registerElement( name : String, ?options : ElementRegistrationOptions ) : Dynamic;
	function enableStyleSheetsForSet( name : String ) : Void;
	function elementFromPoint( x : Float, y : Float ) : Element;
	function caretPositionFromPoint( x : Float, y : Float ) : CaretPosition;
	/** @throws DOMError */
	function querySelector( selectors : String ) : Element;
	/** @throws DOMError */
	function querySelectorAll( selectors : String ) : NodeList;
	function createTouch( ?view : Window, ?target : EventTarget, ?identifier : Int = 0, ?pageX : Int = 0, ?pageY : Int = 0, ?screenX : Int = 0, ?screenY : Int = 0, ?clientX : Int = 0, ?clientY : Int = 0, ?radiusX : Int = 0, ?radiusY : Int = 0, ?rotationAngle : Float = 0.0, ?force : Float = 0.0 ) : Touch;
	@:overload( function( touch : Touch, touches : haxe.extern.Rest<Touch> ) : TouchList {} )
	@:overload( function() : TouchList {} )
	function createTouchList( touches : Array<Touch> ) : TouchList;
	/** @throws DOMError */
	function convertQuadFromNode( quad : DOMQuad, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	function convertRectFromNode( rect : DOMRectReadOnly, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	function convertPointFromNode( point : DOMPointInit, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMPoint;
	/** @throws DOMError */
	function createExpression( expression : String, resolver : XPathNSResolver ) : XPathExpression;
	function createNSResolver( nodeResolver : Node ) : Node;
	/** @throws DOMError */
	function evaluate( expression : String, contextNode : Node, resolver : XPathNSResolver, type : Int, result : Dynamic ) : XPathResult;
}