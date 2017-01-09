/*
 * Copyright (C)2005-2017 Haxe Foundation
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

// This file is generated from mozilla\Element.webidl. Do not edit!

package js.html;

/**
	The `Element` interface represents an object of a `Document`. This interface describes methods and properties common to all kinds of elements. Specific behaviors are described in interfaces which inherit from `Element` but add additional functionality.

	Documentation [Element](https://developer.mozilla.org/en-US/docs/Web/API/Element) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Element$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Element>
**/
@:native("Element")
extern class DOMElement extends Node
{
	var tagName(default,null) : String;
	var id : String;
	var className : String;
	var classList(default,null) : DOMTokenList;
	var attributes(default,null) : NamedNodeMap;
	var onwheel : haxe.Constraints.Function;
	var title : String;
	var lang : String;
	var dir : String;
	var dataset(default,null) : DOMStringMap;
	var innerText : String;
	var itemScope : Bool;
	var itemType(default,null) : DOMTokenList;
	var itemId : String;
	var itemRef(default,null) : DOMTokenList;
	var itemProp(default,null) : DOMTokenList;
	var properties(default,null) : HTMLPropertiesCollection;
	var itemValue : Dynamic;
	var hidden : Bool;
	var tabIndex : Int;
	var accessKey : String;
	var accessKeyLabel(default,null) : String;
	var draggable : Bool;
	var contentEditable : String;
	var isContentEditable(default,null) : Bool;
	var contextMenu(default,null) : MenuElement;
	var spellcheck : Bool;
	var style(default,null) : CSSStyleDeclaration;
	var oncopy : haxe.Constraints.Function;
	var oncut : haxe.Constraints.Function;
	var onpaste : haxe.Constraints.Function;
	var offsetParent(default,null) : Element;
	var offsetTop(default,null) : Int;
	var offsetLeft(default,null) : Int;
	var offsetWidth(default,null) : Int;
	var offsetHeight(default,null) : Int;
	var scrollTop : Int;
	var scrollLeft : Int;
	var scrollWidth(default,null) : Int;
	var scrollHeight(default,null) : Int;
	var clientTop(default,null) : Int;
	var clientLeft(default,null) : Int;
	var clientWidth(default,null) : Int;
	var clientHeight(default,null) : Int;
	var scrollTopMax(default,null) : Int;
	var scrollLeftMax(default,null) : Int;
	var innerHTML : String;
	var outerHTML : String;
	var shadowRoot(default,null) : ShadowRoot;
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
	var onresize : haxe.Constraints.Function;
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
	var onfullscreenchange : haxe.Constraints.Function;
	var onfullscreenerror : haxe.Constraints.Function;
	var onpointerlockchange : haxe.Constraints.Function;
	var onpointerlockerror : haxe.Constraints.Function;
	var previousElementSibling(default,null) : Element;
	var nextElementSibling(default,null) : Element;
	var onerror : haxe.Constraints.Function;
	var children(default,null) : HTMLCollection;
	var firstElementChild(default,null) : Element;
	var lastElementChild(default,null) : Element;
	var childElementCount(default,null) : Int;
	var ontouchstart : haxe.Constraints.Function;
	var ontouchend : haxe.Constraints.Function;
	var ontouchmove : haxe.Constraints.Function;
	var ontouchcancel : haxe.Constraints.Function;
	
	function getAttributeNames() : Array<String>;
	function getAttribute( name : String ) : String;
	function getAttributeNS( namespace_ : String, localName : String ) : String;
	/** @throws DOMError */
	function setAttribute( name : String, value : String ) : Void;
	/** @throws DOMError */
	function setAttributeNS( namespace_ : String, name : String, value : String ) : Void;
	/** @throws DOMError */
	function removeAttribute( name : String ) : Void;
	/** @throws DOMError */
	function removeAttributeNS( namespace_ : String, localName : String ) : Void;
	function hasAttribute( name : String ) : Bool;
	function hasAttributeNS( namespace_ : String, localName : String ) : Bool;
	function hasAttributes() : Bool;
	/** @throws DOMError */
	function closest( selector : String ) : Element;
	/** @throws DOMError */
	function matches( selector : String ) : Bool;
	/** @throws DOMError */
	function webkitMatchesSelector( selector : String ) : Bool;
	function getElementsByTagName( localName : String ) : HTMLCollection;
	/** @throws DOMError */
	function getElementsByTagNameNS( namespace_ : String, localName : String ) : HTMLCollection;
	function getElementsByClassName( classNames : String ) : HTMLCollection;
	/** @throws DOMError */
	
	/**
		Designates a specific element as the capture target of future `PointerEvent`.
	**/
	function setPointerCapture( pointerId : Int ) : Void;
	/** @throws DOMError */
	function releasePointerCapture( pointerId : Int ) : Void;
	function setCapture( ?retargetToElement : Bool = false ) : Void;
	function releaseCapture() : Void;
	function requestPointerLock() : Void;
	function getAttributeNode( name : String ) : Attr;
	/** @throws DOMError */
	function setAttributeNode( newAttr : Attr ) : Attr;
	/** @throws DOMError */
	function removeAttributeNode( oldAttr : Attr ) : Attr;
	function getAttributeNodeNS( namespaceURI : String, localName : String ) : Attr;
	/** @throws DOMError */
	function setAttributeNodeNS( newAttr : Attr ) : Attr;
	function click() : Void;
	/** @throws DOMError */
	function focus() : Void;
	/** @throws DOMError */
	function blur() : Void;
	function getClientRects() : DOMRectList;
	function getBoundingClientRect() : DOMRect;
	@:overload( function( top : Bool ) : Void {} )
	function scrollIntoView( ?options : ScrollIntoViewOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scroll( ?options : ScrollToOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scrollTo( ?options : ScrollToOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scrollBy( ?options : ScrollToOptions ) : Void;
	/** @throws DOMError */
	function insertAdjacentHTML( position : String, text : String ) : Void;
	/** @throws DOMError */
	function querySelector( selectors : String ) : Element;
	/** @throws DOMError */
	function querySelectorAll( selectors : String ) : NodeList;
	/** @throws DOMError */
	function createShadowRoot() : ShadowRoot;
	
	/**
		…
	**/
	function getDestinationInsertionPoints() : NodeList;
	/** @throws DOMError */
	function requestFullscreen( ?options : Dynamic ) : Void;
	/** @throws DOMError */
	
	/**
		A shortcut method to create and run an animation on an element. Returns the created Animation object instance.
	**/
	function animate( frames : Dynamic, ?options : haxe.extern.EitherType<Float,Dynamic/*MISSING KeyframeAnimationOptions*/> ) : Animation;
	
	/**
		Returns an array of Animation objects currently active on the element.
	**/
	function getAnimations() : Array<Animation>;
	function remove() : Void;
	/** @throws DOMError */
	function convertQuadFromNode( quad : DOMQuad, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	function convertRectFromNode( rect : DOMRectReadOnly, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	function convertPointFromNode( point : DOMPointInit, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMPoint;
}