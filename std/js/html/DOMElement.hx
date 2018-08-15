/*
 * Copyright (C)2005-2018 Haxe Foundation
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
	`Element` is the most general base class from which all objects in a `Document` inherit. It only has methods and properties common to all kinds of elements. More specific classes inherit from `Element`.

	Documentation [Element](https://developer.mozilla.org/en-US/docs/Web/API/Element) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Element$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Element>
**/
@:native("Element")
extern class DOMElement extends Node
{
	
	/**
		The namespace URI of the element, or `null` if it is no namespace.
		 
		 Note: In Firefox 3.5 and earlier, HTML elements are in no namespace. In later versions, HTML elements are in the `http://www.w3.org/1999/xhtml` namespace in both HTML and XML trees. `1.9.2`
		 
		 
	**/
	var namespaceURI(default,null) : String;
	var prefix(default,null) : String;
	var localName(default,null) : String;
	var tagName(default,null) : String;
	var id : String;
	var className : String;
	var classList(default,null) : DOMTokenList;
	var attributes(default,null) : NamedNodeMap;
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
	var itemValue : Any;
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
	var assignedSlot(default,null) : SlotElement;
	
	/**
		Returns the name of the shadow DOM slot the element is inserted in.
	**/
	var slot : String;
	var onabort : haxe.Constraints.Function;
	var onblur : haxe.Constraints.Function;
	var onfocus : haxe.Constraints.Function;
	var onauxclick : haxe.Constraints.Function;
	var oncanplay : haxe.Constraints.Function;
	var oncanplaythrough : haxe.Constraints.Function;
	var onchange : haxe.Constraints.Function;
	var onclick : haxe.Constraints.Function;
	var onclose : haxe.Constraints.Function;
	var oncontextmenu : haxe.Constraints.Function;
	var ondblclick : haxe.Constraints.Function;
	var ondrag : haxe.Constraints.Function;
	var ondragend : haxe.Constraints.Function;
	var ondragenter : haxe.Constraints.Function;
	var ondragexit : haxe.Constraints.Function;
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
	var onloadend : haxe.Constraints.Function;
	var onloadstart : haxe.Constraints.Function;
	var onmousedown : haxe.Constraints.Function;
	var onmouseenter : haxe.Constraints.Function;
	var onmouseleave : haxe.Constraints.Function;
	var onmousemove : haxe.Constraints.Function;
	var onmouseout : haxe.Constraints.Function;
	var onmouseover : haxe.Constraints.Function;
	var onmouseup : haxe.Constraints.Function;
	var onwheel : haxe.Constraints.Function;
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
	var ontoggle : haxe.Constraints.Function;
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
	var onanimationcancel : haxe.Constraints.Function;
	var onanimationend : haxe.Constraints.Function;
	var onanimationiteration : haxe.Constraints.Function;
	var onanimationstart : haxe.Constraints.Function;
	var ontransitioncancel : haxe.Constraints.Function;
	var ontransitionend : haxe.Constraints.Function;
	var ontransitionrun : haxe.Constraints.Function;
	var ontransitionstart : haxe.Constraints.Function;
	var onwebkitanimationend : haxe.Constraints.Function;
	var onwebkitanimationiteration : haxe.Constraints.Function;
	var onwebkitanimationstart : haxe.Constraints.Function;
	var onwebkittransitionend : haxe.Constraints.Function;
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
	
	@:pure function getAttributeNames() : Array<String>;
	@:pure function getAttribute( name : String ) : String;
	@:pure function getAttributeNS( namespace_ : String, localName : String ) : String;
	
	/**
		Toggles a boolean attribute, removing it if it is present and adding it if it is not present, on the specified element.
		@throws DOMError
	**/
	function toggleAttribute( name : String, ?force : Bool ) : Bool;
	/** @throws DOMError */
	function setAttribute( name : String, value : String ) : Void;
	/** @throws DOMError */
	function setAttributeNS( namespace_ : String, name : String, value : String ) : Void;
	/** @throws DOMError */
	function removeAttribute( name : String ) : Void;
	/** @throws DOMError */
	function removeAttributeNS( namespace_ : String, localName : String ) : Void;
	@:pure function hasAttribute( name : String ) : Bool;
	@:pure function hasAttributeNS( namespace_ : String, localName : String ) : Bool;
	@:pure function hasAttributes() : Bool;
	/** @throws DOMError */
	@:pure function closest( selector : String ) : Element;
	/** @throws DOMError */
	@:pure function matches( selector : String ) : Bool;
	/** @throws DOMError */
	@:pure function webkitMatchesSelector( selector : String ) : Bool;
	@:pure function getElementsByTagName( localName : String ) : HTMLCollection;
	/** @throws DOMError */
	@:pure function getElementsByTagNameNS( namespace_ : String, localName : String ) : HTMLCollection;
	@:pure function getElementsByClassName( classNames : String ) : HTMLCollection;
	/** @throws DOMError */
	@:pure function insertAdjacentElement( where : String, element : Element ) : Element;
	/** @throws DOMError */
	function insertAdjacentText( where : String, data : String ) : Void;
	
	/**
		Designates a specific element as the capture target of future pointer events.
		@throws DOMError
	**/
	function setPointerCapture( pointerId : Int ) : Void;
	/** @throws DOMError */
	function releasePointerCapture( pointerId : Int ) : Void;
	function hasPointerCapture( pointerId : Int ) : Bool;
	function setCapture( ?retargetToElement : Bool = false ) : Void;
	function releaseCapture() : Void;
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
	function scrollIntoView( ?arg : haxe.extern.EitherType<Bool,ScrollIntoViewOptions> ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scroll( ?options : ScrollToOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scrollTo( ?options : ScrollToOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scrollBy( ?options : ScrollToOptions ) : Void;
	/** @throws DOMError */
	function insertAdjacentHTML( position : String, text : String ) : Void;
	/** @throws DOMError */
	@:pure function querySelector( selectors : String ) : Element;
	/** @throws DOMError */
	@:pure function querySelectorAll( selectors : String ) : NodeList;
	
	/**
		Attatches a shadow DOM tree to the specified element and returns a reference to its `ShadowRoot`.
		@throws DOMError
	**/
	function attachShadow( shadowRootInitDict : ShadowRootInit ) : ShadowRoot;
	function requestPointerLock() : Void;
	
	/**
		A shortcut method to create and run an animation on an element. Returns the created Animation object instance.
		@throws DOMError
	**/
	function animate( keyframes : Any, ?options : haxe.extern.EitherType<Float,KeyframeAnimationOptions> ) : Animation;
	
	/**
		Returns an array of Animation objects currently active on the element.
	**/
	function getAnimations( ?filter : AnimationFilter ) : Array<Animation>;
	/** @throws DOMError */
	function before( nodes : haxe.extern.Rest<haxe.extern.EitherType<Node,String>> ) : Void;
	/** @throws DOMError */
	function after( nodes : haxe.extern.Rest<haxe.extern.EitherType<Node,String>> ) : Void;
	/** @throws DOMError */
	function replaceWith( nodes : haxe.extern.Rest<haxe.extern.EitherType<Node,String>> ) : Void;
	function remove() : Void;
	/** @throws DOMError */
	function convertQuadFromNode( quad : DOMQuad, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	function convertRectFromNode( rect : DOMRectReadOnly, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	function convertPointFromNode( point : DOMPointInit, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMPoint;
	/** @throws DOMError */
	function prepend( nodes : haxe.extern.Rest<haxe.extern.EitherType<Node,String>> ) : Void;
	/** @throws DOMError */
	function append( nodes : haxe.extern.Rest<haxe.extern.EitherType<Node,String>> ) : Void;
}