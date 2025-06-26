/*
 * Copyright (C)2005-2019 Haxe Foundation
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
extern class DOMElement extends Node {
	
	/**
		The namespace URI of the element, or `null` if it is no namespace.
		 
		 Note: In Firefox 3.5 and earlier, HTML elements are in no namespace. In later versions, HTML elements are in the `http://www.w3.org/1999/xhtml` namespace in both HTML and XML trees. `1.9.2`
		 
		 
	**/
	var namespaceURI(default,null) : String;
	
	/**
		A `DOMString` representing the namespace prefix of the element, or `null` if no prefix is specified.
	**/
	var prefix(default,null) : String;
	
	/**
		A `DOMString` representing the local part of the qualified name of the element.
	**/
	var localName(default,null) : String;
	
	/**
		Returns a `String` with the name of the tag for the given element.
	**/
	var tagName(default,null) : String;
	
	/**
		Is a `DOMString` representing the id of the element.
	**/
	var id : String;
	
	/**
		Is a `DOMString` representing the class of the element.
	**/
	var className : String;
	
	/**
		Returns a `DOMTokenList` containing the list of class attributes.
	**/
	var classList(default,null) : DOMTokenList;
	
	/**
		Returns a `NamedNodeMap` object containing the assigned attributes of the corresponding HTML element.
	**/
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
	
	/**
		A `Number` representing number of pixels the top of the document is scrolled vertically.
	**/
	var scrollTop : Int;
	
	/**
		Is a `Number` representing the left scroll offset of the element.
	**/
	var scrollLeft : Int;
	
	/**
		Returns a `Number` representing the scroll view width of the element.
	**/
	var scrollWidth(default,null) : Int;
	
	/**
		Returns a `Number` representing the scroll view height of an element.
	**/
	var scrollHeight(default,null) : Int;
	
	/**
		Returns a `Number` representing the width of the top border of the element.
	**/
	var clientTop(default,null) : Int;
	
	/**
		Returns a `Number` representing the width of the left border of the element.
	**/
	var clientLeft(default,null) : Int;
	
	/**
		Returns a `Number` representing the inner width of the element.
	**/
	var clientWidth(default,null) : Int;
	
	/**
		Returns a `Number` representing the inner height of the element.
	**/
	var clientHeight(default,null) : Int;
	
	/**
		Returns a `Number` representing the maximum top scroll offset possible for the element.
	**/
	var scrollTopMax(default,null) : Int;
	
	/**
		Returns a `Number` representing the maximum left scroll offset possible for the element.
	**/
	var scrollLeftMax(default,null) : Int;
	
	/**
		Is a `DOMString` representing the markup of the element's content.
	**/
	var innerHTML : String;
	
	/**
		Is a `DOMString` representing the markup of the element including its content. When used as a setter, replaces the element with nodes parsed from the given string.
	**/
	var outerHTML : String;
	
	/**
		Returns the open shadow root that is hosted by the element, or null if no open shadow root is present.
	**/
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
	
	/**
		Returns the event handling code for the `wheel` event. This is now implemented on `GlobalEventHandlers.onwheel`.
	**/
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
	var onselectstart : haxe.Constraints.Function;
	var ontoggle : haxe.Constraints.Function;
	var onpointercancel : haxe.Constraints.Function;
	var onpointerdown : haxe.Constraints.Function;
	var onpointerup : haxe.Constraints.Function;
	var onpointermove : haxe.Constraints.Function;
	var onpointerout : haxe.Constraints.Function;
	var onpointerover : haxe.Constraints.Function;
	var onpointerenter : haxe.Constraints.Function;
	var onpointerleave : haxe.Constraints.Function;
	
	/**
		Returns the event handler for the `gotpointercapture` event type.
	**/
	var ongotpointercapture : haxe.Constraints.Function;
	
	/**
		Returns the event handler for the `lostpointercapture` event type.
	**/
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
	
	
	/**
		Returns an array of attribute names from the current element.
	**/
	@:pure
	function getAttributeNames() : Array<String>;
	
	/**
		Retrieves the value of the named attribute from the current node and returns it as an `Object`.
	**/
	@:pure
	function getAttribute( name : String ) : String;
	
	/**
		Retrieves the value of the attribute with the specified name and namespace, from the current node and returns it as an `Object`.
	**/
	@:pure
	function getAttributeNS( namespace : String, localName : String ) : String;
	
	/**
		Toggles a boolean attribute, removing it if it is present and adding it if it is not present, on the specified element.
		@throws DOMError
	**/
	function toggleAttribute( name : String, ?force : Bool ) : Bool;
	
	/**
		Sets the value of a named attribute of the current node.
		@throws DOMError
	**/
	function setAttribute( name : String, value : String ) : Void;
	
	/**
		Sets the value of the attribute with the specified name and namespace, from the current node.
		@throws DOMError
	**/
	function setAttributeNS( namespace : String, name : String, value : String ) : Void;
	
	/**
		Removes the named attribute from the current node.
		@throws DOMError
	**/
	function removeAttribute( name : String ) : Void;
	
	/**
		Removes the attribute with the specified name and namespace, from the current node.
		@throws DOMError
	**/
	function removeAttributeNS( namespace : String, localName : String ) : Void;
	
	/**
		Returns a `Boolean` indicating if the element has the specified attribute or not.
	**/
	@:pure
	function hasAttribute( name : String ) : Bool;
	
	/**
		Returns a `Boolean` indicating if the element has the specified attribute, in the specified namespace, or not.
	**/
	@:pure
	function hasAttributeNS( namespace : String, localName : String ) : Bool;
	
	/**
		Returns a `Boolean` indicating if the element has one or more HTML attributes present.
	**/
	@:pure
	function hasAttributes() : Bool;
	
	/**
		Returns the `Element` which is the closest ancestor of the current element (or the current element itself) which matches the selectors given in parameter.
		@throws DOMError
	**/
	@:pure
	function closest( selector : String ) : Element;
	
	/**
		Returns a `Boolean` indicating whether or not the element would be selected by the specified selector string.
		@throws DOMError
	**/
	@:pure
	function matches( selector : String ) : Bool;
	/** @throws DOMError */
	@:pure
	function webkitMatchesSelector( selector : String ) : Bool;
	
	/**
		Returns a live `HTMLCollection` containing all descendant elements, of a particular tag name, from the current element.
	**/
	@:pure
	function getElementsByTagName( localName : String ) : HTMLCollection;
	
	/**
		Returns a live `HTMLCollection` containing all descendant elements, of a particular tag name and namespace, from the current element.
		@throws DOMError
	**/
	@:pure
	function getElementsByTagNameNS( namespace : String, localName : String ) : HTMLCollection;
	
	/**
		Returns a live `HTMLCollection` that contains all descendants of the current element that possess the list of classes given in the parameter.
	**/
	@:pure
	function getElementsByClassName( classNames : String ) : HTMLCollection;
	
	/**
		Inserts a given element node at a given position relative to the element it is invoked upon.
		@throws DOMError
	**/
	function insertAdjacentElement( where : String, element : Element ) : Element;
	
	/**
		Inserts a given text node at a given position relative to the element it is invoked upon.
		@throws DOMError
	**/
	function insertAdjacentText( where : String, data : String ) : Void;
	
	/**
		Designates a specific element as the capture target of future pointer events.
		@throws DOMError
	**/
	function setPointerCapture( pointerId : Int ) : Void;
	
	/**
		Releases (stops) pointer capture that was previously set for a specific `PointerEvent`.
		@throws DOMError
	**/
	function releasePointerCapture( pointerId : Int ) : Void;
	function hasPointerCapture( pointerId : Int ) : Bool;
	
	/**
		Sets up mouse event capture, redirecting all mouse events to this element.
	**/
	function setCapture( retargetToElement : Bool = false ) : Void;
	function releaseCapture() : Void;
	
	/**
		Retrieves the node representation of the named attribute from the current node and returns it as an `Attr`.
	**/
	function getAttributeNode( name : String ) : Attr;
	
	/**
		Sets the node representation of the named attribute from the current node.
		@throws DOMError
	**/
	function setAttributeNode( newAttr : Attr ) : Attr;
	
	/**
		Removes the node representation of the named attribute from the current node.
		@throws DOMError
	**/
	function removeAttributeNode( oldAttr : Attr ) : Attr;
	
	/**
		Retrieves the node representation of the attribute with the specified name and namespace, from the current node and returns it as an `Attr`.
	**/
	function getAttributeNodeNS( namespaceURI : String, localName : String ) : Attr;
	
	/**
		Sets the node representation of the attribute with the specified name and namespace, from the current node.
		@throws DOMError
	**/
	function setAttributeNodeNS( newAttr : Attr ) : Attr;
	function click() : Void;
	/** @throws DOMError */
	function focus() : Void;
	/** @throws DOMError */
	function blur() : Void;
	
	/**
		Returns a collection of rectangles that indicate the bounding rectangles for each line of text in a client.
	**/
	function getClientRects() : DOMRectList;
	
	/**
		Returns the size of an element and its position relative to the viewport.
	**/
	function getBoundingClientRect() : DOMRect;
	
	/**
		Scrolls the page until the element gets into the view.
	**/
	@:overload( function( ?arg : ScrollIntoViewOptions) : Void {} )
	function scrollIntoView( ?arg : Bool ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scroll( ?options : ScrollToOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scrollTo( ?options : ScrollToOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scrollBy( ?options : ScrollToOptions ) : Void;
	
	/**
		Parses the text as HTML or XML and inserts the resulting nodes into the tree in the position given.
		@throws DOMError
	**/
	function insertAdjacentHTML( position : String, text : String ) : Void;
	
	/**
		Returns the first `Node` which matches the specified selector string relative to the element.
		@throws DOMError
	**/
	@:pure
	function querySelector( selectors : String ) : Element;
	
	/**
		Returns a `NodeList` of nodes which match the specified selector string relative to the element.
		@throws DOMError
	**/
	@:pure
	function querySelectorAll( selectors : String ) : NodeList;
	
	/**
		Attatches a shadow DOM tree to the specified element and returns a reference to its `ShadowRoot`.
		@throws DOMError
	**/
	function attachShadow( shadowRootInitDict : ShadowRootInit ) : ShadowRoot;
	
	/**
		Asynchronously asks the browser to make the element full-screen.
		@throws DOMError
	**/
	function requestFullscreen(?options: FullscreenOptions) : js.lib.Promise<Void>;

	/**
		Allows to asynchronously ask for the pointer to be locked on the given element.
	**/
	function requestPointerLock() : Void;
	
	/**
		A shortcut method to create and run an animation on an element. Returns the created Animation object instance.
		@throws DOMError
	**/
	@:overload( function( keyframes : Dynamic, ?options : KeyframeAnimationOptions) : Animation {} )
	function animate( keyframes : Dynamic, ?options : Float ) : Animation;
	
	/**
		Returns an array of Animation objects currently active on the element.
	**/
	function getAnimations( ?filter : AnimationFilter ) : Array<Animation>;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function before( nodes : haxe.extern.Rest<Node> ) : Void;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function after( nodes : haxe.extern.Rest<Node> ) : Void;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function replaceWith( nodes : haxe.extern.Rest<Node> ) : Void;
	function remove() : Void;
	/** @throws DOMError */
	@:overload( function( quad : DOMQuad, from : Element, ?options : ConvertCoordinateOptions) : DOMQuad {} )
	@:overload( function( quad : DOMQuad, from : HTMLDocument, ?options : ConvertCoordinateOptions) : DOMQuad {} )
	function convertQuadFromNode( quad : DOMQuad, from : Text, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	@:overload( function( rect : DOMRectReadOnly, from : Element, ?options : ConvertCoordinateOptions) : DOMQuad {} )
	@:overload( function( rect : DOMRectReadOnly, from : HTMLDocument, ?options : ConvertCoordinateOptions) : DOMQuad {} )
	function convertRectFromNode( rect : DOMRectReadOnly, from : Text, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	@:overload( function( point : DOMPointInit, from : Element, ?options : ConvertCoordinateOptions) : DOMPoint {} )
	@:overload( function( point : DOMPointInit, from : HTMLDocument, ?options : ConvertCoordinateOptions) : DOMPoint {} )
	function convertPointFromNode( point : DOMPointInit, from : Text, ?options : ConvertCoordinateOptions ) : DOMPoint;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function prepend( nodes : haxe.extern.Rest<Node> ) : Void;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function append( nodes : haxe.extern.Rest<Node> ) : Void;
}
