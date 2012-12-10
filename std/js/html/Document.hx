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

// This file is generated, do not edit!
package js.html;

/** <p>Each web page loaded in the browser has its own <strong>document</strong> object. This object serves as an entry point to the web page's content (the <a title="en/Using_the_W3C_DOM_Level_1_Core" rel="internal" href="https://developer.mozilla.org/en/Using_the_W3C_DOM_Level_1_Core">DOM tree</a>, including elements such as <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/body">&lt;body&gt;</a></code>
 and <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/table">&lt;table&gt;</a></code>
) and provides functionality global to the document (such as obtaining the page's URL and creating new elements in the document).</p>
<p>A document object can be obtained from various APIs:</p>
<ul> <li>Most commonly, you work with the document the script is running in by using <code>document</code> in document's <a title="en/HTML/Element/Script" rel="internal" href="https://developer.mozilla.org/En/HTML/Element/Script">scripts</a>. (The same document can also be referred to as <a title="window.document" rel="internal" href="https://developer.mozilla.org/en/DOM/window.document"><code>window.document</code></a>.)</li> <li>The document of an iframe via the iframe's <code><a title="en/DOM/HTMLIFrameElement#Properties" rel="internal" href="https://developer.mozilla.org/en/DOM/HTMLIFrameElement#Properties">contentDocument</a></code> property.</li> <li>The <a title="en/XMLHttpRequest#Attributes" rel="internal" href="https://developer.mozilla.org/en/nsIXMLHttpRequest#Attributes"><code>responseXML</code> of an <code>XMLHttpRequest</code> object</a>.</li> <li>The document, that given node or element belongs to, can be retrieved using the node's <code><a title="en/DOM/Node.ownerDocument" rel="internal" href="https://developer.mozilla.org/En/DOM/Node.ownerDocument">ownerDocument</a></code> property.</li> <li>...and more.</li>
</ul>
<p>Depending on the kind of the document (e.g. <a title="en/HTML" rel="internal" href="https://developer.mozilla.org/en/HTML">HTML</a> or <a title="en/XML" rel="internal" href="https://developer.mozilla.org/en/XML">XML</a>) different APIs may be available on the document object. This theoretical availability of APIs is usually described in terms of <em>implementing interfaces</em> defined in the relevant W3C DOM specifications:</p>
<ul> <li>All document objects implement the DOM Core <a class="external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-Core/core.html#i-Document" title="http://www.w3.org/TR/DOM-Level-2-Core/core.html#i-Document" target="_blank"><code>Document</code></a> and <code><a title="en/DOM/Node" rel="internal" href="https://developer.mozilla.org/en/DOM/Node">Node</a></code> interfaces, meaning that the "core" properties and methods are available for all kinds of documents.</li> <li>In addition to the generalized DOM Core document interface, HTML documents also implement the <code><a class="external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-26809268" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-26809268" target="_blank">HTMLDocument</a></code> interface, which is a more specialized interface for dealing with HTML documents (e.g., <a title="en/DOM/document.cookie" rel="internal" href="https://developer.mozilla.org/en/DOM/document.cookie">document.cookie</a>, <a title="en/DOM/document.alinkColor" rel="internal" href="https://developer.mozilla.org/en/DOM/document.alinkColor">document.alinkColor</a>).</li> <li><a title="en/XUL" rel="internal" href="https://developer.mozilla.org/en/XUL">XUL</a> documents (available to Mozilla add-on and application developers) implement their own additions to the core Document functionality.</li>
</ul>
<p>Methods or properties listed here that are part of a more specialized interface have an asterisk (*) next to them and have additional information in the&nbsp; Availability column.</p>
<p>Note that some APIs listed below are not available in all browsers for various reasons:</p>
<ul> <li><strong>Obsolete</strong>: on its way of being removed from supporting browsers.</li> <li><strong>Non-standard</strong>: either an experimental feature not (yet?) agreed upon by all vendors, or a feature targeted specifically at the code running in a specific browser (e.g. Mozilla has a few DOM APIs created for its add-ons and application development).</li> <li>Part of a completed or an emerging standard, but not (yet?) implemented in all browsers or implemented in the newest versions of the browsers.</li>
</ul>
<p>Detailed browser compatibility tables are located at the pages describing each property or method.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/document">MDN</a>. */
@:native("Document")
extern class Document extends Node
{
    /** Returns a string containing the URL of the current document. */
    var URL (default,null) :String;

    /** Returns the currently focused element */
    var activeElement (default,null) :Element;

    /** Returns or sets the color of active links in the document body. */
    var alinkColor :String;

    var all :HTMLAllCollection;

    /** Returns a list of all of the anchors in the document. */
    var anchors (default,null) :HTMLCollection;

    /** Returns an ordered list of the applets within a document. */
    var applets (default,null) :HTMLCollection;

    /** Gets/sets the background color of the current document. */
    var bgColor :String;

    /** Returns the BODY node of the current document. Setter throws DOMException. */
    var body :Element;

    /** Returns the character set being used by the document. */
    var characterSet (default,null) :String;

    var charset :String;

    /** Indicates whether the document is rendered in Quirks or Strict mode. */
    var compatMode (default,null) :String;

    /** Returns a semicolon-separated list of the cookies for that document or sets a single cookie. Getter throws DOMException. Setter throws DOMException. */
    var cookie :String;

    var currentFullScreenElement (default,null) :Element;

    var defaultCharset (default,null) :String;

    /** Returns a reference to the window object. */
    var defaultView (default,null) :DOMWindow;

    /** Gets/sets WYSYWIG editing capability of <a title="en/Midas" rel="internal" href="https://developer.mozilla.org/en/Midas">Midas</a>. It can only be used for HTML documents. */
    var designMode :String;

    /** Gets/sets directionality (rtl/ltr) of the document */
    var dir :String;

    /** Returns the Document Type Definition (DTD) of the current document. */
    var doctype (default,null) :DocumentType;

    /** Returns the Element that is a direct child of document. For HTML documents, this is normally the HTML element. */
    var documentElement (default,null) :Element;

    /** Returns the document location. */
    var documentURI (default,null) :String;

    /** Returns the domain of the current document. Setter throws DOMException. */
    var domain :String;

    /** Returns a list of the embedded OBJECTS within the current document. */
    var embeds (default,null) :HTMLCollection;

    /** Gets/sets the foreground color, or text color, of the current document. */
    var fgColor :String;

    /** Returns a list of the FORM elements within the current document. */
    var forms (default,null) :HTMLCollection;

    var fullScreenKeyboardInputAllowed (default,null) :Bool;

    var fullscreenElement (default,null) :Element;

    var fullscreenEnabled (default,null) :Bool;

    /** Returns the HEAD node of the current document. */
    var head (default,null) :HeadElement;

    /** Gets/sets the height of the current document. */
    var height (default,null) :Int;

    var hidden (default,null) :Bool;

    /** Returns a list of the images in the current document. */
    var images (default,null) :HTMLCollection;

    /** Returns the DOM implementation associated with the current document. */
    var implementation (default,null) :DOMImplementation;

    /** Returns the encoding used when the document was parsed. */
    var inputEncoding (default,null) :String;

    var isFullScreen (default,null) :Bool;

    /** Returns the date on which the document was last modified. */
    var lastModified (default,null) :String;

    /** Gets/sets the color of hyperlinks in the document. */
    var linkColor :String;

    /** Returns a list of all the hyperlinks in the document. */
    var links (default,null) :HTMLCollection;

    /** Returns the URI of the current document. */
    var location :Location;

    var onabort :EventListener;

    var onbeforecopy :EventListener;

    var onbeforecut :EventListener;

    var onbeforepaste :EventListener;

    var onblur :EventListener;

    var onchange :EventListener;

    var onclick :EventListener;

    var oncontextmenu :EventListener;

    var oncopy :EventListener;

    var oncut :EventListener;

    var ondblclick :EventListener;

    var ondrag :EventListener;

    var ondragend :EventListener;

    var ondragenter :EventListener;

    var ondragleave :EventListener;

    var ondragover :EventListener;

    var ondragstart :EventListener;

    var ondrop :EventListener;

    var onerror :EventListener;

    var onfocus :EventListener;

    var onfullscreenchange :EventListener;

    var onfullscreenerror :EventListener;

    var oninput :EventListener;

    var oninvalid :EventListener;

    var onkeydown :EventListener;

    var onkeypress :EventListener;

    var onkeyup :EventListener;

    var onload :EventListener;

    var onmousedown :EventListener;

    var onmousemove :EventListener;

    var onmouseout :EventListener;

    var onmouseover :EventListener;

    var onmouseup :EventListener;

    var onmousewheel :EventListener;

    var onpaste :EventListener;

    var onpointerlockchange :EventListener;

    var onpointerlockerror :EventListener;

    /** <dl><dd>Returns the event handling code for the <code>readystatechange</code> event.</dd>
</dl>
<div class="geckoVersionNote"> <p>
</p><div class="geckoVersionHeading">Gecko 9.0 note<div>(Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
</div></div>
<p></p> <p>Starting in Gecko 9.0 (Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
, you can now use the syntax <code>if ("onabort" in document)</code> to determine whether or not a given event handler property exists. This is because event handler interfaces have been updated to be proper web IDL interfaces. See <a title="en/DOM/DOM event handlers" rel="internal" href="https://developer.mozilla.org/en/DOM/DOM_event_handlers">DOM event handlers</a> for details.</p>
</div> */
    var onreadystatechange :EventListener;

    var onreset :EventListener;

    var onscroll :EventListener;

    var onsearch :EventListener;

    var onselect :EventListener;

    var onselectionchange :EventListener;

    var onselectstart :EventListener;

    var onsubmit :EventListener;

    var ontouchcancel :EventListener;

    var ontouchend :EventListener;

    var ontouchmove :EventListener;

    var ontouchstart :EventListener;

    /** Returns a list of the available plugins. */
    var plugins (default,null) :HTMLCollection;

    var pointerLockElement (default,null) :Element;

    var preferredStylesheetSet (default,null) :String;

    /** Returns loading status of the document */
    var readyState (default,null) :String;

    /** Returns the URI of the page that linked to this page. */
    var referrer (default,null) :String;

    /** Returns all the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/script">&lt;script&gt;</a></code>
 elements on the document. */
    var scripts (default,null) :HTMLCollection;

    var selectedStylesheetSet :String;

    /** Returns a list of the stylesheet objects on the current document. */
    var styleSheets (default,null) :StyleSheetList;

    /** Returns the title of the current document. */
    var title :String;

    var visibilityState (default,null) :String;

    /** Gets/sets the color of visited hyperlinks. */
    var vlinkColor :String;

    /** Returns the width of the current document. */
    var width (default,null) :Int;

    /** Returns the encoding as determined by the XML declaration.<br> <div class="note">Firefox 10 and later don't implement it anymore.</div> */
    var xmlEncoding (default,null) :String;

    /** Returns <code>true</code> if the XML declaration specifies the document is standalone (<em>e.g.,</em> An external part of the DTD affects the document's content), else <code>false</code>. Setter throws DOMException. */
    var xmlStandalone :Bool;

    /** Returns the version number as specified in the XML declaration or <code>"1.0"</code> if the declaration is absent. Setter throws DOMException. */
    var xmlVersion :String;

    function adoptNode (source :Node) :Node;

    function cancelFullScreen () :Void;

    function captureEvents () :Void;

    function caretRangeFromPoint (x :Int, y :Int) :Range;

    function clear () :Void;

    function close () :Void;

    function createAttribute (name :String) :Attr;

    function createAttributeNS (namespaceURI :String, qualifiedName :String) :Attr;

    function createCDATASection (data :String) :CDATASection;

    function createComment (data :String) :Comment;

    function createDocumentFragment () :DocumentFragment;

    function createElement (tagName :String) :Element;

    function createElementNS (namespaceURI :String, qualifiedName :String) :Element;

    function createEntityReference (name :String) :EntityReference;

    function createEvent (eventType :String) :Event;

    function createExpression (expression :String, resolver :XPathNSResolver) :XPathExpression;

    function createNSResolver (nodeResolver :Node) :XPathNSResolver;

    function createNodeIterator (root :Node, whatToShow :Int, filter :NodeFilter, expandEntityReferences :Bool) :NodeIterator;

    function createProcessingInstruction (target :String, data :String) :ProcessingInstruction;

    function createRange () :Range;

    function createTextNode (data :String) :Text;

    function createTouch (window :DOMWindow, target :EventTarget, identifier :Int, pageX :Int, pageY :Int, screenX :Int, screenY :Int, radiusX :Int, radiusY :Int, rotationAngle :Float, force :Float) :Touch;

    function createTouchList () :TouchList;

    function createTreeWalker (root :Node, whatToShow :Int, filter :NodeFilter, expandEntityReferences :Bool) :TreeWalker;

    function elementFromPoint (x :Int, y :Int) :Element;

    function evaluate (expression :String, contextNode :Node, resolver :XPathNSResolver, type :Int, inResult :XPathResult) :XPathResult;

    function execCommand (command :String, userInterface :Bool, value :String) :Bool;

    function exitFullscreen () :Void;

    function exitPointerLock () :Void;

    function getCSSCanvasContext (contextId :String, name :String, width :Int, height :Int) :CanvasRenderingContext;

    function getElementById (elementId :String) :Element;

    function getElementsByClassName (tagname :String) :NodeList;

    function getElementsByName (elementName :String) :NodeList;

    function getElementsByTagName (tagname :String) :NodeList;

    function getElementsByTagNameNS (namespaceURI :String, localName :String) :NodeList;

    function getOverrideStyle (element :Element, pseudoElement :String) :CSSStyleDeclaration;

    function getSelection () :DOMSelection;

    function hasFocus () :Bool;

    function importNode (importedNode :Node, ?deep :Bool) :Node;

    function open () :Void;

    function queryCommandEnabled (command :String) :Bool;

    function queryCommandIndeterm (command :String) :Bool;

    function queryCommandState (command :String) :Bool;

    function queryCommandSupported (command :String) :Bool;

    function queryCommandValue (command :String) :String;

    function querySelector (selectors :String) :Element;

    function querySelectorAll (selectors :String) :NodeList;

    function releaseEvents () :Void;

    function write (text :String) :Void;

    function writeln (text :String) :Void;

    /** A typed shortcut for <code>createElement("td")</code>. */
    public inline function createTableCellElement () :TableCellElement { return cast createElement("td"); }
    /** A typed shortcut for <code>createElement("hr")</code>. */
    public inline function createHRElement () :HRElement { return cast createElement("hr"); }
    /** A typed shortcut for <code>createElement("marquee")</code>. */
    public inline function createMarqueeElement () :MarqueeElement { return cast createElement("marquee"); }
    /** A typed shortcut for <code>createElement("basefont")</code>. */
    public inline function createBaseFontElement () :BaseFontElement { return cast createElement("basefont"); }
    /** A typed shortcut for <code>createElement("select")</code>. */
    public inline function createSelectElement () :SelectElement { return cast createElement("select"); }
    /** A typed shortcut for <code>createElement("map")</code>. */
    public inline function createMapElement () :MapElement { return cast createElement("map"); }
    /** A typed shortcut for <code>createElement("form")</code>. */
    public inline function createFormElement () :FormElement { return cast createElement("form"); }
    /** A typed shortcut for <code>createElement("option")</code>. */
    public inline function createOptionElement () :OptionElement { return cast createElement("option"); }
    /** A typed shortcut for <code>createElement("label")</code>. */
    public inline function createLabelElement () :LabelElement { return cast createElement("label"); }
    /** A typed shortcut for <code>createElement("meta")</code>. */
    public inline function createMetaElement () :MetaElement { return cast createElement("meta"); }
    /** A typed shortcut for <code>createElement("img")</code>. */
    public inline function createImageElement () :ImageElement { return cast createElement("img"); }
    /** A typed shortcut for <code>createElement("dl")</code>. */
    public inline function createDListElement () :DListElement { return cast createElement("dl"); }
    /** A typed shortcut for <code>createElement("frame")</code>. */
    public inline function createFrameElement () :FrameElement { return cast createElement("frame"); }
    /** A typed shortcut for <code>createElement("mod")</code>. */
    public inline function createModElement () :ModElement { return cast createElement("mod"); }
    /** A typed shortcut for <code>createElement("ul")</code>. */
    public inline function createUListElement () :UListElement { return cast createElement("ul"); }
    /** A typed shortcut for <code>createElement("output")</code>. */
    public inline function createOutputElement () :OutputElement { return cast createElement("output"); }
    /** A typed shortcut for <code>createElement("ol")</code>. */
    public inline function createOListElement () :OListElement { return cast createElement("ol"); }
    /** A typed shortcut for <code>createElement("shadow")</code>. */
    public inline function createShadowElement () :ShadowElement { return cast createElement("shadow"); }
    /** A typed shortcut for <code>createElement("li")</code>. */
    public inline function createLIElement () :LIElement { return cast createElement("li"); }
    /** A typed shortcut for <code>createElement("datalist")</code>. */
    public inline function createDataListElement () :DataListElement { return cast createElement("datalist"); }
    /** A typed shortcut for <code>createElement("param")</code>. */
    public inline function createParamElement () :ParamElement { return cast createElement("param"); }
    /** A typed shortcut for <code>createElement("font")</code>. */
    public inline function createFontElement () :FontElement { return cast createElement("font"); }
    /** A typed shortcut for <code>createElement("track")</code>. */
    public inline function createTrackElement () :TrackElement { return cast createElement("track"); }
    /** A typed shortcut for <code>createElement("applet")</code>. */
    public inline function createAppletElement () :AppletElement { return cast createElement("applet"); }
    /** A typed shortcut for <code>createElement("area")</code>. */
    public inline function createAreaElement () :AreaElement { return cast createElement("area"); }
    /** A typed shortcut for <code>createElement("link")</code>. */
    public inline function createLinkElement () :LinkElement { return cast createElement("link"); }
    /** A typed shortcut for <code>createElement("div")</code>. */
    public inline function createDivElement () :DivElement { return cast createElement("div"); }
    /** A typed shortcut for <code>createElement("title")</code>. */
    public inline function createTitleElement () :TitleElement { return cast createElement("title"); }
    /** A typed shortcut for <code>createElement("style")</code>. */
    public inline function createStyleElement () :StyleElement { return cast createElement("style"); }
    /** A typed shortcut for <code>createElement("progress")</code>. */
    public inline function createProgressElement () :ProgressElement { return cast createElement("progress"); }
    /** A typed shortcut for <code>createElement("button")</code>. */
    public inline function createButtonElement () :ButtonElement { return cast createElement("button"); }
    /** A typed shortcut for <code>createElement("fieldset")</code>. */
    public inline function createFieldSetElement () :FieldSetElement { return cast createElement("fieldset"); }
    /** A typed shortcut for <code>createElement("a")</code>. */
    public inline function createAnchorElement () :AnchorElement { return cast createElement("a"); }
    /** A typed shortcut for <code>createElement("iframe")</code>. */
    public inline function createIFrameElement () :IFrameElement { return cast createElement("iframe"); }
    /** A typed shortcut for <code>createElement("span")</code>. */
    public inline function createSpanElement () :SpanElement { return cast createElement("span"); }
    /** A typed shortcut for <code>createElement("details")</code>. */
    public inline function createDetailsElement () :DetailsElement { return cast createElement("details"); }
    /** A typed shortcut for <code>createElement("body")</code>. */
    public inline function createBodyElement () :BodyElement { return cast createElement("body"); }
    /** A typed shortcut for <code>createElement("input")</code>. */
    public inline function createInputElement () :InputElement { return cast createElement("input"); }
    /** A typed shortcut for <code>createElement("embed")</code>. */
    public inline function createEmbedElement () :EmbedElement { return cast createElement("embed"); }
    /** A typed shortcut for <code>createElement("meter")</code>. */
    public inline function createMeterElement () :MeterElement { return cast createElement("meter"); }
    /** A typed shortcut for <code>createElement("pre")</code>. */
    public inline function createPreElement () :PreElement { return cast createElement("pre"); }
    /** A typed shortcut for <code>createElement("thead")</code>. */
    public inline function createTableSectionElement () :TableSectionElement { return cast createElement("thead"); }
    /** A typed shortcut for <code>createElement("head")</code>. */
    public inline function createHeadElement () :HeadElement { return cast createElement("head"); }
    /** A typed shortcut for <code>createElement("base")</code>. */
    public inline function createBaseElement () :BaseElement { return cast createElement("base"); }
    /** A typed shortcut for <code>createElement("optgroup")</code>. */
    public inline function createOptGroupElement () :OptGroupElement { return cast createElement("optgroup"); }
    /** A typed shortcut for <code>createElement("quote")</code>. */
    public inline function createQuoteElement () :QuoteElement { return cast createElement("quote"); }
    /** A typed shortcut for <code>createElement("audio")</code>. */
    public inline function createAudioElement () :AudioElement { return cast createElement("audio"); }
    /** A typed shortcut for <code>createElement("video")</code>. */
    public inline function createVideoElement () :VideoElement { return cast createElement("video"); }
    /** A typed shortcut for <code>createElement("legend")</code>. */
    public inline function createLegendElement () :LegendElement { return cast createElement("legend"); }
    /** A typed shortcut for <code>createElement("menu")</code>. */
    public inline function createMenuElement () :MenuElement { return cast createElement("menu"); }
    /** A typed shortcut for <code>createElement("frameset")</code>. */
    public inline function createFrameSetElement () :FrameSetElement { return cast createElement("frameset"); }
    /** A typed shortcut for <code>createElement("canvas")</code>. */
    public inline function createCanvasElement () :CanvasElement { return cast createElement("canvas"); }
    /** A typed shortcut for <code>createElement("keygen")</code>. */
    public inline function createKeygenElement () :KeygenElement { return cast createElement("keygen"); }
    /** A typed shortcut for <code>createElement("col")</code>. */
    public inline function createTableColElement () :TableColElement { return cast createElement("col"); }
    /** A typed shortcut for <code>createElement("dir")</code>. */
    public inline function createDirectoryElement () :DirectoryElement { return cast createElement("dir"); }
    /** A typed shortcut for <code>createElement("table")</code>. */
    public inline function createTableElement () :TableElement { return cast createElement("table"); }
    /** A typed shortcut for <code>createElement("tr")</code>. */
    public inline function createTableRowElement () :TableRowElement { return cast createElement("tr"); }
    /** A typed shortcut for <code>createElement("script")</code>. */
    public inline function createScriptElement () :ScriptElement { return cast createElement("script"); }
    /** A typed shortcut for <code>createElement("source")</code>. */
    public inline function createSourceElement () :SourceElement { return cast createElement("source"); }
    /** A typed shortcut for <code>createElement("p")</code>. */
    public inline function createParagraphElement () :ParagraphElement { return cast createElement("p"); }
    /** A typed shortcut for <code>createElement("content")</code>. */
    public inline function createContentElement () :ContentElement { return cast createElement("content"); }
    /** A typed shortcut for <code>createElement("br")</code>. */
    public inline function createBRElement () :BRElement { return cast createElement("br"); }
    /** A typed shortcut for <code>createElement("html")</code>. */
    public inline function createHtmlElement () :HtmlElement { return cast createElement("html"); }
    /** A typed shortcut for <code>createElement("textarea")</code>. */
    public inline function createTextAreaElement () :TextAreaElement { return cast createElement("textarea"); }
    /** A typed shortcut for <code>createElement("media")</code>. */
    public inline function createMediaElement () :MediaElement { return cast createElement("media"); }
    /** A typed shortcut for <code>createElement("object")</code>. */
    public inline function createObjectElement () :ObjectElement { return cast createElement("object"); }
    /** A typed shortcut for <code>createElement("caption")</code>. */
    public inline function createTableCaptionElement () :TableCaptionElement { return cast createElement("caption"); }
}
