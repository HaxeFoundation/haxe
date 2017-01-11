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

// This file is generated from mozilla\HTMLDocument.webidl. Do not edit!

package js.html;

/**
	`HTMLDocument` is an abstract interface of the DOM which provides access to special properties and methods not present by default on a regular (XML) document.

	Documentation [HTMLDocument](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDocument) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDocument$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDocument>
**/
@:native("HTMLDocument")
extern class HTMLDocument extends Document
{
	var domain : String;
	var cookie : String;
	var body : BodyElement;
	var head(default,null) : HeadElement;
	var images(default,null) : HTMLCollection;
	var embeds(default,null) : HTMLCollection;
	var plugins(default,null) : HTMLCollection;
	var links(default,null) : HTMLCollection;
	var forms(default,null) : HTMLCollection;
	var scripts(default,null) : HTMLCollection;
	var designMode : String;
	var fgColor : String;
	var linkColor : String;
	var vlinkColor : String;
	var alinkColor : String;
	var bgColor : String;
	var anchors(default,null) : HTMLCollection;
	var applets(default,null) : HTMLCollection;
	var all(default,null) : HTMLAllCollection;
	
	function getElementsByName( elementName : String ) : NodeList;
	function getItems( ?typeNames : String = "" ) : NodeList;
	/** @throws DOMError */
	@:overload( function( ?type : String = "text/html", ?replace : String = "" ) : HTMLDocument {} )
	function open( url : String, name : String, features : String, ?replace : Bool = false ) : Window;
	/** @throws DOMError */
	function close() : Void;
	/** @throws DOMError */
	function write( text : haxe.extern.Rest<String> ) : Void;
	/** @throws DOMError */
	function writeln( text : haxe.extern.Rest<String> ) : Void;
	/** @throws DOMError */
	function execCommand( commandId : String, ?showUI : Bool = false, ?value : String = "" ) : Bool;
	/** @throws DOMError */
	function queryCommandEnabled( commandId : String ) : Bool;
	/** @throws DOMError */
	function queryCommandIndeterm( commandId : String ) : Bool;
	/** @throws DOMError */
	function queryCommandState( commandId : String ) : Bool;
	function queryCommandSupported( commandId : String ) : Bool;
	/** @throws DOMError */
	function queryCommandValue( commandId : String ) : String;
	function clear() : Void;
	/** @throws DOMError */
	function getSelection() : Selection;
	function captureEvents() : Void;
	function releaseEvents() : Void;
	/** Shorthand for creating an HTML `<td>` element. */
	inline function createTableCellElement() : TableCellElement { return cast createElement("td"); }
	/** Shorthand for creating an HTML `<hr>` element. */
	inline function createHRElement() : HRElement { return cast createElement("hr"); }
	/** Shorthand for creating an HTML `<select>` element. */
	inline function createSelectElement() : SelectElement { return cast createElement("select"); }
	/** Shorthand for creating an HTML `<map>` element. */
	inline function createMapElement() : MapElement { return cast createElement("map"); }
	/** Shorthand for creating an HTML `<form>` element. */
	inline function createFormElement() : FormElement { return cast createElement("form"); }
	/** Shorthand for creating an HTML `<option>` element. */
	inline function createOptionElement() : OptionElement { return cast createElement("option"); }
	/** Shorthand for creating an HTML `<label>` element. */
	inline function createLabelElement() : LabelElement { return cast createElement("label"); }
	/** Shorthand for creating an HTML `<meta>` element. */
	inline function createMetaElement() : MetaElement { return cast createElement("meta"); }
	/** Shorthand for creating an HTML `<img>` element. */
	inline function createImageElement() : ImageElement { return cast createElement("img"); }
	/** Shorthand for creating an HTML `<dl>` element. */
	inline function createDListElement() : DListElement { return cast createElement("dl"); }
	/** Shorthand for creating an HTML `<frame>` element. */
	inline function createFrameElement() : FrameElement { return cast createElement("frame"); }
	/** Shorthand for creating an HTML `<mod>` element. */
	inline function createModElement() : ModElement { return cast createElement("mod"); }
	/** Shorthand for creating an HTML `<ul>` element. */
	inline function createUListElement() : UListElement { return cast createElement("ul"); }
	/** Shorthand for creating an HTML `<output>` element. */
	inline function createOutputElement() : OutputElement { return cast createElement("output"); }
	/** Shorthand for creating an HTML `<ol>` element. */
	inline function createOListElement() : OListElement { return cast createElement("ol"); }
	/** Shorthand for creating an HTML `<shadow>` element. */
	inline function createShadowElement() : ShadowElement { return cast createElement("shadow"); }
	/** Shorthand for creating an HTML `<li>` element. */
	inline function createLIElement() : LIElement { return cast createElement("li"); }
	/** Shorthand for creating an HTML `<datalist>` element. */
	inline function createDataListElement() : DataListElement { return cast createElement("datalist"); }
	/** Shorthand for creating an HTML `<param>` element. */
	inline function createParamElement() : ParamElement { return cast createElement("param"); }
	/** Shorthand for creating an HTML `<font>` element. */
	inline function createFontElement() : FontElement { return cast createElement("font"); }
	/** Shorthand for creating an HTML `<track>` element. */
	inline function createTrackElement() : TrackElement { return cast createElement("track"); }
	/** Shorthand for creating an HTML `<applet>` element. */
	inline function createAppletElement() : AppletElement { return cast createElement("applet"); }
	/** Shorthand for creating an HTML `<area>` element. */
	inline function createAreaElement() : AreaElement { return cast createElement("area"); }
	/** Shorthand for creating an HTML `<link>` element. */
	inline function createLinkElement() : LinkElement { return cast createElement("link"); }
	/** Shorthand for creating an HTML `<div>` element. */
	inline function createDivElement() : DivElement { return cast createElement("div"); }
	/** Shorthand for creating an HTML `<title>` element. */
	inline function createTitleElement() : TitleElement { return cast createElement("title"); }
	/** Shorthand for creating an HTML `<style>` element. */
	inline function createStyleElement() : StyleElement { return cast createElement("style"); }
	/** Shorthand for creating an HTML `<progress>` element. */
	inline function createProgressElement() : ProgressElement { return cast createElement("progress"); }
	/** Shorthand for creating an HTML `<button>` element. */
	inline function createButtonElement() : ButtonElement { return cast createElement("button"); }
	/** Shorthand for creating an HTML `<fieldset>` element. */
	inline function createFieldSetElement() : FieldSetElement { return cast createElement("fieldset"); }
	/** Shorthand for creating an HTML `<a>` element. */
	inline function createAnchorElement() : AnchorElement { return cast createElement("a"); }
	/** Shorthand for creating an HTML `<iframe>` element. */
	inline function createIFrameElement() : IFrameElement { return cast createElement("iframe"); }
	/** Shorthand for creating an HTML `<span>` element. */
	inline function createSpanElement() : SpanElement { return cast createElement("span"); }
	/** Shorthand for creating an HTML `<body>` element. */
	inline function createBodyElement() : BodyElement { return cast createElement("body"); }
	/** Shorthand for creating an HTML `<input>` element. */
	inline function createInputElement() : InputElement { return cast createElement("input"); }
	/** Shorthand for creating an HTML `<embed>` element. */
	inline function createEmbedElement() : EmbedElement { return cast createElement("embed"); }
	/** Shorthand for creating an HTML `<meter>` element. */
	inline function createMeterElement() : MeterElement { return cast createElement("meter"); }
	/** Shorthand for creating an HTML `<picture>` element. */
	inline function createPictureElement() : PictureElement { return cast createElement("picture"); }
	/** Shorthand for creating an HTML `<pre>` element. */
	inline function createPreElement() : PreElement { return cast createElement("pre"); }
	/** Shorthand for creating an HTML `<thead>` element. */
	inline function createTableSectionElement() : TableSectionElement { return cast createElement("thead"); }
	/** Shorthand for creating an HTML `<head>` element. */
	inline function createHeadElement() : HeadElement { return cast createElement("head"); }
	/** Shorthand for creating an HTML `<base>` element. */
	inline function createBaseElement() : BaseElement { return cast createElement("base"); }
	/** Shorthand for creating an HTML `<optgroup>` element. */
	inline function createOptGroupElement() : OptGroupElement { return cast createElement("optgroup"); }
	/** Shorthand for creating an HTML `<quote>` element. */
	inline function createQuoteElement() : QuoteElement { return cast createElement("quote"); }
	/** Shorthand for creating an HTML `<audio>` element. */
	inline function createAudioElement() : AudioElement { return cast createElement("audio"); }
	/** Shorthand for creating an HTML `<video>` element. */
	inline function createVideoElement() : VideoElement { return cast createElement("video"); }
	/** Shorthand for creating an HTML `<legend>` element. */
	inline function createLegendElement() : LegendElement { return cast createElement("legend"); }
	/** Shorthand for creating an HTML `<menu>` element. */
	inline function createMenuElement() : MenuElement { return cast createElement("menu"); }
	/** Shorthand for creating an HTML `<frameset>` element. */
	inline function createFrameSetElement() : FrameSetElement { return cast createElement("frameset"); }
	/** Shorthand for creating an HTML `<canvas>` element. */
	inline function createCanvasElement() : CanvasElement { return cast createElement("canvas"); }
	/** Shorthand for creating an HTML `<p>` element. */
	inline function createParagraphElement() : ParagraphElement { return cast createElement("p"); }
	/** Shorthand for creating an HTML `<col>` element. */
	inline function createTableColElement() : TableColElement { return cast createElement("col"); }
	/** Shorthand for creating an HTML `<dir>` element. */
	inline function createDirectoryElement() : DirectoryElement { return cast createElement("dir"); }
	/** Shorthand for creating an HTML `<table>` element. */
	inline function createTableElement() : TableElement { return cast createElement("table"); }
	/** Shorthand for creating an HTML `<tr>` element. */
	inline function createTableRowElement() : TableRowElement { return cast createElement("tr"); }
	/** Shorthand for creating an HTML `<script>` element. */
	inline function createScriptElement() : ScriptElement { return cast createElement("script"); }
	/** Shorthand for creating an HTML `<source>` element. */
	inline function createSourceElement() : SourceElement { return cast createElement("source"); }
	/** Shorthand for creating an HTML `<content>` element. */
	inline function createContentElement() : ContentElement { return cast createElement("content"); }
	/** Shorthand for creating an HTML `<br>` element. */
	inline function createBRElement() : BRElement { return cast createElement("br"); }
	/** Shorthand for creating an HTML `<html>` element. */
	inline function createHtmlElement() : HtmlElement { return cast createElement("html"); }
	/** Shorthand for creating an HTML `<textarea>` element. */
	inline function createTextAreaElement() : TextAreaElement { return cast createElement("textarea"); }
	/** Shorthand for creating an HTML `<media>` element. */
	inline function createMediaElement() : MediaElement { return cast createElement("media"); }
	/** Shorthand for creating an HTML `<object>` element. */
	inline function createObjectElement() : ObjectElement { return cast createElement("object"); }
	/** Shorthand for creating an HTML `<caption>` element. */
	inline function createTableCaptionElement() : TableCaptionElement { return cast createElement("caption"); }
	
}