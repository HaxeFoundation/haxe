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

/** DOM <code>Object</code> objects expose the <a title="http://dev.w3.org/html5/spec/the-iframe-element.html#htmlobjectelement" class=" external" rel="external nofollow" href="http://dev.w3.org/html5/spec/the-iframe-element.html#htmlobjectelement" target="_blank">HTMLObjectElement</a> (or <span><a rel="custom nofollow" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a class=" external" rel="external nofollow" href="http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109/html.html#ID-9893177" title="http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109/html.html#ID-9893177" target="_blank">HTMLObjectElement</a>) interface, which provides special properties and methods (beyond the regular <a rel="internal" href="https://developer.mozilla.org/en/DOM/element">element</a> object interface they also have available to them by inheritance) for manipulating the layout and presentation of Object element, representing external resources.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLObjectElement">MDN</a>. */
@:native("HTMLObjectElement")
extern class ObjectElement extends Element
{
    /** Alignment of the object relative to its context. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var align :String;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/object#attr-archive">archive</a></code>
&nbsp;HTML attribute, containing a list of archives for resources for this object. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var archive :String;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/object#attr-border">border</a></code>
&nbsp;HTML attribute, specifying the width of a border around the object. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var border :String;

    /** The name of an applet class file, containing either the applet's subclass, or the path to get to the class, including the class file itself. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var code :String;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/object#attr-codebase">codebase</a></code>
&nbsp;HTML attribute, specifying the base path to use to resolve relative URIs. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var codeBase :String;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/object#attr-codetype">codetype</a></code>
&nbsp;HTML attribute, specifying the content type of the data. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var codeType :String;

    /** The active document of the object element's nested browsing context, if any; otherwise null. */
    var contentDocument (default,null) :Document;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/object#attr-data">data</a></code>
 HTML&nbsp;attribute, specifying the address of a resource's data. */
    var data :String;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/object#attr-declare">declare</a></code>
 HTML&nbsp;attribute, indicating that this is a declaration, not an instantiation, of the object. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var declare :Bool;

    /** The object element's form owner, or null if there isn't one. */
    var form (default,null) :FormElement;

    /** Reflects the {{htmlattrxref("height", "object)}}&nbsp;HTML attribute, specifying the displayed height of the resource in CSS pixels. */
    var height :String;

    /** Horizontal space in pixels around the control. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var hspace :Int;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/object#attr-name">name</a></code>
&nbsp;HTML attribute, specifying the name of the object (
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span>, or of a browsing context (
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var name :String;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/object#attr-standby">standby</a></code>
 HTML&nbsp;attribute, specifying a message to display while the object loads. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var standby :String;

    /** Reflects the {{htmlattrxref("type", "object)}}&nbsp;HTML attribute, specifying the MIME type of the resource. */
    var type :String;

    /** Reflects the {{htmlattrxref("usemap", "object)}}&nbsp;HTML attribute, specifying a {{HTMLElement("map")}} element to use. */
    var useMap :String;

    /** A localized message that describes the validation constraints that the control does not satisfy (if any). This is the empty string if the control is not a candidate for constraint validation (<strong>willValidate</strong> is false), or it satisfies its constraints. */
    var validationMessage (default,null) :String;

    /** The validity states that this element is in. */
    var validity (default,null) :ValidityState;

    /** Horizontal space in pixels around the control. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. */
    var vspace :Int;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/object#attr-width">width</a></code>
&nbsp;HTML attribute, specifying the displayed width of the resource in CSS pixels. */
    var width :String;

    /** Indicates whether the element is a candidate for constraint validation. Always false for <code>object</code> objects. */
    var willValidate (default,null) :Bool;

    function checkValidity () :Bool;

    function getSVGDocument () :js.html.svg.Document;

    function setCustomValidity (error :String) :Void;

}
