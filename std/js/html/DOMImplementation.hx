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

/** Provides methods which are not dependent on any particular DOM instances. Returned by <code><a title="En/DOM/Document.implementation" class="internal" rel="internal" href="https://developer.mozilla.org/en/DOM/document.implementation">document.implementation</a></code>.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/DOMImplementation">MDN</a>. */
@:native("DOMImplementation")
extern class DOMImplementation
{
    function createCSSStyleSheet (title :String, media :String) :CSSStyleSheet;

    function createDocument (namespaceURI :String, qualifiedName :String, doctype :DocumentType) :Document;

    function createDocumentType (qualifiedName :String, publicId :String, systemId :String) :DocumentType;

    function createHTMLDocument (title :String) :Document;

    function hasFeature (feature :String, version :String) :Bool;

}
