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

/** <div><strong>DRAFT</strong>
<div>This page is not complete.</div>
</div>

<p></p>
<p>A <code>MediaQueryList</code> object maintains a list of <a title="En/CSS/Media queries" rel="internal" href="https://developer.mozilla.org/En/CSS/Media_queries">media queries</a> on a <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/document">document</a></code>
, and handles sending notifications to listeners when the media queries on the document change.</p>
<p>This makes it possible to observe a document to detect when its media queries change, instead of polling the values periodically, if you need to programmatically detect changes to the values of media queries on a document.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/MediaQueryListListener">MDN</a>. */
typedef MediaQueryListListener = MediaQueryList -> Void;