/*
 * Copyright (C)2005-2013 Haxe Foundation
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

/** The <em>HTML Link Element</em> (&lt;link&gt;) specifies relationships between the current document and other documents. Possible uses for this element include defining a relational framework for navigation and linking the document to a style sheet.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/HTML/Element/link">MDN</a>. */
@:native("HTMLLinkElement")
extern class LinkElement extends Element
{
	/** This attribute defines the character encoding of the linked resource. The value is a space- and/or comma-delimited list of character sets as defined in <a class="external" title="http://tools.ietf.org/html/rfc2045" rel="external" href="http://tools.ietf.org/html/rfc2045" target="_blank">RFC 2045</a>. The default value is ISO-8859-1. <div class="note"><strong>Usage note: </strong>This attribute is obsolete in HTML5 and <span>must</span><strong> not be used by authors</strong>. To achieve its effect, use the <span>Content-Type:</span> HTTP header on the linked resource.</div> */
	var charset : String;

	/** This attribute is used to disable a link relationship. In conjunction with scripting, this attribute could be used to turn on and off various style sheet relationships. <div class="note"> <p><strong>Note: </strong>While there is no <code>disabled</code> attribute in the HTML standard, there <strong>is</strong> a <code>disabled</code> attribute on the <code>HTMLLinkElement</code> DOM object.</p> <p>The use of&nbsp;<code>disabled</code> as an HTML attribute is non-standard and only used by some Microsoft browsers. <span>Do not use it</span>. To achieve a similar effect, use one of the following techniques:</p> <ul> <li>If the <code>disabled</code> attribute has been added directly to the element on the page, do not include the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/link">&lt;link&gt;</a></code>
 element instead;</li> <li>Set the <code>disabled</code> <strong>property</strong> of the DOM object via scripting.</li> </ul> </div> */
	var disabled : Bool;

	/** This attribute specifies the <a title="https://developer.mozilla.org/en/URIs_and_URLs" rel="internal" href="https://developer.mozilla.org/en/URIs_and_URLs">URL</a> of the linked resource. A URL might be absolute or relative. */
	var href : String;

	/** This attribute indicates the language of the linked resource. It is purely advisory. Allowed values are determined by <a class="external" title="http://www.ietf.org/rfc/bcp/bcp47.txt" rel="external" href="http://www.ietf.org/rfc/bcp/bcp47.txt" target="_blank">BCP47</a> for HTML5 and by <a class="external" title="http://www.ietf.org/rfc/rfc1766.txt" rel="external" href="http://www.ietf.org/rfc/rfc1766.txt" target="_blank">RFC1766</a> for HTML 4. Use this attribute only if the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/a#attr-href">href</a></code>
 attribute is present. */
	var hreflang : String;

	/** This attribute specifies the media which the linked resource applies to. Its value must be a <a title="En/CSS/Media queries" rel="internal" href="https://developer.mozilla.org/en/CSS/Media_queries">media query</a>. This attribute is mainly useful when linking to external stylesheets by allowing the user agent to pick the best adapted one for the device it runs on.<br> <div class="note"><strong>Usage note:&nbsp;</strong> <p>&nbsp;</p> <ul> <li>In HTML 4, this can only be a simple white-space-separated list of media description literals, i.e., <a title="https://developer.mozilla.org/en/CSS/@media" rel="internal" href="https://developer.mozilla.org/en/CSS/@media">media types and groups</a>, where defined and allowed as values for this attribute, such as <span>print</span>, <span>screen</span>, <span>aural</span>, <span>braille.</span> HTML5 extended this to any kind of <a title="En/CSS/Media queries" rel="internal" href="https://developer.mozilla.org/en/CSS/Media_queries">media queries</a>, which are a superset of the allowed values of HTML 4.</li> <li>Browsers not supporting the <a title="En/CSS/Media queries" rel="internal" href="https://developer.mozilla.org/en/CSS/Media_queries">CSS3 Media Queries</a> won't necessary recognized the adequate link; do not forget to set fallback links,&nbsp; the restricted set of media queries defined in HTML 4.</li> </ul> </div> */
	var media : String;

	/** This attribute names a relationship of the linked document to the current document. The attribute must be a space-separated list of the <a title="en/HTML/Link types" rel="internal" href="https://developer.mozilla.org/en/HTML/Link_types">link types values</a>. The most common use of this attribute is to specify a link to an external style sheet:&nbsp;the <strong>rel</strong> attribute is set to <code>stylesheet</code>, and the <strong>href</strong> attribute is set to the URL of an external style sheet to format the page. WebTV also supports the use of the value <code>next</code> for <strong>rel</strong> to preload the next page in a document series. */
	var rel : String;

	/** The value of this attribute shows the relationship of the current document to the linked document, as defined by the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/link#attr-href">href</a></code>
 attribute. The attribute thus defines the reverse relationship compared to the value of the <strong>rel</strong> attribute. <a title="en/HTML/Link types" rel="internal" href="https://developer.mozilla.org/en/HTML/Link_types">Link types values</a> for the attribute are similar to the possible values for 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/link#attr-rel">rel</a></code>
.<br> <div class="note"><strong>Usage note: </strong>This attribute is obsolete in HTML5. <strong>Do not use it</strong>. To achieve its effect, use the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/link#attr-rel">rel</a></code>
 attribute with the opposite <a title="en/HTML/Link types" rel="internal" href="https://developer.mozilla.org/en/HTML/Link_types">link types values</a>, e.g. <span>made</span> should be replaced by <span>author</span>. Also this attribute doesn't mean <em>revision</em> and must not be used with a version number, which is unfortunately the case on numerous sites.</div> */
	var rev : String;

	var sheet(default,null) : StyleSheet;

	/** This attribute defines the sizes of the icons for visual media contained in the resource. It must be present only if the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/link#attr-rel">rel</a></code>
 contains the <span>icon</span> <a title="en/HTML/Link types" rel="internal" href="https://developer.mozilla.org/en/HTML/Link_types">link types value</a>. It may have the following values: <ul> <li><span>any</span>, meaning that the icon can be scaled to any size as it is in a vectorial format, like <span>image/svg</span>.</li> <li>a white-space separated list of sizes, each in the format <span><em>&lt;width in pixels&gt;</em>x<em>&lt;height in pixels&gt;</em></span> or <span><em>&lt;width in pixels&gt;</em>X<em>&lt;height in pixels&gt;</em></span>. Each of these sizes must be contained in the resource.</li> </ul> <div class="note"><strong>Usage note:&nbsp;</strong> <p>&nbsp;</p> <ul> <li>Most icon format are only able to store one single icon; therefore most of the time the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element#attr-sizes">sizes</a></code>
 contains only one entry. Among the major browsers, only the Apple's ICNS format allows the storage of multiple icons, and this format is only supported in WebKit.</li> <li>Apple's iOS does not support this attribute, hence Apple's iPhone and iPad use special, non-standard <a title="en/HTML/Link types" rel="internal" href="https://developer.mozilla.org/en/HTML/Link_types">link types values</a> to define icon to be used as Web Clip or start-up placeholder: <span>apple-touch-icon</span> and <span>apple-touch-startup-icon</span>.</li> </ul> </div> */
	var sizes : DOMSettableTokenList;

	/** Defines the frame or window name that has the defined linking relationship or that will show the rendering of any linked resource. */
	var target : String;

	/** This attribute is used to define the type of the content linked to. The value of the attribute should be a MIME type such as <strong>text/html</strong>, <strong>text/css</strong>, and so on. The common use of this attribute is to define the type of style sheet linked and the most common current value is <strong>text/css</strong>, which indicates a Cascading Style Sheet format. */
	var type : String;

}
