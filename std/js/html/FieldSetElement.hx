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

/** DOM&nbsp;<code>fieldset</code> elements expose the <a class=" external" title="http://dev.w3.org/html5/spec/forms.html#htmlfieldsetelement" rel="external" href="http://dev.w3.org/html5/spec/forms.html#htmlfieldsetelement" target="_blank">HTMLFieldSetElement</a>&nbsp; (
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a class=" external" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-7365882" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-7365882" target="_blank">HTMLFieldSetElement</a>) interface, which provides special properties and methods (beyond the regular <a rel="internal" href="https://developer.mozilla.org/en/DOM/element">element</a> object interface they also have available to them by inheritance) for manipulating the layout and presentation of field-set elements.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLFieldSetElement">MDN</a>. */
@:native("HTMLFieldSetElement")
extern class FieldSetElement extends Element
{
    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/fieldset#attr-disabled">disabled</a></code>
 HTML&nbsp;attribute, indicating whether the user can interact with the control. */
    var disabled :Bool;

    /** The elements belonging to this field set. */
    var elements (default,null) :HTMLCollection;

    /** The containing form element, if this element is in a form. Otherwise, the element the <a title="en/HTML/Element/fieldset#attr-name" rel="internal" href="https://developer.mozilla.org/en/HTML/Element/fieldset#attr-name">name content attribute</a> points to 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>. (<code>null</code> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span>.) */
    var form (default,null) :FormElement;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/fieldset#attr-name">name</a></code>
 HTML&nbsp;attribute, containing the name of the field set, used for submitting the form. */
    var name :String;

    /** Must be the string <code>fieldset</code>. */
    var type (default,null) :String;

    /** A localized message that describes the validation constraints that the element does not satisfy (if any). This is the empty string if the element&nbsp; is not a candidate for constraint validation (<strong>willValidate</strong> is false), or it satisfies its constraints. */
    var validationMessage (default,null) :String;

    /** The validity states that this element is in. */
    var validity (default,null) :ValidityState;

    /** Always false because <code>fieldset</code> objects are never candidates for constraint validation. */
    var willValidate (default,null) :Bool;

    function checkValidity () :Bool;

    function setCustomValidity (error :String) :Void;

}
