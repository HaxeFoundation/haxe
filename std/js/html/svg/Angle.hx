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
package js.html.svg;

/** <p>The <code>SVGAngle</code> interface correspond to the <a title="https://developer.mozilla.org/en/SVG/Content_type#Angle" rel="internal" href="https://developer.mozilla.org/en/SVG/Content_type#Angle">&lt;angle&gt;</a> basic data type.</p>
<p>An <code>SVGAngle</code> object can be designated as read only, which means that attempts to modify the object will result in an exception being thrown.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGAngle">MDN</a>. */
@:native("SVGAngle")
extern class Angle
{
    /** The unit type was explicitly set to degrees. */
    static inline var SVG_ANGLETYPE_DEG :Int = 2;

    /** The unit type is gradians. */
    static inline var SVG_ANGLETYPE_GRAD :Int = 4;

    /** The unit type is radians. */
    static inline var SVG_ANGLETYPE_RAD :Int = 3;

    /** The unit type is not one of predefined unit types. It is invalid to attempt to define a new value of this type or to attempt to switch an existing value to this type. */
    static inline var SVG_ANGLETYPE_UNKNOWN :Int = 0;

    /** No unit type was provided (i.e., a unitless value was specified). For angles, a unitless value is treated the same as if degrees were specified. */
    static inline var SVG_ANGLETYPE_UNSPECIFIED :Int = 1;

    /** The type of the value as specified by one of the SVG_ANGLETYPE_* constants defined on this interface. */
    var unitType (default,null) :Int;

    /** <p>The value as a floating point value, in user units. Setting this attribute will cause <code>valueInSpecifiedUnits</code> and <code>valueAsString</code> to be updated automatically to reflect this setting.</p> <p><strong>Exceptions on setting:</strong> a <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOMException">DOMException</a></code>
 with code <code>NO_MODIFICATION_ALLOWED_ERR</code> is raised when the length corresponds to a read only attribute or when the object itself is read only.</p> */
    var value :Float;

    /** <p>The value as a string value, in the units expressed by <code>unitType</code>. Setting this attribute will cause <code>value</code>, <code>valueInSpecifiedUnits</code> and <code>unitType</code> to be updated automatically to reflect this setting.</p> <p><strong>Exceptions on setting:</strong></p> <ul> <li>a <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOMException">DOMException</a></code>
 with code <code>SYNTAX_ERR</code> is raised if the assigned string cannot be parsed as a valid <a title="https://developer.mozilla.org/en/SVG/Content_type#Angle" rel="internal" href="https://developer.mozilla.org/en/SVG/Content_type#Angle">&lt;angle&gt;</a>.</li> <li>a <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOMException">DOMException</a></code>
 with code <code>NO_MODIFICATION_ALLOWED_ERR</code> is raised when the length corresponds to a read only attribute or when the object itself is read only.</li> </ul> Setter throws DOMException. */
    var valueAsString :String;

    /** <p>The value as a floating point value, in the units expressed by <code>unitType</code>. Setting this attribute will cause <code>value</code> and <code>valueAsString</code> to be updated automatically to reflect this setting.</p> <p><strong>Exceptions on setting:</strong> a <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOMException">DOMException</a></code>
 with code <code>NO_MODIFICATION_ALLOWED_ERR</code> is raised when the length corresponds to a read only attribute or when the object itself is read only.</p> */
    var valueInSpecifiedUnits :Float;

    function convertToSpecifiedUnits (unitType :Int) :Void;

    function newValueSpecifiedUnits (unitType :Int, valueInSpecifiedUnits :Float) :Void;

}
