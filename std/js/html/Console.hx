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

/** <p>Beginning with Firefox 4, the old <a title="en/Error Console" rel="internal" href="https://developer.mozilla.org/en/Error_Console">Error Console</a> has been deprecated in favor of the new, improved Web&nbsp;Console. The Web Console is something of a heads-up display for the web, letting you view error messages and other logged information. In addition, there are methods you can call to output information to the console, making it a useful debugging aid, and you can evaluate JavaScript on the fly.</p>
<p><a title="webconsole.png" rel="internal" href="https://developer.mozilla.org/@api/deki/files/4748/=webconsole.png"><img alt="webconsole.png" class="internal default" src="https://developer.mozilla.org/@api/deki/files/4748/=webconsole.png"></a></p>
<p>The Web Console won't replace more advanced debugging tools like <a class="external" title="http://getfirebug.com/" rel="external" href="http://getfirebug.com/" target="_blank">Firebug</a>; what it does give you, however, is a way to let remote users of your site or web application gather and report console logs and other information to you. It also provides a lightweight way to debug content if you don't happen to have Firebug installed when something goes wrong.</p>
<div class="note"><strong>Note:</strong> The Error Console is still available; you can re-enable it by changing the <code>devtools.errorconsole.enabled</code> preference to <code>true</code> and restarting the browser.</div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/Using_the_Web_Console">MDN</a>. */
@:native("Console")
extern class Console
{
    var memory (default,null) :MemoryInfo;

    var profiles (default,null) :Array<js.html.ScriptProfile>;

    function assert (condition :Bool) :Void;

    function clear () :Void;

    function count () :Void;

    function debug () :Void;

    function dir () :Void;

    function dirxml () :Void;

    function error () :Void;

    function group () :Void;

    function groupCollapsed () :Void;

    function groupEnd () :Void;

    function info () :Void;

    function log () :Void;

    function markTimeline () :Void;

    function profile (title :String) :Void;

    function profileEnd (title :String) :Void;

    function time (title :String) :Void;

    function timeEnd (title :String) :Void;

    function timeStamp () :Void;

    function trace () :Void;

    function warn () :Void;

}
