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

@:native("SpeechRecognition")
extern class SpeechRecognition extends EventTarget
{
    var continuous :Bool;

    var grammars :SpeechGrammarList;

    var interimResults :Bool;

    var lang :String;

    var maxAlternatives :Int;

    var onaudioend :EventListener;

    var onaudiostart :EventListener;

    var onend :EventListener;

    var onerror :EventListener;

    var onnomatch :EventListener;

    var onresult :EventListener;

    var onsoundend :EventListener;

    var onsoundstart :EventListener;

    var onspeechend :EventListener;

    var onspeechstart :EventListener;

    var onstart :EventListener;

    function new () :Void;

    function abort () :Void;

    function start () :Void;

    function stop () :Void;

}
