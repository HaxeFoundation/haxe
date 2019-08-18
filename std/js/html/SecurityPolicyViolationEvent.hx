/*
 * Copyright (C)2005-2019 Haxe Foundation
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

// This file is generated from mozilla\SecurityPolicyViolationEvent.webidl. Do not edit!

package js.html;

/**
	The `SecurityPolicyViolationEvent` interface inherits from `Event`, and represents the event object of an event sent on a document or worker when its content security policy is violated.

	Documentation [SecurityPolicyViolationEvent](https://developer.mozilla.org/en-US/docs/Web/API/SecurityPolicyViolationEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SecurityPolicyViolationEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SecurityPolicyViolationEvent>
**/
@:native("SecurityPolicyViolationEvent")
extern class SecurityPolicyViolationEvent extends Event {
	
	/**
		A `USVString` representing the URI of the document or worker in which the violation was found.
	**/
	var documentURI(default,null) : String;
	
	/**
		A `USVString` representing the referrer of the resources whose policy was violated. This will be a URL or `null`.
	**/
	var referrer(default,null) : String;
	
	/**
		A `USVString` representing the URI of the resource that was blocked because it violates a policy.
	**/
	var blockedURI(default,null) : String;
	
	/**
		A `DOMString` representing the directive whose enforcement uncovered the violation.
	**/
	var violatedDirective(default,null) : String;
	
	/**
		A `DOMString` representing the directive whose enforcement uncovered the violation.
	**/
	var effectiveDirective(default,null) : String;
	
	/**
		A `DOMString` containing the policy whose enforcement uncovered the violation.
	**/
	var originalPolicy(default,null) : String;
	
	/**
		A `USVString` representing the URI of the document or worker in which the violation was found.
	**/
	var sourceFile(default,null) : String;
	
	/**
		A `DOMString` representing a sample of the resource that caused the violation, usually the first 40 characters. This will only be populated if the resource is an inline script, event handler, or style — external resources causing a violation will not generate a sample.
	**/
	var sample(default,null) : String;
	
	/**
		Indicates how the violated policy is configured to be treated by the user agent. This will be `"enforce"` or `"report"`.
	**/
	var disposition(default,null) : SecurityPolicyViolationEventDisposition;
	
	/**
		A number representing the HTTP status code of the document or worker in which the violation occurred.
	**/
	var statusCode(default,null) : Int;
	
	/**
		The line number in the document or worker at which the violation occurred.
	**/
	var lineNumber(default,null) : Int;
	
	/**
		The column number in the document or worker at which the violation occurred.
	**/
	var columnNumber(default,null) : Int;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : SecurityPolicyViolationEventInit ) : Void;
}