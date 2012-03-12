/*
 * Copyright (c) 2005-2010, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package haxe.macro;
import haxe.macro.Type;

/**
	This is the api that is passed to the custom JS generator.
**/
typedef JSGenApi = {
	/** the file in which the JS code can be generated **/
	var outputFile : String;
	/** all the types that were compiled by haXe **/
	var types : Array<Type>;
	/** the main call expression, if a -main class is defined **/
	var main : Null<TypedExpr>;
	/** generate the JS code for any given typed expression **/
	function generateStatement( e : TypedExpr ) : String;
	/** generate the JS code for a given typed expression-value **/
	function generateValue( e : TypedExpr ) : String;
	/** define the JS code that gets generated when a class or enum is accessed in a typed expression **/
	function setTypeAccessor( callb : Type -> String ) : Void;
	/** tells if the given identifier is a JS keyword **/
	function isKeyword( ident : String ) : Bool;
	/** quote and escape the given string constant **/
	function quoteString( s : String ) : String;
	/** create the metadata expression for the given type **/
	function buildMetaData( t : BaseType ) : Null<TypedExpr>;
}
