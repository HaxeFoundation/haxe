/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package haxe.macro;
import haxe.macro.Type;

/**
	This is the api that is passed to the custom JS generator.
**/
typedef JSGenApi = {
	/** the file in which the JS code can be generated **/
	var outputFile : String;
	/** all the types that were compiled by Haxe **/
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
	/** add a feature **/
	function addFeature( f : String ) : Bool;
	/** check if a feature is used **/
	function hasFeature( f : String ) : Bool;
	/** quote and escape the given string constant **/
	function quoteString( s : String ) : String;
	/** create the metadata expression for the given type **/
	function buildMetaData( t : BaseType ) : Null<TypedExpr>;
	/** select the current classe **/
	function setCurrentClass( c : ClassType ) : Void;
}
