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

import haxe.macro.Expr;
import haxe.macro.Type.TypedExpr;

@:noDoc @:hlNative("macro")
class Context {

#if macro

	public static function error( msg : String, pos : Position ) : Dynamic {
		return null;
	}

	public static function fatalError( msg : String, pos : Position ) : Dynamic {
		return null;
	}

	public static function warning( msg : String, pos : Position ) : Void {
	}

	public static function resolvePath( file : String ) : String {
		return null;
	}

	public static function getClassPath() : Array<String> {
		return null;
	}

	public static function currentPos() : Position {
		return null;
	}

	@:require(haxe_ver >= 3.1)
	public static function getExpectedType():Null<Type> {
		return null;
	}

	@:require(haxe_ver >= 3.2)
	public static function getCallArguments():Null<Array<Expr>> {
		return null;
	}

	public static function getLocalClass() : Null<Type.Ref<Type.ClassType>> {
		var l = getLocalType();
		if( l == null ) return null;
		return switch( l ) {
		case TInst(c,_): c;
		default: null;
		}
	}

	public static function getLocalModule() : String {
		return null;
	}

	public static function getLocalType() : Null<Type> {
		return null;
	}

	public static function getLocalMethod() : Null<String> {
		return null;
	}

	public static function getLocalUsing() :  Array<Type.Ref<Type.ClassType>> {
		return null;
	}

	public static function getLocalImports() :  Array<ImportExpr> {
		return null;
	}

	private static function localVars(b:Bool) : Map<String,Dynamic> {
		return null;
	}

	@:deprecated("Use Context.getLocalTVars() instead")
	public static function getLocalVars() : Map<String,Type> {
		return cast localVars(false);
	}

	@:require(haxe_ver >= 3.102)
	public static function getLocalTVars() : Map<String,Type.TVar> {
		return cast localVars(true);
	}

	public static function defined( s : String ) : Bool {
		return null;
	}

	public static function definedValue( key : String ) : String {
		return null;
	}

	public static function getDefines() : Map<String,String> {
		return null;
	}

	public static function getType( name : String ) : Type {
		return null;
	}

	public static function getModule( name : String ) : Array<Type> {
		return null;
	}

	static function doParse( expr : String, pos : Position, isInline : Bool ) : Expr {
		return null;
	}

	public static function parse( expr : String, pos : Position ) : Expr {
		return doParse(expr,pos,false);
	}

	public static function parseInlineString( expr : String, pos : Position ) : Expr {
		return doParse(expr,pos,true);
	}

	public static function makeExpr( v : Dynamic, pos : Position ) : Expr {
		return null;
	}

	public static function signature( v : Dynamic ) : String {
		return null;
	}

	public static function onGenerate( callback : Array<Type> -> Void ) : Void  {
	}

	@:require(haxe_ver >= 3.1)
	public static function onAfterGenerate( callback : Void -> Void ) : Void {
	}

	public static function onAfterTyping( callback : Array<haxe.macro.Type.ModuleType> -> Void ) : Void {
	}

	public static function onTypeNotFound ( callback : String -> TypeDefinition ) : Void {
	}

	public static function typeof( e : Expr ) : Type {
		return null;
	}

	@:require(haxe_ver >= 3.1)
	public static function typeExpr( e : Expr ) : TypedExpr {
		return null;
	}

	@:require(haxe_ver >= 3.3)
	public static function resolveType( t : ComplexType, p : Position ) : Type {
		return null;
	}

	public static function toComplexType( t : Type ) : Null<ComplexType> {
		return null;
	}

	public static function unify( t1 : Type, t2 : Type) : Bool {
		return false;
	}

	public static function follow( t : Type, ?once : Bool ) : Type {
		return null;
	}

	public static function followWithAbstracts(t : Type, once : Bool = false ) : Type {
		return null;
	}

	public static function getPosInfos( p : Position ) : { min : Int, max : Int, file : String } {
		return null;
	}

	public static function makePosition( inf : { min : Int, max : Int, file : String } ) : Position {
		return null;
	}

	public static function getResources():Map<String,haxe.io.Bytes> {
		return null;
	}

	public static function addResource( name : String, data : haxe.io.Bytes ) : Void {
	}

	public static function getBuildFields() : Array<Field> {
		return null;
	}

	public static function defineType( t : TypeDefinition ) : Void {
	}

	public static function defineModule( modulePath : String, types : Array<TypeDefinition>, ?imports: Array<ImportExpr>, ?usings : Array<TypePath> ) : Void {
	}

	public static function getTypedExpr( t : Type.TypedExpr ) : Expr {
		return null;
	}

	@:require(haxe_ver >= 3.2)
	public static function storeTypedExpr( t : Type.TypedExpr ) : Expr {
		return null;
	}

	public static function registerModuleDependency( modulePath : String, externFile : String ) : Void {
	}

	public static function registerModuleReuseCall( modulePath : String, macroCall : String ) : Void {
	}

	public static function onMacroContextReused( callb : Void -> Bool ) : Void {
	}

	private static function includeFile( file : String, position : String ) {
	}

	private static function sExpr( e : TypedExpr, pretty : Bool ) : String {
		return null;
	}

#end

}
