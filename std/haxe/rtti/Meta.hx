/*
 * Copyright (c) 2005-2009, The haXe Project Contributors
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
package haxe.rtti;

/**
	An api to access classes and enums metadata at runtime.
**/
class Meta {

	/**
		Returns the metadata that were declared for the given type (class or enum)
	**/
	public static function getType( t : Dynamic ) : Dynamic<Array<Dynamic>> {
		#if (java || cs)
		var meta : Dynamic = Reflect.field(t, "__meta__");
		#else
		var meta : Dynamic = untyped t.__meta__;
		#end
		return (meta == null || meta.obj == null) ? {} : meta.obj;
	}

	/**
		Returns the metadata that were declared for the given class static fields
	**/	
	public static function getStatics( t : Dynamic ) : Dynamic<Dynamic<Array<Dynamic>>> {
		#if (java || cs)
		var meta : Dynamic = Reflect.field(t, "__meta__");
		#else
		var meta : Dynamic = untyped t.__meta__;
		#end
		return (meta == null || meta.statics == null) ? {} : meta.statics;
	}

	/**
		Returns the metadata that were declared for the given class fields or enum constructors
	**/
	public static function getFields( t : Dynamic ) : Dynamic<Dynamic<Array<Dynamic>>> {
		#if (java || cs)
		var meta : Dynamic = Reflect.field(t, "__meta__");
		#else
		var meta : Dynamic = untyped t.__meta__;
		#end
		return (meta == null || meta.fields == null) ? {} : meta.fields;
	}

}