/*
 * Copyright (c) 2005, The haXe Project Contributors
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
package neko.vm;

/**
	The abstract Neko module handle.
**/
enum ModuleHandle {
}

/**
	A Neko Module represent a execution unit for the Neko Virtual Machine. Each compiled [.n] bytecode
	file is a module once loaded by the NekoVM.
**/
class Module {

	/**
		The abstract handle.
	**/
	public var m : ModuleHandle;
	public var name(getName,setName) : String;

	public function new( m ) {
		this.m = m;
	}

	/**
		Execute a module and returns its result (the latest evaluated expression).
		A module can be executed several times but its globals are only initialized once
		the first time the Module is loaded.
	**/
	public function execute() : Dynamic {
		return _module_exec(m);
	}

	function getName() {
		return new String(_module_name(m));
	}

	function setName( n : String ) {
		_module_set_name(m,untyped n.__s);
		return n;
	}


	/**
		Returns the Loader that this Module was loaded with.s
	**/
	public function loader() {
		return new Loader(_module_loader(m));
	}

	/**
		Returns the codeSize of the Module.
	**/
	public function codeSize() : Int {
		return _module_code_size(m);
	}

	/**
		Returns the number of globals in this Module global table.
	**/
	public function globalsCount() : Int {
		return _module_nglobals(m);
	}

	/**
		Get a Module global value.
	**/
	public function getGlobal( n : Int ) : Dynamic {
		return _module_global_get(m,n);
	}

	/**
		Set a Module global value.
	**/
	public function setGlobal( n : Int, v : Dynamic ) {
		_module_global_set(m,n,v);
	}

	public function toString() {
		return "[Module:"+name+"]";
	}

	/**
		Each Module has an export table which can be useful to transfert
		values between modules.
	**/
	public function getExports() : Hash<Dynamic> {
		var h = new Hash();
		var exp = _module_exports(m);
		for( f in Reflect.fields(exp) )
			h.set(f,Reflect.field(exp,f));
		return h;
	}

	/**
		The raw export table.
	**/
	public function exportsTable() : Dynamic {
		return _module_exports(m);
	}

	/**
		Set a value in the Module export table.
	**/
	public function setExport( name : String, value : Dynamic ) {
		var exp = _module_exports(m);
		Reflect.setField(exp,name,value);
	}

	/**
		Returns the local Module, which is the one in which this
		method is included.
	**/
	public static function local() {
		return new Module(untyped __dollar__exports.__module);
	}

	/**
		Reads a module from an Input by using the given Loader.
		The module is initialized but has not yet been executed.
	**/
	public static function read( i : haxe.io.Input, l : Loader ) : Module {
		var m = _module_read(function(buf,pos,len) {
			return i.readBytes(untyped new haxe.io.Bytes(len,buf),pos,len);
		},l.l);
		return new Module(m);
	}

	/**
		Reads a module from Bytes using the given Loader.
		The module is initialized but has not yet been executed.
	**/
	public static function readBytes( b : haxe.io.Bytes, loader : Loader ) : Module {
		return new Module(_module_read_string(b.getData(),loader.l));
	}

	/**
		Reads a module from a name and using the specified seach path and loader.
		The module is initialized but has not yet been executed.
	**/
	public static function readPath( name : String, path : Array<String>, loader : Loader ) {
		var p = null;
		var i = path.length;
		while( --i >= 0 )
			p = untyped __dollar__array(path[i].__s,p);
		var m = _module_read_path(p,untyped name.__s,loader.l);
		return new Module(m);
	}

	/**
		Extract the globals names from the given module
	**/
	public static function readGlobalsNames( i : haxe.io.Input ) {
		if( i.readByte() != 0x4E || i.readByte() != 0x45 || i.readByte() != 0x4B || i.readByte() != 0x4F )
			throw "Not a neko file";
		function readInt() {
			return #if haxe3 i.readInt32() #else i.readUInt30() #end;
		}
		var nglobals = readInt();
		var nfields = readInt();
		var codesize = readInt();
		var a = new Array();
		for( k in 0...nglobals ) {
			switch(i.readByte()) {
			case 1:
				a.push(i.readUntil(0));
			case 2:
				a.push("<fun:"+(readInt()&0xFFFFFF)+">");
			case 3:
				a.push("STRING:"+i.readString(i.readUInt16()));
			case 4:
				a.push("FLOAT:"+i.readUntil(0));
			case 5:
				a.push("DEBUG");
			default:
				throw "assert";
			}
		}
		return a;
	}

	function __compare( other : Module ) {
		return untyped __dollar__compare(this.m,other.m);
	}

	static var _module_read = neko.Lib.load("std","module_read",2);
	static var _module_read_path = neko.Lib.load("std","module_read_path",3);
	static var _module_exec = neko.Lib.load("std","module_exec",1);
	static var _module_name = neko.Lib.load("std","module_name",1);
	static var _module_exports = neko.Lib.load("std","module_exports",1);
	static var _module_loader = neko.Lib.load("std","module_loader",1);
	static var _module_code_size = neko.Lib.load("std","module_code_size",1);
	static var _module_nglobals = neko.Lib.load("std","module_nglobals",1);
	static var _module_global_get = neko.Lib.load("std","module_global_get",2);
	static var _module_global_set = neko.Lib.load("std","module_global_set",3);
	static var _module_read_string = neko.Lib.loadLazy("std","module_read_string",2);
	static var _module_set_name = neko.Lib.loadLazy("std","module_set_name",2);

}
