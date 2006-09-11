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
package haxe;

/**
	Elements return by [Stack] methods.
**/
enum StackItem {
	CFunction;
	Module( m : String );
	FilePos( name : String, line : Int );
	Method( classname : String, method : String );
}

/**
	Get informations about the call stack.
**/
class Stack {

	/**
		Return the call stack elements.
	**/
	public static function callStack() : Array<StackItem> {
		#if neko
		var a = makeStack(untyped __dollar__callstack());
		a.pop(); // remove Stack.callStack()
		return a;
		#else flash9
		return null;
		#else true
		return makeStack("$s");
		#end
	}

	/**
		Return the exception stack : this is the stack elements between
		the place the last exception was thrown and the place it was
		catched.
	**/
	public static function exceptionStack() : Array<StackItem> {
		#if neko
		return makeStack(untyped __dollar__excstack());
		#else flash9
		return null;		
		#else true
		return makeStack("$e");
		#end
	}

	/**
		Returns a representation of the stack as a printable string.
	**/
	public static function toString( stack : Array<StackItem> ) {
		var b = new StringBuf();
		for( s in stack )
			switch( s ) {
			case CFunction:
				b.add("Called from a C function\n");
			case Module(m):
				b.add("Called from module ");
				b.add(m);
				b.add("\n");
			case FilePos(name,line):
				b.add("Called from ");
				b.add(name);
				b.add(" line ");
				b.add(line);
				b.add("\n");
			case Method(cname,meth):
				b.add("Called from ");
				b.add(cname);
				b.add(" method ");
				b.add(meth);
				b.add("\n");
			}
		return b.toString();
	}

	private static function makeStack(s) {
		#if neko
		var a = new Array();
		var l = untyped __dollar__asize(s);
		var i = 0;
		while( i < l ) {
			var x = s[i++];
			if( x == null )
				a.push(CFunction);
			else if( untyped __dollar__typeof(x) == __dollar__tstring )
				a.push(Module(new String(x)));
			else
				a.push(FilePos(new String(untyped x[0]),untyped x[1]));
		}
		return a;
		#else flash9
		#else true
		var a : Array<String> = untyped #if flash __eval__(s) #else true __js__("eval")(s) #end;
		var m = new Array();
		for( i in 0...a.length - if(s == "$s") 2 else 0 ) {
			var d = a[i].split("::");
			m.unshift(Method(d[0],d[1]));
		}
		return m;
		#end
	}

}
