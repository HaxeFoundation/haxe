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
package haxe;

/**
	Elements return by [Stack] methods.
**/
enum StackItem {
	CFunction;
	Module( m : String );
	FilePos( s : Null<StackItem>, file : String, line : Int );
	Method( classname : String, method : String );
	Lambda( v : Int );
}

/**
	Get informations about the call stack.
**/
class Stack {

	/**
		Return the call stack elements, or an empty array if not available.
	**/
	public static function callStack() : Array<StackItem> {
		#if neko
			var a = makeStack(untyped __dollar__callstack());
			a.shift(); // remove Stack.callStack()
			return a;
		#elseif flash9
			var a = makeStack( new flash.errors.Error().getStackTrace() );
			a.shift(); // remove Stack.callStack()
			return a;
		#elseif flash
			return makeStack("$s");
		#elseif php
			return makeStack("%s");
		#elseif cpp
			var s:Array<String> = untyped __global__.__hxcpp_get_call_stack(true);
			return makeStack(s);
		#elseif js
			// https://code.google.com/p/v8/wiki/JavaScriptStackTraceApi
			var oldValue = (untyped Error).prepareStackTrace;
			(untyped Error).prepareStackTrace = function (error, callsites :Array<Dynamic>) {
				var stack = [];
				for (site in callsites) {
					var method = null;
					var fullName :String = site.getFunctionName();
					if (fullName != null) {
						var idx = fullName.lastIndexOf(".");
						if (idx >= 0) {
							var className = fullName.substr(0, idx);
							var methodName = fullName.substr(idx+1);
							method = Method(className, methodName);
						}
					}
					stack.push(FilePos(method, site.getFileName(), site.getLineNumber()));
				}
				return stack;
			}
			var a = makeStack(untyped __new__("Error").stack);
			a.shift(); // remove Stack.callStack()
			(untyped Error).prepareStackTrace = oldValue;
			return a;
		#else
			return []; // Unsupported
		#end
	}

	/**
		Return the exception stack : this is the stack elements between
		the place the last exception was thrown and the place it was
		caught, or an empty array if not available.
	**/
	#if cpp @:noStack #end /* Do not mess up the exception stack */
	public static function exceptionStack() : Array<StackItem> {
		#if neko
			return makeStack(untyped __dollar__excstack());
		#elseif as3
			return new Array();
		#elseif flash9
			var err : flash.errors.Error = untyped flash.Boot.lastError;
			if( err == null ) return new Array();
			var a = makeStack( err.getStackTrace() );
			var c = callStack();
			var i = c.length - 1;
			while( i > 0 ) {
				if( Std.string(a[a.length-1]) == Std.string(c[i]) )
					a.pop();
				else
					break;
				i--;
			}
			return a;
		#elseif flash
			return makeStack("$e");
		#elseif php
			return makeStack("%e");
		#elseif cpp
			var s:Array<String> = untyped __global__.__hxcpp_get_exception_stack();
			return makeStack(s);
		#else
			return []; // Unsupported
		#end
	}

	/**
		Returns a representation of the stack as a printable string.
	**/
	public static function toString( stack : Array<StackItem> ) {
		var b = new StringBuf();
		for( s in stack ) {
			b.add("\nCalled from ");
			itemToString(b,s);
		}
		return b.toString();
	}

	private static function itemToString( b : StringBuf, s ) {
		switch( s ) {
		case CFunction:
			b.add("a C function");
		case Module(m):
			b.add("module ");
			b.add(m);
		case FilePos(s,file,line):
			if( s != null ) {
				itemToString(b,s);
				b.add(" (");
			}
			b.add(file);
			b.add(" line ");
			b.add(line);
			if( s != null ) b.add(")");
		case Method(cname,meth):
			b.add(cname);
			b.add(".");
			b.add(meth);
		case Lambda(n):
			b.add("local function #");
			b.add(n);
		}
	}

	#if cpp @:noStack #end /* Do not mess up the exception stack */
	private static function makeStack(s) {
		#if neko
			var a = new Array();
			var l = untyped __dollar__asize(s);
			var i = 0;
			while( i < l ) {
				var x = s[i++];
				if( x == null )
					a.unshift(CFunction);
				else if( untyped __dollar__typeof(x) == __dollar__tstring )
					a.unshift(Module(new String(x)));
				else
					a.unshift(FilePos(null,new String(untyped x[0]),untyped x[1]));
			}
			return a;
		#elseif flash9
			var a = new Array();
			var r = ~/at ([^\/]+?)\$?(\/[^\(]+)?\(\)(\[(.*?):([0-9]+)\])?/;
			var rlambda = ~/^MethodInfo-([0-9]+)$/g;
			while( r.match(s) ) {
				var cl = r.matched(1).split("::").join(".");
				var meth = r.matched(2);
				var item;
				if( meth == null ) {
					if( rlambda.match(cl) )
						item = Lambda(Std.parseInt(rlambda.matched(1)));
					else
						item = Method(cl,"new");
				} else
					item = Method(cl,meth.substr(1));
				if( r.matched(3) != null )
					item = FilePos( item, r.matched(4), Std.parseInt(r.matched(5)) );
				a.push(item);
				s = r.matchedRight();
			}
			return a;
		#elseif flash
			var a : Array<String> = untyped __eval__(s);
			var m = new Array();
			for( i in 0...a.length - if(s == "$s") 2 else 0 ) {
				var d = a[i].split("::");
				m.unshift(Method(d[0],d[1]));
			}
			return m;
		#elseif php
			if (!untyped __call__("isset", __var__("GLOBALS", s)))
				return [];
			var a : Array<String> = untyped __var__("GLOBALS", s);
			var m = [];
			for( i in 0...a.length - ((s == "%s") ? 2 : 0)) {
				var d = a[i].split("::");
				m.unshift(Method(d[0],d[1]));
			}
			return m;
		#elseif cpp
			var stack : Array<String> = s;
			var m = new Array<StackItem>();
			for(func in stack) {
				var words = func.split("::");
				if (words.length==0)
					m.unshift(CFunction)
				else if (words.length==2)
					m.unshift(Method(words[0],words[1]));
				else if (words.length==4)
					m.unshift(FilePos( Method(words[0],words[1]),words[2],Std.parseInt(words[3])));
			}
			return m;
		#elseif js
			if ((untyped __js__("typeof"))(s) == "string") {
				// Return the raw lines in browsers that don't support prepareStackTrace
				var stack : Array<String> = s.split("\n");
				var m = [];
				for( line in stack ) {
					m.push(Module(line)); // A little weird, but better than nothing
				}
				return m;
			} else {
				return cast s;
			}
		#else
			return null;
		#end
	}

}
