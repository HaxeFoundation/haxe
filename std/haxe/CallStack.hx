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
package haxe;

/**
	Elements return by `CallStack` methods.
**/
enum StackItem {
	CFunction;
	Module( m : String );
	FilePos( s : Null<StackItem>, file : String, line : Int );
	Method( classname : String, method : String );
	LocalFunction( ?v : Int );
}

/**
	Get information about the call stack.
**/
class CallStack {
	#if js
	static var lastException:js.Error;

	static function getStack(e:js.Error):Array<StackItem> {
		if (e == null) return [];
		// https://code.google.com/p/v8/wiki/JavaScriptStackTraceApi
		var oldValue = (untyped Error).prepareStackTrace;
		(untyped Error).prepareStackTrace = function (error, callsites :Array<Dynamic>) {
			var stack = [];
			for (site in callsites) {
				if (wrapCallSite != null) site = wrapCallSite(site);
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
		var a = makeStack(e.stack);
		(untyped Error).prepareStackTrace = oldValue;
		return a;
	}

	// support for source-map-support module
	@:noCompletion
	public static var wrapCallSite:Dynamic->Dynamic;
	#end

	/**
		Return the call stack elements, or an empty array if not available.
	**/
	public static function callStack() : Array<StackItem> {
		#if neko
			var a = makeStack(untyped __dollar__callstack());
			a.shift(); // remove Stack.callStack()
			return a;
		#elseif flash
			var a = makeStack( new flash.errors.Error().getStackTrace() );
			a.shift(); // remove Stack.callStack()
			return a;
		#elseif php
			return makeStack("%s");
		#elseif cpp
			var s:Array<String> = untyped __global__.__hxcpp_get_call_stack(true);
			return makeStack(s);
		#elseif js
			try {
				throw new js.Error();
			} catch( e : Dynamic ) {
				var a = getStack(e);
				a.shift(); // remove Stack.callStack()
				return a;
			}

		#elseif java
			var stack = [];
			for ( el in java.lang.Thread.currentThread().getStackTrace() ) {
				var className = el.getClassName();
				var methodName = el.getMethodName();
				var fileName = el.getFileName();
				var lineNumber = el.getLineNumber();
				var method = Method( className, methodName );
				if ( fileName != null || lineNumber >= 0 ) {
					stack.push( FilePos( method, fileName, lineNumber ) );
				}
				else {
					stack.push( method );
				}
			}
			stack.shift();
			stack.shift();
			stack.pop();
			return stack;
		#elseif cs
			return makeStack(new cs.system.diagnostics.StackTrace(1, true));
		#elseif python
			var stack = [];
			var infos = python.lib.Traceback.extract_stack();
			infos.pop();
			infos.reverse();
			for (elem in infos)
				stack.push(FilePos(null, elem._1, elem._2));
			return stack;
		#elseif lua
			var stack = [];
			var infos = lua.Debug.traceback();
			var luastack = infos.split("\n").slice(2,-1);
			for (s in luastack){
				var parts = s.split(":");
				var file  = parts[0];
				var line  = parts[1];
				// TODO: Give more information for FilePos
				stack.push(FilePos(null, file, Std.parseInt(line)));
			}
			return stack;
		#elseif hl
			try {
				throw null;
			} catch( e : Dynamic ) {
				var st = _getExceptionStack();
				return makeStack(st.length > 2 ? st.sub(2,st.length - 2) : st);
			}
		#else
			return []; // Unsupported
		#end
	}

	#if hl
	@:hlNative("std", "exception_stack") static function _getExceptionStack() : hl.NativeArray<hl.Bytes> { return null; }
	#end

	/**
		Return the exception stack : this is the stack elements between
		the place the last exception was thrown and the place it was
		caught, or an empty array if not available.
	**/
	#if cpp @:noDebug #end /* Do not mess up the exception stack */
	public static function exceptionStack() : Array<StackItem> {
		#if neko
			return makeStack(untyped __dollar__excstack());
		#elseif as3
			return new Array();
		#elseif hl
			return makeStack(_getExceptionStack());
		#elseif flash
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
		#elseif php
			return makeStack("%e");
		#elseif cpp
			var s:Array<String> = untyped __global__.__hxcpp_get_exception_stack();
			return makeStack(s);
		#elseif java
			var stack = [];
			for ( el in java.internal.Exceptions.currentException().getStackTrace() ) {
				var className = el.getClassName();
				var methodName = el.getMethodName();
				var fileName = el.getFileName();
				var lineNumber = el.getLineNumber();
				var method = Method( className, methodName );
				if ( fileName != null || lineNumber >= 0 ) {
					stack.push( FilePos( method, fileName, lineNumber ) );
				}
				else {
					stack.push( method );
				}
			}
			// stack.shift();
			stack.shift();
			stack.pop();
			return stack;
		#elseif cs
			return makeStack(new cs.system.diagnostics.StackTrace(cs.internal.Exceptions.exception, true));
		#elseif python
			var stack = [];
			var exc = python.lib.Sys.exc_info();
			if (exc._3 != null)
			{
				var infos = python.lib.Traceback.extract_tb(exc._3);
				infos.reverse();
				for (elem in infos)
					stack.push(FilePos(null, elem._1, elem._2));
			}
			return stack;
		#elseif js
			return untyped __define_feature__("haxe.CallStack.exceptionStack", getStack(lastException));
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
		case LocalFunction(n):
			b.add("local function #");
			b.add(n);
		}
	}

	#if cpp @:noDebug #end /* Do not mess up the exception stack */
	private static function makeStack(s #if cs : cs.system.diagnostics.StackTrace #elseif hl : hl.NativeArray<hl.Bytes> #end) {
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
		#elseif flash
			var a = new Array();
			var r = ~/at ([^\/]+?)\$?(\/[^\(]+)?\(\)(\[(.*?):([0-9]+)\])?/;
			var rlambda = ~/^MethodInfo-([0-9]+)$/g;
			while( r.match(s) ) {
				var cl = r.matched(1).split("::").join(".");
				var meth = r.matched(2);
				var item;
				if( meth == null ) {
					if( rlambda.match(cl) )
						item = LocalFunction(Std.parseInt(rlambda.matched(1)));
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
					m.push(CFunction)
				else if (words.length==2)
					m.push(Method(words[0],words[1]));
				else if (words.length==4)
					m.push(FilePos( Method(words[0],words[1]),words[2],Std.parseInt(words[3])));
			}
			return m;
		#elseif js
			if (s == null) {
				return [];
			} else if ((untyped __js__("typeof"))(s) == "string") {
				// Return the raw lines in browsers that don't support prepareStackTrace
				var stack : Array<String> = s.split("\n");
				if( stack[0] == "Error" ) stack.shift();
				var m = [];
				var rie10 = ~/^   at ([A-Za-z0-9_. ]+) \(([^)]+):([0-9]+):([0-9]+)\)$/;
				for( line in stack ) {
					if( rie10.match(line) ) {
						var path = rie10.matched(1).split(".");
						var meth = path.pop();
						var file = rie10.matched(2);
						var line = Std.parseInt(rie10.matched(3));
						m.push(FilePos( meth == "Anonymous function" ? LocalFunction() : meth == "Global code" ? null : Method(path.join("."),meth), file, line ));
					} else
						m.push(Module(StringTools.trim(line))); // A little weird, but better than nothing
				}
				return m;
			} else {
				return cast s;
			}
		#elseif cs
			var stack = [];
			for (i in 0...s.FrameCount)
			{
				var frame = s.GetFrame(i);
				var m = frame.GetMethod();

				if (m == null) {
					continue;
				}
				var method = StackItem.Method(m.ReflectedType.ToString(), m.Name);

				var fileName = frame.GetFileName();
				var lineNumber = frame.GetFileLineNumber();

				if (fileName != null || lineNumber >= 0)
					stack.push(FilePos(method, fileName, lineNumber));
				else
					stack.push(method);
			}
			return stack;
		#elseif hl
			var stack = [];
			var r = ~/^([A-Za-z0-9.$_]+)\.([A-Za-z0-9_]+)\((.+):([0-9]+)\)$/;
			for( i in 0...s.length-1 ) {
				var str = @:privateAccess String.fromUCS2(s[i]);
				if( r.match(str) )
					stack.push(FilePos(Method(r.matched(1), r.matched(2)), r.matched(3), Std.parseInt(r.matched(4))));
				else
					stack.push(Module(str));
			}
			return stack;
		#else
			return null;
		#end
	}

}
