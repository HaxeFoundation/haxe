package unit;

#if (!macro && emscripten)
import cpp.link.StaticStd;
import cpp.link.StaticRegexp;
import cpp.link.StaticSqlite;
import cpp.link.StaticZlib;
#end

@:expose("unit.Test")
@:keepSub
#if as3
@:publicFields
#end
class Test {

	public function new() {
	}

	//static var out = sys.io.File.write("debug.txt", false);

	static inline function incrCount(?pos:haxe.PosInfos) {
		++count;
		//out.writeString(pos.methodName +":" +pos.lineNumber + "\n");
	}

	function eq<T>( v : T, v2 : T, ?pos ) {
		incrCount(pos);
		if( v != v2 ) {
			report(Std.string(v)+" should be "+Std.string(v2),pos);
			success = false;
		}
	}

	function feq( v : Float, v2 : Float, ?pos ) {
		incrCount(pos);
		if (!Math.isFinite(v) || !Math.isFinite(v2))
			eq(v, v2, pos);
		else if ( Math.abs(v - v2) > 1e-10 ) {
			report(v+" should be "+v2,pos);
			success = false;
		}
	}

	function aeq<T>(expected:Array<T>, actual:Array<T>, ?pos:haxe.PosInfos) {
		if (expected.length != actual.length) {
			report('Array length differs (${actual.length} should be ${expected.length})', pos);
			success = false;
		} else {
			for (i in 0...expected.length) {
				if (expected[i] != actual[i]) {
					report('[${i}] ${actual[i]} should be ${expected[i]}', pos);
					success = false;
				}
			}
		}
	}

	function t( v, ?pos ) {
		eq(v,true,pos);
	}

	function f( v, ?pos ) {
		eq(v,false,pos);
	}

	function assert( ?pos ) {
		report("Assert",pos);
	}

	function exc( f : Void -> Void, ?pos ) {
		incrCount(pos);
		try {
			f();
			report("No exception occured",pos);
			success = false;
		} catch( e : Dynamic ) {
		}
	}

	function unspec( f : Void -> Void, ?pos ) {
		incrCount(pos);
		try {
			f();
		} catch( e : Dynamic ) {
		}
	}

	function allow<T>( v : T, values : Array<T>, ?pos ) {
		incrCount(pos);
		for( v2 in values )
			if( v == v2 )
				return;
		report(v+" not in "+Std.string(values),pos);
		success = false;
	}

	function hf(c:Class<Dynamic>, n:String, ?pos:haxe.PosInfos) {
		Test.incrCount(pos);
		if (!Lambda.has(Type.getInstanceFields(c), n)) {
			Test.report(Type.getClassName(c) + " should have member field " +n, pos);
			success = false;
		}
	}

	function nhf(c:Class<Dynamic>, n:String, ?pos:haxe.PosInfos) {
		Test.incrCount(pos);
		if (Lambda.has(Type.getInstanceFields(c), n)) {
			Test.report(Type.getClassName(c) + " should not have member field " +n, pos);
			success = false;
		}
	}

	function hsf(c:Class<Dynamic> , n:String, ?pos:haxe.PosInfos) {
		Test.incrCount(pos);
		if (!Lambda.has(Type.getClassFields(c), n)) {
			Test.report(Type.getClassName(c) + " should have static field " +n, pos);
			success = false;
		}
	}

	function nhsf(c:Class<Dynamic> , n:String, ?pos:haxe.PosInfos) {
		Test.incrCount(pos);
		if (Lambda.has(Type.getClassFields(c), n)) {
			Test.report(Type.getClassName(c) + " should not have static field " +n, pos);
			success = false;
		}
	}

	function infos( m : String ) {
		reportInfos = m;
	}

	function async<Args,T>( f : Args -> (T -> Void) -> Void, args : Args, v : T, ?pos : haxe.PosInfos ) : Void {
		if( asyncWaits.length >= AMAX ) {
			asyncCache.push(async.bind(f,args,v,pos));
			return;
		}
		asyncWaits.push(pos);
		f(args,function(v2) {
			incrCount(pos);
			if( !asyncWaits.remove(pos) ) {
				report("Double async result",pos);
				success = false;
				return;
			}
			if( v != v2 ) {
				report(v2+" should be "+v,pos);
				success = false;
			}
			checkDone();
		});
	}

	function asyncExc<Args>( seterror : (Dynamic -> Void) -> Void, f : Args -> (Dynamic -> Void) -> Void, args : Args, ?pos : haxe.PosInfos ) : Void {
		if( asyncWaits.length >= AMAX ) {
		asyncCache.push(asyncExc.bind(seterror,f,args,pos));
			return;
		}
		asyncWaits.push(pos);
		seterror(function(e) {
			incrCount(pos);
			if( asyncWaits.remove(pos) )
				checkDone();
			else {
				report("Multiple async events",pos);
				success = false;
			}
		});
		f(args,function(v) {
			incrCount(pos);
			if( asyncWaits.remove(pos) ) {
				report("No exception occured",pos);
				success = false;
				checkDone();
			} else {
				report("Multiple async events",pos);
				success = false;
			}
		});
	}

	function log( msg, ?pos : haxe.PosInfos ) {
		haxe.Log.trace(msg,pos);
	}

   static function logVerbose(msg:String) {
      #if (cpp || neko || php)
      Sys.println(msg);
      #end
   }

	static var count = 0;
	static var reportInfos = null;
	static var reportCount = 0;
	static var checkCount = 0;
	static var asyncWaits = new Array<haxe.PosInfos>();
	static var asyncCache = new Array<Void -> Void>();
	static var AMAX = 3;
	static var timer : haxe.Timer;
	static var success = true;

	dynamic static function report( msg : String, ?pos : haxe.PosInfos ) {
		if( reportInfos != null ) {
			msg += " ("+reportInfos+")";
			reportInfos = null;
		}
		haxe.Log.trace(msg,pos);
		reportCount++;
#if !(java || cs)
		if( reportCount == 50 ) {
			trace("Too many errors");
			report = function(msg,?pos) {};
		}
#end
	}

	static function checkDone() {
		if( asyncWaits.length != 0 ) return;
		if( asyncCache.length == 0 ) {
			report("DONE ["+count+" tests]");
			report("SUCCESS: " + success);

			//out.close();

			#if js
			if (js.Browser.supported) {
				untyped js.Browser.window.success = success;
			}
			#end

			#if sys
			Sys.exit(success ? 0 : 1);
			#end

			return;
		}
		resetTimer();
		while( asyncCache.length > 0 && asyncWaits.length < AMAX )
			asyncCache.shift()();
	}

	static function asyncTimeout() {
		if( asyncWaits.length == 0 )
			return;
		for( pos in asyncWaits ) {
			report("TIMEOUT",pos);
			success = false;
		}
		asyncWaits = new Array();
		checkDone();
	}

	static function resetTimer() {
		#if (neko || php || cpp || java || cs || python)
		#else
		if( timer != null ) timer.stop();
		timer = new haxe.Timer(10000);
		timer.run = asyncTimeout;
		#end
	}

	static function onError( e : Dynamic, msg : String, context : String ) {
		var msg = "???";
		var stack :String = #if js
			e.stack;
		#else
			haxe.CallStack.toString(haxe.CallStack.exceptionStack());
		#end
		try msg = Std.string(e) catch( e : Dynamic ) {};
		reportCount = 0;
		report("ABORTED : "+msg+" in "+context);
		success = false;
		reportInfos = null;
		trace("STACK :\n"+stack);
	}

	static function main() {
      var verbose = #if ( cpp || neko || php ) Sys.args().indexOf("-v") >= 0 #else false #end;

		#if cs //"Turkey Test" - Issue #996
		cs.system.threading.Thread.CurrentThread.CurrentCulture = new cs.system.globalization.CultureInfo('tr-TR');
		cs.Lib.applyCultureChanges();
		#end
		#if neko
		if( neko.Web.isModNeko )
			neko.Web.setHeader("Content-Type","text/plain");
		#elseif php
		if( php.Web.isModNeko )
			php.Web.setHeader("Content-Type","text/plain");
		#end
		resetTimer();
		#if !macro
		trace("Generated at: " + TestType.getCompilationDate());
		#end
		trace("START");
		#if flash
		var tf : flash.text.TextField = untyped flash.Boot.getTrace();
		tf.selectable = true;
		tf.mouseEnabled = true;
		#end
		var classes = [
			new TestOps(),
			new TestBasetypes(),
			new TestBytes(),
			new TestIO(),
			new TestLocals(),
			new TestEReg(),
			new TestXML(),
			new TestMisc(),
			new TestJson(),
			new TestResource(),
			new TestInt64(),
			new TestReflect(),
			new TestSerialize(),
			new TestMeta(),
			new TestType(),
			new TestOrder(),
			new TestGADT(),
			new TestGeneric(),
			#if !no_pattern_matching
			new TestMatch(),
			#end
			new TestSpecification(),
			#if cs
			new TestCSharp(),
			#end
			#if java
			new TestJava(),
			#end
			#if python
			new TestPython(),
			#end
			#if php
			new TestPhp(),
			#end
			#if (java || cs)
			new TestOverloads(),
			#end
			#if ((dce == "full") && !interp && !as3)
			new TestDCE(),
			#end
			// #if ( (java || neko) && !macro && !interp)
			// new TestThreads(),
			// #end
			//new TestUnspecified(),
			//new TestRemoting(),
		];
		// SPOD tests
		#if ( (neko || (php && (travis || php_sqlite)) || java || cpp || (cs && travis)) && !macro && !interp)
		#if ( travis && !(cpp || cs) )
		if (Sys.getEnv("CI") != null && Sys.systemName() == "Linux")
		{
			classes.push(new TestSpod(sys.db.Mysql.connect({
				host : "localhost",
				user : "travis",
				pass : "",
				port : 3306,
				database : "haxe_test" })));
		}
		#end
      if (verbose)
         logVerbose("Setup sqlite");
		classes.push(new TestSpod(sys.db.Sqlite.open("db.db3")));
		#end
		TestIssues.addIssueClasses("src/unit/issues", "unit.issues");
		TestIssues.addIssueClasses("src/unit/hxcpp_issues", "unit.hxcpp_issues");
		var current = null;
		#if (!fail_eager)
		try
		#end
		{
			asyncWaits.push(null);
			for( inst in classes ) {
				current = Type.getClass(inst);
            if (verbose)
               logVerbose("Class " + Std.string(current) );
				for( f in Type.getInstanceFields(current) )
					if( f.substr(0,4) == "test" ) {
                  if (verbose)
                     logVerbose("   " + f);
						#if fail_eager
						Reflect.callMethod(inst,Reflect.field(inst,f),[]);
						#else
						try {
							Reflect.callMethod(inst,Reflect.field(inst,f),[]);
						}
						#if !as3
						catch( e : Dynamic ) {
							onError(e,"EXCEPTION",Type.getClassName(current)+"."+f);
						}
						#end
						#end
						reportInfos = null;
					}
			}
			asyncWaits.remove(null);
			checkDone();
		}
		#if (!as3 && !(fail_eager))
		catch( e : Dynamic ) {
			asyncWaits.remove(null);
			onError(e,"ABORTED",Type.getClassName(current));
		}
		#end
	}

}
