package unit;

import unit.Test.*;

@:access(unit.Test)
@:expose("unit.TestMain")
@:keep
class TestMain {

	static var asyncWaits = new Array<haxe.PosInfos>();
	static var asyncCache = new Array<Void -> Void>();

	#if js
	static function nodejsMain() {
		main();
		(untyped process).exit(Test.success ? 0 : 1);
	}
	#end

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
		trace("Generated at: " + HelperMacros.getCompilationDate());
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
			#if lua
			new TestLua(),
			#end
			#if python
			new TestPython(),
			#end
			#if hl
			new TestHL(),
			#end
			#if php
			new TestPhp(),
			#end
			#if (java || cs)
			new TestOverloads(),
			#end
			// #if ((dce == "full") && !interp && !as3)
			// new TestDCE(),
			// #end
			// #if ( (java || neko) && !macro && !interp)
			// new TestThreads(),
			// #end
			//new TestUnspecified(),
			//new TestRemoting(),
		];


		#if js
		if (js.Browser.supported) {
			classes.push(new TestJQuery());
		}
		#end

		// SPOD tests
		#if ( (neko || (php && (travis || appveyor || php_sqlite)) || java || cpp || (cs && (travis || appveyor))) && !macro && !interp)
		#if ( (travis || appveyor) && !(cpp || cs) )
		classes.push(new TestSpod(sys.db.Mysql.connect({
			host : "127.0.0.1",
			user : "travis",
			pass : "",
			port : 3306,
			database : "haxe_test" })));
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