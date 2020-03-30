package unit;

import utest.ui.Report;
import utest.Runner;
import unit.Test.*;
import haxe.ds.List;

@:access(unit.Test)
@:expose("unit.TestMain")
@:keep
class TestMain {

	static var asyncWaits = new Array<haxe.PosInfos>();
	static var asyncCache = new Array<Void -> Void>();

	static function main() {
		#if js
		if (js.Browser.supported) {
			var oTrace = haxe.Log.trace;
			var traceElement = js.Browser.document.getElementById("haxe:trace");
			haxe.Log.trace = function(v, ?infos) {
				oTrace(v, infos);
				traceElement.innerHTML += infos.fileName + ":" + infos.lineNumber + ": " + StringTools.htmlEscape(v) + "<br/>";
			}
		}
		#end

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
			new TestExceptions(),
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
			new TestSerializerCrossTarget(),
			new TestMeta(),
			new TestType(),
			new TestOrder(),
			new TestGADT(),
			new TestGeneric(),
			new TestArrowFunctions(),
			new TestCasts(),
			new TestSyntaxModule(),
			new TestNull(),
			new TestNumericCasts(),
			new TestHashMap(),
			#if (!no_http && (!azure || !(php && Windows)))
			new TestHttp(),
			#end
			#if !no_pattern_matching
			new TestMatch(),
			#end
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
			new TestInterface(),
			new TestNaN(),
			#if ((dce == "full") && !interp)
			new TestDCE(),
			#end
			new TestMapComprehension(),
			new TestMacro(),
			new TestKeyValueIterator(),
			new TestFieldVariance()
			//new TestUnspecified(),
			//new TestRemoting(),
		];

		for (specClass in unit.UnitBuilder.generateSpec("src/unitstd")) {
			classes.push(specClass);
		}
		TestIssues.addIssueClasses("src/unit/issues", "unit.issues");
		TestIssues.addIssueClasses("src/unit/hxcpp_issues", "unit.hxcpp_issues");

		var runner = new Runner();
		for (c in classes) {
			runner.addCase(c);
		}
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		var success = true;
		runner.onProgress.add(function(e) {
			for(a in e.result.assertations) {
				switch a {
					case Success(pos):
					case Warning(msg):
					case Ignore(reason):
					case _: success = false;
				}
			}
			#if js
			if (js.Browser.supported && e.totals == e.done) {
				untyped js.Browser.window.success = success;
			};
			#end
		});
		#if sys
		if (verbose)
			runner.onTestStart.add(function(test) {
				Sys.println(' $test...'); // TODO: need utest success state for this
			});
		#end
		runner.run();
	}
}
