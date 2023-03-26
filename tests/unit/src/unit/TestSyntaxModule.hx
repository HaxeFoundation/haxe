package unit;

#if js
import js.Syntax;
#elseif php
import php.Syntax;
#elseif python
import python.Syntax;
#elseif cs
import cs.Syntax;
#end

class TestSyntaxModule extends Test {
#if (php || js || python || cs)
	function testCode() {
		var i1 = 1;
		var i2 = 2;
		var result = Syntax.code('{0} + {1}', i1, i2);
		eq(i1 + i2, result);
	}
#end

#if (php || js || python)
	function testField() {
		var o = {field:'hello'};
		var value = Syntax.field(o, 'field');
		eq(o.field, value);
	}

	#if analyzer_optimize
	function testConstruct() {
		var className:String =
			#if php
				#if php_prefix
					'\\${php.Boot.getPrefix()}\\unit\\_TestSyntaxModule\\Construct';
				#else
					'\\unit\\_TestSyntaxModule\\Construct';
				#end
			#elseif js
				#if js_unflatten "unit._TestSyntaxModule.Construct";
				#else "unit__$TestSyntaxModule_Construct";
				#end
			#elseif python "unit__TestSyntaxModule_Construct";
			#end
		var a:Construct = Syntax.construct(className, 10);
		t(Std.isOfType(a, Construct));
		eq(10, a.value);

		var b = Syntax.construct(Construct, 10);
		t(Std.isOfType(b, Construct));
		eq(10, b.value);
	}
	#end
#end

#if (js || cs)
	function testPlainCode() {
		var s = Syntax.plainCode('"{0}"');
		eq('{0}', s);
	}
#end
}

private class Construct {
	public var value:Int;
	public function new(arg:Int) value = arg;
}
