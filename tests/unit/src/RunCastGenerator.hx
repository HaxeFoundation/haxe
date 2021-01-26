import sys.io.File;
import haxe.macro.Expr;
import haxe.macro.Printer;

class RunCastGenerator {
	static function main() {
		Sys.println("Starting cast generation...");
		var td = macro class TestNumericCasts extends unit.Test {};
		var intTypes = [macro:Int8, macro:Int16, macro:Int32, macro:Int64];
		var floatTypes = [macro:Float32, macro:Float64];
		var boxedIntTypes = [macro:Null<Int8>, macro:Null<Int16>, macro:Null<Int32>, macro:Null<Int64>];
		var boxedFloatTypes = [macro:Null<Float32>, macro:Null<Float64>];
		var allTypes = intTypes.concat(floatTypes).concat(boxedIntTypes).concat(boxedFloatTypes);
		function getInfo(c:ComplexType) {
			return switch (c) {
				case macro:Int8: {nullable: false, floatable: false, name: "Int8"};
				case macro:Int16: {nullable: false, floatable: false, name: "Int16"};
				case macro:Int32: {nullable: false, floatable: false, name: "Int32"};
				case macro:Int64: {nullable: false, floatable: false, name: "Int64"};
				case macro:Float32: {nullable: false, floatable: true, name: "Float32"};
				case macro:Float64: {nullable: false, floatable: true, name: "Float64"};
				case macro:Null<Int8>: {nullable: true, floatable: false, name: "BoxedInt8"};
				case macro:Null<Int16>: {nullable: true, floatable: false, name: "BoxedInt16"};
				case macro:Null<Int32>: {nullable: true, floatable: false, name: "BoxedInt32"};
				case macro:Null<Int64>: {nullable: true, floatable: false, name: "BoxedInt64"};
				case macro:Null<Float32>: {nullable: true, floatable: true, name: "BoxedFloat32"};
				case macro:Null<Float64>: {nullable: true, floatable: true, name: "BoxedFloat64"};
				case _: throw false;
			}
		}
		var tests = [];
		for (typeFrom in allTypes) {
			for (typeTo in allTypes) {
				if (typeFrom == typeTo) {
					continue;
				}
				var infoFrom = getInfo(typeFrom);
				var infoTo = getInfo(typeTo);
				var name = infoFrom.name + "_" + infoTo.name;
				var dynamicName = "Dynamic" + name;
				var fields = (macro class C {
					static function $name(v : $typeFrom):$typeTo return cast v;
					static function $dynamicName(v : $typeFrom):$typeTo {
						var x:Dynamic = v;
						return x;
					}
				}).fields;
				td.fields.push(fields[0]);
				td.fields.push(fields[1]);
				function generateCalls(name:String) {
					tests.push(macro deq(0, $i{name}(0)));
					tests.push(macro deq(1, $i{name}(1)));
					if (infoFrom.floatable) {
						tests.push(macro deq(0., $i{name}(0.)));
						tests.push(macro deq(1., $i{name}(1.)));
					}
					if (infoFrom.nullable) {
						if (infoTo.nullable) {
							tests.push(macro deq(null, $i{name}(null)));
						} else {
							tests.push(macro deq(CastHelper.nullOr0, $i{name}(null)));
						}
					}
				}
				generateCalls(name);
				generateCalls(dynamicName);
			}
		}
		td.fields = td.fields.concat((macro class C {
			public function test() {
				$b{tests};
			}

			function deq(expected:Dynamic, actual:Dynamic, ?p:haxe.PosInfos) {
				eq(expected, actual, p);
			}
		}).fields);
		var printer = new Printer();
		var buffer = new StringBuf();
		function line(content:String) {
			buffer.add(content);
			buffer.addChar("\n".code);
		}
		line("// This file is auto-generated from RunCastGenerator.hx - do not edit!");
		line("package unit;");
		line("#if java");
		line("import java.StdTypes;");
		line("private typedef Int32 = Int;");
		line("private typedef Float32 = Single;");
		line("private typedef Float64 = Float;");
		line("#else");
		line("private typedef Int8 = Int;");
		line("private typedef Int16 = Int;");
		line("private typedef Int32 = Int;");
		line("private typedef Int64 = Int;");
		line("private typedef Float32 = Float;");
		line("private typedef Float64 = Float;");
		line("#end");
		line("private class CastHelper {");
		line("\tstatic public var nullOr0 = #if target.static 0 #else null #end;");
		line("}");
		line(printer.printTypeDefinition(td));
		File.saveContent("src/unit/TestNumericCasts.hx", buffer.toString());
		Sys.println('Done with cast generation! " Generated ${td.fields.length} functions and ${tests.length} tests.');
	}
}
