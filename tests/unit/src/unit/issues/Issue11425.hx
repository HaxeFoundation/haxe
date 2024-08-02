package unit.issues;

private enum VariantType {
	VT_String(s:String);
	VT_Float(s:Float);
}

@:transitive
private abstract Variant(VariantType) from VariantType {
	@:from static function fromString(s:String):Variant {
		return VT_String(s);
	}

	@:to public function toString():String {
		if (this == null) {
			return null;
		}
		return switch (this) {
			case VT_String(s): s;
			case VT_Float(s): Std.string(s);
		}
	}

	@:from static function fromFloat(s:Float):Variant {
		return VT_Float(s);
	}

	@:to public function toFloat():Null<Float> {
		if (this == null) {
			return null;
		}
		return switch (this) {
			case VT_Float(s): s;
			default: throw "Variant Type Error (" + this + ")";
		}
	}
}

class Issue11425 extends Test {
	function test() {
		var variant1:Variant = 4.0;

		var testValue1:Float = variant1; // Works fine.
		// discrepency between "... != ... ? ... : ..." and "... ?? ... "
		var testValue2:Float = (variant1 != null) ? variant1 : 1.0; // Works fine.
		var testValue3:Float = variant1 ?? 1.0; // ERROR: Float should be Variant

		// type hint testValue6 as Variant
		var testValue6:Variant = (variant1 != null) ? variant1 : 1.0; // Works fine.
		// type hint testValue7 as Variant
		var testValue7:Variant = variant1 ?? 1.0; // ERROR: Float should be Variant

		// using "cast" to get around compilation and see generated output:
		// though presumably cast is influencing the output also
		var testValue8:Float = (variant1 != null) ? variant1 : 1.0; // Works fine.
		// generated: variant1 != null ? Variant.toFloat(variant1) : 1.0
		var testValue9:Float = variant1 ?? cast 1.0; // Works fine.
		// generated: Variant.toFloat(variant1 != null ? variant1 : 1.0)
		// testValue10 is inferred as Variant
		// and hxcpp does not like casting Float to that
		#if !cpp 
		var testValue10 = variant1 ?? cast 1.0; // Works fine.
		// generated: variant1 != null ? variant1 : 1.0
		#end

		utest.Assert.pass();
	}
}
