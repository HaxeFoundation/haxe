package unit.issues;

class Issue8978 extends unit.Test {
#if cs
	var classField:cs.types.Int64 = 1;
	var anon:{field:cs.types.Int64} = {field:1};

	function test() {
		var local:cs.types.Int64 = 1;
		var shift = 1;

		classField <<= shift;
		t(2 == classField);
		t(1 == classField >> shift);

		anon.field <<= shift;
		t(2 == anon.field);
		t(1 == anon.field >> shift);

		local <<= shift;
		t(2 == local);
		t(1 == local >> shift);
	}
#end
}