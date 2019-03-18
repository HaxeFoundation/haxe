package unit;

import unit.HelperMacros.typeError;

private typedef PublicField = {
	var aField:Int;
}

private typedef FinalField = {
	final aField:Int;
}

private typedef NeverField = {
	var aField(default, never):Int;
}

private typedef NullField = {
	var aField(default, null):Int;
}

private class PublicFieldClass {
	public var aField:Int;
}


private class FinalFieldClass {
	public final aField:Int = 0;
}

private class NeverFieldClass {
	public var aField(default, never):Int;
}

private class NullFieldClass {
	public var aField(default, null):Int;
}

class TestFieldVariance extends Test {
	public function test() {
		var publicField:PublicField = null;
		var finalField:FinalField = null;
		var neverField:NeverField = null;
		var nullField:NullField = null;

		var publicFieldClass:PublicFieldClass = null;
		var finalFieldClass:FinalFieldClass = null;
		var neverFieldClass:NeverFieldClass = null;
		var nullFieldClass:NullFieldClass = null;

		// anon to final anon

		t(typeError(finalField = publicField));
		finalField = neverField;
		t(typeError(finalField = nullField));

		// anon to never anon

		neverField = publicField;
		neverField = finalField;
		neverField = nullField;

		// anon to null anon

		nullField = publicField;
		nullField = finalField;
		nullField = neverField;

		// class to final anon

		t(typeError(finalField = publicFieldClass));
		finalField = finalFieldClass;
		t(typeError(finalField = neverFieldClass));
		t(typeError(finalField = nullFieldClass));

		// class to never anon

		neverField = publicFieldClass;
		neverField = finalFieldClass;
		neverField = neverFieldClass;
		neverField = nullFieldClass;

		// class to null anon

		nullField = publicFieldClass;
		nullField = finalFieldClass;
		nullField = neverFieldClass;
		nullField = nullFieldClass;
	}
}