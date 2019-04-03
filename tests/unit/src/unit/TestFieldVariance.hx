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

private interface PublicFieldInterface {
	public var aField:Int;
}

private interface FinalFieldInterface {
	public final aField:Int;
}

private interface NeverFieldInterface {
	public var aField(default, never):Int;
}

private interface NullFieldInterface {
	public var aField(default, null):Int;
}

private class PublicFieldClass
	implements PublicFieldInterface // ok, same
	implements NullFieldInterface // ok, narrowing access
	implements NeverFieldInterface // ok, narrowing access
	/* implements FinalFieldInterface */ // not ok, final invariance
	{
	public var aField:Int;
}

private class FinalFieldClass
	implements FinalFieldInterface // ok,same
	implements NeverFieldInterface // ok, no write-access
	implements NullFieldInterface // ok, no write-access
	/* implements PublicFieldInterface */ // not ok, final invariance
	{
	public final aField:Int = 0;
}

private class NeverFieldClass
	implements NeverFieldInterface // ok, same
	implements NullFieldInterface // ok, no write-access
	/* implements PublicFieldInterface */ // not ok, widening access
	/* implements FinalFieldInterface */ // not ok, final invariance
	{
	public var aField(default, never):Int;
}

private class NullFieldClass
	implements NullFieldInterface // ok, same
	implements NeverFieldInterface // ok, no write-access
	/* implements PublicFieldInterface */ // not ok, widening access
	/* implements FinalFieldInterface */ // not ok, final invariance
	{
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
		t(typeError(nullField = finalField));
		nullField = neverField;

		// class to final anon

		t(typeError(finalField = publicFieldClass));
		finalField = finalFieldClass;
		t(typeError(finalField = neverFieldClass));
		t(typeError(finalField = nullFieldClass));

		// class to never anon

		neverField = publicFieldClass;
		t(typeError(neverField = finalFieldClass));
		neverField = neverFieldClass;
		neverField = nullFieldClass;

		// class to null anon

		nullField = publicFieldClass;
		t(typeError(nullField = finalFieldClass));
		nullField = neverFieldClass;
		nullField = nullFieldClass;
	}
}