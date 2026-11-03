abstract MyInt(Int) from Int to Int {
	@:op(A += B)
	public inline function addEq(x:Int):MyInt {
		return this += x;
	}

	@:op(A++)
	public inline function increment():MyInt {
		return this++;
	}
}

class MyClass {
	final field:MyInt = 1;

	function func():Void {
		final local:MyInt = 1;

		// --- overriden operators ---

		field++; // Error: The field or identifier field is not accessible for writing
		local++;

		field += 1;
		local += 1; // Error: Cannot assign to final

		// --- raw operators ---

		field--; // Error: The field or identifier field is not accessible for writing
		local--; // Error: Cannot assign to final

		field -= 1; // Error: Cannot access field or identifier field for writing
		local -= 1; // Error: Cannot assign to final

		// --- method calls ---

		field.addEq(1);
		local.addEq(1);

		field.increment();
		local.increment();
	}
}

function main() {

}