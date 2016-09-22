package unit.issues;

class Issue5572 extends unit.Test {
	// these two fields have the same hash value
	static var field1 = "baseReward";
	static var field2 = "user_3235_65290";

	function test() {
		#if !(neko || hl)
		var o = {};
		Reflect.setField(o, field1, 1); // first, goes into hashes array 
		Reflect.setField(o, field2, 2); // second, added to object's "conflicts"
		Reflect.setField(o, field2, 3); // should find one from "conflicts" and change the value
		eq(Reflect.field(o, field1), 1); // retrieved from the hashes array
		eq(Reflect.field(o, field2), 3); // retreived from "conflicts"
		var expectedFields = [field1, field2];
		for (field in Reflect.fields(o)) {
			if (!expectedFields.remove(field))
				t(false);
		}
		eq(expectedFields.length, 0);
		Reflect.deleteField(o, field1); // removed from hashes array
		Reflect.deleteField(o, field2); // removed from "conflicts"
		eq(Reflect.field(o, field1), null);
		eq(Reflect.field(o, field2), null);
		eq(Reflect.fields(o).length, 0);
		#end
	}
}
