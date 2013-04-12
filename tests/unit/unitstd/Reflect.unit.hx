// hasField
var x = { a: 1, b: null };
Reflect.hasField(x, "a") == true;
Reflect.hasField(x, "b") == true;
Reflect.hasField(x, "c") == false;

// field
Reflect.field(x, "a") == 1;
Reflect.field(x, "b") == null;
Reflect.field(x, "c") == null;
var c = new C2();
Reflect.field(c, "v") == "var";
Reflect.field(c, "prop") == "prop";
Reflect.field(c, "func")() == "foo";
Reflect.field(c, "propAcc") == "0";
Reflect.field(null, null) == null;
Reflect.field(1, "foo") == null;

// setField
Reflect.setField(x, "a", 2);
x.a == 2;
Reflect.setField(x, "c", "foo");
Reflect.field(x, "c") == "foo";
var c = new C2();
Reflect.setField(c, "v", "bar");
c.v == "bar";
//Reflect.setField(c, "v2", "bar2");
//c.v2 == "bar";
//Reflect.setField(c, "func2", function() return "x");
//Reflect.field(c, "func2")() == "x";

// getProperty
var c = new C2();
Reflect.getProperty(c, "v") == "var";
Reflect.getProperty(c, "prop") == "prop";
//Reflect.getProperty(c, "func")() == "foo";
Reflect.getProperty(c, "propAcc") == "1";
//Reflect.getProperty(null, "a") == null;
//Reflect.getProperty(null, null) == null;

// setProperty
Reflect.setProperty(x, "a", 2);
x.a == 2;
Reflect.setProperty(x, "c", "foo");
Reflect.field(x, "c") == "foo";
var c = new C2();
Reflect.setProperty(c, "v", "bar");
c.v == "bar";
//Reflect.setProperty(c, "v2", "bar2");
//c.v2 == "bar";
//Reflect.setProperty(c, "func2", function() return "x");
//Reflect.field(c, "func2")() == "x";
Reflect.setProperty(c, "propAcc", "abc");
Reflect.field(c, "propAcc") == "ABC";

// fields
var names = ["a", "b", "c"];
for (name in Reflect.fields(x)) {
	names.remove(name);
}
names == [];

// isFunction
var c = new C2();
Reflect.isFunction(function() return 1) == true;
Reflect.isFunction(1) == false;
Reflect.isFunction(null) == false;
Reflect.isFunction(Reflect.field(c, "func")) == true;

// deleteField
Reflect.hasField(x, "c") == true;
Reflect.deleteField(x, "c");
Reflect.hasField(x, "c") == false;
Reflect.deleteField(x, "c");
Reflect.hasField(x, "c") == false;

// copy
var y = Reflect.copy(x);
Reflect.field(y, "a") == 2;
Reflect.field(y, "b") == null;
Reflect.field(y, "c") == null;