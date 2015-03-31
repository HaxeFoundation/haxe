#if !php
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
// As3 invokes the getter
Reflect.field(c, "propAcc") == #if as3 "1" #else "0" #end;
var n = null;
Reflect.field(n, n) == null;
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
#if !as3
// not supported on AS3
Reflect.field(c, "propAcc") == "ABC";
#end

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

//compare
Reflect.compare(1,2) < 0;
Reflect.compare(2,1) > 0;
Reflect.compare(1,1) == 0;
Reflect.compare("abcd","e") < 0;
Reflect.compare("abcd","abcd") == 0;
Reflect.compare("e","abcd") > 0;
Reflect.compare(null,null) == 0;
Reflect.compare("abcd",null) != 0;
Reflect.compare(null, "abcd") != 0;

// compareMethods
var x = function(t) return 1;
var y = function(t) return -1;
var z = function(t) return 1;
Reflect.compareMethods(x,y) == false;
Reflect.compareMethods(x,z) == false;
Reflect.compareMethods(y,z) == false;
Reflect.compareMethods(x,x) == true;
Reflect.compareMethods(y,y) == true;
Reflect.compareMethods(z,z) == true;
//Reflect.compareMethods(x,null) == false;
//Reflect.compareMethods(null,x) == false;
//Reflect.compareMethods(null,null) == false; // varies

// isObject
Reflect.isObject({}) == true;
Reflect.isObject({v:"f"}) == true;
Reflect.isObject(new C()) == true;
Reflect.isObject(new C2()) == true;
Reflect.isObject(new CChild()) == true;
Reflect.isObject(new CDyn()) == true;
Reflect.isObject(new EmptyClass()) == true;
Reflect.isObject(Type.createEmptyInstance(ReallyEmptyClass)) == true;
Reflect.isObject("foo") == true;
Reflect.isObject(E) == true;
Reflect.isObject(C) == true;

Reflect.isObject(1) == false;
Reflect.isObject(1.1) == false;
Reflect.isObject(true) == false;
Reflect.isObject(EA) == false;
Reflect.isObject(EVMB()) == false;
Reflect.isObject(null) == false;
var x:C = null;
Reflect.isObject(x) == false;

// isEnumValue
Reflect.isEnumValue(EA) == true;
Reflect.isEnumValue(EVMB()) == true;

Reflect.isEnumValue({}) == false;
Reflect.isEnumValue({v:"f"}) == false;
Reflect.isEnumValue(new C()) == false;
Reflect.isEnumValue(new C2()) == false;
Reflect.isEnumValue(new CChild()) == false;
Reflect.isEnumValue(new CDyn()) == false;
Reflect.isEnumValue(new EmptyClass()) == false;
Reflect.isEnumValue(Type.createEmptyInstance(ReallyEmptyClass)) == false;
Reflect.isEnumValue("foo") == false;
Reflect.isEnumValue(E) == false;
Reflect.isEnumValue(C) == false;
Reflect.isEnumValue(1) == false;
Reflect.isEnumValue(1.1) == false;
Reflect.isEnumValue(true) == false;
Reflect.isEnumValue(null) == false;
var x:C = null;
Reflect.isEnumValue(x) == false;
#end