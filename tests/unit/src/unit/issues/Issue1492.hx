package unit.issues;

class Issue1492 extends Test {
function test() {
eq("foo", myFunc());
var d:Dynamic = this;
eq("foo", d.myNativeFunc());

eq("bar", Issue1492.myStaticVar);
var d:Dynamic = Issue1492;
eq("bar", d.myNativeStaticVar);
}

@:native("myNativeFunc")
function myFunc() {
return "foo";
}

@:native("myNativeStaticVar")
static var myStaticVar = "bar";
}
