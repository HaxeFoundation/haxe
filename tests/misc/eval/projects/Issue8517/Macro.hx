class Macro {
	macro static public function assignable() return macro Foo.a;
	macro static public function notAssignable() return macro Foo.bar();
}