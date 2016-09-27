package unit.issues;

#if php
@:rtti
@somemeta
class ClassWithRttiMeta {

}
#end

class Issue4445 extends Test {
	#if php
	function test() {
		eq(true, haxe.rtti.Rtti.hasRtti(ClassWithRttiMeta));
	}
	#end
}