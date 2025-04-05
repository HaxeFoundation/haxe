package unit.issues;

import unit.Test;

class Issue9149 extends Test {
	public function test() {
		eq(-3, new Bleh<-3>().say());
	}
}

@:generic
private class Bleh<@:const Root> {
	public function new() { }
	public function say() return Root;
}