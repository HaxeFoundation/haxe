import pack.BasePack;
import pack.IPack;
import pack.Pvt;

class Main {
	static function main() {

	}
}

class Sub extends BasePack implements IPack {
	override public function test() {
		// should fail, because it allows access to specific package - pack
		Pvt1.testPack();
		Pvt2.testPack();
		// should pass, because it allows access to all instances of pack.BasePack (including descendant classes)
		Pvt2.testClass();
		// should pass, because it allows access to all implementers of pack.IPack
		Pvt2.testInterface();
	}
}