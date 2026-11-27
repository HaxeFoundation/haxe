package cases.display;

class FindReferencesCachedModules extends TestCase {
	function test(_) {
		var anim = "package p;
class Anim {
	public var frame(default, null):Float;
	public function new() {
		frame = 0;
	}
}";
		vfs.putContent("p/Anim.hx", anim);
		vfs.putContent("p/Smooth.hx", "package p;
class Smooth extends Anim {
	public function new(target:Anim) {
		super();
		this.frame = target.frame;
	}
}");
		vfs.putContent("Main.hx", "class Main {
	static function main() {
		trace(new p.Anim().frame);
	}
}");
		var args = ["-main", "Main", "--interp"];
		runHaxe(args.concat(["p.Smooth"]));
		assertSuccess();
		var result = runHaxeJson(args, DisplayMethods.FindReferences, {file: new FsPath("p/Anim.hx"), offset: anim.indexOf("frame")});
		var files = [for (l in result) haxe.io.Path.withoutDirectory(l.file.toString())];
		files.sort(Reflect.compare);
		Assert.same(["Anim.hx", "Main.hx", "Smooth.hx", "Smooth.hx"], files);
	}
}
