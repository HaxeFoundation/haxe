import haxe.macro.Context;
import sys.FileSystem;

function run() {
	for (cp in Context.getClassPath()) {
		if (cp == "") continue;
		Sys.println(FileSystem.fullPath(cp));
	}
}
