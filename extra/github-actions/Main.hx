package;

import sys.FileSystem;
import sys.io.File;
import haxe.io.Path;
using StringTools;

class Main {

	static final matchImport = ~/^([ \t]*)@import (.+)$/gm;
	static final matchRunnable = ~/^([ \t]*)jobs:/gm;

	static function main():Void new Main();

	function new() {
		final folder = FileSystem.absolutePath(".");
		final outFolder = "../../.github";

		iterFolderItems(folder, (dir, name) -> {
			final ext = Path.extension(name);
			if (ext != "yaml" && ext != "yml") return;

			final data = File.getContent('$dir/$name');
			var newData = matchImport.map(data, reg -> {
				final spaces = reg.matched(1);
				final path = reg.matched(2);
				final template = File.getContent('./$path');
				final lines = template.split("\n");
				for (i in 0...lines.length) lines[i] = spaces + lines[i];
				lines.join("\n");
			});

			if (!matchRunnable.match(newData)) return;
			final first = "# DO NOT EDIT. Generated from /extra/github-actions\n";
			newData = first + newData;
			final relativeDir = dir.replace(folder, "");
			File.saveContent('$outFolder$relativeDir/$name', newData);
		});
	}

	function iterFolderItems(dir:String, func:(dir:String, name:String)->Void):Void {
		for (name in FileSystem.readDirectory(dir)) {
			if (FileSystem.isDirectory(name)) iterFolderItems('$dir/$name', func);
			func(dir, name);
		}
	}

}
