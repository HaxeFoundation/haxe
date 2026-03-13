package refactor;

import haxe.PosInfos;
import haxe.Timer;
import refactor.discover.FileList;
import refactor.discover.NameMap;
import refactor.discover.TraverseSources;
import refactor.discover.TypeList;
import refactor.discover.UsageCollector;
import refactor.discover.UsageContext;
import refactor.edits.EditableDocument;
import refactor.rename.RenameWhat;

class Cli {
	var verbose:Bool = false;
	var forReal:Bool = false;
	var exitCode:Int = 0;

	static function main() {
		new Cli();
	}

	function new() {
		var args = Sys.args();

		#if neko
		// JS version is faster (if available)
		try {
			var process = new sys.io.Process("node", ["-v"]);
			var nodeExists = process.exitCode() == 0;
			process.close();
			if (nodeExists && FileSystem.exists("bin/rename.js")) {
				var exitCode = Sys.command("node", ["bin/rename.js"].concat(args));
				Sys.exit(exitCode);
			}
		} catch (e:Any) {}
		#end

		if (Sys.getEnv("HAXELIB_RUN") == "1") {
			if (args.length > 0) {
				Sys.setCwd(args.pop());
			}
		}

		var paths:Array<String> = [];
		var loc:String = "";
		var toName:String = "";
		var help:Bool = false;
		var execute:Bool = false;
		var verboseLog:VerboseLogger = function(text:String, ?pos:PosInfos) {};
		var argHandler = hxargs.Args.generate([
			@doc("file or directory with .hx files (multiple allowed)")
			["-s", "--source"] => function(path:String) paths.push(path),

			@doc("location (path + filename and byte offset from beginning of file) of identifier to rename - <src/pack/Filename.hx@123>")
			["-l"] => function(location:String) loc = location,

			@doc("new name for all occurences of identifier")
			["-n"] => function(newName:String) toName = newName,

			@doc("Print additional information")
			["-v"] => function() verboseLog = logMessage,

			@doc("perform renaming operations")
			["-x"] => function() execute = true,

			@doc("you have a backup and you really, really want to rename")
			["--i-have-backups"] => function() forReal = true,

			@doc("display list of options")
			["-h", "--help"] => function() help = true
		]);

		function printHelp() {
			var version:String = RenameVersion.getRefactorVersion();
			Sys.println('Haxe Rename ${version}');
			Sys.println(argHandler.getDoc());
		}

		try {
			argHandler.parse(args);
		} catch (e:Any) {
			Sys.stderr().writeString(e + "\n");
			printHelp();
			Sys.exit(1);
		}
		if (args.length == 0 || help) {
			printHelp();
			Sys.exit(0);
		}
		var what:Null<RenameWhat> = makeWhat(loc, toName);
		if (what == null) {
			printHelp();
			Sys.exit(1);
		}

		var usageContext:UsageContext = {
			fileReader: simpleFileReader,
			fileName: "",
			file: null,
			usageCollector: new UsageCollector(),
			nameMap: new NameMap(),
			fileList: new FileList(),
			typeList: new TypeList(),
			type: null,
			cache: null // new FileCache()
		};

		var startTime = Timer.stamp();
		TraverseSources.traverseSources(paths, usageContext);
		usageContext.usageCollector.updateImportHx(usageContext);

		var result:Promise<RefactorResult> = Rename.rename({
			nameMap: usageContext.nameMap,
			fileList: usageContext.fileList,
			typeList: usageContext.typeList,
			what: what,
			forRealExecute: execute && forReal,
			docFactory: EditableDocument.new,
			verboseLog: verboseLog,
			typer: null,
			fileReader: null,
			converter: null,
		});
		result.then(function(result:RefactorResult) {
			switch (result) {
				case NoChange:
					Sys.println("nothing to do");
				case NotFound:
					Sys.println("could not find identifier at " + loc);
				case Unsupported(name):
					Sys.println("renaming not supported for " + name);
				case DryRun:
					Sys.println("");
				case Done:
					Sys.println("changes were made");
			}
		}).catchError(function(msg:String) {
			Sys.println('renaming failed $msg');
		}).finally(function() {
			Sys.println(Timer.stamp() - startTime);
			Sys.exit(exitCode);
		});
	}

	function logMessage(text:String, ?pos:PosInfos) {
		Sys.println(text);
	}

	function makeWhat(location:String, toName:String):Null<RenameWhat> {
		var parts:Array<String> = location.split("@");
		if (parts.length != 2) {
			return null;
		}
		if (parts[0].length <= 0) {
			return null;
		}
		var pos:Null<Int> = Std.parseInt(parts[1]);
		if (pos == null) {
			return null;
		}
		return {
			fileName: parts[0],
			toName: toName,
			pos: pos
		}
	}
}
