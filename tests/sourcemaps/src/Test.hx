import haxe.Json;
import validation.ValidationReport;
import haxe.io.Path;
import sys.io.Process;
import validation.Target;

using sys.FileSystem;
using StringTools;
using Test;
using Lambda;

class Test {
	static public function main() {
		installDependencies();

		var success = true;
		for(target in [Js, Php]) {
			Sys.println('[Testing ${target.getName()}]');
			collectCases().iter(module -> success = runCase(module, target) && success);
		}
		// the same cases with the map inside the JS output, as a data: URL: with -D source-map
		// too (inline takes precedence), and on its own (it makes the map by itself)
		Sys.println('[Testing Js with -D source-map and -D source-map-inline]');
		collectCases().iter(module -> success = runCase(module, Js, ['-D', 'source-map-inline']) && success);
		Sys.println('[Testing Js with -D source-map-inline alone]');
		collectCases().iter(module -> success = runCase(module, Js, ['-D', 'source-map-inline'], false) && success);

		Sys.exit(success ? 0 : 1);
	}

	static function runCase(module:String, target:Target, ?extraFlags:Array<String>, sourceMap:Bool = true):Bool {
		var proc = new Process(
			'haxe',
			targetCompilerFlags(target, extraFlags != null).concat(extraFlags == null ? [] : extraFlags).concat(sourceMap ? ['-D', 'source-map'] : []).concat([
				'-cp', 'src',
				'-lib', 'sourcemap',
				'-D', 'analyzer-optimize',
				'-dce', 'full',
				'-main', module
			])
		);
		var stdErr = proc.stderr.readAll().toString();
		var stdOut = proc.stdout.readAll().toString();
		if(stdErr.trim().length > 0) {
			Sys.println(stdErr.trim());
		}

		var report:ValidationReport = try {
			Json.parse(stdOut);
		} catch(e:Dynamic) {
			Sys.println('$module: Failed to parse compilation result:\n$stdOut');
			return false;
		}

		Sys.println('$module: ${report.summary.assertions} tests, ${report.summary.failures} failures');

		if(!report.success) {
			for(error in report.errors) {
				Sys.println('${error.pos.file}:${error.pos.line}:${error.pos.column}: Failed to find expected code: ${error.expected}');
			}
		}

		return report.success;
	}

	static function targetCompilerFlags(target:Target, inlineMap:Bool = false):Array<String> {
		return switch(target) {
			case Js: ['-js', inlineMap ? 'bin/test-inline.js' : 'bin/test.js'];
			case Php: ['-php', 'bin/php'];
		}
	}

	static function collectCases():Array<String> {
		var result = [];
		for(fileName in 'src/cases'.readDirectory()) {
			var path = new Path(fileName);
			if(path.isTestCase()) {
				result.push('cases.${path.file}');
			}
		}
		return result;
	}

	static function isTestCase(path:Path):Bool {
		var firstChar = path.file.charAt(0);
		return path.ext == 'hx' && firstChar == firstChar.toUpperCase();
	}

	static function installDependencies() {
		Sys.command('haxelib', ['install', 'sourcemap']);
	}
}