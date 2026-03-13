package formatter.config;

#if (sys || nodejs)
import sys.FileSystem;
import sys.io.File;
#end
import json2object.JsonParser;

class Config {
	public var emptyLines:EmptyLinesConfig;
	public var indentation:IndentationConfig;
	public var lineEnds:LineEndConfig;
	public var sameLine:SameLineConfig;
	public var whitespace:WhitespaceConfig;
	public var wrapping:WrapConfig;
	public var disableFormatting:Bool;
	public var excludes:Array<EReg>;
	public var configFileName:String;

	public function new() {
		excludes = [];
		readConfigFromString("{}", "hxformat.json");
	}

	#if (sys || nodejs)
	public function readConfig(fileName:String) {
		if (!FileSystem.exists(fileName)) {
			return;
		}
		readConfigFromString(File.getContent(fileName), fileName);
	}
	#end

	public function readConfigFromString(jsonContent:String, fileName:String) {
		var parser:JsonParser<FormatterConfig> = new JsonParser<FormatterConfig>();
		var data:FormatterConfig = parser.fromJson(jsonContent, fileName);
		emptyLines = data.emptyLines;
		indentation = data.indentation;
		lineEnds = data.lineEnds;
		sameLine = data.sameLine;
		whitespace = data.whitespace;
		wrapping = data.wrapping;
		disableFormatting = data.disableFormatting;
		excludes = [];
		configFileName = fileName;
		for (exclude in data.excludes) {
			excludes.push(new EReg(exclude, ""));
		}
	}

	public function isExcluded(fileName:String):Bool {
		for (exclude in excludes) {
			if (exclude.match(fileName)) {
				return true;
			}
		}
		return false;
	}
}
