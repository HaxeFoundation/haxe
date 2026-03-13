package formatter;

#if (sys || nodejs)
import sys.FileSystem;
import sys.io.File;
#end
import formatter.codedata.CodeLines;
import formatter.codedata.FormatterInputData;
import formatter.config.Config;
import formatter.marker.Indenter;
import formatter.marker.MarkAdditionalIndentation;
import formatter.marker.MarkEmptyLines;
import formatter.marker.MarkLineEnds;
import formatter.marker.MarkSameLine;
import formatter.marker.MarkTokenText;
import formatter.marker.MarkWhitespace;
import formatter.marker.wrapping.MarkWrapping;
import haxe.CallStack;
import haxe.io.Path;
import tokentree.TokenTreeBuilder.TokenTreeEntryPoint;

enum Result {
	Success(formattedCode:String);
	Failure(errorMessage:String);
	Disabled;
}

class Formatter {
	static inline var FORMATTER_JSON:String = "hxformat.json";

	public static function format(input:FormatterInput, ?config:Config, ?lineSeparator:String, ?entryPoint:TokenTreeEntryPoint, ?range:FormatterInputRange,
			?indentOffset:Int):Result {
		if (config == null) {
			config = new Config();
		}
		var inputData:FormatterInputData;
		switch (input) {
			#if (sys || nodejs)
			case FileInput(fileName):
				if (!FileSystem.exists(fileName)) {
					Sys.println('Skipping \'$fileName\' (path does not exist)');
					return Failure('File "$fileName" not found');
				}
				var content:Bytes = File.getBytes(fileName);
				inputData = {
					fileName: fileName,
					content: content,
					config: config,
					lineSeparator: lineSeparator,
					entryPoint: entryPoint,
					range: range,
					indentOffset: indentOffset
				};
				return formatInputData(inputData);
			#end
			case Code(code, origin):
				var content:Bytes = Bytes.ofString(code);
				inputData = {
					fileName: switch (origin) {
						case SourceFile(fileName): fileName;
						case Snippet: "code snippet";
					},
					content: content,
					config: config,
					lineSeparator: lineSeparator,
					entryPoint: entryPoint,
					range: range,
					indentOffset: indentOffset
				};
				return formatInputData(inputData);
			case Tokens(tokenList, tokenTree, code, origin):
				inputData = {
					fileName: switch (origin) {
						case SourceFile(fileName): fileName;
						case Snippet: "code snippet";
					},
					content: code,
					tokenList: tokenList,
					tokenTree: tokenTree,
					config: config,
					lineSeparator: lineSeparator,
					entryPoint: entryPoint,
					range: range,
					indentOffset: indentOffset
				};
				return formatInputData(inputData);
		}
		return Failure("implement me");
	}

	#if (sys || nodejs)
	/**
		Determines the config to be used for a particular `path` (either a directory or a file),
		based on the `hxformat.json` that is closest to it.

		If there is no `hxformat.json`, `null` is returned.
	**/
	public static function loadConfig(path:String):Null<Config> {
		var configFileName:Null<String> = determineConfig(path);
		if (configFileName == null) {
			return null;
		}
		var config = new Config();
		config.readConfig(configFileName);
		return config;
	}
	#end

	static function formatInputData(inputData:FormatterInputData):Result {
		try {
			var config:Config = inputData.config;
			if (config.disableFormatting) {
				return Disabled;
			}
			if (config.isExcluded(inputData.fileName)) {
				return Disabled;
			}

			tokentree.TokenStream.MODE = Relaxed;
			var parsedCode = new ParsedCode(inputData);
			FormatStats.addOrigLines(parsedCode.lines.length);

			var indenter = new Indenter(config.indentation);
			indenter.setParsedCode(parsedCode);
			if (inputData.indentOffset != null) {
				indenter.setIndentOffset(inputData.indentOffset);
			}

			var markTokenText = new MarkTokenText(config, parsedCode, indenter);
			var markWhitespace = new MarkWhitespace(config, parsedCode, indenter);
			var markLineEnds = new MarkLineEnds(config, parsedCode, indenter);
			var markSameLine = new MarkSameLine(config, parsedCode, indenter);
			var markWrapping = new MarkWrapping(config, parsedCode, indenter);
			var markEmptyLines = new MarkEmptyLines(config, parsedCode, indenter);
			var markAdditionalIndent = new MarkAdditionalIndentation(config, parsedCode, indenter);

			markTokenText.run();
			markWhitespace.run();
			markLineEnds.run();
			markSameLine.run();
			markWrapping.run();
			markEmptyLines.run();

			markTokenText.finalRun();
			markAdditionalIndent.run();

			var outputLineEnds:String = MarkLineEnds.outputLineSeparator(config.lineEnds, parsedCode);
			var lines:CodeLines = new CodeLines(parsedCode, indenter, inputData.range);
			lines.applyWrapping(config.wrapping, outputLineEnds);
			markEmptyLines.finalRun(lines);

			var formatted:String = lines.print(outputLineEnds);
			FormatStats.addFormattedLines(formatted.split(outputLineEnds).length);
			return Success(formatted);
		} catch (e:Any) {
			var callstack = CallStack.toString(CallStack.exceptionStack());
			return Failure(e + "\n" + callstack + "\n\n");
		}
	}

	#if (sys || nodejs)
	static function determineConfig(fileName:String):Null<String> {
		var path:String = FileSystem.absolutePath(fileName);
		if (!FileSystem.isDirectory(path)) {
			path = Path.directory(path);
		}
		while (path.length > 0) {
			var configFile:String = Path.join([path, FORMATTER_JSON]);
			if (sys.FileSystem.exists(configFile)) {
				return configFile;
			}
			path = Path.normalize(Path.join([path, ".."]));
		}
		return null;
	}
	#end

	#if (js && !nodejs)
	public static function main() {
		var result:Result = Formatter.format(Code(" trace ( 'foo' ) ; ", Snippet), new Config(), ExpressionLevel);
		switch (result) {
			case Success(formattedCode):
				js.Browser.console.log("Success: " + formattedCode);
			case Failure(errorMessage):
				js.Browser.console.log("Failed to format: " + errorMessage);
			case Disabled:
				js.Browser.console.log("Formatting disabled");
		}
	}
	#end
}

enum FormatterInput {
	#if (sys || nodejs)
	FileInput(fileName:String);
	#end
	Code(code:String, origin:CodeOrigin);
	Tokens(tokenList:Array<Token>, tokenTree:TokenTree, code:Bytes, origin:CodeOrigin);
}

enum CodeOrigin {
	SourceFile(fileName:String);
	Snippet;
}
