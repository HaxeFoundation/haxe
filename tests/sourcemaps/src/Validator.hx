#if macro
import validation.Target;
import validation.Lines;
import validation.ValidationReport;
import validation.ValidationError;
import validation.Exception;
import haxe.display.Position.Location;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Compiler;
import haxe.io.Path;

using sys.io.File;
using haxe.macro.Tools;
using StringTools;
#end

class Validator {
	macro static public function expect(hxExpr:Expr, output:Expr):Expr {
		var outMap = try {
			parseExpectedOutput(output);
		} catch(e:ExpectedOutputException) {
			Context.error(e.msg, e.pos);
			return macro {};
		}

		expected.push({pos:hxExpr.pos, out:outMap});

		if(!onAfterGenerateAdded) {
			onAfterGenerateAdded = true;
			Context.onAfterGenerate(verifyExpectations);
		}

		return hxExpr;
	}

#if macro
	static var onAfterGenerateAdded = false;
	static var expected = new Array<{pos:Position, out:Map<Target,String>}>();
	static var sourceMapsCache = new Map<String,SourceMap>();

	static function verifyExpectations() {
		var target = try {
			getCurrentTarget();
		} catch(e:InvalidTargetException) {
			Context.error(e.msg, e.pos);
			return;
		}

		var errors = [];

		for(expectation in expected) {
			var expectedCode = expectation.out.get(target);
			var location = expectation.pos.toLocation();
			var hxPos = location.range.start;
			var generated = getGeneratedContent(target, location);
			var found = false;
			//look for the mappings generated from the `hxPos`
			generated.map.eachMapping(genPos -> {
				if(found) return;
				if(genPos.originalLine == hxPos.line && genPos.originalColumn + 1 == hxPos.character) {
					var code = generated.code[genPos.generatedLine].substr(genPos.generatedColumn);
					//compare with expected output ignoring leading whitespaces
					if(code.trim().startsWith(expectedCode)) {
						found = true;
					}
				}
			});
			if(!found) {
				errors.push({
					expected: expectedCode,
					pos: {
						file: location.file,
						line: hxPos.line,
						column: hxPos.character
					}
				});
			}
		}

		Sys.println(haxe.Json.stringify(buildValidationReport(errors), '    '));
	}

	static function buildValidationReport(errors:Array<ValidationError>):ValidationReport {
		return {
			success: errors.length == 0,
			summary: {
				assertions: expected.length,
				failures: errors.length
			},
			errors: errors
		}
	}

	static function getCurrentTarget():Target {
		return if(Context.defined('js')) {
			Js;
		} else if(Context.defined('php')) {
			Php;
		} else {
			throw new InvalidTargetException('Current target is not supported', haxe.macro.PositionTools.here());
		}
	}

	static function getGeneratedContent(target:Target, location:Location):{code:Lines, map:SourceMap} {
		var sourceFile;
		var mapFile;
		switch(target) {
			case Js:
				sourceFile = Compiler.getOutput();
				mapFile = '${Compiler.getOutput()}.map';
			case Php:
				var phpFilePath = new Path(location.file.toString().substr(location.file.toString().indexOf('cases')));
				phpFilePath.ext = 'php';
				sourceFile = Path.join([Compiler.getOutput(), 'lib', phpFilePath.toString()]);
				mapFile = '$sourceFile.map';
		}

		return {
			code: sourceFile.getContent().split('\n'),
			map: getSourceMap(mapFile)
		}
	}

	static function getSourceMap(mapFile:String):SourceMap {
		var sourceMap = sourceMapsCache.get(mapFile);
		if(sourceMap == null) {
			sourceMap = new SourceMap(mapFile.getContent());
			sourceMapsCache.set(mapFile, sourceMap);
		}
		return sourceMap;
	}

	static function parseExpectedOutput(output:Expr):Null<Map<Target,String>> {
		var result = new Map();
		switch(output) {
			case macro $a{exprs}:
				var expectedTargets = Target.getConstructors();
				for(expr in exprs) {
					switch(expr) {
						case macro $target => ${{expr:EConst(CString(outStr))}}:
							var target = switch(target) {
								case macro Js: Js;
								case macro Php: Php;
								case _: throw new ExpectedOutputException('Invalid target in expected output: ${target.toString()}', target.pos);
							}
							expectedTargets.remove(target.getName());
							result.set(target, outStr);
						case _:
							throw new ExpectedOutputException('Invalid item in map declaration for expected output. Should be `Target => "generated_code"`', expr.pos);
					}
				}
				if(expectedTargets.length > 0) {
					throw new ExpectedOutputException('Missing expected code for targets: ${expectedTargets.join(', ')}.', output.pos);
				}
			case _:
				throw new ExpectedOutputException('The second argument for "expect" should be a map declaration.', output.pos);
		}
		return result;

	}
#end
}