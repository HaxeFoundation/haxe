package formatter.codedata;

import formatter.config.WrapConfig;
import formatter.marker.Indenter;

class VerbatimCodeLine extends CodeLine {
	var content:String;

	public function new(content:String) {
		super(0);
		this.content = content;
		verbatim = true;
	}

	override public function applyWrapping(config:WrapConfig, parsedCode:ParsedCode, indenter:Indenter, lineSeparator:String):Array<CodeLine> {
		return [this];
	}

	override public function print(indenter:Indenter, lineSeparator:String):String {
		return content;
	}
}
