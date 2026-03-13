package formatter.marker;

import haxe.PosInfos;
import formatter.codedata.ParsedCode;
import formatter.config.Config;
import formatter.config.WhitespacePolicy;

#if (!macro && !debugLog)
@:build(formatter.debug.PosInfosMacro.clean())
#end
class MarkerBase {
	var config:Config;
	var parsedCode:ParsedCode;
	var indenter:Indenter;

	public function new(config:Config, parsedCode:ParsedCode, indenter:Indenter) {
		this.config = config;
		this.parsedCode = parsedCode;
		this.indenter = indenter;
	}

	function getCloseToken(token:TokenTree):Null<TokenTree> {
		return parsedCode.tokenList.getCloseToken(token);
	}

	function getNextToken(token:TokenTree):Null<TokenInfo> {
		return parsedCode.tokenList.getNextToken(token);
	}

	function getPreviousToken(token:TokenTree):Null<TokenInfo> {
		return parsedCode.tokenList.getPreviousToken(token);
	}

	function getTokenInfo(token:TokenTree):Null<TokenInfo> {
		return parsedCode.tokenList.getTokenAt(token.index);
	}

	function getTokenAt(index:Int):Null<TokenInfo> {
		return parsedCode.tokenList.getTokenAt(index);
	}

	public function whitespace(token:TokenTree, where:WhitespacePolicy, ?pos:PosInfos) {
		parsedCode.tokenList.whitespace(token, where, pos);
	}

	public function spacesAfter(token:TokenTree, count:Int, ?pos:PosInfos) {
		parsedCode.tokenList.spacesAfter(token, count, pos);
	}

	public function spacesBefore(token:TokenTree, count:Int, ?pos:PosInfos) {
		parsedCode.tokenList.spacesBefore(token, count, pos);
	}

	public function lineEndAfter(token:TokenTree, ?pos:PosInfos) {
		parsedCode.tokenList.lineEndAfter(token, pos);
	}

	public function lineEndBefore(token:TokenTree, ?pos:PosInfos) {
		parsedCode.tokenList.lineEndBefore(token, pos);
	}

	public function noLineEndAfter(token:TokenTree, ?pos:PosInfos) {
		parsedCode.tokenList.noLineEndAfter(token, pos);
	}

	public function noLineEndBefore(token:TokenTree, ?pos:PosInfos) {
		parsedCode.tokenList.noLineEndBefore(token, pos);
	}

	public function emptyLinesAfter(token:TokenTree, count:Int, ?pos:PosInfos) {
		parsedCode.tokenList.emptyLinesAfter(token, count, pos);
	}

	public function emptyLinesBefore(token:TokenTree, count:Int, ?pos:PosInfos) {
		parsedCode.tokenList.emptyLinesBefore(token, count, pos);
	}

	public function emptyLinesAfterSubTree(token:TokenTree, count:Int, ?pos:PosInfos) {
		parsedCode.tokenList.emptyLinesAfterSubTree(token, count, pos);
	}

	public function tokenText(token:TokenTree, text:String, ?pos:PosInfos) {
		parsedCode.tokenList.tokenText(token, text, pos);
	}

	public function wrapAfter(token:TokenTree, wrap:Bool, ?pos:PosInfos) {
		parsedCode.tokenList.wrapAfter(token, wrap, pos);
	}

	public function wrapBefore(token:TokenTree, wrap:Bool, ?pos:PosInfos) {
		parsedCode.tokenList.wrapBefore(token, wrap, pos);
	}

	public function noWrappingBetween(tokenStart:TokenTree, tokenEnd:TokenTree, allowCommas:Bool = true, ?pos:PosInfos) {
		parsedCode.tokenList.noWrappingBetween(tokenStart, tokenEnd, config, allowCommas, pos);
	}

	public function additionalIndent(token:TokenTree, indent:Null<Int>, ?pos:PosInfos) {
		parsedCode.tokenList.additionalIndent(token, indent, pos);
	}

	public function increaseIndentBetween(start:Null<TokenTree>, end:Null<TokenTree>, depth:Int, ?pos:PosInfos) {
		parsedCode.tokenList.increaseIndentBetween(start, end, depth, config, parsedCode, indenter, pos);
	}

	public function findTokenAtOffset(offset:Int):Null<TokenInfo> {
		return parsedCode.tokenList.findTokenAtOffset(offset);
	}

	public function isSameLine(first:TokenTree, second:TokenTree):Bool {
		return parsedCode.tokenList.isSameLine(first, second);
	}

	public function calcLength(token:TokenTree):Int {
		return parsedCode.tokenList.calcLength(token);
	}

	public function isMultilineToken(token:TokenTree):Bool {
		return parsedCode.tokenList.isMultilineToken(token);
	}

	public function calcLengthUntilNewline(token:TokenTree, stop:Null<TokenTree>):Int {
		return parsedCode.tokenList.calcLengthUntilNewline(token, stop);
	}

	public function calcLengthBetween(tokenStart:TokenTree, tokenEnd:TokenTree):Int {
		return parsedCode.tokenList.calcLengthBetween(tokenStart, tokenEnd);
	}

	public function calcLineLength(token:TokenTree):Int {
		return parsedCode.tokenList.calcLineLength(token);
	}

	public function calcLineLengthBefore(token:TokenTree):Int {
		return parsedCode.tokenList.calcLineLengthBefore(token);
	}

	public function calcLineLengthAfter(token:TokenTree):Int {
		return parsedCode.tokenList.calcLineLengthAfter(token);
	}

	public function calcTokenLength(token:TokenTree):Int {
		return parsedCode.tokenList.calcTokenLength(token);
	}

	public function isNewLineBefore(token:TokenTree):Bool {
		return parsedCode.tokenList.isNewLineBefore(token);
	}

	public function isNewLineAfter(token:TokenTree):Bool {
		return parsedCode.tokenList.isNewLineAfter(token);
	}

	public function isSameLineBetween(tokenStart:TokenTree, tokenEnd:TokenTree, exclude:Bool):Bool {
		return parsedCode.tokenList.isSameLineBetween(tokenStart, tokenEnd, exclude);
	}

	public function findLineStartToken(token:Null<TokenTree>):Null<TokenTree> {
		return parsedCode.tokenList.findLineStartToken(token);
	}
}
