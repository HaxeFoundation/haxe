package refactor.refactor.extractmethod;

interface ICodeGen {
	function makeCallSite():String;
	function makeReturnTypeHint():Promise<String>;
	function makeBody():String;
}
