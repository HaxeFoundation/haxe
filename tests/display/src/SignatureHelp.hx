// from vshaxe

typedef SignatureHelp = {
    var signatures:Array<SignatureInformation>;
    @:optional var activeSignature:Int;
    @:optional var activeParameter:Int;
}

typedef SignatureInformation = {
    var label:String;
    @:optional var documentation:String;
    @:optional var parameters:Array<ParameterInformation>;
}

typedef ParameterInformation = {
    var label:String;
    @:optional var documentation:String;
}