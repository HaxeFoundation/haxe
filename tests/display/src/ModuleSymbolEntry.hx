// Taken from vshaxe... not ideal to copy it here

@:enum
private abstract ModuleSymbolKind(Int) {
    var MClass = 1;
    var MInterface = 2;
    var MEnum = 3;
    var MTypedef = 4;
    var MAbstract = 5;
    var MField = 6;
    var MProperty = 7;
    var MMethod = 8;
    var MConstructor = 9;
    var MFunction = 10;
    var MVariable = 11;
}

typedef ModuleSymbolEntry = {
    var name:String;
    var kind:ModuleSymbolKind;
    @:optional var containerName:String;
}