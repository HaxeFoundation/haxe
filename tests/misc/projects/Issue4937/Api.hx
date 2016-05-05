import Some;

interface Api {
    function getResult():Result;
    function test():Other;
}

typedef Result = {
    var id:Int;
    var name:String;
}
