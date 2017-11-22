package php.db;

@:native('Mysqli_warning')
extern class Mysqli_warning {
    var message (default,null): String;
    var sqlstate (default,null): Dynamic;
    var errno (default,null): Int;

    function next() : Void;
}