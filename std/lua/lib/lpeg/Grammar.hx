package lua.lib.lpeg;
import haxe.ds.StringMap;
abstract Grammar(Table<String,String>){
    public function new(name:String, rules : StringMap<String>) {
       this = Table.fromMap(rules);
       this[0] = name;
    }
}
