
abstract Grammar<Table<String, String>>{
    public funtion new(name:String, rules : StringMap<String>) {
       this = Table.fromMap(rules);
       this[0] = name;
    }
}
