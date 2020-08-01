package haxe;

import lua.Lua.select;
import lua.TableTools.maxn;
import lua.TableTools.pack;
import lua.Table;

@:coreType
abstract Rest<T> {
    public var length(get, never):Int;

    inline function get_length():Int {
        return maxn(cast this);
    }

    @:arrayAccess inline function get(index:Int):T {
        return select(index + 1, this);
    }

    @:to public inline function toArray():Array<T> {
        return [for (i in 0...length) get(i)];
    }

    public inline function toString():String {
        return this.toArray().toString();
    }

    public inline function iterator():RestIterator<T> {
        return new RestIterator<T>(cast this);
    }

    public inline function keyValueIterator():RestKeyValueIterator<T> {
        return new RestKeyValueIterator<T>(cast this);
    }
}

private class RestIterator<T> {
    final args:Table<Int, T>;
    var current:Int = 0;

    public inline function new(args:Table<Int, T>) {
        this.args = args;
    }

    public inline function hasNext():Bool {
        return current < maxn(args);
    }

    public inline function next():T {
        var old = current;
        current = current + 1;
        return args[old + 1];
    }
}

private class RestKeyValueIterator<T> {
    final args:Table<Int, T>;
    var current:Int = 0;

    public inline function new(args:Table<Int, T>) {
        this.args = args;
    }

    public inline function hasNext():Bool {
        return current < maxn(args);
    }

    public inline function next():{key:Int, value:T} {
        var old = current;
        current = current + 1;
        return {key: old, value: args[old + 1]};
    }
}
