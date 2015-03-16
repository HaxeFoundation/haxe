package haxe.ds;

#if js

class FastIteratingStringMap<T> implements haxe.Constraints.IMap<String,T> {
    private static var emptyIterator = {
        hasNext: function() {
            return false;
        },

        next: function() {
            return null;
        }
    };

    private var data:Dynamic;
    private var dataReserved:Dynamic;
    private var head:Dynamic;
    private var tail:Dynamic;

    static function __init__():Void {
        untyped __js__("var __map_reserved = {}");
    }

    public function new():Void {
        data = {};
        dataReserved = {};
        head = null;
        tail = null;
    }

    public function set(key:String, value:T):Void {
        if (untyped __js__("__map_reserved")[key] != null) {
            var _key = "$" + key;

            if (untyped dataReserved.hasOwnProperty(_key)) {
                untyped dataReserved[_key].value = value;
            } else {
                var item = {
                    prev: tail,
                    key: key,
                    value: value,
                    next: null
                };

                untyped dataReserved[_key] = item;

                if (tail != null) {
                    untyped tail.next = item;
                }

                tail = item;

                if (head == null) {
                    head = item;
                }
            }
        } else {
            if (untyped data.hasOwnProperty(key)) {
                untyped data[key].value = value;
            } else {
                var item = {
                    prev: tail,
                    key: key,
                    value: value,
                    next: null
                };

                untyped data[key] = item;

                if (tail != null) {
                    untyped tail.next = item;
                }

                tail = item;

                if (head == null) {
                    head = item;
                }
            }
        }
    }

    public function get(key:String):Null<T> {
        if (untyped __js__("__map_reserved")[key] != null) {
            key = "$" + key;

            if (untyped dataReserved.hasOwnProperty(key)) {
                return untyped dataReserved[key].value;
            } else {
                return null;
            }
        } else {
            if (untyped data.hasOwnProperty(key)) {
                return untyped data[key].value;
            } else {
                return null;
            }
        }
    }

    public inline function exists(key:String):Bool {
        if (untyped __js__("__map_reserved")[key] != null) {
            return untyped dataReserved.hasOwnProperty("$" + key);
        } else {
            return untyped data.hasOwnProperty(key);
        }
    }

    public function remove(key:String):Bool {
        if (untyped __js__("__map_reserved")[key] != null) {
            key = "$" + key;

            if (untyped !dataReserved.hasOwnProperty(key)) {
                return false;
            }

            var item = untyped dataReserved[key];
            var prev:Dynamic = item.prev;
            var next:Dynamic = item.next;

            untyped __js__("delete")(dataReserved[key]);

            if (prev != null) {
                untyped prev.next = next;
            }

            if (next != null) {
                untyped next.prev = prev;
            }

            if (head == item) {
                head = next;
            }

            if (tail == item) {
                tail = prev;
            }

            item.prev = null;
            item.next = null;

            return true;
        } else {
            if (untyped !data.hasOwnProperty(key)) {
                return false;
            }

            var item = untyped data[key];
            var prev:Dynamic = item.prev;
            var next:Dynamic = item.next;

            untyped __js__("delete")(data[key]);

            if (prev != null) {
                untyped prev.next = next;
            }

            if (next != null) {
                untyped next.prev = prev;
            }

            if (head == item) {
                head = next;
            }

            if (tail == item) {
                tail = prev;
            }

            item.prev = null;
            item.next = null;

            return true;
        }
    }

    public function keys():Iterator<String> {
        if (head == null) {
            return untyped emptyIterator;
        }

        return untyped {
            _item: head,

            hasNext: function() {
                return (__this__._item != null);
            },

            next: function() {
                var result = __this__._item.key;
                __this__._item = __this__._item.next;
                return result;
            }
        };
    }

    public function iterator():Iterator<T> {
        if (head == null) {
            return untyped emptyIterator;
        }

        return untyped {
            _item: head,

            hasNext: function() {
                return (__this__._item != null);
            },

            next: function() {
                var result = __this__._item.value;
                __this__._item = __this__._item.next;
                return result;
            }
        };
    }

    public function toString():String {
        var s = new StringBuf();
        s.add("{");

        untyped {
            var item = head;

            while (item != null) {
                s.add(item.key);
                s.add(" => ");
                s.add(Std.string(item.value));

                item = item.next;

                if (item != null) {
                    s.add(", ");
                }
            }
        }

        s.add("}");
        return s.toString();
    }

    /*
    public function toDebugString():String {
        var s = new StringBuf();
        s.add("{");
        s.add(" head: ");
        s.add(head);
        s.add(", tail: ");
        s.add(tail);
        s.add(", data: ");
        s.add(data);
        s.add(", dataReserved: ");
        s.add(dataReserved);
        s.add(" }");
        return s.toString();
    }
    */
}

#else

typedef FastIteratingStringMap<T> = haxe.ds.StringMap<T>;

#end
