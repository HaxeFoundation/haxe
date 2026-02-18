using System;
using System.Collections.Generic;

namespace haxe.lang
{
    public class HaxeDynamicObject : global::haxe.lang.HaxeObject
    {
        private static int __hx_toString_depth = 0;

        private global::System.Collections.Generic.Dictionary<string, object> _hx_fields;

        public HaxeDynamicObject()
        {
            _hx_fields = new global::System.Collections.Generic.Dictionary<string, object>();
        }

        // Factory method to create with initial field values
        // Usage: _hx_create(["field1", value1, "field2", value2, ...])
        public static global::haxe.lang.HaxeDynamicObject _hx_create(global::haxe.root.Array args)
        {
            var obj = new global::haxe.lang.HaxeDynamicObject();
            for (int i = 0; i < args.length; i += 2)
            {
                var name = (string)args.__objectArray[i];
                var value = args.__objectArray[i + 1];
                obj._hx_setField(name, value);
            }
            return obj;
        }

        public override string ToString()
        {
            if (__hx_toString_depth >= 5)
            {
                return "...";
            }
            ++__hx_toString_depth;
            _hx_initFields();
            var buf = "{";
            bool first = true;
            try
            {
                foreach (var key in _hx_fields.Keys)
                {
                    if (first)
                        first = false;
                    else
                        buf += ", ";
                    buf += key;
                    buf += ": ";
                    buf += _hx_fields[key];
                }
            }
            catch (global::System.Exception)
            {
                --__hx_toString_depth;
                throw;
            }
            --__hx_toString_depth;
            buf += "}";
            return buf;
        }

        /// <summary>
        /// Haxe toString() method - checks for custom toString field first.
        /// This is called by Cs.toString() for string conversion.
        /// </summary>
        public override string toString()
        {
            // Check if there's a toString field that is a function
            object toStringField = _hx_getField("toString");
            if (toStringField is global::haxe.lang.Function f)
            {
                var result = f.__hx_invoke0().toDynamic();
                return global::haxe.lang.Runtime.toStr(result);
            }
            // Fallback to default ToString() which formats as {field: value, ...}
            return ToString();
        }

        public override bool _hx_deleteField(string name)
        {
            _hx_initFields();
            return _hx_fields.Remove(name);
        }

        public override global::haxe.root.Array _hx_getFields()
        {
            _hx_initFields();
            var result = new global::haxe.root.Array();
            foreach (var key in _hx_fields.Keys)
            {
                result.push(key);
            }
            return result;
        }

        public override object _hx_getField(string name)
        {
            _hx_initFields();
            object value;
            _hx_fields.TryGetValue(name, out value);
            return value;
        }

        public bool _hx_hasField(string name)
        {
            _hx_initFields();
            return _hx_fields.ContainsKey(name);
        }

        public override void _hx_setField(string name, object value)
        {
            _hx_initFields();
            _hx_fields[name] = value;
        }

        private void _hx_initFields()
        {
            if (_hx_fields == null)
            {
                _hx_fields = new global::System.Collections.Generic.Dictionary<string, object>();
            }
        }

        public static new void _hx_bind() { }
    }
}
