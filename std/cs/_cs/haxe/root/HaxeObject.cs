using System;
using System.Collections.Generic;

namespace haxe.root
{
    public class HaxeObject
    {
        public HaxeObject() { _hx_ctor(); }

        public virtual void _hx_ctor() { }

        public virtual object _hx_getField(string name)
        {
            return null;
        }

        public virtual void _hx_setField(string name, object value)
        {
        }

        public virtual bool _hx_deleteField(string name)
        {
            return false;
        }

        public virtual string[] _hx_getFields()
        {
            return new string[0];
        }
    }
}
