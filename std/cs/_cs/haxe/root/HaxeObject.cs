using System;
using System.Collections.Generic;

namespace haxe.root
{
    public class HaxeObject
    {
        public HaxeObject() { _hx_ctor(); }

        public virtual void _hx_ctor() { }

        /// <summary>
        /// Get a field by name. Subclasses should override this with a switch statement for AOT compatibility.
        /// Default implementation uses reflection as fallback.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2075",
            Justification = "Fallback reflection - subclasses should override for AOT")]
        public virtual object _hx_getField(string name)
        {
            // Default: use reflection (works in JIT, may fail in AOT for some types)
            var type = this.GetType();
            var field = type.GetField(name, System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
            if (field != null) return field.GetValue(this);
            var prop = type.GetProperty(name, System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
            if (prop != null) return prop.GetValue(this);
            return null;
        }

        /// <summary>
        /// Set a field by name. Subclasses should override this with a switch statement for AOT compatibility.
        /// Default implementation uses reflection as fallback.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2075",
            Justification = "Fallback reflection - subclasses should override for AOT")]
        public virtual void _hx_setField(string name, object value)
        {
            // Default: use reflection (works in JIT, may fail in AOT for some types)
            var type = this.GetType();
            var field = type.GetField(name, System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
            if (field != null) { field.SetValue(this, value); return; }
            var prop = type.GetProperty(name, System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
            if (prop != null) { prop.SetValue(this, value); return; }
        }

        public virtual bool _hx_deleteField(string name)
        {
            return false;
        }

        public virtual Array<string> _hx_getFields()
        {
            return new Array<string>();
        }
    }
}
