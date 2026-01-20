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
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2075",
            Justification = "Fallback reflection - subclasses should override for AOT")]
        public virtual object _hx_getField(string name)
        {
            // Default: use reflection (works in JIT, may fail in AOT for some types)
            var type = this.GetType();
            var field = type.GetField(name, global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Instance);
            if (field != null) return field.GetValue(this);
            var prop = type.GetProperty(name, global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Instance);
            if (prop != null) return prop.GetValue(this);
            return null;
        }

        /// <summary>
        /// Set a field by name. Subclasses should override this with a switch statement for AOT compatibility.
        /// Default implementation uses reflection as fallback.
        /// </summary>
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2075",
            Justification = "Fallback reflection - subclasses should override for AOT")]
        public virtual void _hx_setField(string name, object value)
        {
            // Default: use reflection (works in JIT, may fail in AOT for some types)
            var type = this.GetType();
            var field = type.GetField(name, global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Instance);
            if (field != null) { field.SetValue(this, value); return; }
            var prop = type.GetProperty(name, global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Instance);
            if (prop != null) { prop.SetValue(this, value); return; }
        }

        public virtual bool _hx_deleteField(string name)
        {
            return false;
        }

        public virtual global::haxe.root.Array<string> _hx_getFields()
        {
            return new global::haxe.root.Array<string>();
        }

        // ============================================================
        // Method invocation dispatchers for MethodClosure
        // Subclasses should override these to dispatch by method index
        // ============================================================

        public virtual global::haxe.lang.FunctionValue _hx_invokeMethod0(int index)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found");
        }

        public virtual global::haxe.lang.FunctionValue _hx_invokeMethod1(int index, global::haxe.lang.FunctionValue a1)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found");
        }

        public virtual global::haxe.lang.FunctionValue _hx_invokeMethod2(int index, global::haxe.lang.FunctionValue a1, global::haxe.lang.FunctionValue a2)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found");
        }

        public virtual global::haxe.lang.FunctionValue _hx_invokeMethod3(int index, global::haxe.lang.FunctionValue a1, global::haxe.lang.FunctionValue a2, global::haxe.lang.FunctionValue a3)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found");
        }

        public virtual global::haxe.lang.FunctionValue _hx_invokeMethod4(int index, global::haxe.lang.FunctionValue a1, global::haxe.lang.FunctionValue a2, global::haxe.lang.FunctionValue a3, global::haxe.lang.FunctionValue a4)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found");
        }

        public virtual global::haxe.lang.FunctionValue _hx_invokeMethod5(int index, global::haxe.lang.FunctionValue a1, global::haxe.lang.FunctionValue a2, global::haxe.lang.FunctionValue a3, global::haxe.lang.FunctionValue a4, global::haxe.lang.FunctionValue a5)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found");
        }

        public virtual global::haxe.lang.FunctionValue _hx_invokeMethod6(int index, global::haxe.lang.FunctionValue a1, global::haxe.lang.FunctionValue a2, global::haxe.lang.FunctionValue a3, global::haxe.lang.FunctionValue a4, global::haxe.lang.FunctionValue a5, global::haxe.lang.FunctionValue a6)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found");
        }

        public virtual global::haxe.lang.FunctionValue _hx_invokeMethod7(int index, global::haxe.lang.FunctionValue a1, global::haxe.lang.FunctionValue a2, global::haxe.lang.FunctionValue a3, global::haxe.lang.FunctionValue a4, global::haxe.lang.FunctionValue a5, global::haxe.lang.FunctionValue a6, global::haxe.lang.FunctionValue a7)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found");
        }

        public virtual global::haxe.lang.FunctionValue _hx_invokeMethod8(int index, global::haxe.lang.FunctionValue a1, global::haxe.lang.FunctionValue a2, global::haxe.lang.FunctionValue a3, global::haxe.lang.FunctionValue a4, global::haxe.lang.FunctionValue a5, global::haxe.lang.FunctionValue a6, global::haxe.lang.FunctionValue a7, global::haxe.lang.FunctionValue a8)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found");
        }

        public virtual global::haxe.lang.FunctionValue _hx_invokeMethod9(int index, global::haxe.lang.FunctionValue a1, global::haxe.lang.FunctionValue a2, global::haxe.lang.FunctionValue a3, global::haxe.lang.FunctionValue a4, global::haxe.lang.FunctionValue a5, global::haxe.lang.FunctionValue a6, global::haxe.lang.FunctionValue a7, global::haxe.lang.FunctionValue a8, global::haxe.lang.FunctionValue a9)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found");
        }

        /// <summary>
        /// Fallback for methods with 10+ arguments. Uses object[] allocation.
        /// </summary>
        public virtual object _hx_invokeMethodDynamic(int index, global::haxe.root.Array<object> args)
        {
            throw new global::System.NotImplementedException($"Method index {index} not found (dynamic)");
        }
    }
}
