using System;
using System.Collections.Generic;

namespace haxe.lang
{
    public class HaxeObject
    {
        public HaxeObject() { _hx_ctor(); }
        protected HaxeObject(global::haxe.lang.EmptyConstructor _) { }  // No initialization — for createEmptyInstance

        public virtual void _hx_ctor() { }

        // ============================================================
        // Method closure infrastructure
        // ============================================================

        /// <summary>
        /// Cache for method closures. Single array in base class, sized by _hx_methodCount.
        /// Subclasses override _hx_methodCount to return their total method count.
        /// When _hx_methodCount is not overridden (returns 0), caching is disabled and
        /// closures are created fresh on each access.
        /// </summary>
        protected global::haxe.lang.ClassMethodFunction[] _hx_closureCache;

        /// <summary>
        /// Returns the total number of indexable methods for this class (own + all ancestors).
        /// Subclasses with instance methods override this to return their total count.
        /// When not overridden (returns 0), _hx_getMethodClosure skips caching.
        /// </summary>
        protected virtual int _hx_methodCount
        {
            get { return 0; }
        }

        /// <summary>
        /// Gets or creates a method closure for the given method index.
        /// Non-virtual. Fast path checks cache directly (no virtual calls).
        /// Slow path checks _hx_methodCount to decide cache vs no-cache.
        /// Always sets identity fields (_methodTarget + _methodId) for compareMethods.
        /// </summary>
        public global::haxe.lang.ClassMethodFunction _hx_getMethodClosure(int index)
        {
            // Fast path: cache hit — zero virtual calls, zero allocations
            var cache = _hx_closureCache;
            if (cache != null)
            {
                var result = cache[index];
                if (result != null) return result;
            }
            // Slow path: create closure, optionally cache
            return _hx_getMethodClosureMiss(index);
        }

        private global::haxe.lang.ClassMethodFunction _hx_getMethodClosureMiss(int index)
        {
            int count = _hx_methodCount;
            if (count > 0)
            {
                // Cache mode: initialize cache if needed, store closure
                var cache = _hx_closureCache;
                if (cache == null)
                {
                    global::System.Threading.Interlocked.CompareExchange(ref _hx_closureCache,
                        new global::haxe.lang.ClassMethodFunction[count], null);
                    cache = _hx_closureCache;
                }
                var newClosure = _hx_createMethodClosure(index);
                if (newClosure == null) return null;
                newClosure._methodTarget = this;
                newClosure._methodId = index;
                global::System.Threading.Interlocked.CompareExchange(ref cache[index], newClosure, null);
                return cache[index];
            }
            else
            {
                // No-cache mode (when _hx_methodCount is not overridden)
                var newClosure = _hx_createMethodClosure(index);
                if (newClosure != null)
                {
                    newClosure._methodTarget = this;
                    newClosure._methodId = index;
                }
                return newClosure;
            }
        }

        /// <summary>
        /// Factory method for creating method closures. Subclasses override this with a
        /// switch expression that creates ClassMethodFunction instances with direct lambdas.
        /// Each class handles its own method indices, falling through to base for inherited methods.
        /// </summary>
        protected virtual global::haxe.lang.ClassMethodFunction _hx_createMethodClosure(int index)
        {
            return null;
        }

        /// <summary>
        /// Get a field by name. Subclasses should override this with a switch statement for AOT compatibility.
        /// Default implementation uses reflection as fallback.
        /// </summary>
#if !NETSTANDARD
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2075",
            Justification = "Fallback reflection - subclasses should override for AOT")]
#endif
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
#if !NETSTANDARD
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2075",
            Justification = "Fallback reflection - subclasses should override for AOT")]
#endif
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

        /// <summary>
        /// Get all instance field names. Subclasses override this with a hardcoded array in AOT mode.
        /// Default implementation uses reflection as fallback (works in JIT mode).
        /// </summary>
#if !NETSTANDARD
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2075",
            Justification = "Fallback reflection - subclasses should override for AOT")]
#endif
        public virtual global::haxe.root.Array _hx_getFields()
        {
            var type = this.GetType();
            var members = type.GetMembers(
                global::System.Reflection.BindingFlags.Public |
                global::System.Reflection.BindingFlags.Instance);
            var result = new global::haxe.root.Array();
            foreach (var m in members)
            {
                if (m is global::System.Reflection.PropertyInfo) continue;
                if (m is global::System.Reflection.MethodInfo) continue;
                if (m is global::System.Reflection.ConstructorInfo) continue;
                var name = m.Name;
                if (name.StartsWith("_hx_")) continue;
                result.push(name);
            }
            return result;
        }

        public static void _hx_bind() { }

        /// <summary>
        /// Default toString implementation. Subclasses with a Haxe toString() method will override this.
        /// Returns the type name by default.
        /// </summary>
        public virtual string toString()
        {
            return this.GetType().FullName;
        }
    }
}
