// Haxe C# Runtime Support
using System;

namespace haxe.lang
{
    /// <summary>
    /// Low-level C# runtime support for the Haxe C# target.
    ///
    /// This class provides AOT-safe implementations for operations that require
    /// direct C# code for correctness, performance, or .NET type system handling.
    ///
    /// Responsibilities:
    /// - Type conversions (toInt, toDouble, toBool, toLong)
    /// - AOT-safe field access (getField, setField)
    /// - Function invocation with argument handling (invokeFunction)
    /// - Null&lt;T&gt; creation helpers for AOT (createDefaultValue, createNullOfT)
    /// - Checked cast implementation (genericCast&lt;T&gt;)
    ///
    /// For high-level Haxe semantic operations (dynamic arithmetic, array access),
    /// see cs.Cs which is written in Haxe for maintainability.
    ///
    /// INVARIANT: haxe.lang.Null&lt;T&gt; values must NEVER be boxed into object.
    /// The code generator (gencs.ml) ensures all Null&lt;T&gt; to object transitions
    /// use .toDynamic(), which produces a properly boxed T value (or null).
    /// If a boxed Null&lt;T&gt; reaches these conversion functions, it indicates
    /// a bug in the OCaml code generator that must be fixed there.
    /// </summary>
    public static class Runtime
    {
        /// <summary>
        /// Converts a dynamic value to int, handling null and type conversions.
        /// </summary>
        public static int toInt(object d)
        {
            if (d == null) return 0;
            if (d is int i) return i;
            if (d is double dbl) return (int)dbl;
            if (d is float f) return (int)f;
            if (d is long l) return (int)l;
            if (d is IConvertible c) return c.ToInt32(null);
            return 0;
        }

        /// <summary>
        /// Just an overload to make sure that calling toInt on int
        /// won't cause any unexpected boxing.
        /// </summary>
        public static int toInt(int i)
        {
            return i;
        }

        /// <summary>
        /// Just an overload to make sure that calling toInt on long
        /// won't cause any unexpected boxing.
        /// </summary>
        public static int toInt(long l)
        {
            return (int)l;
        }

        /// <summary>
        /// Just an overload to make sure that calling toInt on double
        /// won't cause any unexpected boxing.
        /// </summary>
        public static int toInt(double d)
        {
            return (int)d;
        }

        /// <summary>
        /// Just an overload to make sure that calling toInt on float
        /// won't cause any unexpected boxing.
        /// </summary>
        public static int toInt(float f)
        {
            return (int)f;
        }

        /// <summary>
        /// Converts a dynamic value to double, handling null and type conversions.
        /// </summary>
        public static double toDouble(object d)
        {
            if (d == null) return 0.0;
            if (d is double dbl) return dbl;
            if (d is float f) return (double)f;
            if (d is int i) return (double)i;
            if (d is long l) return (double)l;
            if (d is IConvertible c) return c.ToDouble(null);
            return 0.0;
        }

        /// <summary>
        /// Just an overload to make sure that calling toDouble on double
        /// won't cause any unexpected boxing.
        /// </summary>
        public static double toDouble(double d)
        {
            return d;
        }

        /// <summary>
        /// Just an overload to make sure that calling toDouble on int
        /// won't cause any unexpected boxing.
        /// </summary>
        public static double toDouble(float f)
        {
            return (double)f;
        }

        /// <summary>
        /// Just an overload to make sure that calling toDouble on long
        /// won't cause any unexpected boxing.
        /// </summary>
        public static double toDouble(long l)
        {
            return (double)l;
        }

        /// <summary>
        /// Just an overload to make sure that calling toDouble on float
        /// won't cause any unexpected boxing.
        /// </summary>
        public static double toDouble(int i)
        {
            return (double)i;
        }

        /// <summary>
        /// Converts a dynamic value to bool, handling null.
        /// </summary>
        public static bool toBool(object d)
        {
            if (d == null) return false;
            if (d is bool b) return b;
            return false;
        }

        /// <summary>
        /// Just an overload to make sure that calling toBool on bool
        /// won't cause any unexpected boxing.
        /// </summary>
        public static bool toBool(bool b)
        {
            return b;
        }

        /// <summary>
        /// Converts a dynamic value to long, handling null and type conversions.
        /// Used for the _l suffix return methods in FunctionArg pattern.
        /// Numeric types are stored as their bit representation in long.
        /// </summary>
        public static long toLong(object d)
        {
            if (d == null) return 0L;
            if (d is long l) return l;
            if (d is int i) return i;
            if (d is double dbl) return (long)dbl;
            if (d is float f) return (long)f;
            if (d is bool b) return b ? 1L : 0L;
            if (d is IConvertible c) return c.ToInt64(null);
            return 0L;
        }

        /// <summary>
        /// Just an overload to make sure that calling toLong on long
        /// won't cause any unexpected boxing.
        /// </summary>
        public static long toLong(long l)
        {
            return l;
        }

        /// <summary>
        /// Just an overload to make sure that calling toLong on int
        /// won't cause any unexpected boxing.
        /// </summary>
        public static long toLong(int i)
        {
            return i;
        }

        /// <summary>
        /// Just an overload to make sure that calling toLong on double
        /// won't cause any unexpected boxing.
        /// </summary>
        public static long toLong(double d)
        {
            return (long)d;
        }

        /// <summary>
        /// Just an overload to make sure that calling toLong on float
        /// won't cause any unexpected boxing.
        /// </summary>
        public static long toLong(float f)
        {
            return (long)f;
        }

        /// <summary>
        /// Converts a dynamic value to string, handling null and boxed primitives.
        /// Direct cast (string)obj fails when obj is a boxed int/long/bool.
        /// This method handles all cases: null returns null, strings pass through,
        /// other types use ToString().
        /// </summary>
        public static string toStr(object d)
        {
            if (d == null) return null;
            if (d is string s) return s;
            if (d is bool b) return b ? "true" : "false";  // Lowercase for Haxe/JSON compatibility
            if (d is double dbl) return dbl.ToString(global::System.Globalization.CultureInfo.InvariantCulture);
            if (d is float flt) return flt.ToString(global::System.Globalization.CultureInfo.InvariantCulture);
            return d.ToString();
        }

        /// <summary>
        /// Converts object to string for Haxe string concatenation.
        /// Unlike toStr, this returns the literal string "null" for null inputs,
        /// matching Haxe semantics where "hello" + null produces "hellonull".
        /// </summary>
        public static string toStrConcat(object d)
        {
            if (d == null) return "null";
            if (d is string s) return s;
            if (d is bool b) return b ? "true" : "false";  // Lowercase for Haxe/JSON compatibility
            if (d is double dbl) return dbl.ToString(global::System.Globalization.CultureInfo.InvariantCulture);
            if (d is float flt) return flt.ToString(global::System.Globalization.CultureInfo.InvariantCulture);
            return d.ToString();
        }

        /// <summary>
        /// Value-based equality for Dynamic/boxed values.
        /// Unlike object.Equals(), handles cross-type numeric equality
        /// (e.g., boxed int 0 == boxed double 0.0 returns true)
        /// and preserves IEEE 754 NaN semantics (NaN != NaN).
        /// Used by gencs.ml for == with Dynamic or generic type parameters.
        /// </summary>
        public static bool valEq(object a, object b)
        {
            if (a == null && b == null) return true;
            if (a == null || b == null) return false;
            // IEEE 754: NaN != anything (including NaN)
            // Must check BEFORE ReferenceEquals (two NaN boxes could share identity)
            // and BEFORE a.Equals(b) (double.Equals/float.Equals treat NaN as equal)
            if (a is double da && double.IsNaN(da)) return false;
            if (b is double db && double.IsNaN(db)) return false;
            if (a is float fa && float.IsNaN(fa)) return false;
            if (b is float fb && float.IsNaN(fb)) return false;
            if (object.ReferenceEquals(a, b)) return true;
            if (a.GetType() == b.GetType()) return a.Equals(b);
            // Cross-type numeric: convert both to double
            if (IsNumeric(a) && IsNumeric(b))
            {
                return global::System.Convert.ToDouble(a) == global::System.Convert.ToDouble(b);
            }
            return object.Equals(a, b);
        }

        /// <summary>
        /// Check if a boxed value is a numeric primitive type.
        /// AOT-safe: uses explicit type checks, no reflection.
        /// </summary>
        private static bool IsNumeric(object obj)
        {
            return obj is int || obj is double || obj is float || obj is long
                || obj is short || obj is byte || obj is sbyte
                || obj is uint || obj is ulong || obj is ushort;
        }

        /// <summary>
        /// Invoke a delegate dynamically with the given arguments.
        /// Works with Func&lt;&gt;, Action, and other delegate types.
        /// AOT-compatible: does not use GetMethod or Activator.CreateInstance.
        /// </summary>
        public static object invokeFunction(object func, global::haxe.root.Array args)
        {
            // Throw when func is null - calling a null function in Haxe raises an exception
            if (func == null) throw new global::System.NullReferenceException("Cannot call null function");

            // If it's a HaxeFunction, use its invokeDynamic method
            if (func is global::haxe.lang.Function hf)
            {
                return hf.invokeDynamic(args);
            }

            // Otherwise, it should be a Delegate
            if (func is global::System.Delegate del)
            {
                var method = del.Method;
                var parameters = method.GetParameters();
                var invokeArgs = new object[parameters.Length];

                // Unify arguments: handle missing args and type conversions
                for (int i = 0; i < parameters.Length; i++)
                {
                    var param = parameters[i];
                    // Use __getDyn() for array access since storage type is not known
                    object argValue = (args != null && i < args.length) ? args.__getDyn(i) : null;

                    if (argValue == null)
                    {
                        // Null handling for primitives and Null<T> - AOT-safe, no reflection
                        invokeArgs[i] = createDefaultValue(param.ParameterType);
                    }
                    else
                    {
                        // Try to convert the argument to the parameter type
                        invokeArgs[i] = ConvertArg(argValue, param.ParameterType);
                    }
                }

                return del.DynamicInvoke(invokeArgs);
            }

            throw new global::System.InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Invoke a function dynamically with 0 arguments, using Value to avoid boxing.
        /// Fast path for Function; slow path for Delegate.
        /// </summary>
        public static Value invokeFunction0(object func)
        {
            if (func is Function hf)
            {
                if (hf is InstanceMethodFunction imf && imf.Arity != 0)
                    return Value.fromObject(hf.invokeDynamic(null));
                return hf.__hx_invoke0();
            }
            if (func == null) throw new NullReferenceException("Cannot call null function");
            if (func is Delegate del)
            {
                var parameters = del.Method.GetParameters();
                var invokeArgs = new object[parameters.Length];
                for (int i = 0; i < parameters.Length; i++)
                    invokeArgs[i] = createDefaultValue(parameters[i].ParameterType);
                return Value.fromObject(del.DynamicInvoke(invokeArgs));
            }
            throw new InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Invoke a function dynamically with 1 argument, using Value to avoid boxing.
        /// </summary>
        public static Value invokeFunction1(object func, Value a1)
        {
            if (func is Function hf)
            {
                if (hf is InstanceMethodFunction imf && imf.Arity != 1)
                {
                    var arr = new global::haxe.root.Array();
                    arr.push(a1.toDynamic());
                    return Value.fromObject(hf.invokeDynamic(arr));
                }
                return hf.__hx_invoke1(a1);
            }
            if (func == null) throw new NullReferenceException("Cannot call null function");
            if (func is Delegate del)
            {
                var parameters = del.Method.GetParameters();
                var invokeArgs = new object[parameters.Length];
                if (parameters.Length > 0) invokeArgs[0] = ConvertArg(a1.toDynamic(), parameters[0].ParameterType);
                for (int i = 1; i < parameters.Length; i++)
                    invokeArgs[i] = createDefaultValue(parameters[i].ParameterType);
                return Value.fromObject(del.DynamicInvoke(invokeArgs));
            }
            throw new InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Invoke a function dynamically with 2 arguments, using Value to avoid boxing.
        /// </summary>
        public static Value invokeFunction2(object func, Value a1, Value a2)
        {
            if (func is Function hf)
            {
                if (hf is InstanceMethodFunction imf && imf.Arity != 2)
                {
                    var arr = new global::haxe.root.Array();
                    arr.push(a1.toDynamic());
                    arr.push(a2.toDynamic());
                    return Value.fromObject(hf.invokeDynamic(arr));
                }
                return hf.__hx_invoke2(a1, a2);
            }
            if (func == null) throw new NullReferenceException("Cannot call null function");
            if (func is Delegate del)
            {
                var parameters = del.Method.GetParameters();
                var invokeArgs = new object[parameters.Length];
                if (parameters.Length > 0) invokeArgs[0] = ConvertArg(a1.toDynamic(), parameters[0].ParameterType);
                if (parameters.Length > 1) invokeArgs[1] = ConvertArg(a2.toDynamic(), parameters[1].ParameterType);
                for (int i = 2; i < parameters.Length; i++)
                    invokeArgs[i] = createDefaultValue(parameters[i].ParameterType);
                return Value.fromObject(del.DynamicInvoke(invokeArgs));
            }
            throw new InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Invoke a function dynamically with 3 arguments, using Value to avoid boxing.
        /// </summary>
        public static Value invokeFunction3(object func, Value a1, Value a2, Value a3)
        {
            if (func is Function hf)
            {
                if (hf is InstanceMethodFunction imf && imf.Arity != 3)
                {
                    var arr = new global::haxe.root.Array();
                    arr.push(a1.toDynamic());
                    arr.push(a2.toDynamic());
                    arr.push(a3.toDynamic());
                    return Value.fromObject(hf.invokeDynamic(arr));
                }
                return hf.__hx_invoke3(a1, a2, a3);
            }
            if (func == null) throw new NullReferenceException("Cannot call null function");
            if (func is Delegate del)
            {
                var parameters = del.Method.GetParameters();
                var invokeArgs = new object[parameters.Length];
                if (parameters.Length > 0) invokeArgs[0] = ConvertArg(a1.toDynamic(), parameters[0].ParameterType);
                if (parameters.Length > 1) invokeArgs[1] = ConvertArg(a2.toDynamic(), parameters[1].ParameterType);
                if (parameters.Length > 2) invokeArgs[2] = ConvertArg(a3.toDynamic(), parameters[2].ParameterType);
                for (int i = 3; i < parameters.Length; i++)
                    invokeArgs[i] = createDefaultValue(parameters[i].ParameterType);
                return Value.fromObject(del.DynamicInvoke(invokeArgs));
            }
            throw new InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Invoke a function dynamically with 4 arguments, using Value to avoid boxing.
        /// </summary>
        public static Value invokeFunction4(object func, Value a1, Value a2, Value a3, Value a4)
        {
            if (func is Function hf)
            {
                if (hf is InstanceMethodFunction imf && imf.Arity != 4)
                {
                    var arr = new global::haxe.root.Array();
                    arr.push(a1.toDynamic());
                    arr.push(a2.toDynamic());
                    arr.push(a3.toDynamic());
                    arr.push(a4.toDynamic());
                    return Value.fromObject(hf.invokeDynamic(arr));
                }
                return hf.__hx_invoke4(a1, a2, a3, a4);
            }
            if (func == null) throw new NullReferenceException("Cannot call null function");
            if (func is Delegate del)
            {
                var parameters = del.Method.GetParameters();
                var invokeArgs = new object[parameters.Length];
                if (parameters.Length > 0) invokeArgs[0] = ConvertArg(a1.toDynamic(), parameters[0].ParameterType);
                if (parameters.Length > 1) invokeArgs[1] = ConvertArg(a2.toDynamic(), parameters[1].ParameterType);
                if (parameters.Length > 2) invokeArgs[2] = ConvertArg(a3.toDynamic(), parameters[2].ParameterType);
                if (parameters.Length > 3) invokeArgs[3] = ConvertArg(a4.toDynamic(), parameters[3].ParameterType);
                for (int i = 4; i < parameters.Length; i++)
                    invokeArgs[i] = createDefaultValue(parameters[i].ParameterType);
                return Value.fromObject(del.DynamicInvoke(invokeArgs));
            }
            throw new InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Invoke a function dynamically with 5 arguments, using Value to avoid boxing.
        /// </summary>
        public static Value invokeFunction5(object func, Value a1, Value a2, Value a3, Value a4, Value a5)
        {
            if (func is Function hf)
            {
                if (hf is InstanceMethodFunction imf && imf.Arity != 5)
                {
                    var arr = new global::haxe.root.Array();
                    arr.push(a1.toDynamic());
                    arr.push(a2.toDynamic());
                    arr.push(a3.toDynamic());
                    arr.push(a4.toDynamic());
                    arr.push(a5.toDynamic());
                    return Value.fromObject(hf.invokeDynamic(arr));
                }
                return hf.__hx_invoke5(a1, a2, a3, a4, a5);
            }
            if (func == null) throw new NullReferenceException("Cannot call null function");
            if (func is Delegate del)
            {
                var parameters = del.Method.GetParameters();
                var invokeArgs = new object[parameters.Length];
                if (parameters.Length > 0) invokeArgs[0] = ConvertArg(a1.toDynamic(), parameters[0].ParameterType);
                if (parameters.Length > 1) invokeArgs[1] = ConvertArg(a2.toDynamic(), parameters[1].ParameterType);
                if (parameters.Length > 2) invokeArgs[2] = ConvertArg(a3.toDynamic(), parameters[2].ParameterType);
                if (parameters.Length > 3) invokeArgs[3] = ConvertArg(a4.toDynamic(), parameters[3].ParameterType);
                if (parameters.Length > 4) invokeArgs[4] = ConvertArg(a5.toDynamic(), parameters[4].ParameterType);
                for (int i = 5; i < parameters.Length; i++)
                    invokeArgs[i] = createDefaultValue(parameters[i].ParameterType);
                return Value.fromObject(del.DynamicInvoke(invokeArgs));
            }
            throw new InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Invoke a function dynamically with 6 arguments, using Value to avoid boxing.
        /// </summary>
        public static Value invokeFunction6(object func, Value a1, Value a2, Value a3, Value a4, Value a5, Value a6)
        {
            if (func is Function hf)
            {
                if (hf is InstanceMethodFunction imf && imf.Arity != 6)
                {
                    var arr = new global::haxe.root.Array();
                    arr.push(a1.toDynamic());
                    arr.push(a2.toDynamic());
                    arr.push(a3.toDynamic());
                    arr.push(a4.toDynamic());
                    arr.push(a5.toDynamic());
                    arr.push(a6.toDynamic());
                    return Value.fromObject(hf.invokeDynamic(arr));
                }
                return hf.__hx_invoke6(a1, a2, a3, a4, a5, a6);
            }
            if (func == null) throw new NullReferenceException("Cannot call null function");
            if (func is Delegate del)
            {
                var parameters = del.Method.GetParameters();
                var invokeArgs = new object[parameters.Length];
                if (parameters.Length > 0) invokeArgs[0] = ConvertArg(a1.toDynamic(), parameters[0].ParameterType);
                if (parameters.Length > 1) invokeArgs[1] = ConvertArg(a2.toDynamic(), parameters[1].ParameterType);
                if (parameters.Length > 2) invokeArgs[2] = ConvertArg(a3.toDynamic(), parameters[2].ParameterType);
                if (parameters.Length > 3) invokeArgs[3] = ConvertArg(a4.toDynamic(), parameters[3].ParameterType);
                if (parameters.Length > 4) invokeArgs[4] = ConvertArg(a5.toDynamic(), parameters[4].ParameterType);
                if (parameters.Length > 5) invokeArgs[5] = ConvertArg(a6.toDynamic(), parameters[5].ParameterType);
                for (int i = 6; i < parameters.Length; i++)
                    invokeArgs[i] = createDefaultValue(parameters[i].ParameterType);
                return Value.fromObject(del.DynamicInvoke(invokeArgs));
            }
            throw new InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Invoke a function dynamically with 7 arguments, using Value to avoid boxing.
        /// </summary>
        public static Value invokeFunction7(object func, Value a1, Value a2, Value a3, Value a4, Value a5, Value a6, Value a7)
        {
            if (func is Function hf)
            {
                if (hf is InstanceMethodFunction imf && imf.Arity != 7)
                {
                    var arr = new global::haxe.root.Array();
                    arr.push(a1.toDynamic());
                    arr.push(a2.toDynamic());
                    arr.push(a3.toDynamic());
                    arr.push(a4.toDynamic());
                    arr.push(a5.toDynamic());
                    arr.push(a6.toDynamic());
                    arr.push(a7.toDynamic());
                    return Value.fromObject(hf.invokeDynamic(arr));
                }
                return hf.__hx_invoke7(a1, a2, a3, a4, a5, a6, a7);
            }
            if (func == null) throw new NullReferenceException("Cannot call null function");
            if (func is Delegate del)
            {
                var parameters = del.Method.GetParameters();
                var invokeArgs = new object[parameters.Length];
                if (parameters.Length > 0) invokeArgs[0] = ConvertArg(a1.toDynamic(), parameters[0].ParameterType);
                if (parameters.Length > 1) invokeArgs[1] = ConvertArg(a2.toDynamic(), parameters[1].ParameterType);
                if (parameters.Length > 2) invokeArgs[2] = ConvertArg(a3.toDynamic(), parameters[2].ParameterType);
                if (parameters.Length > 3) invokeArgs[3] = ConvertArg(a4.toDynamic(), parameters[3].ParameterType);
                if (parameters.Length > 4) invokeArgs[4] = ConvertArg(a5.toDynamic(), parameters[4].ParameterType);
                if (parameters.Length > 5) invokeArgs[5] = ConvertArg(a6.toDynamic(), parameters[5].ParameterType);
                if (parameters.Length > 6) invokeArgs[6] = ConvertArg(a7.toDynamic(), parameters[6].ParameterType);
                for (int i = 7; i < parameters.Length; i++)
                    invokeArgs[i] = createDefaultValue(parameters[i].ParameterType);
                return Value.fromObject(del.DynamicInvoke(invokeArgs));
            }
            throw new InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Invoke a function dynamically with 8 arguments, using Value to avoid boxing.
        /// </summary>
        public static Value invokeFunction8(object func, Value a1, Value a2, Value a3, Value a4, Value a5, Value a6, Value a7, Value a8)
        {
            if (func is Function hf)
            {
                if (hf is InstanceMethodFunction imf && imf.Arity != 8)
                {
                    var arr = new global::haxe.root.Array();
                    arr.push(a1.toDynamic());
                    arr.push(a2.toDynamic());
                    arr.push(a3.toDynamic());
                    arr.push(a4.toDynamic());
                    arr.push(a5.toDynamic());
                    arr.push(a6.toDynamic());
                    arr.push(a7.toDynamic());
                    arr.push(a8.toDynamic());
                    return Value.fromObject(hf.invokeDynamic(arr));
                }
                return hf.__hx_invoke8(a1, a2, a3, a4, a5, a6, a7, a8);
            }
            if (func == null) throw new NullReferenceException("Cannot call null function");
            if (func is Delegate del)
            {
                var parameters = del.Method.GetParameters();
                var invokeArgs = new object[parameters.Length];
                if (parameters.Length > 0) invokeArgs[0] = ConvertArg(a1.toDynamic(), parameters[0].ParameterType);
                if (parameters.Length > 1) invokeArgs[1] = ConvertArg(a2.toDynamic(), parameters[1].ParameterType);
                if (parameters.Length > 2) invokeArgs[2] = ConvertArg(a3.toDynamic(), parameters[2].ParameterType);
                if (parameters.Length > 3) invokeArgs[3] = ConvertArg(a4.toDynamic(), parameters[3].ParameterType);
                if (parameters.Length > 4) invokeArgs[4] = ConvertArg(a5.toDynamic(), parameters[4].ParameterType);
                if (parameters.Length > 5) invokeArgs[5] = ConvertArg(a6.toDynamic(), parameters[5].ParameterType);
                if (parameters.Length > 6) invokeArgs[6] = ConvertArg(a7.toDynamic(), parameters[6].ParameterType);
                if (parameters.Length > 7) invokeArgs[7] = ConvertArg(a8.toDynamic(), parameters[7].ParameterType);
                for (int i = 8; i < parameters.Length; i++)
                    invokeArgs[i] = createDefaultValue(parameters[i].ParameterType);
                return Value.fromObject(del.DynamicInvoke(invokeArgs));
            }
            throw new InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Invoke a function dynamically with 9 arguments, using Value to avoid boxing.
        /// </summary>
        public static Value invokeFunction9(object func, Value a1, Value a2, Value a3, Value a4, Value a5, Value a6, Value a7, Value a8, Value a9)
        {
            if (func is Function hf)
            {
                if (hf is InstanceMethodFunction imf && imf.Arity != 9)
                {
                    var arr = new global::haxe.root.Array();
                    arr.push(a1.toDynamic());
                    arr.push(a2.toDynamic());
                    arr.push(a3.toDynamic());
                    arr.push(a4.toDynamic());
                    arr.push(a5.toDynamic());
                    arr.push(a6.toDynamic());
                    arr.push(a7.toDynamic());
                    arr.push(a8.toDynamic());
                    arr.push(a9.toDynamic());
                    return Value.fromObject(hf.invokeDynamic(arr));
                }
                return hf.__hx_invoke9(a1, a2, a3, a4, a5, a6, a7, a8, a9);
            }
            if (func == null) throw new NullReferenceException("Cannot call null function");
            if (func is Delegate del)
            {
                var parameters = del.Method.GetParameters();
                var invokeArgs = new object[parameters.Length];
                if (parameters.Length > 0) invokeArgs[0] = ConvertArg(a1.toDynamic(), parameters[0].ParameterType);
                if (parameters.Length > 1) invokeArgs[1] = ConvertArg(a2.toDynamic(), parameters[1].ParameterType);
                if (parameters.Length > 2) invokeArgs[2] = ConvertArg(a3.toDynamic(), parameters[2].ParameterType);
                if (parameters.Length > 3) invokeArgs[3] = ConvertArg(a4.toDynamic(), parameters[3].ParameterType);
                if (parameters.Length > 4) invokeArgs[4] = ConvertArg(a5.toDynamic(), parameters[4].ParameterType);
                if (parameters.Length > 5) invokeArgs[5] = ConvertArg(a6.toDynamic(), parameters[5].ParameterType);
                if (parameters.Length > 6) invokeArgs[6] = ConvertArg(a7.toDynamic(), parameters[6].ParameterType);
                if (parameters.Length > 7) invokeArgs[7] = ConvertArg(a8.toDynamic(), parameters[7].ParameterType);
                if (parameters.Length > 8) invokeArgs[8] = ConvertArg(a9.toDynamic(), parameters[8].ParameterType);
                for (int i = 9; i < parameters.Length; i++)
                    invokeArgs[i] = createDefaultValue(parameters[i].ParameterType);
                return Value.fromObject(del.DynamicInvoke(invokeArgs));
            }
            throw new InvalidOperationException("Cannot invoke non-delegate: " + func.GetType().Name);
        }

        /// <summary>
        /// Create a default value for the given type.
        /// AOT-safe: handles Null&lt;T&gt; without reflection by checking common types directly.
        /// </summary>
#if !NETSTANDARD
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2067",
            Justification = "Null<T> is a struct; parameterless struct constructors are intrinsic and always available")]
#endif
        private static object createDefaultValue(global::System.Type type)
        {
            // Handle Null<T> first - for null/missing args, default(Null<T>) has hasValue=false
            if (type.IsGenericType && type.GetGenericTypeDefinition() == typeof(global::haxe.lang.Null<>))
            {
                if (type == typeof(global::haxe.lang.Null<int>)) return default(global::haxe.lang.Null<int>);
                if (type == typeof(global::haxe.lang.Null<double>)) return default(global::haxe.lang.Null<double>);
                if (type == typeof(global::haxe.lang.Null<float>)) return default(global::haxe.lang.Null<float>);
                if (type == typeof(global::haxe.lang.Null<bool>)) return default(global::haxe.lang.Null<bool>);
                if (type == typeof(global::haxe.lang.Null<long>)) return default(global::haxe.lang.Null<long>);
                if (type == typeof(global::haxe.lang.Null<short>)) return default(global::haxe.lang.Null<short>);
                if (type == typeof(global::haxe.lang.Null<byte>)) return default(global::haxe.lang.Null<byte>);
                if (type == typeof(global::haxe.lang.Null<sbyte>)) return default(global::haxe.lang.Null<sbyte>);
                if (type == typeof(global::haxe.lang.Null<uint>)) return default(global::haxe.lang.Null<uint>);
                if (type == typeof(global::haxe.lang.Null<ulong>)) return default(global::haxe.lang.Null<ulong>);
                if (type == typeof(global::haxe.lang.Null<ushort>)) return default(global::haxe.lang.Null<ushort>);
                if (type == typeof(global::haxe.lang.Null<char>)) return default(global::haxe.lang.Null<char>);
                if (type == typeof(global::haxe.lang.Null<string>)) return default(global::haxe.lang.Null<string>);
                if (type == typeof(global::haxe.lang.Null<object>)) return default(global::haxe.lang.Null<object>);
                // For other Null<T> types (custom classes), use Activator.CreateInstance
                // This works for structs in AOT because parameterless struct constructors are intrinsic
                return global::System.Activator.CreateInstance(type);
            }

            // Primitive types
            if (type == typeof(int)) return 0;
            if (type == typeof(double)) return 0.0;
            if (type == typeof(float)) return 0.0f;
            if (type == typeof(bool)) return false;
            if (type == typeof(long)) return 0L;
            if (type == typeof(short)) return (short)0;
            if (type == typeof(byte)) return (byte)0;
            if (type == typeof(sbyte)) return (sbyte)0;
            if (type == typeof(uint)) return 0u;
            if (type == typeof(ulong)) return 0ul;
            if (type == typeof(ushort)) return (ushort)0;
            if (type == typeof(char)) return '\0';

            // Reference types default to null
            return null;
        }

        /// <summary>
        /// Create a Null&lt;T&gt; from a dynamic value, AOT-safe.
        /// </summary>
#if !NETSTANDARD
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2067",
            Justification = "Null<T> is a struct; parameterless struct constructors are intrinsic and always available")]
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2090",
            Justification = "Null<T> constructor with value parameter is always available")]
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL3050",
            Justification = "MakeGenericType for Null<T> works in AOT when the inner type is used elsewhere in the program")]
#endif
        private static object createNullOfT(global::System.Type innerType, object value)
        {
            if (innerType == typeof(int)) return global::haxe.lang.Null<int>._ofDynamic(value);
            if (innerType == typeof(double)) return global::haxe.lang.Null<double>._ofDynamic(value);
            if (innerType == typeof(float)) return global::haxe.lang.Null<float>._ofDynamic(value);
            if (innerType == typeof(bool)) return global::haxe.lang.Null<bool>._ofDynamic(value);
            if (innerType == typeof(long)) return global::haxe.lang.Null<long>._ofDynamic(value);
            if (innerType == typeof(short)) return global::haxe.lang.Null<short>._ofDynamic(value);
            if (innerType == typeof(byte)) return global::haxe.lang.Null<byte>._ofDynamic(value);
            if (innerType == typeof(sbyte)) return global::haxe.lang.Null<sbyte>._ofDynamic(value);
            if (innerType == typeof(uint)) return global::haxe.lang.Null<uint>._ofDynamic(value);
            if (innerType == typeof(ulong)) return global::haxe.lang.Null<ulong>._ofDynamic(value);
            if (innerType == typeof(ushort)) return global::haxe.lang.Null<ushort>._ofDynamic(value);
            if (innerType == typeof(char)) return global::haxe.lang.Null<char>._ofDynamic(value);
            if (innerType == typeof(string)) return global::haxe.lang.Null<string>._ofDynamic(value);
            if (innerType == typeof(object)) return global::haxe.lang.Null<object>._ofDynamic(value);
            // Fallback for custom types: use the generic Null<T>.ofDynamic<D> method
            // which handles the conversion properly. We need to call it via reflection.
            var ofDynamicMethod = typeof(global::haxe.lang.Null<>).MakeGenericType(innerType)
                .GetMethod("_ofDynamic", global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Static);
            return ofDynamicMethod.Invoke(null, new[] { value });
        }

        /// <summary>
        /// Convert an argument value to the target type.
        /// AOT-safe: does not use reflection for Null&lt;T&gt; creation.
        /// </summary>
        private static object ConvertArg(object value, global::System.Type targetType)
        {
            if (value == null) return createDefaultValue(targetType);

            var valueType = value.GetType();
            if (targetType.IsAssignableFrom(valueType)) return value;

            // Handle Null<T> wrapper - AOT-safe using direct type checks
            if (targetType.IsGenericType && targetType.GetGenericTypeDefinition() == typeof(global::haxe.lang.Null<>))
            {
                var innerType = targetType.GetGenericArguments()[0];
                return createNullOfT(innerType, value);
            }

            // Numeric conversions
            if (targetType == typeof(int)) return toInt(value);
            if (targetType == typeof(double)) return toDouble(value);
            if (targetType == typeof(float)) return (float)toDouble(value);
            if (targetType == typeof(bool)) return toBool(value);
            if (targetType == typeof(long)) return (long)toDouble(value);
            if (targetType == typeof(string)) return value?.ToString();

            // Try explicit conversion
            try
            {
                return global::System.Convert.ChangeType(value, targetType);
            }
            catch
            {
                return value;
            }
        }

        /// <summary>
        /// Get a field from an object dynamically.
        /// For HaxeObject subclasses, uses _hx_getField (AOT-safe).
        /// For other objects, uses reflection (may not work in AOT for all types).
        /// </summary>
#if !NETSTANDARD
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2075",
            Justification = "Fallback reflection for non-Haxe objects; Haxe objects use _hx_getField")]
#endif
        public static object getField(object obj, string name)
        {
            if (obj == null) throw new global::System.NullReferenceException("Cannot get field from null");

            // Check for Type object (static field access via Dynamic)
            if (obj is global::System.Type t)
            {
                return global::haxe.lang.HaxeReflection.getField(t, name);
            }

            // For HaxeObject subclasses, use _hx_getField (AOT-safe)
            if (obj is global::haxe.lang.HaxeObject ho)
            {
                return ho._hx_getField(name);
            }

            // Special handling for strings (native System.String)
            if (obj is string str)
            {
                return getStringField(str, name);
            }

            // Fallback to reflection for non-Haxe objects
            var type = obj.GetType();

            // Try field first
            var field = type.GetField(name, global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Instance);
            if (field != null) return field.GetValue(obj);

            // Try property
            var prop = type.GetProperty(name, global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Instance);
            if (prop != null) return prop.GetValue(obj);

            return null;
        }

        /// <summary>
        /// Set a field on an object dynamically (non-generic version for null values).
        /// For HaxeObject subclasses, uses _hx_setField (AOT-safe).
        /// For other objects, uses reflection (may not work in AOT for all types).
        /// </summary>
#if !NETSTANDARD
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2075",
            Justification = "Fallback reflection for non-Haxe objects; Haxe objects use _hx_setField")]
#endif
        public static object setField(object obj, string name, object value)
        {
            if (obj == null) throw new global::System.NullReferenceException("Cannot set field on null");

            // For HaxeObject subclasses, use _hx_setField (AOT-safe)
            if (obj is global::haxe.lang.HaxeObject ho)
            {
                ho._hx_setField(name, value);
                return value;
            }

            var type = obj.GetType();

            // Try field first
            var field = type.GetField(name, global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Instance);
            if (field != null)
            {
                field.SetValue(obj, value);
                return value;
            }

            // Try property
            var prop = type.GetProperty(name, global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Instance);
            if (prop != null)
            {
                prop.SetValue(obj, value);
                return value;
            }

            return value;
        }

        /// <summary>
        /// Set a field on an object dynamically and return the value (for use in expression context).
        /// For HaxeObject subclasses, uses _hx_setField (AOT-safe).
        /// For other objects, uses reflection (may not work in AOT for all types).
        /// </summary>
#if !NETSTANDARD
        [global::System.Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("AOT", "IL2075",
            Justification = "Fallback reflection for non-Haxe objects; Haxe objects use _hx_setField")]
#endif
        public static T setField<T>(object obj, string name, T value)
        {
            if (obj == null) throw new global::System.NullReferenceException("Cannot set field on null");

            // For HaxeObject subclasses, use _hx_setField (AOT-safe)
            if (obj is global::haxe.lang.HaxeObject ho)
            {
                ho._hx_setField(name, value);
                return value;
            }

            var type = obj.GetType();

            // Try field first
            var field = type.GetField(name, global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Instance);
            if (field != null)
            {
                field.SetValue(obj, value);
                return value;
            }

            // Try property
            var prop = type.GetProperty(name, global::System.Reflection.BindingFlags.Public | global::System.Reflection.BindingFlags.Instance);
            if (prop != null)
            {
                prop.SetValue(obj, value);
                return value;
            }

            return value;
        }

        /// <summary>
        /// Check if an object is a function (HaxeFunction or C# delegate).
        /// </summary>
        public static bool isFunction(object obj)
        {
            if (obj == null) return false;
            if (obj is global::haxe.lang.Function) return true;
            if (obj is global::System.Delegate) return true;
            return false;
        }

        // String method closure cache: caches StringMethodFunction instances per string
        // to avoid allocation on each field access.
        private static readonly global::System.Runtime.CompilerServices.ConditionalWeakTable<string, global::haxe.lang.Function[]> stringMethodCache
            = new global::System.Runtime.CompilerServices.ConditionalWeakTable<string, global::haxe.lang.Function[]>();

        /// <summary>
        /// Get a field from a string, with caching for method closures.
        /// This is the single source of truth for string field access.
        /// </summary>
        public static object getStringField(string str, string name)
        {
            switch (name)
            {
                case "length": return str.Length;
                case "charAt": return GetStringMethodClosure(str, 0);
                case "charCodeAt": return GetStringMethodClosure(str, 1);
                case "indexOf": return GetStringMethodClosure(str, 2);
                case "lastIndexOf": return GetStringMethodClosure(str, 3);
                case "split": return GetStringMethodClosure(str, 4);
                case "substr": return GetStringMethodClosure(str, 5);
                case "substring": return GetStringMethodClosure(str, 6);
                case "toLowerCase": return GetStringMethodClosure(str, 7);
                case "toUpperCase": return GetStringMethodClosure(str, 8);
                case "toString": return GetStringMethodClosure(str, 9);
                default: return null;
            }
        }

        private static object GetStringMethodClosure(string str, int methodIndex)
        {
            global::haxe.lang.Function[] cache;
            if (!stringMethodCache.TryGetValue(str, out cache))
            {
                cache = new global::haxe.lang.Function[10];
                stringMethodCache.Add(str, cache);
            }
            if (cache[methodIndex] == null)
                cache[methodIndex] = new StringMethodFunction(str, methodIndex);
            return cache[methodIndex];
        }

        /// <summary>
        /// Perform a checked cast, throwing if the cast is invalid.
        /// This is used for Haxe's cast(expr, Type) syntax.
        /// Throws System.InvalidCastException if the cast fails.
        /// </summary>
        public static T genericCast<T>(object value)
        {
            if (value == null)
            {
                // Null is valid for reference types, invalid for value types
                if (typeof(T).IsValueType && Nullable.GetUnderlyingType(typeof(T)) == null)
                    throw new global::System.InvalidCastException("Cannot cast null to value type " + typeof(T).Name);
                return default(T);
            }

            // Direct type check
            if (value is T t)
                return t;

            // Try numeric conversions for primitive types
            var targetType = typeof(T);
            try
            {
                if (targetType == typeof(int)) return (T)(object)global::System.Convert.ToInt32(value);
                if (targetType == typeof(double)) return (T)(object)global::System.Convert.ToDouble(value);
                if (targetType == typeof(float)) return (T)(object)global::System.Convert.ToSingle(value);
                if (targetType == typeof(long)) return (T)(object)global::System.Convert.ToInt64(value);
                if (targetType == typeof(short)) return (T)(object)global::System.Convert.ToInt16(value);
                if (targetType == typeof(byte)) return (T)(object)global::System.Convert.ToByte(value);
                if (targetType == typeof(bool)) return (T)(object)global::System.Convert.ToBoolean(value);
            }
            catch (global::System.FormatException)
            {
                throw new global::System.InvalidCastException("Cannot cast " + value.GetType().Name + " to " + typeof(T).Name);
            }
            catch (global::System.InvalidCastException)
            {
                throw;
            }

            // If we get here, cast is not possible
            throw new global::System.InvalidCastException("Cannot cast " + value.GetType().Name + " to " + typeof(T).Name);
        }
    }

    /// <summary>
    /// Function closure for string instance methods.
    /// Cached per-string to avoid allocation on each field access.
    /// </summary>
    internal class StringMethodFunction : global::haxe.lang.Function
    {
        private readonly string str;
        private readonly int methodIndex;

        public StringMethodFunction(string str, int methodIndex)
        {
            this.str = str;
            this.methodIndex = methodIndex;
        }

        public override object invokeDynamic(global::haxe.root.Array args)
        {
            int len = args != null ? args.length : 0;
            switch (methodIndex)
            {
                case 0: return global::cs.StringExt.charAt(str, len > 0 ? Runtime.toInt(args.__getDyn(0)) : 0);
                case 1: return global::cs.StringExt.charCodeAt(str, len > 0 ? Runtime.toInt(args.__getDyn(0)) : 0).toDynamic();
                case 2: return global::cs.StringExt.indexOf(str, len > 0 ? (string)args.__getDyn(0) : "", len > 1 ? global::haxe.lang.Null<int>._ofDynamic(args.__getDyn(1)) : new global::haxe.lang.Null<int>());
                case 3: return global::cs.StringExt.lastIndexOf(str, len > 0 ? (string)args.__getDyn(0) : "", len > 1 ? global::haxe.lang.Null<int>._ofDynamic(args.__getDyn(1)) : new global::haxe.lang.Null<int>());
                case 4: return global::cs.StringExt.split(str, len > 0 ? (string)args.__getDyn(0) : "");
                case 5: return global::cs.StringExt.substr(str, len > 0 ? Runtime.toInt(args.__getDyn(0)) : 0, len > 1 ? global::haxe.lang.Null<int>._ofDynamic(args.__getDyn(1)) : new global::haxe.lang.Null<int>());
                case 6: return global::cs.StringExt.substring(str, len > 0 ? Runtime.toInt(args.__getDyn(0)) : 0, len > 1 ? global::haxe.lang.Null<int>._ofDynamic(args.__getDyn(1)) : new global::haxe.lang.Null<int>());
                case 7: return str.ToLower();
                case 8: return str.ToUpper();
                case 9: return str;
                default: return null;
            }
        }
    }
}
