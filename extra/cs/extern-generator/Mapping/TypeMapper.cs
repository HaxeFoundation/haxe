namespace ExternGenerator.Mapping;

/// <summary>
/// Maps C# types to Haxe types for extern generation.
/// </summary>
public class TypeMapper
{
    // Dynamically detected multi-arity type families
    private HashSet<string>? _dynamicMultiArityTypes;

    /// <summary>
    /// Sets the dynamically detected multi-arity type families.
    /// </summary>
    public void SetMultiArityTypes(HashSet<string> multiArityTypes)
    {
        _dynamicMultiArityTypes = multiArityTypes;
    }

    // Primitive type mappings from C# to Haxe
    private static readonly Dictionary<string, string> PrimitiveMappings = new()
    {
        // Integers
        ["System.Int32"] = "Int",
        ["System.Int64"] = "haxe.Int64",
        ["System.Int16"] = "cs.Int16",
        ["System.SByte"] = "cs.Int8",
        ["System.Byte"] = "cs.UInt8",
        ["System.UInt16"] = "cs.UInt16",
        ["System.UInt32"] = "cs.UInt",
        ["System.UInt64"] = "cs.UInt64",

        // Floating point
        ["System.Single"] = "Single",
        ["System.Double"] = "Float",

        // Boolean
        ["System.Boolean"] = "Bool",

        // Character and string
        ["System.Char"] = "cs.Char16",
        ["System.String"] = "String",

        // Void
        ["System.Void"] = "Void",

        // Object
        ["System.Object"] = "Dynamic",
    };

    // Multi-arity type families are now detected automatically in GenerateCommand.cs
    // by analyzing all types and finding those with the same base name but different arities

    /// <summary>
    /// Checks if a type should be skipped during extern generation.
    /// Skips compiler-generated types and types that map to Haxe built-in types.
    /// </summary>
    public bool ShouldSkipType(string fullTypeName)
    {
        // Skip types with special naming (compiler-generated, etc.)
        // These have angle brackets in their names like "<>c__DisplayClass"
        if (fullTypeName.Contains('<') || fullTypeName.Contains('>'))
            return true;

        // Skip types that are mapped to Haxe built-in types (not cs.* types)
        // e.g., System.String -> String, System.Int32 -> Int
        // We don't want to generate cs/system/String.hx because it conflicts with Haxe's String
        if (PrimitiveMappings.TryGetValue(fullTypeName, out string? haxeType))
        {
            // Skip if the Haxe type doesn't start with "cs." (it's a built-in type)
            if (!haxeType.StartsWith("cs."))
                return true;
        }

        return false;
    }

    /// <summary>
    /// Gets the base type name without generic arity suffix.
    /// </summary>
    public string GetBaseTypeName(string typeName)
    {
        int backtickIndex = typeName.IndexOf('`');
        return backtickIndex >= 0 ? typeName.Substring(0, backtickIndex) : typeName;
    }

    /// <summary>
    /// Gets the generic arity from a type name (e.g., "Dictionary`2" returns 2).
    /// </summary>
    public int GetGenericArity(string typeName)
    {
        int backtickIndex = typeName.IndexOf('`');
        if (backtickIndex < 0 || backtickIndex == typeName.Length - 1)
            return 0;

        string arityStr = typeName.Substring(backtickIndex + 1);
        // Handle nested types like "Dictionary`2+Enumerator"
        int plusIndex = arityStr.IndexOf('+');
        if (plusIndex >= 0)
            arityStr = arityStr.Substring(0, plusIndex);

        return int.TryParse(arityStr, out int arity) ? arity : 0;
    }

    /// <summary>
    /// Checks if a type belongs to a multi-arity family.
    /// Uses the dynamically detected list from analyzing the assembly.
    /// </summary>
    public bool IsMultiArityFamily(string fullTypeName)
    {
        string baseTypeName = GetBaseTypeName(fullTypeName);
        return _dynamicMultiArityTypes?.Contains(baseTypeName) ?? false;
    }

    /// <summary>
    /// Maps a C# type to its Haxe equivalent.
    /// </summary>
    public string MapType(string csharpType, List<string>? typeParameters = null)
    {
        // Handle ref return types (strip ref prefix for Haxe)
        if (csharpType.StartsWith("ref "))
        {
            csharpType = csharpType.Substring(4);
        }

        // Check primitive mappings first
        if (PrimitiveMappings.TryGetValue(csharpType, out string? primitive))
            return primitive;

        // Handle arrays
        if (csharpType.EndsWith("[]"))
        {
            string elementType = csharpType.Substring(0, csharpType.Length - 2);
            string mappedElement = MapType(elementType, typeParameters);
            return $"cs.NativeArray<{mappedElement}>";
        }

        // Handle multi-dimensional arrays
        if (csharpType.Contains("[,"))
        {
            // For now, map to Dynamic - multi-dim arrays need special handling
            return "Dynamic";
        }

        // Handle pointer types
        if (csharpType.EndsWith("*"))
        {
            string elementType = csharpType.Substring(0, csharpType.Length - 1);
            string mappedElement = MapType(elementType, typeParameters);
            return $"cs.Pointer<{mappedElement}>";
        }

        // Handle byref types (ref/out parameters)
        if (csharpType.EndsWith("&"))
        {
            string innerType = csharpType.Substring(0, csharpType.Length - 1);
            return MapType(innerType, typeParameters);
        }

        // Handle Nullable<T>
        if (csharpType.StartsWith("System.Nullable`1"))
        {
            // Extract inner type and wrap in Null<T>
            int argsStart = csharpType.IndexOf('<');
            if (argsStart > 0 && csharpType.EndsWith(">"))
            {
                string innerType = csharpType.Substring(argsStart + 1, csharpType.Length - argsStart - 2);
                string mappedInner = MapType(innerType.Trim(), typeParameters);
                return $"Null<{mappedInner}>";
            }
            // Raw Nullable`1 without type args - shouldn't happen in practice
            return "Null<Dynamic>";
        }

        // Handle generic type parameters (T, T1, TResult, etc.)
        if (typeParameters != null && typeParameters.Contains(csharpType))
        {
            return csharpType;
        }

        // Handle generic type instantiations like System.Collections.Generic.IEnumerable`1<System.Char>
        // or ConfiguredValueTaskAwaiter<TResult>
        int genericArgsStart = csharpType.IndexOf('<');
        if (genericArgsStart > 0 && csharpType.EndsWith(">"))
        {
            string baseType = csharpType.Substring(0, genericArgsStart);
            string argsStr = csharpType.Substring(genericArgsStart + 1, csharpType.Length - genericArgsStart - 2);

            // Parse and map type arguments
            var args = ParseGenericArgs(argsStr);
            var mappedArgs = args.Select(a => MapType(a.Trim(), typeParameters)).ToList();

            // Check if the base type already has backtick notation (e.g., IEnumerable`1)
            int existingArity = GetGenericArity(baseType);
            if (existingArity == 0)
            {
                // Base type doesn't have backtick, so we need to add the arity
                // based on the number of type arguments, if it's a multi-arity family
                int arity = args.Count;
                string fullBaseName = baseType;

                // Check if it's a multi-arity family
                if (IsMultiArityFamily(fullBaseName))
                {
                    // Add backtick notation so MapTypeToHaxeClass knows the arity
                    baseType = $"{baseType}`{arity}";
                }
            }

            string mappedBase = MapTypeToHaxeClass(baseType);
            return $"{mappedBase}<{string.Join(", ", mappedArgs)}>";
        }

        // Map namespace to Haxe package
        return MapTypeToHaxeClass(csharpType);
    }

    /// <summary>
    /// Maps a full C# type name to a Haxe class reference.
    /// </summary>
    public string MapTypeToHaxeClass(string csharpType)
    {
        // Check primitives first
        if (PrimitiveMappings.TryGetValue(csharpType, out string? primitive))
            return primitive;

        // For nested types, we need to handle each part separately
        // E.g., "System.Runtime.CompilerServices.ConfiguredValueTaskAwaitable`1+ConfiguredValueTaskAwaiter"
        // Should become "cs.system.runtime.compilerservices.ConfiguredValueTaskAwaitable_1_ConfiguredValueTaskAwaiter"

        // First, extract namespace and type name (type name may include nested types)
        int lastDotBeforeNested = csharpType.LastIndexOf('.');
        int nestedStart = csharpType.IndexOf('+');
        if (nestedStart > 0 && nestedStart < lastDotBeforeNested)
        {
            // The + is after a dot in the namespace, find the last dot before +
            lastDotBeforeNested = csharpType.LastIndexOf('.', nestedStart);
        }

        string ns = "";
        string typePart = csharpType;
        if (lastDotBeforeNested > 0)
        {
            ns = csharpType.Substring(0, lastDotBeforeNested);
            typePart = csharpType.Substring(lastDotBeforeNested + 1);
        }

        // Now process the type part (which may contain nested types with +)
        // E.g., "ConfiguredValueTaskAwaitable`1+ConfiguredValueTaskAwaiter"
        var nestedParts = typePart.Split('+');
        var processedParts = new List<string>();

        // Get the total arity from the root type for multi-arity family detection
        int rootArity = GetGenericArity(nestedParts[0]);
        string rootBaseName = GetBaseTypeName(nestedParts[0]);
        string fullRootBaseName = string.IsNullOrEmpty(ns) ? rootBaseName : $"{ns}.{rootBaseName}";

        foreach (var part in nestedParts)
        {
            string baseName = GetBaseTypeName(part);
            int partArity = GetGenericArity(part);

            // Check if this part needs an arity suffix
            if (partArity > 0 && IsMultiArityFamily(fullRootBaseName))
            {
                processedParts.Add($"{baseName}_{partArity}");
            }
            else
            {
                processedParts.Add(baseName);
            }
        }

        string haxeTypeName = string.Join("_", processedParts);

        if (string.IsNullOrEmpty(ns))
        {
            return haxeTypeName;
        }

        // Convert namespace to lowercase for Haxe package
        string[] nsParts = ns.Split('.');
        string package = string.Join(".", nsParts.Select(p => p.ToLowerInvariant()));

        return $"cs.{package}.{haxeTypeName}";
    }

    /// <summary>
    /// Gets the Haxe package from a C# namespace.
    /// </summary>
    public string NamespaceToPackage(string csharpNamespace)
    {
        if (string.IsNullOrEmpty(csharpNamespace))
            return "cs";

        string[] parts = csharpNamespace.Split('.');
        string lowerParts = string.Join(".", parts.Select(p => p.ToLowerInvariant()));
        return "cs." + lowerParts;
    }

    /// <summary>
    /// Gets the Haxe class name from a C# type name.
    /// Handles nested types, generic arities, and multi-arity families.
    /// </summary>
    public (string Package, string ClassName, string? NativeName) GetHaxeTypeName(
        string csharpNamespace,
        string csharpTypeName,
        bool hasMultipleArities = false)
    {
        string package = NamespaceToPackage(csharpNamespace);

        // Handle nested types by processing each part separately
        // E.g., "ConfiguredValueTaskAwaitable`1+ConfiguredValueTaskAwaiter"
        // should become "ConfiguredValueTaskAwaitable_1_ConfiguredValueTaskAwaiter" if multi-arity
        bool isNested = csharpTypeName.Contains('+');
        var parts = csharpTypeName.Split('+');
        var classNameParts = new List<string>();

        // Get total arity from the root type (first part)
        int totalArity = GetGenericArity(parts[0]);

        // Determine if we need arity suffix (based on root type)
        string rootTypeBaseName = GetBaseTypeName(parts[0]);
        string fullBaseTypeName = string.IsNullOrEmpty(csharpNamespace)
            ? rootTypeBaseName
            : $"{csharpNamespace}.{rootTypeBaseName}";

        bool needsAritySuffix = hasMultipleArities || (totalArity > 0 && IsMultiArityFamily(fullBaseTypeName));

        // Process each part of the nested type
        foreach (var part in parts)
        {
            string baseName = GetBaseTypeName(part);
            int partArity = GetGenericArity(part);

            // Add arity suffix if needed (only for the first part that has the arity)
            if (partArity > 0 && needsAritySuffix)
            {
                classNameParts.Add($"{baseName}_{partArity}");
            }
            else
            {
                classNameParts.Add(baseName);
            }
        }

        string className = string.Join("_", classNameParts);

        // Build native name for @:native annotation
        // For non-multi-arity generic types, Haxe handles the generic suffix automatically
        // So we strip the backtick notation (e.g., Dictionary`2 -> Dictionary)
        // For multi-arity types, we keep the backtick so Haxe knows which variant to use
        string nativeName;
        if (isNested || needsAritySuffix)
        {
            // Need @:native annotation with backtick for multi-arity or nested types
            string fullTypeName = string.IsNullOrEmpty(csharpNamespace)
                ? csharpTypeName
                : $"{csharpNamespace}.{csharpTypeName}";
            // Convert nested type separator for .NET
            nativeName = fullTypeName.Replace('+', '.');
            return (package, className, nativeName);
        }
        else if (totalArity > 0)
        {
            // Generic type but not multi-arity - strip the backtick notation
            // Haxe will add the generic suffix automatically
            string baseTypeName = string.Join("+", parts.Select(GetBaseTypeName));
            string fullTypeName = string.IsNullOrEmpty(csharpNamespace)
                ? baseTypeName
                : $"{csharpNamespace}.{baseTypeName}";
            nativeName = fullTypeName.Replace('+', '.');
            return (package, className, nativeName);
        }
        else
        {
            // Simple case - native name matches Haxe name (with proper casing)
            string fullTypeName = string.IsNullOrEmpty(csharpNamespace)
                ? csharpTypeName
                : $"{csharpNamespace}.{csharpTypeName}";
            return (package, className, fullTypeName);
        }
    }

    /// <summary>
    /// Gets the Haxe file path for a type.
    /// </summary>
    public string GetHaxeFilePath(string outputDir, string package, string className)
    {
        // Convert package to path
        string packagePath = package.Replace('.', Path.DirectorySeparatorChar);

        // Remove "cs" prefix since we're outputting to std/cs/
        if (packagePath.StartsWith("cs" + Path.DirectorySeparatorChar))
        {
            packagePath = packagePath.Substring(3);
        }

        string directory = Path.Combine(outputDir, packagePath);
        return Path.Combine(directory, className + ".hx");
    }

    /// <summary>
    /// Maps a C# access modifier to Haxe equivalent.
    /// </summary>
    public string MapAccessModifier(string csharpAccess)
    {
        return csharpAccess switch
        {
            "public" => "public",
            "protected" => "private", // Haxe doesn't have protected, use private
            "private" => "private",
            "internal" => "private",
            _ => "private"
        };
    }

    /// <summary>
    /// Determines if a type is inherently nullable (reference type).
    /// </summary>
    public bool IsInherentlyNullable(string csharpType)
    {
        // Primitives are not inherently nullable
        if (PrimitiveMappings.ContainsKey(csharpType))
        {
            // String and Object are reference types
            return csharpType == "System.String" || csharpType == "System.Object";
        }

        // Arrays are reference types
        if (csharpType.EndsWith("[]"))
            return true;

        // Pointers are not nullable in the Haxe sense
        if (csharpType.EndsWith("*"))
            return false;

        // By default, assume reference type (classes)
        return true;
    }

    /// <summary>
    /// Parses generic type arguments, handling nested generics.
    /// </summary>
    private List<string> ParseGenericArgs(string argsStr)
    {
        var args = new List<string>();
        int depth = 0;
        int start = 0;

        for (int i = 0; i < argsStr.Length; i++)
        {
            char c = argsStr[i];
            if (c == '<') depth++;
            else if (c == '>') depth--;
            else if (c == ',' && depth == 0)
            {
                args.Add(argsStr.Substring(start, i - start).Trim());
                start = i + 1;
            }
        }

        // Add the last argument
        if (start < argsStr.Length)
        {
            args.Add(argsStr.Substring(start).Trim());
        }

        return args;
    }
}
