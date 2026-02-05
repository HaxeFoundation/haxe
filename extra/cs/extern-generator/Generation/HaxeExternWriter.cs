using ExternGenerator.Analysis;
using ExternGenerator.Mapping;
using ExternGenerator.Models;

namespace ExternGenerator.Generation;

/// <summary>
/// Generates Haxe extern files from analyzed .NET types.
/// </summary>
public class HaxeExternWriter
{
    private readonly TypeMapper _typeMapper;
    private readonly XmlDocParser? _docParser;
    private readonly string _outputDir;
    private readonly HashSet<string> _multiArityTypes;

    public HaxeExternWriter(
        TypeMapper typeMapper,
        XmlDocParser? docParser,
        string outputDir,
        HashSet<string> multiArityTypes)
    {
        _typeMapper = typeMapper;
        _docParser = docParser;
        _outputDir = outputDir;
        _multiArityTypes = multiArityTypes;
    }

    /// <summary>
    /// Writes a Haxe extern file for a type.
    /// </summary>
    public void WriteType(TypeInfo type)
    {
        if (_typeMapper.ShouldSkipType(type.FullName))
            return;

        bool hasMultipleArities = _multiArityTypes.Contains(_typeMapper.GetBaseTypeName(type.FullName));

        var (package, className, nativeName) = _typeMapper.GetHaxeTypeName(
            type.Namespace,
            type.MetadataName,  // Use MetadataName to preserve backtick notation for arity
            hasMultipleArities);

        var builder = new HaxeCodeBuilder();

        // Package declaration
        builder.WritePackage(package);

        // Get type documentation
        var typeDoc = _docParser?.GetTypeDoc(type.FullName);
        if (typeDoc != null)
        {
            var docComment = typeDoc.ToHaxeDocComment("");
            if (docComment != null)
                builder.AppendRawLine(docComment);
        }

        // @:native annotation
        if (nativeName != null)
        {
            builder.WriteNative(nativeName);
        }

        // Write type based on kind
        switch (type.Kind)
        {
            case TypeKind.Enum:
                WriteEnum(builder, type, className);
                break;

            case TypeKind.Interface:
                WriteInterface(builder, type, className);
                break;

            case TypeKind.Delegate:
                WriteDelegate(builder, type, className);
                break;

            case TypeKind.Struct:
            case TypeKind.Class:
            default:
                WriteClass(builder, type, className);
                break;
        }

        // Write to file
        string filePath = _typeMapper.GetHaxeFilePath(_outputDir, package, className);
        string? directory = Path.GetDirectoryName(filePath);
        if (directory != null && !Directory.Exists(directory))
        {
            Directory.CreateDirectory(directory);
        }

        File.WriteAllText(filePath, builder.ToString());
    }

    private void WriteClass(HaxeCodeBuilder builder, TypeInfo type, string className)
    {
        // Prepare type parameters
        var typeParams = type.GenericParameters?.Select(p => p).ToList();

        // Prepare extends (no implements - Haxe requires all interface methods to be present,
        // but C# classes often use explicit interface implementations or inherit from base classes)
        string? extends = null;

        if (!string.IsNullOrEmpty(type.BaseType) && type.BaseType != "System.Object")
        {
            var mappedBase = MapTypeReference(type.BaseType, typeParams);
            // Avoid recursive extends (e.g., MulticastDelegate extends MulticastDelegate)
            if (mappedBase != $"cs.{_typeMapper.NamespaceToPackage(type.Namespace).Substring(3)}.{className}" &&
                !mappedBase.EndsWith($".{className}"))
            {
                extends = mappedBase;
            }
        }

        // Write class declaration (no implements to avoid missing method errors)
        builder.WriteClassDecl(
            className,
            typeParams,
            extends,
            implements: null,
            isExtern: true);

        // Write static fields
        foreach (var field in type.Fields.Where(f => f.IsStatic).OrderBy(f => f.Name))
        {
            WriteField(builder, type, field, typeParams);
        }

        // Write static properties
        foreach (var prop in type.Properties.Where(p => p.IsStatic).OrderBy(p => p.Name))
        {
            WriteProperty(builder, type, prop, typeParams);
        }

        // Write instance fields
        foreach (var field in type.Fields.Where(f => !f.IsStatic).OrderBy(f => f.Name))
        {
            WriteField(builder, type, field, typeParams);
        }

        // Write instance properties (non-indexers)
        foreach (var prop in type.Properties.Where(p => !p.IsStatic && !p.IsIndexer && p.Name != "Item").OrderBy(p => p.Name))
        {
            WriteProperty(builder, type, prop, typeParams);
        }

        // Write indexers (grouped for overload handling)
        var indexers = type.Properties.Where(p => p.IsIndexer || (p.Name == "Item" && p.IndexerParameters?.Any() == true)).ToList();
        if (indexers.Any())
        {
            WriteIndexerGroup(builder, type, indexers, typeParams);
        }

        // Write constructors
        WriteConstructors(builder, type, typeParams);

        // Write static methods (grouped by name for overloads)
        var staticMethods = type.Methods
            .Where(m => m.IsStatic && !m.IsConstructor)
            .GroupBy(m => m.Name)
            .OrderBy(g => g.Key);
        foreach (var methodGroup in staticMethods)
        {
            WriteMethodGroup(builder, type, methodGroup.ToList(), typeParams);
        }

        // Write instance methods (grouped by name for overloads)
        var instanceMethods = type.Methods
            .Where(m => !m.IsStatic && !m.IsConstructor)
            .GroupBy(m => m.Name)
            .OrderBy(g => g.Key);
        foreach (var methodGroup in instanceMethods)
        {
            WriteMethodGroup(builder, type, methodGroup.ToList(), typeParams);
        }

        builder.CloseBlock();
    }

    private void WriteMethodGroup(HaxeCodeBuilder builder, TypeInfo type, List<MethodInfo> methods, List<string>? typeParams)
    {
        if (methods.Count == 0) return;

        // Skip special methods
        var filtered = methods.Where(m =>
            !m.Name.StartsWith("get_") && !m.Name.StartsWith("set_") &&
            !m.Name.StartsWith("add_") && !m.Name.StartsWith("remove_") &&
            m.Name != ".ctor" && m.Name != ".cctor").ToList();

        if (filtered.Count == 0) return;

        // Sort by parameter count (simplest first)
        filtered = filtered.OrderBy(m => m.Parameters?.Count ?? 0).ToList();

        if (filtered.Count == 1)
        {
            // Single method, no overloads
            WriteMethod(builder, type, filtered[0], typeParams);
        }
        else
        {
            // Multiple overloads - write @:overload for all but last
            for (int i = 0; i < filtered.Count - 1; i++)
            {
                WriteMethodOverload(builder, filtered[i], typeParams);
            }
            // Write the actual method (last one)
            WriteMethod(builder, type, filtered.Last(), typeParams);
        }
    }

    private void WriteMethodOverload(HaxeCodeBuilder builder, MethodInfo method, List<string>? typeParams)
    {
        // For static methods, find which class type params are used in the signature
        var usedClassTypeParams = GetUsedClassTypeParams(method, typeParams);
        var methodTypeParams = method.GenericParameters?.ToList() ?? new List<string>();
        if (usedClassTypeParams.Any())
        {
            foreach (var tp in usedClassTypeParams)
            {
                if (!methodTypeParams.Contains(tp))
                    methodTypeParams.Add(tp);
            }
        }

        var allTypeParams = new List<string>();
        if (typeParams != null)
            allTypeParams.AddRange(typeParams);
        if (method.GenericParameters != null)
            allTypeParams.AddRange(method.GenericParameters);

        var parameters = method.Parameters?
            .Select(p => {
                string paramType = MapTypeReference(p.Type, allTypeParams);
                if (p.IsRef || p.IsOut) paramType = $"cs.Ref<{paramType}>";
                return (SanitizeName(p.Name), paramType, p.IsOptional);
            })
            .ToList();

        string returnType = MapTypeReference(method.ReturnType, allTypeParams);

        builder.WriteOverload(parameters, returnType, methodTypeParams.Any() ? methodTypeParams : null);
    }

    private void WriteInterface(HaxeCodeBuilder builder, TypeInfo type, string className)
    {
        var typeParams = type.GenericParameters?.Select(p => p).ToList();

        // Prepare extends (interfaces can extend other interfaces)
        List<string>? extends = null;
        if (type.Interfaces != null && type.Interfaces.Any())
        {
            extends = type.Interfaces
                .Select(i => MapTypeReference(i, typeParams))
                .ToList();
        }

        // Write interface declaration
        var sb = new System.Text.StringBuilder();
        sb.Append("extern interface ");
        sb.Append(className);

        if (typeParams != null && typeParams.Any())
        {
            sb.Append('<');
            sb.Append(string.Join(", ", typeParams));
            sb.Append('>');
        }

        if (extends != null && extends.Any())
        {
            sb.Append(" extends ");
            sb.Append(string.Join(" extends ", extends));
        }

        builder.OpenBlock(sb.ToString());

        // Write properties
        foreach (var prop in type.Properties.OrderBy(p => p.Name))
        {
            WriteProperty(builder, type, prop, typeParams);
        }

        // Write methods (grouped by name for overloads)
        var methodGroups = type.Methods
            .GroupBy(m => m.Name)
            .OrderBy(g => g.Key);
        foreach (var methodGroup in methodGroups)
        {
            WriteMethodGroup(builder, type, methodGroup.ToList(), typeParams);
        }

        builder.CloseBlock();
    }

    private void WriteEnum(HaxeCodeBuilder builder, TypeInfo type, string className)
    {
        // [Flags] enums need enum abstract to support bitwise operators
        // Regular enums use plain extern enum for proper type preservation
        if (type.IsFlags)
        {
            // Determine underlying type for flags enum
            string underlyingType = type.BaseType switch
            {
                "System.Byte" => "cs.UInt8",
                "System.SByte" => "cs.Int8",
                "System.Int16" => "cs.Int16",
                "System.UInt16" => "cs.UInt16",
                "System.Int32" => "Int",
                "System.UInt32" => "cs.UInt",
                "System.Int64" => "haxe.Int64",
                "System.UInt64" => "cs.UInt64",
                _ => "Int"
            };

            builder.WriteEnumAbstract(className, underlyingType);

            foreach (var field in type.Fields.Where(f => f.IsStatic && f.IsConst).OrderBy(f => f.Name))
            {
                builder.WriteEnumAbstractValue(field.Name, field.ConstValue?.ToString());
            }

            // Add bitwise operator overloads for [Flags] enums
            builder.AppendRawLine($"\t@:op(A | B) static function or(lhs:{className}, rhs:{className}):{className};");
            builder.AppendRawLine($"\t@:op(A & B) static function and(lhs:{className}, rhs:{className}):{className};");
            builder.AppendRawLine($"\t@:op(A ^ B) static function xor(lhs:{className}, rhs:{className}):{className};");
            builder.AppendRawLine($"\t@:op(~A) static function complement(value:{className}):{className};");
        }
        else
        {
            // Regular enums use plain extern enum for proper type preservation in C#
            builder.WriteEnum(className);

            foreach (var field in type.Fields.Where(f => f.IsStatic && f.IsConst).OrderBy(f => f.Name))
            {
                builder.WriteEnumValue(field.Name);
            }
        }

        builder.CloseBlock();
    }

    private void WriteDelegate(HaxeCodeBuilder builder, TypeInfo type, string className)
    {
        var typeParams = type.GenericParameters?.Select(p => p).ToList();

        // Delegates extend MulticastDelegate (except MulticastDelegate and Delegate themselves)
        string? extends = (className == "MulticastDelegate" || className == "Delegate")
            ? null
            : "cs.system.MulticastDelegate";

        builder.WriteClassDecl(
            className,
            typeParams,
            extends: extends,
            isExtern: true);

        // Find Invoke method for delegate signature
        var invokeMethod = type.Methods.FirstOrDefault(m => m.Name == "Invoke");
        if (invokeMethod != null)
        {
            // Constructor takes a function matching the delegate signature
            var funcType = BuildFunctionType(invokeMethod, typeParams);
            builder.WriteConstructor(new[] { ("func", funcType, false) });

            // Invoke method
            WriteMethod(builder, type, invokeMethod, typeParams);
        }
        else
        {
            // Fallback constructor
            builder.WriteConstructor(new[] { ("func", "Dynamic", false) });
        }

        builder.CloseBlock();
    }

    private void WriteField(HaxeCodeBuilder builder, TypeInfo type, FieldInfo field, List<string>? typeParams)
    {
        // Skip backing fields
        if (field.Name.Contains('<') || field.Name.StartsWith("_"))
            return;

        var fieldDoc = _docParser?.GetFieldDoc(type.FullName, field.Name);
        if (fieldDoc != null)
        {
            var docComment = fieldDoc.ToHaxeDocComment(builder.CurrentIndent);
            if (docComment != null)
                builder.AppendRawLine(docComment);
        }

        // For static fields that use class type parameters, replace them with Dynamic
        string haxeType;
        if (field.IsStatic && typeParams != null && typeParams.Any())
        {
            haxeType = ReplaceClassTypeParamsWithDynamic(field.Type, typeParams);
        }
        else
        {
            haxeType = MapTypeReference(field.Type, typeParams);
        }

        bool isReadOnly = field.IsReadOnly || field.IsConst;

        builder.WriteField(field.Name, haxeType, field.IsStatic, isReadOnly);
    }

    private void WriteProperty(HaxeCodeBuilder builder, TypeInfo type, PropertyInfo prop, List<string>? typeParams)
    {
        var propDoc = _docParser?.GetPropertyDoc(type.FullName, prop.Name);
        if (propDoc != null)
        {
            var docComment = propDoc.ToHaxeDocComment(builder.CurrentIndent);
            if (docComment != null)
                builder.AppendRawLine(docComment);
        }

        // For static properties that use class type parameters, replace them with Dynamic
        // since static members don't have access to class type parameters in Haxe
        string haxeType;
        if (prop.IsStatic && typeParams != null && typeParams.Any())
        {
            haxeType = ReplaceClassTypeParamsWithDynamic(prop.Type, typeParams);
        }
        else
        {
            haxeType = MapTypeReference(prop.Type, typeParams);
        }

        // Handle indexers - these become get_Item/set_Item methods
        if (prop.IsIndexer || (prop.Name == "Item" && prop.IndexerParameters != null && prop.IndexerParameters.Any()))
        {
            WriteIndexer(builder, type, prop, typeParams);
            return;
        }

        // Determine getter/setter access
        string? getter = prop.HasGetter ? "default" : "never";
        string? setter = prop.HasSetter ? "default" : "never";

        builder.WriteField(prop.Name, haxeType, prop.IsStatic, isReadOnly: false, getter, setter);
    }

    private void WriteIndexerGroup(HaxeCodeBuilder builder, TypeInfo type, List<PropertyInfo> indexers, List<string>? typeParams)
    {
        // Separate indexers with getters and setters
        var withGetters = indexers.Where(i => i.HasGetter).ToList();
        var withSetters = indexers.Where(i => i.HasSetter).ToList();

        // Write getters (with @:overload for multiple)
        if (withGetters.Any())
        {
            bool isStatic = withGetters.First().IsStatic;

            // Write overloads for all but the last getter
            for (int i = 0; i < withGetters.Count - 1; i++)
            {
                var prop = withGetters[i];
                string haxeType = MapTypeReference(prop.Type, typeParams);
                var parameters = prop.IndexerParameters?
                    .Select(p => (SanitizeName(p.Name), MapTypeReference(p.Type, typeParams), p.HasDefault))
                    .ToList();
                builder.WriteOverload(parameters, haxeType);
            }

            // Write the primary getter method
            var lastGetter = withGetters.Last();
            string lastGetterType = MapTypeReference(lastGetter.Type, typeParams);
            var lastGetterParams = lastGetter.IndexerParameters?
                .Select(p => (SanitizeName(p.Name), MapTypeReference(p.Type, typeParams), p.HasDefault))
                .ToList();
            builder.WriteMeta("@:native(\"get_Item\")");
            builder.WriteMethod("get_Item", lastGetterParams, lastGetterType, isStatic);
        }

        // Write setters (with @:overload for multiple)
        if (withSetters.Any())
        {
            bool isStatic = withSetters.First().IsStatic;

            // Write overloads for all but the last setter
            for (int i = 0; i < withSetters.Count - 1; i++)
            {
                var prop = withSetters[i];
                string haxeType = MapTypeReference(prop.Type, typeParams);
                var parameters = prop.IndexerParameters?
                    .Select(p => (SanitizeName(p.Name), MapTypeReference(p.Type, typeParams), p.HasDefault))
                    .ToList() ?? new List<(string, string, bool)>();
                parameters.Add(("value", haxeType, false));
                builder.WriteOverload(parameters, "Void");
            }

            // Write the primary setter method
            var lastSetter = withSetters.Last();
            string lastSetterType = MapTypeReference(lastSetter.Type, typeParams);
            var lastSetterParams = lastSetter.IndexerParameters?
                .Select(p => (SanitizeName(p.Name), MapTypeReference(p.Type, typeParams), p.HasDefault))
                .ToList() ?? new List<(string, string, bool)>();
            lastSetterParams.Add(("value", lastSetterType, false));
            builder.WriteMeta("@:native(\"set_Item\")");
            builder.WriteMethod("set_Item", lastSetterParams, "Void", isStatic);
        }
    }

    private void WriteIndexer(HaxeCodeBuilder builder, TypeInfo type, PropertyInfo prop, List<string>? typeParams)
    {
        // This is now only called for single indexer (fallback)
        WriteIndexerGroup(builder, type, new List<PropertyInfo> { prop }, typeParams);
    }

    private void WriteConstructors(HaxeCodeBuilder builder, TypeInfo type, List<string>? typeParams)
    {
        var constructors = type.Methods.Where(m => m.IsConstructor).ToList();
        if (!constructors.Any())
            return;

        // Sort by parameter count to put simplest first
        constructors = constructors.OrderBy(c => c.Parameters?.Count ?? 0).ToList();

        // Write overloads for all but the last constructor
        for (int i = 0; i < constructors.Count - 1; i++)
        {
            var ctor = constructors[i];
            var parameters = ctor.Parameters?
                .Select(p => {
                    string paramType = MapTypeReference(p.Type, typeParams);
                    if (p.IsRef || p.IsOut) paramType = $"cs.Ref<{paramType}>";
                    return (SanitizeName(p.Name), paramType, p.IsOptional);
                })
                .ToList();

            builder.WriteOverload(parameters, "Void");
        }

        // Write the actual constructor (last one)
        var lastCtor = constructors.Last();
        var ctorParams = lastCtor.Parameters?
            .Select(p => {
                string paramType = MapTypeReference(p.Type, typeParams);
                if (p.IsRef || p.IsOut) paramType = $"cs.Ref<{paramType}>";
                return (SanitizeName(p.Name), paramType, p.IsOptional);
            })
            .ToList();

        builder.WriteConstructor(ctorParams);
    }

    private void WriteMethod(HaxeCodeBuilder builder, TypeInfo type, MethodInfo method, List<string>? typeParams)
    {
        // Skip special methods
        if (method.Name.StartsWith("get_") || method.Name.StartsWith("set_") ||
            method.Name.StartsWith("add_") || method.Name.StartsWith("remove_") ||
            method.Name == ".ctor" || method.Name == ".cctor")
            return;

        // For static methods, find which class type params are used in the signature
        // and add them as the method's own type parameters (since static methods
        // don't have access to class type params in Haxe)
        var usedClassTypeParams = GetUsedClassTypeParams(method, typeParams);
        var methodTypeParams = method.GenericParameters?.ToList() ?? new List<string>();
        if (usedClassTypeParams.Any())
        {
            // Add used class type params to method's type params
            foreach (var tp in usedClassTypeParams)
            {
                if (!methodTypeParams.Contains(tp))
                    methodTypeParams.Add(tp);
            }
        }

        // Combine type parameters for type resolution
        var allTypeParams = new List<string>();
        if (typeParams != null)
            allTypeParams.AddRange(typeParams);
        if (method.GenericParameters != null)
            allTypeParams.AddRange(method.GenericParameters);

        var methodDoc = _docParser?.GetMethodDoc(type.FullName, method.Name);
        if (methodDoc != null)
        {
            var docComment = methodDoc.ToHaxeDocComment(builder.CurrentIndent);
            if (docComment != null)
                builder.AppendRawLine(docComment);
        }

        var parameters = method.Parameters?
            .Select(p => {
                string paramType = MapTypeReference(p.Type, allTypeParams);
                if (p.IsRef || p.IsOut) paramType = $"cs.Ref<{paramType}>";
                return (SanitizeName(p.Name), paramType, p.IsOptional);
            })
            .ToList();

        string returnType = MapTypeReference(method.ReturnType, allTypeParams);

        string methodName = method.Name;

        builder.WriteMethod(
            methodName,
            parameters,
            returnType,
            method.IsStatic,
            methodTypeParams.Any() ? methodTypeParams : null);
    }

    private string MapTypeReference(string typeName, List<string>? typeParams)
    {
        if (string.IsNullOrEmpty(typeName))
            return "Dynamic";

        // Check if it's a type parameter
        if (typeParams != null && typeParams.Contains(typeName))
            return typeName;

        return _typeMapper.MapType(typeName, typeParams);
    }

    private string BuildFunctionType(MethodInfo method, List<string>? typeParams)
    {
        var paramParts = new List<string>();

        if (method.Parameters != null && method.Parameters.Any())
        {
            foreach (var param in method.Parameters)
            {
                string paramType = MapTypeReference(param.Type, typeParams);
                paramParts.Add($"{SanitizeName(param.Name)}:{paramType}");
            }
        }

        string returnType = MapTypeReference(method.ReturnType, typeParams);

        if (paramParts.Count == 0)
        {
            return $"()->{returnType}";
        }
        return $"({string.Join(", ", paramParts)})->{returnType}";
    }

    private static string SanitizeName(string name)
    {
        // Haxe reserved words
        var reserved = new HashSet<string>
        {
            "abstract", "break", "case", "cast", "catch", "class", "continue",
            "default", "do", "dynamic", "else", "enum", "extends", "extern",
            "false", "final", "for", "function", "if", "implements", "import",
            "in", "inline", "interface", "macro", "new", "null", "operator",
            "override", "package", "private", "public", "return", "static",
            "switch", "this", "throw", "true", "try", "typedef", "untyped",
            "using", "var", "while"
        };

        if (reserved.Contains(name.ToLowerInvariant()))
            return name + "_";

        // Handle names that start with numbers
        if (char.IsDigit(name[0]))
            return "_" + name;

        return name;
    }

    /// <summary>
    /// For static methods, finds which class type parameters are used in the method signature.
    /// These need to be added as the method's own type parameters in Haxe.
    /// </summary>
    private List<string> GetUsedClassTypeParams(MethodInfo method, List<string>? classTypeParams)
    {
        if (classTypeParams == null || !classTypeParams.Any() || !method.IsStatic)
            return new List<string>();

        var usedParams = new List<string>();

        // Check parameters
        if (method.Parameters != null)
        {
            foreach (var param in method.Parameters)
            {
                foreach (var typeParam in classTypeParams)
                {
                    if (!usedParams.Contains(typeParam) && ContainsTypeParam(param.Type, typeParam))
                        usedParams.Add(typeParam);
                }
            }
        }

        // Check return type
        if (!string.IsNullOrEmpty(method.ReturnType))
        {
            foreach (var typeParam in classTypeParams)
            {
                if (!usedParams.Contains(typeParam) && ContainsTypeParam(method.ReturnType, typeParam))
                    usedParams.Add(typeParam);
            }
        }

        return usedParams;
    }

    /// <summary>
    /// Checks if a type string contains a type parameter.
    /// </summary>
    private bool ContainsTypeParam(string type, string typeParam)
    {
        if (string.IsNullOrEmpty(type))
            return false;

        // Check for exact match or match bounded by non-identifier characters
        // e.g., "TResult" matches in "ValueTask_1<TResult>" but not in "TResultFoo"
        int idx = 0;
        while ((idx = type.IndexOf(typeParam, idx, StringComparison.Ordinal)) >= 0)
        {
            bool startOk = idx == 0 || !char.IsLetterOrDigit(type[idx - 1]);
            bool endOk = idx + typeParam.Length >= type.Length ||
                         !char.IsLetterOrDigit(type[idx + typeParam.Length]);

            if (startOk && endOk)
                return true;

            idx++;
        }
        return false;
    }

    /// <summary>
    /// Maps a type reference, replacing any class type parameters with Dynamic.
    /// Used for static members which don't have access to class type parameters in Haxe.
    /// </summary>
    private string ReplaceClassTypeParamsWithDynamic(string typeName, List<string>? classTypeParams)
    {
        if (classTypeParams == null || !classTypeParams.Any())
            return MapTypeReference(typeName, classTypeParams);

        // First map the type normally
        string mapped = _typeMapper.MapType(typeName, classTypeParams);

        // Then replace any class type parameters with Dynamic
        foreach (var typeParam in classTypeParams)
        {
            // Replace type parameter bounded by non-identifier characters
            // E.g., "TaskFactory_1<TResult>" -> "TaskFactory_1<Dynamic>"
            var pattern = $@"(?<![A-Za-z0-9_]){System.Text.RegularExpressions.Regex.Escape(typeParam)}(?![A-Za-z0-9_])";
            mapped = System.Text.RegularExpressions.Regex.Replace(mapped, pattern, "Dynamic");
        }

        return mapped;
    }

}
