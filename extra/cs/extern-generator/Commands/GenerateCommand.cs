using ExternGenerator.Analysis;
using ExternGenerator.Generation;
using ExternGenerator.Mapping;

namespace ExternGenerator.Commands;

public class GenerateCommand
{
    public Task ExecuteAsync(
        FileInfo dll,
        FileInfo? xml,
        DirectoryInfo output,
        string[] namespaces,
        string? package,
        FileInfo? config,
        bool recursive,
        bool overwrite)
    {
        Console.WriteLine($"Generating Haxe externs from: {dll.FullName}");

        if (!dll.Exists)
        {
            Console.Error.WriteLine($"Error: Assembly not found: {dll.FullName}");
            return Task.CompletedTask;
        }

        // Parse XML documentation if provided
        XmlDocParser? docParser = null;
        if (xml != null && xml.Exists)
        {
            Console.WriteLine($"Loading XML documentation from: {xml.FullName}");
            docParser = new XmlDocParser();
            docParser.Load(xml.FullName);
        }

        // Analyze the assembly
        var analyzer = new AssemblyAnalyzer();
        var types = analyzer.AnalyzeAssembly(dll.FullName, xml?.FullName);

        Console.WriteLine($"Found {types.Count} public types");

        // Filter by namespace if specified
        if (namespaces.Length > 0)
        {
            types = types.Where(t =>
                namespaces.Any(ns =>
                    recursive
                        ? t.Namespace.StartsWith(ns, StringComparison.Ordinal) || t.Namespace == ns
                        : t.Namespace == ns
                )
            ).ToList();
            Console.WriteLine($"Filtered to {types.Count} types in specified namespace(s)");
        }

        // Detect types with multiple arities (same base name, different generic param count)
        var multiArityTypes = new HashSet<string>();
        var typeMapper = new TypeMapper();

        foreach (var group in types.GroupBy(t => $"{t.Namespace}.{typeMapper.GetBaseTypeName(t.Name)}"))
        {
            var arities = group.Select(t => t.GenericParameterCount).Distinct().ToList();
            if (arities.Count > 1)
            {
                // This type family has multiple arities
                string baseName = typeMapper.GetBaseTypeName(group.First().FullName);
                multiArityTypes.Add(baseName);
            }
        }

        Console.WriteLine($"Detected {multiArityTypes.Count} multi-arity type families");

        // Pass multi-arity types to TypeMapper
        typeMapper.SetMultiArityTypes(multiArityTypes);

        // Create output directory
        output.Create();

        // Generate externs
        var writer = new HaxeExternWriter(typeMapper, docParser, output.FullName, multiArityTypes);

        int generated = 0;
        int skipped = 0;
        int errors = 0;

        foreach (var typeInfo in types)
        {
            // Check if file already exists when not overwriting
            if (!overwrite)
            {
                bool hasMultipleArities = multiArityTypes.Contains(typeMapper.GetBaseTypeName(typeInfo.FullName));
                var (pkg, className, _) = typeMapper.GetHaxeTypeName(
                    typeInfo.Namespace,
                    typeInfo.Name,
                    hasMultipleArities);

                string outputPath = typeMapper.GetHaxeFilePath(output.FullName, pkg, className);
                if (File.Exists(outputPath))
                {
                    skipped++;
                    continue;
                }
            }

            // Check if type should be skipped
            if (typeMapper.ShouldSkipType(typeInfo.FullName))
            {
                skipped++;
                continue;
            }

            try
            {
                writer.WriteType(typeInfo);
                generated++;

                // Progress indicator every 100 types
                if (generated % 100 == 0)
                {
                    Console.WriteLine($"Generated {generated} files...");
                }
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"Error generating {typeInfo.FullName}: {ex.Message}");
                errors++;
            }
        }

        Console.WriteLine();
        Console.WriteLine($"Generation complete:");
        Console.WriteLine($"  Generated: {generated} files");
        if (skipped > 0)
        {
            Console.WriteLine($"  Skipped: {skipped} files (existing or unsupported)");
        }
        if (errors > 0)
        {
            Console.WriteLine($"  Errors: {errors}");
        }

        return Task.CompletedTask;
    }
}
