using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using static Matcher.Generator.Helpers;

namespace Matcher.Generator
{
    [Generator]
    public sealed class ArithmeticOperatorsGenerator : ISourceGenerator
    {
        private const string AttributeSource =
@"using System;

namespace Matcher
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = true)]
    public sealed class AddableWithAttribute : Attribute
    {
        public AddableWithAttribute(Type other) { Other = other; }
        public Type Other { get; }
    }

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = true)]
    public sealed class AddableFromAttribute : Attribute
    {
        public AddableFromAttribute(Type other) { Other = other; }
        public Type Other { get; }
    }

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = true)]
    public sealed class SubstractableWithAttribute : Attribute
    {
        public SubstractableWithAttribute(Type other) { Other = other; }
        public Type Other { get; }
    }

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = true)]
    public sealed class SubstractableFromAttribute : Attribute
    {
        public SubstractableFromAttribute(Type other) { Other = other; }
        public Type Other { get; }
    }

    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false, AllowMultiple = false)]
    public sealed class FixedAttribute : Attribute { }

    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false, AllowMultiple = false)]
    public sealed class AddableCollectionAttribute : Attribute { }

    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false, AllowMultiple = true)]
    public sealed class FieldAliasAttribute : Attribute
    {
        public FieldAliasAttribute(string alias) { Alias = alias; }
        public string Alias { get; }
    }
}
";
        public void Initialize(GeneratorInitializationContext context)
        {
            context.RegisterForPostInitialization(pi =>
            {
                pi.AddSource("ArithmeticAttributes.g.cs", SourceText.From(AttributeSource, Encoding.UTF8));
            });

            context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
        }
        public void Execute(GeneratorExecutionContext context)
        {
            if (context.SyntaxReceiver is not SyntaxReceiver rx)
                return;

            var compilation = context.Compilation;

            var addToAttr = compilation.GetTypeByMetadataName("Matcher.AddableWithAttribute");
            var addFromAttr = compilation.GetTypeByMetadataName("Matcher.AddableFromAttribute");
            var subToAttr = compilation.GetTypeByMetadataName("Matcher.SubstractableWithAttribute");
            var subFromAttr = compilation.GetTypeByMetadataName("Matcher.SubstractableFromAttribute");
            var fixedAttr = compilation.GetTypeByMetadataName("Matcher.FixedAttribute");
            foreach (var tds in rx.Candidates)
            {
                var model = compilation.GetSemanticModel(tds.SyntaxTree);
                if (model.GetDeclaredSymbol(tds) is not INamedTypeSymbol typeSym)
                    continue;
                if (typeSym.TypeKind is not (TypeKind.Class or TypeKind.Struct))
                    continue;
                var attrs = typeSym.GetAttributes();
                bool hasRelevant =
                    attrs.Any(a => SymbolEquals(a.AttributeClass, addToAttr) ||
                                   SymbolEquals(a.AttributeClass, addFromAttr) ||
                                   SymbolEquals(a.AttributeClass, subToAttr) ||
                                   SymbolEquals(a.AttributeClass, subFromAttr));

                if (!hasRelevant)
                    continue;
                if (!IsPartial(typeSym))
                {
                    ReportNotPartial(context, typeSym);
                    continue;
                }
                var requests = new List<OpRequest>();
                foreach (var a in attrs)
                {
                    var otherArg = a.ConstructorArguments.Length == 1 ? a.ConstructorArguments[0] : default;
                    if (otherArg.Value is not INamedTypeSymbol otherType)
                        continue;

                    if (SymbolEquals(a.AttributeClass, addToAttr))
                        requests.Add(new OpRequest(OperatorKind.Add, Handedness.LeftIsThis_To, otherType));
                    else if (SymbolEquals(a.AttributeClass, addFromAttr))
                        requests.Add(new OpRequest(OperatorKind.Add, Handedness.RightIsThis_From, otherType));
                    else if (SymbolEquals(a.AttributeClass, subToAttr))
                        requests.Add(new OpRequest(OperatorKind.Sub, Handedness.LeftIsThis_To, otherType));
                    else if (SymbolEquals(a.AttributeClass, subFromAttr))
                        requests.Add(new OpRequest(OperatorKind.Sub, Handedness.RightIsThis_From, otherType));
                }

                if (requests.Count == 0)
                    continue;
                var src = GenerateForType(typeSym, requests, compilation, fixedAttr);
                var hint = GetHintName(typeSym);
                context.AddSource(hint, SourceText.From(src, Encoding.UTF8));
            }
        }
        private static string GetHintName(INamedTypeSymbol typeSymbol)
        {
            var full = typeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            if (full.StartsWith("global::", StringComparison.Ordinal))
                full = full.Substring("global::".Length);

            var safe = full
                .Replace('.', '_')
                .Replace('+', '_')
                .Replace('<', '_')
                .Replace('>', '_')
                .Replace(',', '_');

            return $"{safe}.ArithmeticOperators.g.cs";
        }
        private static string GenerateForType(
            INamedTypeSymbol self,
            List<OpRequest> requests,
            Compilation compilation,
            INamedTypeSymbol? fixedAttr)
        {
            var ns = self.ContainingNamespace?.ToDisplayString() ?? string.Empty;
            var sb = new StringBuilder();
            sb.AppendLine("// <auto-generated/>");
            sb.AppendLine("using System;");

            if (!string.IsNullOrEmpty(ns))
            {
                sb.AppendLine($"namespace {ns}");
                sb.AppendLine("{");
            }
            foreach (var ct in GetContainingTypes(self))
            {
                if (!IsPartial(ct)) break;
                sb.AppendLine($"{GetAccessibilityKeyword(ct.DeclaredAccessibility)} partial {(ct.TypeKind == TypeKind.Struct ? "struct" : "class")} {ct.Name}");
                sb.AppendLine("{");
            }

            // generate partial
            sb.AppendLine($"{GetAccessibilityKeyword(self.DeclaredAccessibility)} partial {(self.TypeKind == TypeKind.Struct ? "struct" : "class")} {self.Name}");
            sb.AppendLine("{");
            // gather existing operators
            var existing = GetExistingOperatorPairs(self);

            foreach (var req in requests.Distinct())
            {
                var other = req.Other;
                if (other.TypeKind is not (TypeKind.Struct or TypeKind.Class))
                    continue;

                // already has an overload?
                if (existing.Contains((req.Kind, req.Hand, other, self)))
                    continue;

                // gather pairs
                var pairs = CollectMemberPairs(self, other, req, compilation, fixedAttr);
                if (pairs.Count == 0)
                    continue;

                EmitOperator(sb, self, other, req, pairs, compilation);
            }
            sb.AppendLine("}"); // end self

            var containing = GetContainingTypes(self);
            for (int i = 0; i < containing.Count; i++)
                sb.AppendLine("}");

            if (!string.IsNullOrEmpty(ns))
                sb.AppendLine("}");

            return sb.ToString();
        }
        private static void EmitOperator(
            StringBuilder sb,
            INamedTypeSymbol self,
            INamedTypeSymbol other,
            OpRequest req,
            List<MemberPair> pairs,
            Compilation compilation)
        {
            var selfFull = self.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            var otherFull = other.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            var opToken = req.Kind == OperatorKind.Add ? "+" : "-";
            var opTokenText = req.Kind == OperatorKind.Add ? "Adds" : "Subtracts";

            string signature;
            string resInit;
            string leftName, rightName;
            if (req.Hand == Handedness.LeftIsThis_To)
            {
                leftName = "__left";
                rightName = "__right";
                signature = $"public static {selfFull} operator {opToken}({selfFull} {leftName}, {otherFull} {rightName})";
                resInit = "var __res = __left;";
            }
            else
            {
                leftName = "__left";
                rightName = "__right";
                signature = $"public static {selfFull} operator {opToken}({otherFull} {leftName}, {selfFull} {rightName})";
                resInit = "var __res = __right;";
            }
            sb.AppendLine("    /// <summary>");
            sb.AppendLine($"    /// {opTokenText} matching numeric fields/properties by name, skipping members marked with [Matcher.Fixed].");
            sb.AppendLine("    /// </summary>");
            sb.AppendLine($"    {signature}");
            sb.AppendLine("    {");
            //sb.AppendLine($"        var __res = {DefaultInit(self)};");
            sb.AppendLine($"        {resInit}");
            foreach (var p in pairs)
            {
                var leftExpr = $"__res.{p.SelfName}";
                var rightExpr = $"{(req.Hand == Handedness.LeftIsThis_To ? "__right" : "__left")}.{p.OtherName}";

                sb.AppendLine($"        {leftExpr} = {leftExpr} {opToken} {rightExpr};");
            }
            // Handle [AddableCollection] members
            EmitCollectionOperations(sb, self, other, req, compilation);

            sb.AppendLine("        return __res;");
            sb.AppendLine("    }");
            sb.AppendLine();
        }
        private static string DefaultInit(INamedTypeSymbol t)
            => t.TypeKind == TypeKind.Struct
                ? $"default({t.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)})" // no new object for struct
                : $"new {t.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}()";
        private static HashSet<(OperatorKind kind, Handedness hand, INamedTypeSymbol other, INamedTypeSymbol self)> GetExistingOperatorPairs(INamedTypeSymbol self)
        {
            var set = new HashSet<(OperatorKind, Handedness, INamedTypeSymbol, INamedTypeSymbol)>(new SymTupleComparer());
            foreach (var m in self.GetMembers())
            {
                if (m is not IMethodSymbol ms) continue;
                if (!ms.IsStatic) continue;
                if (ms.MethodKind != MethodKind.UserDefinedOperator) continue;
                if (ms.Parameters.Length != 2) continue;

                var kind = ms.Name switch
                {
                    "op_Addition" => OperatorKind.Add,
                    "op_Subtraction" => OperatorKind.Sub,
                    _ => (OperatorKind?)null
                };
                if (kind is null) continue;

                var p0 = ms.Parameters[0].Type as INamedTypeSymbol;
                var p1 = ms.Parameters[1].Type as INamedTypeSymbol;
                if (p0 is null || p1 is null) continue;

                if (SymbolEqualityComparer.Default.Equals(p0, self))
                    set.Add((kind.Value, Handedness.LeftIsThis_To, p1, self));
                else if (SymbolEqualityComparer.Default.Equals(p1, self))
                    set.Add((kind.Value, Handedness.RightIsThis_From, p0, self));
            }
            return set;
        }
        private static List<MemberPair> CollectMemberPairs(
            INamedTypeSymbol self,
            INamedTypeSymbol other,
            OpRequest req,
            Compilation compilation,
            INamedTypeSymbol? fixedAttr)
        {
            // writable & not [Fixed] on self and inherited

            var selfMembers = new Dictionary<string, ISymbol>(StringComparer.Ordinal);
            for (var cur = self; cur is not null; cur = cur.BaseType)
            {
                foreach (var m in cur.GetMembers())
                {
                    if (m.IsStatic) continue;
                    if (selfMembers.ContainsKey(m.Name)) continue; // keep most derived
                    if (m is IFieldSymbol f)
                    {
                        if (f.IsConst || f.IsImplicitlyDeclared) continue;
                        if (HasAttr(f, "Matcher.FixedAttribute")) continue;
                        bool canWrite = f.DeclaredAccessibility switch
                        {
                            Accessibility.Public => true,
                            Accessibility.Internal => SymbolEquals(f.ContainingAssembly, self.ContainingAssembly),
                            Accessibility.ProtectedOrInternal => true,
                            Accessibility.ProtectedAndInternal => SymbolEquals(f.ContainingAssembly, self.ContainingAssembly),
                            Accessibility.Protected => true,
                            Accessibility.Private => SymbolEquals(f.ContainingType, self),
                            _ => false
                        };
                        if (!canWrite) continue;
                        selfMembers[m.Name] = f;
                    }
                    else if (m is IPropertySymbol p)
                    {
                        if (p.IsIndexer) continue;
                        if (HasAttr(p, "Matcher.FixedAttribute")) continue;
                        if (p.SetMethod is null) continue;
                        var acc = p.SetMethod.DeclaredAccessibility;
                        bool canWrite = acc switch
                        {
                            Accessibility.Public => true,
                            Accessibility.Internal => SymbolEquals(p.ContainingAssembly, self.ContainingAssembly),
                            Accessibility.ProtectedOrInternal => true,
                            Accessibility.ProtectedAndInternal => SymbolEquals(p.ContainingAssembly, self.ContainingAssembly),
                            Accessibility.Protected => true,
                            Accessibility.Private => SymbolEquals(p.ContainingType, self),
                            _ => false
                        };
                        if (!canWrite) continue;
                        selfMembers[m.Name] = p;
                    }
                }
            }
            // readable on other
            var otherReadable = new Dictionary<string, ISymbol>(StringComparer.Ordinal);
            for (var cur = other; cur is not null; cur = cur.BaseType)
            {
                foreach (var m in cur.GetMembers())
                {
                    if (m.IsStatic) continue;
                    if (otherReadable.ContainsKey(m.Name)) continue;
                    if (m is IFieldSymbol f)
                    {
                        if (f.IsImplicitlyDeclared) continue;
                        if (!IsReadableFromAssembly(f, self)) continue;
                        otherReadable[m.Name] = f;
                    }
                    else if (m is IPropertySymbol p)
                    {
                        if (p.IsIndexer) continue;
                        if (p.GetMethod is null) continue;
                        if (!IsReadableFromAssembly(p, self)) continue;
                        otherReadable[m.Name] = p;
                    }
                }
            }
            // aliases on both sides
            var selfAliasesByName = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            foreach (var kv in selfMembers)
            {
                var aliases = GetAliases(kv.Value).Distinct(StringComparer.Ordinal).ToList();
                if (aliases.Count > 0) selfAliasesByName[kv.Key] = aliases;
            }
            var otherAliasesByName = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            foreach (var kv in otherReadable)
            {
                var aliases = GetAliases(kv.Value).Distinct(StringComparer.Ordinal).ToList();
                if (aliases.Count > 0) otherAliasesByName[kv.Key] = aliases;
            }

            var otherAliasToNames = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            // reverse index
            foreach (var kvp in otherAliasesByName)
            {
                string otherName = kvp.Key;
                List<string> aliases = kvp.Value;
                foreach (var a in aliases)
                {
                    if (!otherAliasToNames.TryGetValue(a, out var list))
                    {
                        list = new List<string>();
                        otherAliasToNames[a] = list;
                    }
                    if (!list.Contains(otherName, StringComparer.Ordinal))
                        list.Add(otherName);
                }
            }
            var pairs = new List<MemberPair>();
            var usedOther = new HashSet<string>(StringComparer.Ordinal);
            foreach (var kvp in selfMembers)
            {
                string selfName = kvp.Key;
                ISymbol mSelf = kvp.Value;
                var candidates = new List<string> { selfName };
                if (selfAliasesByName.TryGetValue(selfName, out var sa)) candidates.AddRange(sa);
                if (otherAliasToNames.TryGetValue(selfName, out var viaOther)) candidates.AddRange(viaOther);

                var seen = new HashSet<string>(StringComparer.Ordinal);
                foreach (var cand in candidates)
                {
                    if (!seen.Add(cand)) continue; // deduplicate
                    if (usedOther.Contains(cand)) continue; // mapping
                    if (!otherReadable.TryGetValue(cand, out var mOther)) continue;

                    var tSelf = GetMemberType(mSelf);
                    var tOther = GetMemberType(mOther);
                    if (tSelf is null || tOther is null) continue;

                    // numeric only and implicit conversion
                    if (!IsNumeric(tSelf) || !IsNumeric(tOther)) continue;
                    var conv = compilation.ClassifyConversion(tOther, tSelf);
                    if (!conv.Exists || !conv.IsImplicit) continue;

                    pairs.Add(new MemberPair(selfName, cand));
                    usedOther.Add(cand);
                    break; // next self member
                }
            }

            return pairs;
        }
        private static void EmitCollectionOperations(
            StringBuilder sb,
            INamedTypeSymbol self,
            INamedTypeSymbol other,
            OpRequest req,
            Compilation compilation)
        {
            var otherParam = req.Hand == Handedness.LeftIsThis_To ? "__right" : "__left";
            var otherReadable = new Dictionary<string, ISymbol>(StringComparer.Ordinal);
            for (var cur = other; cur is not null; cur = cur.BaseType)
            {
                foreach (var m in cur.GetMembers())
                {
                    if (m.IsStatic) continue;
                    if (otherReadable.ContainsKey(m.Name)) continue;
                    if (m is IFieldSymbol f)
                    {
                        if (f.IsImplicitlyDeclared) continue;
                        if (!IsReadableFromAssembly(f, self)) continue;
                        otherReadable[m.Name] = f;
                    }
                    else if (m is IPropertySymbol p)
                    {
                        if (p.IsIndexer) continue;
                        if (p.GetMethod is null) continue;
                        if (!IsReadableFromAssembly(p, self)) continue;
                        otherReadable[m.Name] = p;
                    }
                }
            }
            var otherAliasesByName = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            foreach (var kv in otherReadable)
            {
                var aliases = GetAliases(kv.Value).Distinct(StringComparer.Ordinal).ToList();
                if (aliases.Count > 0) otherAliasesByName[kv.Key] = aliases;
            }
            var otherAliasToNames = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            foreach (var kvp in otherAliasesByName)
            {
                foreach (var a in kvp.Value)
                {
                    if (!otherAliasToNames.TryGetValue(a, out var list))
                    {
                        list = new List<string>();
                        otherAliasToNames[a] = list;
                    }
                    if (!list.Contains(kvp.Key, StringComparer.Ordinal))
                        list.Add(kvp.Key);
                }
            }
            foreach (var info in EnumerateAddableCollections(self, compilation))
            {
                string member = info.MemberName;
                string elemFull = info.ElementType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                var selfAliases = new List<string>();
                ISymbol? selfMemberSym = null;
                for (var cur = self; cur is not null && selfMemberSym is null; cur = cur.BaseType)
                {
                    selfMemberSym = cur.GetMembers().FirstOrDefault(m => !m.IsStatic && m.Name == member);
                }
                if (selfMemberSym != null)
                    selfAliases.AddRange(GetAliases(selfMemberSym));
                var candidateKeys = new List<string> { member };
                candidateKeys.AddRange(selfAliases.Distinct(StringComparer.Ordinal));
                // Try resolve a readable member
                string? matchedOtherName = null;
                ISymbol? matchedOtherSym = null;
                bool matchedAsElement = false;
                bool matchedAsSameCollection = false;
                foreach (var key in candidateKeys)
                {
                    // direct name match
                    if (otherReadable.TryGetValue(key, out var m))
                    {
                        var tOther = GetMemberType(m);
                        if (tOther != null)
                        {
                            // Element match
                            var conv = compilation.ClassifyConversion(tOther, info.ElementType);
                            if (conv.Exists && conv.IsImplicit)
                            {
                                matchedOtherName = key; matchedOtherSym = m; matchedAsElement = true; break;
                            }
                            // Collection match
                            if (TryGetCollectionInfo(tOther, compilation, out var k2, out var e2) &&
                                SymbolEquals(e2, info.ElementType) &&
                                ((k2 == CollectionKind.List && info.Kind == CollectionKind.List) ||
                                (k2 == CollectionKind.Array && info.Kind == CollectionKind.Array)))
                            {
                                matchedOtherName = key; matchedOtherSym = m; matchedAsSameCollection = true; break;
                            }
                        }
                    }
                    // match via alias
                    if (matchedOtherSym is null && otherAliasToNames.TryGetValue(key, out var via))
                    {
                        foreach (var oname in via)
                        {
                            if (!otherReadable.TryGetValue(oname, out var m2)) continue;
                            var tOther2 = GetMemberType(m2);
                            if (tOther2 is null) continue;
                            var conv2 = compilation.ClassifyConversion(tOther2, info.ElementType);
                            if (conv2.Exists && conv2.IsImplicit)
                            {
                                matchedOtherName = oname; matchedOtherSym = m2; matchedAsElement = true; break;
                            }
                            if (TryGetCollectionInfo(tOther2, compilation, out var k3, out var e3) &&
                                SymbolEquals(e3, info.ElementType) &&
                                ((k3 == CollectionKind.List && info.Kind == CollectionKind.List) ||
                                (k3 == CollectionKind.Array && info.Kind == CollectionKind.Array)))
                            {
                                matchedOtherName = oname; matchedOtherSym = m2; matchedAsSameCollection = true; break;
                            }
                        }
                    }
                    if (matchedOtherSym != null) break;
                }
                if (matchedOtherSym == null) continue; // nothing to do
                var otherAccess = $"{otherParam}.{matchedOtherName}";
                if (req.Kind == OperatorKind.Add)
                {
                    if (matchedAsElement)
                    {
                        if (info.Kind == CollectionKind.List)
                        {
                            sb.AppendLine($"        if (__res.{member} == null) __res.{member} = new global::System.Collections.Generic.List<{elemFull}>();");
                            sb.AppendLine($"        __res.{member}.Add({otherAccess});");
                        }
                        else if (info.Kind == CollectionKind.Array)
                        {
                            sb.AppendLine($"        var __a_{member} = __res.{member};");
                            sb.AppendLine($"        if (__a_{member} == null) {{ __res.{member} = new {elemFull}[] {{ {otherAccess} }}; }}");
                            sb.AppendLine($"        else {{ var __len_{member} = __a_{member}.Length; var __new_{member} = new {elemFull}[__len_{member} + 1];");
                            sb.AppendLine($"               global::System.Array.Copy(__a_{member}, __new_{member}, __len_{member}); __new_{member}[__len_{member}] " +
                                $"= {otherAccess}; __res.{member} = __new_{member}; }}");
                        }
                    }
                    else if (matchedAsSameCollection)
                    {
                        if (info.Kind == CollectionKind.List)
                        {
                            sb.AppendLine($"        var __o_{member} = {otherAccess};");
                            sb.AppendLine($"        if (__o_{member} != null) {{");
                            sb.AppendLine($"            if (__res.{member} == null) __res.{member} = new global::System.Collections.Generic.List<{elemFull}>(__o_{member});");
                            sb.AppendLine($"            else __res.{member}.AddRange(__o_{member});");
                            sb.AppendLine($"        }}");
                        }
                        else if (info.Kind == CollectionKind.Array)
                        {
                            sb.AppendLine($"        var __a_{member} = __res.{member};");
                            sb.AppendLine($"        var __b_{member} = {otherAccess};");
                            sb.AppendLine($"        if (__b_{member} != null && __b_{member}.Length != 0) {{");
                            sb.AppendLine($"            if (__a_{member} == null || __a_{member}.Length == 0) __res.{member} = ( {elemFull}[] )__b_{member}.Clone();");
                            sb.AppendLine($"            else {{ var __new_{member} = new {elemFull}[__a_{member}.Length + __b_{member}.Length];");
                            sb.AppendLine($"                   global::System.Array.Copy(__a_{member}, __new_{member}, __a_{member}.Length);");
                            sb.AppendLine($"                   global::System.Array.Copy(__b_{member}, 0, __new_{member}, __a_{member}.Length, __b_{member}.Length);");
                            sb.AppendLine($"                   __res.{member} = __new_{member}; }}");
                            sb.AppendLine($"        }}");
                        }
                    }
                }
                else if (req.Kind == OperatorKind.Sub)
                {
                    if (matchedAsElement)
                    {
                        if (info.Kind == CollectionKind.List)
                        {
                            sb.AppendLine($"        if (__res.{member} != null) __res.{member}.Remove({otherAccess});");
                        }
                        else if (info.Kind == CollectionKind.Array)
                        {
                            sb.AppendLine($"        var __a_{member} = __res.{member};");
                            sb.AppendLine($"        if (__a_{member} != null) {{ int __idx_{member} = global::System.Array.IndexOf(__a_{member}, {otherAccess});");
                            sb.AppendLine($"            if (__idx_{member} >= 0) {{ var __new_{member} = new {elemFull}[__a_{member}.Length - 1];");
                            sb.AppendLine($"                if (__idx_{member} > 0) global::System.Array.Copy(__a_{member}, 0, __new_{member}, 0, __idx_{member});");
                            sb.AppendLine($"                if (__idx_{member} + 1 < __a_{member}.Length) global::System.Array.Copy(__a_{member}, __idx_{member} + 1, __new_{member}, __idx_{member}, __a_{member}.Length - __idx_{member} - 1);");
                            sb.AppendLine($"                __res.{member} = __new_{member}; }}");
                            sb.AppendLine($"        }}");
                        }
                    }
                    else if (matchedAsSameCollection)
                    {
                        if (info.Kind == CollectionKind.List)
                        {
                            sb.AppendLine($"        var __lst_{member} = __res.{member}; var __o_{member} = {otherAccess};");
                            sb.AppendLine($"        if (__lst_{member} != null && __o_{member} != null) {{ foreach (var __it in __o_{member}) __lst_{member}.Remove(__it); }}");
                        }
                        else if (info.Kind == CollectionKind.Array)
                        {
                            sb.AppendLine($"        var __a_{member} = __res.{member}; var __b_{member} = {otherAccess};");
                            sb.AppendLine($"        if (__a_{member} != null && __b_{member} != null) {{");
                            sb.AppendLine($"            var __tmp_{member} = new global::System.Collections.Generic.List<{elemFull}>(__a_{member});");
                            sb.AppendLine($"            int __origCnt_{member} = __tmp_{member}.Count;");
                            sb.AppendLine($"            foreach (var __it in __b_{member}) __tmp_{member}.Remove(__it);");
                            sb.AppendLine($"            if (__tmp_{member}.Count != __origCnt_{member}) __res.{member} = __tmp_{member}.ToArray();");
                            sb.AppendLine($"        }}");
                        }
                    }
                }
            }
        }
        private static IEnumerable<(string MemberName, CollectionKind Kind, ITypeSymbol ElementType)> EnumerateAddableCollections(INamedTypeSymbol self, Compilation compilation)
        {
            // writable & not [Fixed] & [AddableCollection]
            for (var cur = self; cur is not null; cur = cur.BaseType)
            {
                foreach (var m in cur.GetMembers())
                {
                    if (m.IsStatic) continue;
                    if (!HasAttr(m, "Matcher.AddableCollectionAttribute")) continue;
                    ISymbol? chosen = null;
                    if (m is IFieldSymbol f)
                    {
                        if (f.IsConst || f.IsImplicitlyDeclared) continue;
                        bool canWrite = f.DeclaredAccessibility switch
                        {
                            Accessibility.Public => true,
                            Accessibility.Internal => SymbolEquals(f.ContainingAssembly, self.ContainingAssembly),
                            Accessibility.ProtectedOrInternal => true,
                            Accessibility.ProtectedAndInternal => SymbolEquals(f.ContainingAssembly, self.ContainingAssembly),
                            Accessibility.Protected => true,
                            Accessibility.Private => SymbolEquals(f.ContainingType, self),
                            _ => false
                        };
                        if (!canWrite) continue;
                        chosen = f;
                    }
                    else if (m is IPropertySymbol p)
                    {
                        if (p.IsIndexer) continue;
                        if (p.SetMethod is null) continue;
                        var acc = p.SetMethod.DeclaredAccessibility;
                        bool canWrite = acc switch
                        {
                            Accessibility.Public => true,
                            Accessibility.Internal => SymbolEquals(p.ContainingAssembly, self.ContainingAssembly),
                            Accessibility.ProtectedOrInternal => true,
                            Accessibility.ProtectedAndInternal => SymbolEquals(p.ContainingAssembly, self.ContainingAssembly),
                            Accessibility.Protected => true,
                            Accessibility.Private => SymbolEquals(p.ContainingType, self),
                            _ => false
                        };
                        if (!canWrite) continue;
                        chosen = p;
                    }
                    if (chosen is null) continue;
                    var t = GetMemberType(chosen);
                    if (t is null) continue;
                    if (TryGetCollectionInfo(t, compilation, out var kind, out var elem))
                        yield return (m.Name, kind, elem);
                }
            }
        }
        private static bool TryGetCollectionInfo(ITypeSymbol t, Compilation compilation, out CollectionKind kind, out ITypeSymbol element)
        {
            // List<T>
            if (t is INamedTypeSymbol nt && nt.IsGenericType)
            {
                var listDef = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1");
                if (listDef != null && SymbolEquals(nt.OriginalDefinition, listDef))
                {
                    kind = CollectionKind.List;
                    element = nt.TypeArguments[0];
                    return true;
                }
            }
            // T[]
            if (t is IArrayTypeSymbol at)
            {
                kind = CollectionKind.Array;
                element = at.ElementType;
                return true;
            }
            kind = default;
            element = null!;
            return false;
        }
        private static void ReportNotPartial(GeneratorExecutionContext context, INamedTypeSymbol type)
        {
            var desc = new DiagnosticDescriptor(
                id: "MATH001",
                title: "Type must be partial to generate operators",
                messageFormat: "Type '{0}' must be declared as partial to generate operator overloads.",
                category: "Matcher.Arithmetic",
                DiagnosticSeverity.Info,
                isEnabledByDefault: true);
            context.ReportDiagnostic(Diagnostic.Create(desc, Location.None, type.ToDisplayString()));
        }
        private enum CollectionKind { List, Array }
        private enum OperatorKind { Add, Sub }
        private enum Handedness { LeftIsThis_To, RightIsThis_From }
        private readonly struct MemberPair
        {
            public string SelfName { get; }
            public string OtherName { get; }
            public MemberPair(string selfName, string otherName)
            {
                SelfName = selfName;
                OtherName = otherName;
            }
        }
        private readonly struct OpRequest : IEquatable<OpRequest>
        {
            public OperatorKind Kind { get; }
            public Handedness Hand { get; }
            public INamedTypeSymbol Other { get; }

            public OpRequest(OperatorKind kind, Handedness hand, INamedTypeSymbol other)
            {
                Kind = kind; Hand = hand; Other = other;
            }

            public bool Equals(OpRequest other) =>
                Kind == other.Kind &&
                Hand == other.Hand &&
                SymbolEqualityComparer.Default.Equals(Other, other.Other);

            public override bool Equals(object? obj) => obj is OpRequest o && Equals(o);
            public override int GetHashCode()
            {
                unchecked
                {
                    int h = (int)Kind;
                    h = (h * 397) ^ (int)Hand;
                    h = (h * 397) ^ SymbolEqualityComparer.Default.GetHashCode(Other);
                    return h;
                }
            }
        }
        
        private sealed class SymTupleComparer : IEqualityComparer<(OperatorKind, Handedness, INamedTypeSymbol, INamedTypeSymbol)>
        {
            public bool Equals((OperatorKind, Handedness, INamedTypeSymbol, INamedTypeSymbol) x,
                               (OperatorKind, Handedness, INamedTypeSymbol, INamedTypeSymbol) y)
            {
                return x.Item1 == y.Item1 && x.Item2 == y.Item2 &&
                       SymbolEqualityComparer.Default.Equals(x.Item3, y.Item3) &&
                       SymbolEqualityComparer.Default.Equals(x.Item4, y.Item4);
            }
            public int GetHashCode((OperatorKind, Handedness, INamedTypeSymbol, INamedTypeSymbol) obj)
            {
                unchecked
                {
                    int h = (int)obj.Item1;
                    h = (h * 397) ^ (int)obj.Item2;
                    h = (h * 397) ^ SymbolEqualityComparer.Default.GetHashCode(obj.Item3);
                    h = (h * 397) ^ SymbolEqualityComparer.Default.GetHashCode(obj.Item4);
                    return h;
                }
            }
        }
        internal static IEnumerable<string> GetAliases(ISymbol m)
        {
            // Read [Matcher.FieldAliasAttribute(string)]
            foreach (var a in m.GetAttributes())
            {
                var ac = a.AttributeClass;
                var isAlias = ac?.ToDisplayString() == "Matcher.FieldAliasAttribute"
                              || ac?.Name == "FieldAliasAttribute";
                if (!isAlias) continue;

                if (a.ConstructorArguments.Length == 1 &&
                    a.ConstructorArguments[0].Value is string s &&
                    !string.IsNullOrWhiteSpace(s))
                {
                    yield return s.Trim();
                }
            }
        }

        private sealed class SyntaxReceiver : ISyntaxReceiver
        {
            public List<TypeDeclarationSyntax> Candidates { get; } = new();

            public void OnVisitSyntaxNode(SyntaxNode node)
            {
                if (node is TypeDeclarationSyntax tds &&
                    (tds is ClassDeclarationSyntax or StructDeclarationSyntax) &&
                    tds.AttributeLists.Count > 0)
                {
                    Candidates.Add(tds);
                }
            }
        }

    }
}
