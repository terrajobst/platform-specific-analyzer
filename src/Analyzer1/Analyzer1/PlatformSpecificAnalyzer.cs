using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

namespace Analyzer1
{
    [DiagnosticAnalyzer(LanguageNames.CSharp, LanguageNames.VisualBasic)]
    public sealed class PlatformSpecificAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor _pc0001 = new DiagnosticDescriptor(
            id: @"PC0001",
            title: new LocalizableResourceString(nameof(Resources.PC0001Title), Resources.ResourceManager, typeof(Resources)),
            messageFormat: new LocalizableResourceString(nameof(Resources.PC0001MessageFormat), Resources.ResourceManager, typeof(Resources)),
            category: @"Portability",
            defaultSeverity: DiagnosticSeverity.Warning,
            isEnabledByDefault: true,
            description: new LocalizableResourceString(nameof(Resources.PC0001Description), Resources.ResourceManager, typeof(Resources))
        );

        private static readonly DiagnosticDescriptor _pc0002 = new DiagnosticDescriptor(
            id: @"PC0002",
            title: new LocalizableResourceString(nameof(Resources.PC0002Title), Resources.ResourceManager, typeof(Resources)),
            messageFormat: new LocalizableResourceString(nameof(Resources.PC0002MessageFormat), Resources.ResourceManager, typeof(Resources)),
            category: @"Portability",
            defaultSeverity: DiagnosticSeverity.Warning,
            isEnabledByDefault: true,
            description: new LocalizableResourceString(nameof(Resources.PC0002Description), Resources.ResourceManager, typeof(Resources))
        );

        private static readonly DiagnosticDescriptor _pc0003 = new DiagnosticDescriptor(
            id: @"PC0003",
            title: new LocalizableResourceString(nameof(Resources.PC0003Title), Resources.ResourceManager, typeof(Resources)),
            messageFormat: new LocalizableResourceString(nameof(Resources.PC0003MessageFormat), Resources.ResourceManager, typeof(Resources)),
            category: @"Portability",
            defaultSeverity: DiagnosticSeverity.Warning,
            isEnabledByDefault: true,
            description: new LocalizableResourceString(nameof(Resources.PC0003Description), Resources.ResourceManager, typeof(Resources))
        );

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(_pc0001, _pc0002, _pc0003);

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            context.RegisterCompilationStartAction(InitializeCompilationAnalysis);
        }

        private void InitializeCompilationAnalysis(CompilationStartAnalysisContext context)
        {
            var analysis = new Analysis(context.Compilation);

            context.RegisterOperationAction(
                analysis.Run,
                OperationKind.ObjectCreation,
                OperationKind.Invocation,
                OperationKind.MethodReference,
                OperationKind.PropertyReference,
                OperationKind.EventReference,
                OperationKind.Throw
            );
        }

        private sealed class Analysis
        {
            private readonly SymbolDisplayFormat _symbolDisplayFormat;
            private readonly INamedTypeSymbol _platformNotSupportedException;
            private readonly INamedTypeSymbol _osPlatformType;
            private readonly IMethodSymbol _isOSPlatformMethod;
            private readonly ConcurrentDictionary<IAssemblySymbol, INamedTypeSymbol> _platformSpecificAttributes = new ConcurrentDictionary<IAssemblySymbol, INamedTypeSymbol>();

            public Analysis(Compilation compilation)
            {
                _symbolDisplayFormat = compilation.Language == LanguageNames.VisualBasic
                        ? SymbolDisplayFormat.VisualBasicShortErrorMessageFormat
                        : SymbolDisplayFormat.CSharpShortErrorMessageFormat;

                _platformNotSupportedException = compilation.GetTypeByMetadataName("System.PlatformNotSupportedException");
                _osPlatformType = compilation.GetTypeByMetadataName("System.Runtime.InteropServices.OSPlatform");
                var runtimeInformationType = compilation.GetTypeByMetadataName("System.Runtime.InteropServices.RuntimeInformation");
                _isOSPlatformMethod = runtimeInformationType?.GetMembers("IsOSPlatform")
                                                             .OfType<IMethodSymbol>()
                                                             .FirstOrDefault(m => m.Parameters.Length == 1 && m.Parameters[0].Type == _osPlatformType);
            }

            public void Run(OperationAnalysisContext context)
            {
                switch (context.Operation.Kind)
                {
                    case OperationKind.ObjectCreation:
                    {
                        var op = (IObjectCreationOperation)context.Operation;
                        CheckForPortability(context, op.Constructor);
                        break;
                    }
                    case OperationKind.Invocation:
                    {
                        var op = (IInvocationOperation)context.Operation;
                        CheckForPortability(context, op.TargetMethod);
                        break;
                    }
                    case OperationKind.MethodReference:
                    case OperationKind.PropertyReference:
                    case OperationKind.EventReference:
                    {
                        var op = (IMemberReferenceOperation)context.Operation;
                        CheckForPortability(context, op.Member);
                        break;
                    }
                    case OperationKind.Throw:
                    {
                        var op = (IThrowOperation)context.Operation;
                        CheckForPlatformNotSupportedException(context, op.Exception.Type);
                        break;
                    }

                    default:
                        throw new ArgumentException("Unexpected value for IOperation", nameof(context));
                }
            }

            private void CheckForPortability(OperationAnalysisContext context, ISymbol symbol)
            {
                // Let's be defensive

                if (symbol == null)
                    return;

                // If the symbol isn't marked as platform-specific, this analyzer doesn't have
                // to do any work.

                if (!TryGetPlatformLimitations(symbol, out var referencedLimitations))
                    return;

                // Now if the method we're calling from is marked to be platform-specific. If so,
                // then the called method needs to support all platforms we're specific for.

                var callSiteLimitations = new List<string>();

                if (TryGetPlatformGuards(context.Operation, out var guardedPlatforms))
                {
                    callSiteLimitations.AddRange(guardedPlatforms);

                    if (TryGetPlatformLimitations(context.ContainingSymbol, out var declaredLimitations))
                    {
                        // OK, the method we're calling from is platform-specific and a guard is in place.
                        // Let's see whether the guard checks for any platform we're running on. If not,
                        // the code behehind the guard isn't reachable.

                        // TODO: We should mark unreachable guards (via WellKnownDiagnosticTags.Unnecessary).
                        //       If all guards are unreachable, we should mark the entire contents of the if
                        //       block as unreachable.
                        //
                        //       I don't think we'll want to do this here; rather we should do this directly
                        //       in TryGetPlatformGuards(). For that to work, we'd need to pass the declared
                        //       platform limitations.

                        var reachable = false;

                        foreach (var g in guardedPlatforms)
                        {
                            if (declaredLimitations.Contains(g))
                                reachable = true;
                        }

                        if (!reachable)
                            return;
                    }
                }
                else
                {
                    if (TryGetPlatformLimitations(context.ContainingSymbol, out var declaredLimitations))
                    {
                        callSiteLimitations.AddRange(declaredLimitations);
                    }
                }

                if (callSiteLimitations.Any())
                {
                    // The call site is platform-specific as is the referenced symbol. That means that
                    // the reference method needs to support all platforms the call site is specific to.

                    foreach (var p in referencedLimitations)
                        callSiteLimitations.Remove(p);

                    if (callSiteLimitations.Count == 0)
                        return;

                    // The symbol doesn't support all the platforms that the call site is supported on.
                    // Report all missing ones.

                    var formattedSymbol = symbol.ToDisplayString(_symbolDisplayFormat);
                    var formattedLimitations = string.Join(", ", callSiteLimitations.Select(p => "'" + p + "'"));
                    var location = context.Operation.Syntax.GetLocation();
                    var diagnostic = Diagnostic.Create(_pc0002, location, formattedSymbol, formattedLimitations);
                    context.ReportDiagnostic(diagnostic);
                }
                else
                {
                    // The call site is supposed to work everywhere but the referenced symbol isn't. Report
                    // a diagnostic.

                    var formattedSymbol = symbol.ToDisplayString(_symbolDisplayFormat);
                    var formattedPlatforms = string.Join(", ", referencedLimitations.Select(p => "'" + p + "'"));
                    var location = context.Operation.Syntax.GetLocation();
                    var diagnostic = Diagnostic.Create(_pc0001, location, formattedSymbol, formattedPlatforms);
                    context.ReportDiagnostic(diagnostic);
                }
            }

            private bool TryGetPlatformLimitations(ISymbol symbol, out List<string> platforms)
            {
                if (TryGetPlatformLimitationsDirect(symbol, out platforms))
                    return true;

                switch (symbol.Kind)
                {
                    case SymbolKind.NetModule:
                        var module = (IModuleSymbol)symbol;
                        return TryGetPlatformLimitations(module.ContainingAssembly, out platforms);
                    case SymbolKind.NamedType:
                        var type = (ITypeSymbol)symbol;
                        if (type.ContainingType != null)
                            return TryGetPlatformLimitations(type.ContainingType, out platforms);
                        else
                            return TryGetPlatformLimitations(type.ContainingModule, out platforms);
                    case SymbolKind.Method:
                        var method = (IMethodSymbol)symbol;
                        return TryGetPlatformLimitations(method.ContainingType, out platforms);
                    case SymbolKind.Property:
                        var property = (IPropertySymbol)symbol;
                        return TryGetPlatformLimitations(property.ContainingType, out platforms);
                    case SymbolKind.Event:
                        var @event = (IEventSymbol)symbol;
                        return TryGetPlatformLimitations(@event.ContainingType, out platforms);
                }

                return false;
            }

            private bool TryGetPlatformLimitationsDirect(ISymbol symbol, out List<string> platforms)
            {
                platforms = null;

                var assembly = symbol is IAssemblySymbol a ? a : symbol.ContainingAssembly;
                var platformSpecificAttribute = LookupPlatformSpecificAttribute(assembly);

                foreach (var ca in symbol.GetAttributes())
                {
                    if (ca.AttributeClass == platformSpecificAttribute)
                    {
                        if (ca.ConstructorArguments.Length == 1 &&
                            ca.ConstructorArguments[0].Value is string platform)
                        {
                            if (platforms == null)
                                platforms = new List<string>();

                            platforms.Add(platform);
                        }
                    }
                }

                if (platforms != null)
                {
                    // This reads better but also ensures that we have a deterministic text
                    // for the diagnostics.
                    platforms.Sort();
                }

                return platforms != null;
            }

            private bool TryGetPlatformGuards(IOperation operation, out List<string> platformGuards)
            {
                List<string> platformGuardBuilder = null;

                while (operation != null)
                {
                    if (operation is IConditionalOperation conditional)
                    {
                        if (TryGetPlatformGuardFromCondition(conditional.Condition, ref platformGuardBuilder))
                        {
                            platformGuards = platformGuardBuilder;
                            return true;
                        }
                    }

                    operation = operation.Parent;
                }

                platformGuards = null;
                return false;
            }

            private bool TryGetPlatformGuardFromCondition(IOperation condition, ref List<string> platformGuardBuilder)
            {
                switch (condition.Kind)
                {
                    case OperationKind.BinaryOperator:
                    {
                        var binary = (IBinaryOperation)condition;
                        if (binary.OperatorKind == BinaryOperatorKind.Or ||
                            binary.OperatorKind == BinaryOperatorKind.ConditionalOr)
                        {
                            return TryGetPlatformGuardFromCondition(binary.LeftOperand, ref platformGuardBuilder) &&
                                   TryGetPlatformGuardFromCondition(binary.RightOperand, ref platformGuardBuilder);
                        }
                        break;
                    }

                    case OperationKind.Invocation:
                    {
                        var invocation = (IInvocationOperation) condition;
                        if (TryGetPlatformGuadFromInvocation(invocation, ref platformGuardBuilder))
                            return true;
                        break;
                    }
                }

                return false;
            }

            private bool TryGetNegatedPlatformGuard(IOperation operation, out List<string> platformGuards)
            {
                List<string> platformGuardBuilder = null;

                while (operation != null)
                {
                    if (operation is IConditionalOperation conditional)
                    {
                        if (TryGetNegatedPlatformGuardFromCondition(conditional.Condition, ref platformGuardBuilder))
                        {
                            platformGuards = platformGuardBuilder;
                            return true;
                        }
                    }

                    operation = operation.Parent;
                }

                platformGuards = null;
                return false;
            }

            private bool TryGetNegatedPlatformGuardFromCondition(IOperation condition, ref List<string> platformGuardBuilder)
            {
                switch (condition.Kind)
                {
                    case OperationKind.BinaryOperator:
                    {
                        var binary = (IBinaryOperation)condition;
                        if (binary.OperatorKind == BinaryOperatorKind.And ||
                            binary.OperatorKind == BinaryOperatorKind.ConditionalAnd)
                        {
                            return TryGetNegatedPlatformGuardFromCondition(binary.LeftOperand, ref platformGuardBuilder) &&
                                   TryGetNegatedPlatformGuardFromCondition(binary.RightOperand, ref platformGuardBuilder);
                        }
                        
                        break;
                    }

                    case OperationKind.UnaryOperator:
                    {
                        var unary = (IUnaryOperation)condition;
                        if (unary.OperatorKind == UnaryOperatorKind.Not &&
                            unary.Operand is IInvocationOperation invocation)
                        {
                            if (TryGetPlatformGuadFromInvocation(invocation, ref platformGuardBuilder))
                                return true;
                        }

                        break;
                    }
                }

                return false;
            }

            private bool TryGetPlatformGuadFromInvocation(IInvocationOperation invocation, ref List<string> platformGuardBuilder)
            {
                if (invocation.TargetMethod == _isOSPlatformMethod &&
                    invocation.Arguments[0].Value is IPropertyReferenceOperation propertyReference &&
                    propertyReference.Property.Type == _osPlatformType)
                {
                    if (platformGuardBuilder == null)
                        platformGuardBuilder = new List<string>();

                    platformGuardBuilder.Add(propertyReference.Property.Name);
                    return true;
                }

                return false;
            }

            private void CheckForPlatformNotSupportedException(OperationAnalysisContext context, ITypeSymbol exceptionType)
            {
                if (exceptionType != _platformNotSupportedException)
                    return;

                if (!TryGetNegatedPlatformGuard(context.Operation, out var platformGuards))
                    return;

                if (TryGetPlatformLimitations(context.ContainingSymbol, out var declaredPlatforms))
                {
                    foreach (var declaredPlatform in declaredPlatforms)
                        platformGuards.Remove(declaredPlatform);

                    if (platformGuards.Count == 0)
                        return;
                }

                // The method throws for certain platforms but the method isn't marked as platform-specific.

                var symbol = context.ContainingSymbol;
                var formattedSymbol = symbol.ToDisplayString(_symbolDisplayFormat);
                var formattedPlatforms = string.Join(", ", platformGuards.Select(p => "'" + p + "'"));
                var location = context.Operation.Syntax.GetLocation();
                var diagnostic = Diagnostic.Create(_pc0003, location, formattedSymbol, formattedPlatforms);
                context.ReportDiagnostic(diagnostic);
            }

            private ITypeSymbol LookupPlatformSpecificAttribute(IAssemblySymbol assembly)
            {
                if (!_platformSpecificAttributes.TryGetValue(assembly, out var result))
                {
                    result = assembly.GetTypeByMetadataName("System.Runtime.CompilerServices.PlatformSpecificAttribute");
                    _platformSpecificAttributes.TryAdd(assembly, result);
                }

                return result;
            }
        }
    }
}
