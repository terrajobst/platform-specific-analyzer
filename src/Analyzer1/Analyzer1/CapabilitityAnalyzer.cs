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
    public sealed class CapabilitityAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor _cp0001 = new DiagnosticDescriptor(
            id: @"CP0001",
            title: "", //new LocalizableResourceString(nameof(Resources.PC0001Title), Resources.ResourceManager, typeof(Resources)),
            messageFormat: "'{0}' requires the capability '{1}'", //new LocalizableResourceString(nameof(Resources.PC0001MessageFormat), Resources.ResourceManager, typeof(Resources)),
            category: @"Portability",
            defaultSeverity: DiagnosticSeverity.Warning,
            isEnabledByDefault: true,
            description: "" //new LocalizableResourceString(nameof(Resources.PC0001Description), Resources.ResourceManager, typeof(Resources))
        );

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(_cp0001);

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
                OperationKind.EventReference
            );
        }

        private sealed class Analysis
        {
            private readonly SymbolDisplayFormat _symbolDisplayFormat;
            private readonly INamedTypeSymbol _capabilityAttribute;
            private readonly INamedTypeSymbol _capabilityCheckAttribute;

            public Analysis(Compilation compilation)
            {
                _symbolDisplayFormat = compilation.Language == LanguageNames.VisualBasic
                        ? SymbolDisplayFormat.VisualBasicShortErrorMessageFormat
                        : SymbolDisplayFormat.CSharpShortErrorMessageFormat;

                _capabilityAttribute = compilation.GetTypeByMetadataName("System.Runtime.InteropServices.CapabilityAttribute");
                _capabilityCheckAttribute = compilation.GetTypeByMetadataName("System.Runtime.InteropServices.CapabilityCheckAttribute");
            }

            public void Run(OperationAnalysisContext context)
            {
                switch (context.Operation.Kind)
                {
                    case OperationKind.ObjectCreation:
                    {
                        var op = (IObjectCreationOperation)context.Operation;
                        AnalyzeSymbolUsage(context, op.Constructor);
                        break;
                    }
                    case OperationKind.Invocation:
                    {
                        var op = (IInvocationOperation)context.Operation;
                        AnalyzeSymbolUsage(context, op.TargetMethod);
                        break;
                    }
                    case OperationKind.MethodReference:
                    case OperationKind.PropertyReference:
                    case OperationKind.EventReference:
                    {
                        var op = (IMemberReferenceOperation)context.Operation;
                        AnalyzeSymbolUsage(context, op.Member);
                        break;
                    }

                    default:
                        throw new ArgumentException("Unexpected value for IOperation", nameof(context));
                }
            }

            private void AnalyzeSymbolUsage(OperationAnalysisContext context, ISymbol symbol)
            {
                var capabilities = GetCapabilities(symbol);
                if (capabilities == null)
                    return;

                foreach (var capability in capabilities)
                {
                    var checkers = GetCapabilityCheckers(capability);
                    if (checkers == null)
                        continue;

                    // If the symbol itself is a capability API, we skip.
                    if (checkers.Contains(symbol))
                        continue;

                    // If the operation is contained in a guard, we skip.
                    if (IsGuarded(context, context.Operation, symbol, checkers))
                        continue;

                    // If the containing symbol requires the capability, we skip.
                    var containingCapabilities = GetCapabilities(context.ContainingSymbol);
                    if (containingCapabilities != null && containingCapabilities.Contains(capability))
                        continue;

                    var location = context.Operation.Syntax.GetLocation();
                    var formattedSymbol = symbol.ToDisplayString(_symbolDisplayFormat);
                    var formattedCapability = GetCapabilityName(capability);
                    var diagnostic = Diagnostic.Create(_cp0001, location, formattedSymbol, formattedCapability);
                    context.ReportDiagnostic(diagnostic);
                }
            }

            private object GetCapabilityName(INamedTypeSymbol capability)
            {
                const string RequiresPrefix = "Requires";
                const string AttributeSuffix = "Attribute";
                const string CapabilitySuffix = "Capability";

                var result = capability.Name;

                if (result.StartsWith(RequiresPrefix))
                    result = result.Substring(RequiresPrefix.Length);

                if (result.EndsWith(AttributeSuffix))
                    result = result.Substring(0, result.Length - AttributeSuffix.Length);

                if (result.EndsWith(CapabilitySuffix))
                    result = result.Substring(0, result.Length - CapabilitySuffix.Length);

                return result;
            }

            private bool IsGuarded(OperationAnalysisContext context, IOperation operation, ISymbol symbol, List<IPropertySymbol> checkers)
            {
                while (operation != null)
                {
                    if (operation is IConditionalOperation conditional)
                    {
                        if (IsGuardedInCondition(conditional.Condition, checkers))
                            return true;
                    }

                    operation = operation.Parent;
                }

                return false;
            }

            private bool IsGuardedInCondition(IOperation condition, List<IPropertySymbol> checkers)
            {
                switch (condition.Kind)
                {
                    case OperationKind.BinaryOperator:
                        var binary = (IBinaryOperation)condition;
                        if (binary.OperatorKind == BinaryOperatorKind.And ||
                            binary.OperatorKind == BinaryOperatorKind.ConditionalAnd)
                        {
                            return IsGuardedInCondition(binary.LeftOperand, checkers) ||
                                   IsGuardedInCondition(binary.RightOperand, checkers);
                        }
                        return false;
                    case OperationKind.PropertyReference:
                        var propertyReference = (IPropertyReferenceOperation)condition;
                        return checkers.Contains(propertyReference.Property);
                    default:
                        return false;
                }
            }

            private List<INamedTypeSymbol> GetCapabilities(ISymbol symbol)
            {
                List<INamedTypeSymbol> result = null;

                while (symbol != null)
                {
                    foreach (var attribute in symbol.GetAttributes())
                    {
                        if (IsCapability(attribute.AttributeClass))
                        {
                            if (result == null)
                                result = new List<INamedTypeSymbol>();

                            result.Add(attribute.AttributeClass);
                        }
                    }

                    symbol = symbol.ContainingSymbol;
                }

                return result;
            }

            private bool IsCapability(ITypeSymbol attributeType)
            {
                while (attributeType != null)
                {
                    if (attributeType.BaseType == _capabilityAttribute)
                        return true;

                    attributeType = attributeType.BaseType;
                }

                return false;
            }

            private List<IPropertySymbol> GetCapabilityCheckers(ITypeSymbol capability)
            {
                List<IPropertySymbol> result = null;

                foreach (var attribute in capability.GetAttributes())
                {
                    if (attribute.AttributeClass == _capabilityCheckAttribute &&
                        attribute.ConstructorArguments.Length == 2 &&
                        attribute.ConstructorArguments[0].Value is ITypeSymbol checkerType &&
                        attribute.ConstructorArguments[1].Value is string propertyName)
                    {
                        var property = checkerType.GetMembers(propertyName).OfType<IPropertySymbol>().FirstOrDefault();
                        if (property != null)
                        {
                            if (result == null)
                                result = new List<IPropertySymbol>();

                            result.Add(property);
                        }
                    }
                }

                return result;
            }
        }
    }
}
