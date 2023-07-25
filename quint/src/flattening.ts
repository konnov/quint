/* ----------------------------------------------------------------------------------
 * Copyright (c) Informal Systems 2023. All rights reserved.
 * Licensed under the Apache 2.0.
 * See License.txt in the project root for license information.
 * --------------------------------------------------------------------------------- */

/**
 * Flattening for modules, replacing instances and (soon) imports with their definitions.
 *
 * @author Gabriela Moreira
 *
 * @module
 */

import { IdGenerator } from './idGenerator'
import { Definition, Flatenner, LookupTable } from './names/base'
import {
  FlatDef,
  FlatModule,
  QuintApp,
  QuintDef,
  QuintImport,
  QuintInstance,
  QuintModule,
  QuintName,
  isFlat,
} from './quintIr'
import { Loc, parsePhase3importAndNameResolution, parsePhase4toposort } from './parsing/quintParserFrontend'
import { AnalysisOutput } from './quintAnalyzer'
import { inlineAliasesInDef, inlineAnalysisOutput, inlineTypeAliases } from './types/aliasInliner'
import { IRVisitor, walkDefinition, walkExpression, walkModule } from './IRVisitor'
import { CallGraphVisitor, mkCallGraphContext } from './static/callgraph'
import { toposort } from './static/toposort'
import { IRTransformer, transformDefinition, transformModule } from './IRTransformer'
import { compact } from 'lodash'
import { definitionToString, moduleToString } from './IRprinting'

/**
 * Flatten an array of modules, replacing instances, imports and exports with
 * their definitions and inlining type aliases.
 *
 * @param modules The modules to flatten
 * @param table The lookup table to for all referred names
 * @param idGenerator The id generator to use for new definitions; should be the same as used for parsing
 * @param sourceMap The source map for all modules involved
 * @param analysisOutput The analysis output for all modules involved
 *
 * @returns An object containing the flattened modules, flattened lookup table and flattened analysis output
 */
export function flattenModules(
  modules: QuintModule[],
  table: LookupTable,
  idGenerator: IdGenerator,
  sourceMap: Map<bigint, Loc>,
  analysisOutput: AnalysisOutput
): { flattenedModules: FlatModule[]; flattenedTable: LookupTable; flattenedAnalysis: AnalysisOutput } {
  // FIXME: use copies of parameters so the original objects are not mutated.
  // This is not a problem atm, but might be in the future.

  // Inline type aliases
  const inlined = inlineTypeAliases(modules, table, analysisOutput)

  // console.log('--- inlined -----')
  // inlined.modules.forEach(m => console.log(moduleToString(m)))
  // console.log('--- END inlined -----')

  const flattener1 = new FlattenerVisitor(inlined.table)
  inlined.modules.forEach(m => {
    walkModule(flattener1, m)
    m.defs = m.defs.filter(d => d.kind !== 'import' && d.kind !== 'export')
  })

  // remove duplicates, probably don't need toposorting here
  // const renamedModules = parsePhase4toposort({ modules: inlined.modules, table: inlined.table, sourceMap }).unwrap()
  // .modules
  const renamedModules = inlined.modules.map(module => {
    // remove duplicates
    const defs = new Map<string, QuintDef>(module.defs.filter(isFlat).map(d => [d.name, d]))
    return { ...module, defs: Array.from(defs.values()).concat(module.defs.filter(d => !isFlat(d))) }
  })
  // const renamedModules = renameToCanonical(table, inlined.modules)

  console.log('--- modules to pre flatten -----')
  renamedModules.forEach(m => console.log(moduleToString(m)))
  console.log('--- END modules to pre flatten -----')

  const preFlattener = new PreFlattener(
    new Map(renamedModules.map(m => [m.name, m])),
    idGenerator,
    inlined.table,
    sourceMap,
    inlined.analysisOutput
  )
  const preFlattenedModules: QuintModule[] = []
  renamedModules.forEach(module => {
    const newModule = transformModule(preFlattener, module)
    preFlattener.newModules.forEach(mod => {
      if (!preFlattenedModules.some(m => m.name === mod.name)) {
        preFlattenedModules.push(mod)
      }
    })

    preFlattener.newModules = []
    preFlattenedModules.push(newModule)
  })
  const modulesToFlatten = preFlattenedModules

  console.log('--- modules to flatten -----')
  modulesToFlatten.forEach(m => console.log(moduleToString(m)))
  console.log('--- END modules to flatten -----')

  const result = parsePhase3importAndNameResolution({
    modules: modulesToFlatten,
    sourceMap,
  })

  // inline aliases here

  if (result.isLeft()) {
    throw new Error('Flattening failed: ' + result.value.map(e => e.explanation))
  }

  const { table: newTable } = result.unwrap()
  // const { table: newModulesTable } = result.unwrap()
  // const newTable = new Map([...newModulesTable, ...table])

  const flattener = new FlattenerVisitor(newTable, true)
  preFlattenedModules.forEach(m => {
    walkModule(flattener, m)
  })

  // Rename to canonical names
  // const renamedModules = renameToCanonical(newTable, resolvedModules)
  // const renamedModules = preFlattenedModules

  // console.log('--- renamed modules -----')
  // renamedModules.forEach(m => console.log(moduleToString(m)))
  // console.log('--- END renamed modules -----')

  const result2 = parsePhase4toposort({ modules: preFlattenedModules, table: newTable, sourceMap })
  // inline aliases later
  if (result2.isLeft()) throw new Error('Flattening failed: ' + result2.value.map(e => e.explanation))

  console.log('--- flattened -----')
  result2.unwrap().modules.forEach(m => console.log(moduleToString(m)))
  console.log('--- END flattened -----')

  return {
    flattenedModules: result2.unwrap().modules as FlatModule[],
    flattenedTable: result2.unwrap().table,
    flattenedAnalysis: inlined.analysisOutput,
  }
}

/**
 * Adds a definition to a flat module, flattening any instances, imports and
 * exports in the process. Note that the provided parameters should include
 * information for the new definition already, i.e. the lookup table should
 * already contain all the references for names in the definition.
 *
 * @param modules The modules possibly reffered by the definition
 * @param table The lookup table to for all referred names
 * @param idGenerator The id generator to use for new definitions
 * @param sourceMap The source map for all modules involved
 * @param analysisOutput The analysis output for all modules involved
 * @param module The flat module to add the definition to
 * @param def The definition to add
 *
 * @returns An object containing the flattened module, flattened lookup table
 * and flattened analysis output
 */
export function addDefToFlatModule(
  modules: QuintModule[],
  table: LookupTable,
  idGenerator: IdGenerator,
  sourceMap: Map<bigint, Loc>,
  analysisOutput: AnalysisOutput,
  module: FlatModule,
  def: QuintDef
): {
  flattenedModule: FlatModule
  flattenedDefs: FlatDef[]
  flattenedTable: LookupTable
  flattenedAnalysis: AnalysisOutput
} {
  // modules.forEach(m => console.log(moduleToString(m)))
  const flattener1 = new FlattenerVisitor(table)
  modules.forEach(m => {
    walkModule(flattener1, m)
    m.defs = m.defs.filter(d => d.kind !== 'import' && d.kind !== 'export')
    const defs = new Map<string, QuintDef>(m.defs.filter(isFlat).map(d => [d.name, d]))
    m.defs = Array.from(defs.values()).concat(m.defs.filter(d => !isFlat(d)))
  })
  const preFlattener = new PreFlattener(
    new Map(modules.map(m => [m.name, m])),
    idGenerator,
    table,
    sourceMap,
    analysisOutput
  )
  const preFlattenedModules: QuintModule[] = modules
  preFlattener.currentModuleName = module.name
  const preFlattenedDef = transformDefinition(preFlattener, def)
  preFlattener.newModules.forEach(mod => {
    if (!preFlattenedModules.some(m => m.name === mod.name)) {
      preFlattenedModules.push(mod)
    }
  })

  preFlattener.newModules = []
  const modulesToFlatten = preFlattenedModules

  modulesToFlatten.pop()
  modulesToFlatten.push({ ...module, defs: [...module.defs, preFlattenedDef] })
  // console.log('--- [INC] modules to flatten -----')
  // modulesToFlatten.forEach(m => console.log(moduleToString(m)))
  // console.log('--- [INC] END modules to flatten -----')
  let newTable: LookupTable
  if (isFlat(preFlattenedDef)) {
    newTable = table
  } else {
    const result = parsePhase3importAndNameResolution({
      modules: modulesToFlatten,
      sourceMap,
    })

    if (result.isLeft()) {
      throw new Error('Flattening failed: ' + result.value.map(e => e.explanation))
    }

    // inline aliases later

    newTable = result.unwrap().table
  }

  const flattener = new FlattenerVisitor(newTable, true)
  walkDefinition(flattener, preFlattenedDef)
  const flattenedDefs = Array.from(flattener.defsToAdd.values())
    .concat(preFlattenedDef)
    .map(d => inlineAliasesInDef(d, table) as FlatDef)

  const flattenedModule = { ...module, defs: [...module.defs, ...flattenedDefs] }

  // const result2 = parsePhase4toposort(result.unwrap())
  // if (result2.isLeft()) throw new Error('Flattening failed: ' + result2.value.map(e => e.explanation))

  // result2.unwrap().modules.forEach(m => console.log(moduleToString(m)))

  const context = mkCallGraphContext([flattenedModule])
  const visitor = new CallGraphVisitor(table, context)
  walkModule(visitor, flattenedModule)
  const sortedDefs = toposort(visitor.graph, flattenedDefs)
  console.log(sortedDefs.unwrap().map(d => definitionToString(d)))
  console.log(moduleToString({ ...module, defs: [...module.defs, ...sortedDefs.unwrap()] }))

  return {
    flattenedModule: { ...module, defs: [...module.defs, ...sortedDefs.unwrap()] } as FlatModule,
    flattenedDefs: sortedDefs.unwrap(),
    flattenedTable: newTable,
    flattenedAnalysis: inlineAnalysisOutput(analysisOutput, newTable),
  }
}

class FlattenerVisitor implements IRVisitor {
  defsToAdd: Map<string, QuintDef> = new Map()
  private flattener: Flatenner

  private lookupTable: LookupTable
  private currentModuleName?: string
  private namespaceForNested?: string
  private flattenInstances: boolean

  // private getNamespaceForDef(name: string): string | undefined {
  //   return name.split('::').slice(0, -1).join('::')
  // }

  constructor(lookupTable: LookupTable, flattenInstances: boolean = false) {
    this.lookupTable = lookupTable
    this.flattener = new Flatenner()
    this.flattenInstances = flattenInstances
  }

  enterModule(quintModule: QuintModule) {
    this.defsToAdd = new Map()
    this.currentModuleName = quintModule.name
  }

  exitModule(quintModule: QuintModule) {
    quintModule.defs.push(...this.defsToAdd.values())
    // quintModule.defs.push(...[...this.defsToAdd].filter(d => d.kind !== 'const' && isFlat(d)))
    // quintModule.defs = quintModule.defs.filter(isFlat)
  }

  enterName(name: QuintName) {
    const def = this.lookupTable.get(name.id)
    if (!def || def.kind === 'param' || (def.kind === 'def' && def.depth && def.depth > 0)) {
      return
    }

    if (
      !isFlat(def) ||
      (def.importedFrom?.kind === 'instance' && !this.flattenInstances) //||
      // (def.importedFrom?.kind !== 'instance' && this.flattenInstances)
    ) {
      return
    }

    const namespace = this.namespaceForNested ?? getNamespaceForDef(def)

    const newDef = (
      namespace && !def.name.startsWith(namespace) ? this.flattener.addNamespaceToDef(namespace, def) : def
    ) as FlatDef
    this.defsToAdd.set(newDef.name, newDef)

    const old = this.namespaceForNested
    this.namespaceForNested = namespace
    walkDefinition(this, newDef)
    this.namespaceForNested = old
  }

  enterApp(expr: QuintApp) {
    const def = this.lookupTable.get(expr.id)
    if (!def || def.kind === 'param' || (def.kind === 'def' && def.depth && def.depth > 0)) {
      return
    }

    // const namespace = this.namespaceForNested ?? this.getNamespaceForDef(expr.opcode)
    // const newDef = def

    if (
      !isFlat(def) ||
      (def.importedFrom?.kind === 'instance' && !this.flattenInstances) // ||
      // (def.importedFrom?.kind !== 'instance' && this.flattenInstances)
    ) {
      return
    }

    const namespace = this.namespaceForNested ?? getNamespaceForDef(def)
    const newDef = (
      namespace && !def.name.startsWith(namespace) ? this.flattener.addNamespaceToDef(namespace, def) : def
    ) as FlatDef
    this.defsToAdd.set(newDef.name, newDef)

    const old = this.namespaceForNested
    this.namespaceForNested = namespace
    walkDefinition(this, newDef)
    this.namespaceForNested = old
  }

  enterInstance(instance: QuintInstance) {
    if (!this.flattenInstances) {
      return
    }

    instance.overrides.forEach(([param, _]) => {
      const def = this.lookupTable.get(param.id)
      if (!def || def.kind === 'param') {
        return
      }
      if (!isFlat(def)) {
        return
      }
      this.defsToAdd.set(def.name, def)
    })
  }
}

function getNamespaceForDef(def?: Definition): string | undefined {
  // builtin, param, import/export/instance, or not originated from import/instance : do nothing
  if (!def || def.kind === 'param' || !isFlat(def) || !def.namespaces) {
    return
  }
  console.log(def.name, [...def.namespaces].reverse().join('::'))

  return [...def.namespaces].reverse().join('::')
}

class PreFlattener implements IRTransformer {
  private modulesByName: Map<string, QuintModule>
  private flattener: Flatenner
  private lookupTable: LookupTable
  currentModuleName?: string
  newModules: QuintModule[] = []

  constructor(
    modulesByName: Map<string, QuintModule>,
    idGenerator: IdGenerator,
    lookupTable: LookupTable,
    sourceMap: Map<bigint, Loc>,
    analysisOutput: AnalysisOutput
  ) {
    this.modulesByName = modulesByName
    this.flattener = new Flatenner(idGenerator, sourceMap, analysisOutput)
    this.lookupTable = lookupTable
  }

  enterModule(quintModule: QuintModule): QuintModule {
    this.currentModuleName = quintModule.name
    return quintModule
  }

  // private getNamespaceForDef(def?: Definition): string | undefined {
  //   if (def && def.kind === 'def' && def.importedFrom) {
  //     const a =
  //       def.importedFrom.kind === 'instance'
  //         ? compact([
  //             this.currentModuleName,
  //             def.importedFrom.qualifiedName ? undefined : def.importedFrom.protoName,
  //           ]).join('::')
  //         : // : def.importedFrom.qualifiedName
  //           // ? this.currentModuleName
  //           undefined //def.importedFrom.protoName
  //     console.log('namespace', a)
  //   }
  //   // builtin, param, import/export/instance, or not originated from import/instance : do nothing
  //   if (!def || def.kind === 'param' || !isFlat(def) || !def.importedFrom) {
  //     return
  //   }

  //   return def.importedFrom.kind === 'instance'
  //     ? compact([this.currentModuleName, def.importedFrom.qualifiedName ? undefined : def.importedFrom.protoName]).join(
  //         '::'
  //       )
  //     : // : def.importedFrom.qualifiedName
  //       // ? this.currentModuleName
  //       undefined //def.importedFrom.protoName
  // }

  enterName(expr: QuintName): QuintName {
    const def = this.lookupTable.get(expr.id)
    if (def?.importedFrom?.kind !== 'instance' || (def.kind !== 'param' && !isFlat(def))) {
      return expr
    }
    console.log(expr.name)
    const namespace = getNamespaceForDef(def)

    return { ...expr, name: compact([namespace, def.name]).join('::') }
  }

  enterApp(expr: QuintApp): QuintApp {
    const def = this.lookupTable.get(expr.id)
    if (def?.importedFrom?.kind !== 'instance' || (def.kind !== 'param' && !isFlat(def))) {
      return expr
    }
    console.log(expr.opcode)
    const namespace = getNamespaceForDef(def)

    return { ...expr, opcode: compact([namespace, def.name]).join('::') }
  }

  // enterConstType(type: QuintConstType): QuintConstType {
  //   if (!type.id) {
  //     return type
  //   }
  //   const def = this.lookupTable.get(type.id)!
  //   const namespace = this.getNamespaceForDef(def)

  //   return { ...type, name: compact([namespace, type.name]).join('::') }
  // }

  enterDef(def: QuintDef): QuintDef {
    if (def.kind !== 'instance') {
      return def
    }

    const module = this.modulesByName.get(def.protoName)!
    const newDefs = []
    const newName = [this.currentModuleName!, def.qualifiedName ?? module.name].join('::')
    const flattener = new FlattenerVisitor(this.lookupTable)
    def.overrides.forEach(([param, ex]) => {
      walkExpression(flattener, ex)
      if (ex.kind === 'name' && ex.name === param.name) {
        // prevent cycles from defs like `import A(x = x) ...`
        return
      }
      newDefs.push({
        kind: 'def',
        qualifier: 'pureval',
        expr: ex,
        id: param.id,
        name: [newName, param.name].join('::'),
      })
    })

    newDefs.push(...flattener.defsToAdd.values())

    newDefs.push(
      ...module.defs.filter(d => d.kind !== 'const').map(d => this.flattener.addNamespaceToDef(newName, d, true))
    )
    this.newModules.push({ ...module, defs: newDefs.map(d => transformDefinition(this, d)), name: newName })

    const r: QuintImport = {
      kind: 'import',
      id: def.id,
      protoName: newName,
      qualifiedName: undefined,
      defName: '*',
    }
    return r
  }
}
