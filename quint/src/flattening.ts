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
  QuintExport,
  QuintImport,
  QuintInstance,
  QuintModule,
  QuintName,
  isFlat,
} from './quintIr'
import { Loc, parsePhase3importAndNameResolution, parsePhase4toposort } from './parsing/quintParserFrontend'
import { AnalysisOutput } from './quintAnalyzer'
import { inlineTypeAliases } from './types/aliasInliner'
import { IRVisitor, walkDefinition, walkExpression, walkModule } from './IRVisitor'
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

  const modulesByName = new Map(inlined.modules.map(m => [m.name, m]))
  let flattenedModules: QuintModule[] = []
  let flattenedTable = inlined.table
  const modulesQueue = inlined.modules

  while (modulesQueue.length > 0) {
    const module = modulesQueue.shift()!
    const flattener1 = new FlattenerVisitor(modulesByName, flattenedTable)
    walkModule(flattener1, module)
    module.defs = module.defs.filter(d => d.kind !== 'import' && d.kind !== 'export')

    // ensure uniqueness
    const defs = new Map<string, QuintDef>(module.defs.filter(isFlat).map(d => [d.name, d]))
    module.defs = Array.from(defs.values()).concat(module.defs.filter(d => !isFlat(d)))

    // console.log('--- modules to pre flatten -----')
    // console.log(moduleToString(module))
    // console.log('--- END modules to pre flatten -----')

    const instanceFlattener = new InstanceFlattener(
      modulesByName,
      idGenerator,
      inlined.table,
      sourceMap,
      inlined.analysisOutput
    )
    const instancedModules: QuintModule[] = []
    const newModule = transformModule(instanceFlattener, module)
    instanceFlattener.newModules.forEach(mod => {
      if (!instancedModules.some(m => m.name === mod.name)) {
        instancedModules.push(mod)
      }
    })

    instanceFlattener.newModules = []
    instancedModules.push(newModule)
    const modulesToFlatten = instancedModules
    // ensure uniqueness
    modulesToFlatten.forEach(module => {
      const defs = new Map<string, QuintDef>(module.defs.filter(isFlat).map(d => [d.name, d]))
      module.defs = Array.from(defs.values()).concat(module.defs.filter(d => !isFlat(d)))
    })

    // console.log('--- modules to flatten -----')
    // flattenedModules.concat(modulesToFlatten).forEach(m => console.log(moduleToString(m)))
    // console.log('--- END modules to flatten -----')

    const result = parsePhase3importAndNameResolution({
      modules: flattenedModules.concat(modulesToFlatten),
      sourceMap,
    })

    // inline aliases here

    if (result.isLeft()) {
      flattenedModules.concat(modulesToFlatten).forEach(m => console.log(moduleToString(m)))
      throw new Error('[1] Flattening failed: ' + result.value.map(e => e.explanation))
    }

    const { table: newTable } = result.unwrap()
    flattenedTable = newTable
    // const { table: newModulesTable } = result.unwrap()
    // const newTable = new Map([...newModulesTable, ...table])

    const flattener = new FlattenerVisitor(modulesByName, newTable, true)
    instancedModules.forEach(m => {
      walkModule(flattener, m)
      modulesByName.set(m.name, m)
    })

    // Rename to canonical names
    // const renamedModules = renameToCanonical(newTable, resolvedModules)
    // const renamedModules = preFlattenedModules

    // console.log('--- renamed modules -----')
    // renamedModules.forEach(m => console.log(moduleToString(m)))
    // console.log('--- END renamed modules -----')

    // const result2 = parsePhase4toposort({ modules: preFlattenedModules, table: newTable, sourceMap })
    // // inline aliases later
    // if (result2.isLeft()) throw new Error('Flattening failed: ' + result2.value.map(e => e.explanation))

    // console.log('--- flattened -----')
    // preFlattenedModules.forEach(m => console.log(moduleToString(m)))
    // console.log('--- END flattened -----')

    flattenedModules.push(...instancedModules)
    const toResolve = flattenedModules.concat(modulesQueue)
    // console.log('--- resolving -----')
    // toResolve.forEach(m => console.log(moduleToString(m)))
    // console.log('--- END resolving -----')
    const result3 = parsePhase3importAndNameResolution({
      modules: toResolve,
      sourceMap,
    }).chain(parsePhase4toposort)

    if (result3.isLeft()) {
      toResolve.forEach(m => console.log(moduleToString(m)))
      throw new Error('[2] Flattening failed: ' + result3.value.map(e => e.explanation))
    }

    // console.log('--- ALL flattened -----')
    // flattenedModules.forEach(m => console.log(moduleToString(m)))
    // console.log('--- END ALL flattened -----')
    flattenedModules = result3.unwrap().modules.slice(0, flattenedModules.length)
    flattenedTable = result3.unwrap().table
  }

  return {
    flattenedModules: flattenedModules as FlatModule[],
    flattenedTable,
    flattenedAnalysis: inlined.analysisOutput,
  }
}

class FlattenerVisitor implements IRVisitor {
  defsToAdd: Map<string, QuintDef> = new Map()
  private flattener: Flatenner
  private modulesByName: Map<string, QuintModule>

  private lookupTable: LookupTable
  private namespaceForNested?: string
  private flattenInstances: boolean

  constructor(modulesByName: Map<string, QuintModule>, lookupTable: LookupTable, flattenInstances: boolean = false) {
    this.modulesByName = modulesByName
    this.lookupTable = lookupTable
    this.flattener = new Flatenner()
    this.flattenInstances = flattenInstances
  }

  enterModule(_quintModule: QuintModule) {
    this.defsToAdd = new Map()
  }

  exitModule(quintModule: QuintModule) {
    quintModule.defs.push(...this.defsToAdd.values())
  }

  enterName(name: QuintName) {
    const def = this.lookupTable.get(name.id)
    if (!def || def.kind === 'param' || (def.kind === 'def' && def.depth && def.depth > 0)) {
      return
    }

    if (!isFlat(def) || (def.importedFrom?.kind === 'instance' && !this.flattenInstances)) {
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

    if (!isFlat(def) || (def.importedFrom?.kind === 'instance' && !this.flattenInstances)) {
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

  enterExport(def: QuintExport) {
    if (!this.modulesByName.has(def.protoName)) {
      // FIXME: this error is not reached anymore, find a proper way to report it
      throw new Error(
        `Export '${definitionToString(def)}' does not have a matching import. This is not supported for now`
      )
    }
    const ids = this.modulesByName.get(def.protoName)!.defs.map(d => d.id)
    const definitions: Definition[] = [...this.lookupTable.values()].filter(d => ids.includes(d.id))

    definitions.forEach(d => {
      if (d.kind === 'param') {
        // I think this is impossible
        return
      }
      if (!isFlat(d)) {
        walkDefinition(this, d)
        return
      }
      if (this.defsToAdd.has(d.name)) {
        return
      }

      const namespace = this.namespaceForNested ?? def.defName ? undefined : def.qualifiedName ?? def.protoName
      const old = this.namespaceForNested
      this.namespaceForNested = namespace
      const newDef: Definition =
        namespace && !d.name.startsWith(namespace) ? this.flattener.addNamespaceToDef(namespace, d) : d
      const newDefWithoutMetadata = { ...newDef, importedFrom: undefined, hidden: false, namespaces: [] }
      // console.log(newDefWithoutMetadata)
      walkDefinition(this, newDefWithoutMetadata)
      this.defsToAdd.set((newDef as FlatDef).name, newDefWithoutMetadata)
      this.namespaceForNested = old
    })
  }

  enterInstance(instance: QuintInstance) {
    if (instance.qualifiedName) {
      this.modulesByName.set(instance.qualifiedName, this.modulesByName.get(instance.protoName)!)
    }

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

  enterImport(def: QuintImport) {
    if (def.qualifiedName) {
      this.modulesByName.set(def.qualifiedName, this.modulesByName.get(def.protoName)!)
    }
  }
}

function getNamespaceForDef(def?: Definition): string | undefined {
  // builtin, param, import/export/instance, or not originated from import/instance : do nothing
  if (!def || (def.kind !== 'param' && !isFlat(def)) || !def.namespaces) {
    return
  }
  // console.log(def.name, [...def.namespaces].reverse().join('::'))

  return [...def.namespaces].reverse().join('::')
}

class InstanceFlattener implements IRTransformer {
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

  enterName(expr: QuintName): QuintName {
    const def = this.lookupTable.get(expr.id)
    if (def?.importedFrom?.kind !== 'instance' || (def.kind !== 'param' && !isFlat(def))) {
      return expr
    }

    const namespace = getNamespaceForDef(def)
    return { ...expr, name: compact([namespace, def.name]).join('::') }
  }

  enterApp(expr: QuintApp): QuintApp {
    const def = this.lookupTable.get(expr.id)
    if (def?.importedFrom?.kind !== 'instance' || (def.kind !== 'param' && !isFlat(def))) {
      return expr
    }

    const namespace = getNamespaceForDef(def)

    return { ...expr, opcode: compact([namespace, def.name]).join('::') }
  }

  enterDef(def: QuintDef): QuintDef {
    if (def.kind !== 'instance') {
      return def
    }

    const module = this.modulesByName.get(def.protoName)!
    const newDefs = []
    const newName = [this.currentModuleName!, def.qualifiedName ?? module.name].join('::')
    const flattener = new FlattenerVisitor(this.modulesByName, this.lookupTable)
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
