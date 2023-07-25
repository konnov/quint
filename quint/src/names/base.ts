/* ----------------------------------------------------------------------------------
 * Copyright (c) Informal Systems 2022-2023. All rights reserved.
 * Licensed under the Apache 2.0.
 * See License.txt in the project root for license information.
 * --------------------------------------------------------------------------------- */

/**
 * Type definitions and utilities for Quint name resolution.
 *
 * @author Gabriela Moreira
 *
 * @module
 */

import { IdGenerator } from '../idGenerator'
import { Loc } from '../parsing/quintParserFrontend'
import { AnalysisOutput } from '../quintAnalyzer'
import {
  QuintDef,
  QuintEx,
  QuintExport,
  QuintImport,
  QuintInstance,
  QuintLambdaParameter,
  QuintOpDef,
  isFlat,
} from '../quintIr'
import { QuintType, Row } from '../quintTypes'

/**
 * A module's definitions, indexed by name.
 *
 * A definition, in this type, can be hidden, meaning it won't be copied over to
 * a module when an import/intance/export statement is resolved. `hidden` can be
 * removed with `export` statements for the hidden definitions.
 */
export type DefinitionsByName = Map<string, Definition>

export type Definition = (QuintDef | ({ kind: 'param' } & QuintLambdaParameter)) & {
  hidden?: boolean
  namespaces?: string[]
  importedFrom?: QuintImport | QuintInstance | QuintExport
}
/**
 * Definitions for each module
 */
export type DefinitionsByModule = Map<string, DefinitionsByName>

/**
 * A lookup table from IR component ids to their definitions.
 * Should have an entry for every IR component with a name
 * That is:
 * - Name expressions
 * - App expressions (opcode is a name)
 * - Override parameters in instance definitions
 * - Constant types (which are references to type aliases or uninterpreted types)
 *
 * This should be created by `resolveNames` from `resolver.ts`
 */
export type LookupTable = Map<bigint, Definition>

/**
 * Copy the names of a definitions table to a new one, ignoring hidden
 * definitions, and optionally adding a namespace.
 *
 * @param originTable - The definitions table to copy from
 * @param namespace - Optional namespace to be added to copied names
 * @param copyAsHidden - Optional, true iff the copied definitions should be tagged as 'hidden'
 *
 * @returns a definitions table with the filtered and namespaced names
 */
export function copyNames(
  originTable: DefinitionsByName,
  namespace?: string,
  copyAsHidden?: boolean
): DefinitionsByName {
  const table = new Map()

  originTable.forEach((def, identifier) => {
    const name = namespace ? [namespace, identifier].join('::') : identifier
    if (!def.hidden || def.kind === 'const') {
      table.set(name, copyAsHidden ? { ...def, hidden: copyAsHidden } : def)
    }
  })

  return table
}

export function addNamespaceToDef(def: Definition, namespaces: string[]): Definition {
  return namespaces.reduce((def, namespace) => {
    if (
      (def.namespaces && def.namespaces[def.namespaces?.length - 1] === namespace) ||
      ((def.kind === 'param' || isFlat(def)) && def.name.startsWith(namespace))
    ) {
      return def
    }
    const namespaces = namespace ? def.namespaces?.concat([namespace]) ?? [namespace] : []
    return { ...def, namespaces }
  }, def)
}

/**
 * Built-in name definitions that are always resolved and generate conflicts if collected.
 */
export const builtinNames = [
  'not',
  'and',
  'or',
  'iff',
  'implies',
  'exists',
  'forall',
  'in',
  'union',
  'contains',
  'fold',
  'intersect',
  'exclude',
  'subseteq',
  'map',
  'applyTo',
  'filter',
  'powerset',
  'flatten',
  'allLists',
  'chooseSome',
  'oneOf',
  'isFinite',
  'size',
  'get',
  'put',
  'keys',
  'mapBy',
  'setToMap',
  'setOfMaps',
  'set',
  'setBy',
  'fields',
  'with',
  'tuples',
  'append',
  'concat',
  'head',
  'tail',
  'length',
  'nth',
  'indices',
  'replaceAt',
  'slice',
  'select',
  'foldl',
  'foldr',
  'to',
  'always',
  'eventually',
  'next',
  'then',
  'reps',
  'fail',
  'assert',
  'orKeep',
  'mustChange',
  'enabled',
  'weakFair',
  'strongFair',
  'guarantees',
  'existsConst',
  'forallConst',
  'chooseConst',
  'Bool',
  'Int',
  'Nat',
  'Set',
  'Map',
  'List',
  'Tup',
  'Rec',
  'range',
  'igt',
  'ilt',
  'igte',
  'ilte',
  'iadd',
  'isub',
  'iuminus',
  'imul',
  'idiv',
  'imod',
  'ipow',
  'actionAll',
  'actionAny',
  'field',
  'fieldNames',
  'item',
  'unionMatch',
  'assign',
  'of',
  'eq',
  'neq',
  'ite',
  'cross',
  'difference',
]

export class Flatenner {
  private idGenerator?: IdGenerator
  private sourceMap?: Map<bigint, Loc>
  private analysisOutput?: AnalysisOutput
  private newId: boolean = false

  constructor(idGenerator?: IdGenerator, sourceMap?: Map<bigint, Loc>, analysisOutput?: AnalysisOutput) {
    this.idGenerator = idGenerator
    this.sourceMap = sourceMap
    this.analysisOutput = analysisOutput
  }

  addNamespaceToDef(name: string | undefined, def: QuintDef, newId: boolean = false): QuintDef {
    this.newId = newId
    switch (def.kind) {
      case 'def':
        return this.addNamespaceToOpDef(name, def)
      case 'assume':
        return {
          ...def,
          name: this.namespacedName(name, def.name),
          assumption: this.addNamespaceToExpr(name, def.assumption),
          id: this.getNewIdWithSameData(def.id),
        }
      case 'const':
      case 'var':
        return { ...def, name: this.namespacedName(name, def.name), id: this.getNewIdWithSameData(def.id) }
      case 'typedef':
        return {
          ...def,
          name: this.namespacedName(name, def.name),
          type: def.type ? this.addNamespaceToType(name, def.type) : undefined,
          id: this.getNewIdWithSameData(def.id),
        }
      case 'instance':
        return {
          ...def,
          qualifiedName: def.qualifiedName ? this.namespacedName(name, def.qualifiedName) : name,
        }
      case 'import':
        return {
          ...def,
          qualifiedName: def.qualifiedName ? this.namespacedName(name, def.qualifiedName) : name,
          defName: undefined,
        }
      case 'export':
        return def
    }
  }

  private addNamespaceToOpDef(name: string | undefined, opdef: QuintOpDef): QuintOpDef {
    return {
      ...opdef,
      name: this.namespacedName(name, opdef.name),
      expr: this.addNamespaceToExpr(name, opdef.expr),
      id: this.getNewIdWithSameData(opdef.id),
    }
  }

  private addNamespaceToExpr(name: string | undefined, expr: QuintEx, newId: boolean = false): QuintEx {
    const id = this.getNewIdWithSameData(expr.id)

    switch (expr.kind) {
      case 'name':
        if (this.shouldAddNamespace(expr.name)) {
          return { ...expr, name: this.namespacedName(name, expr.name), id }
        }

        return { ...expr, id }
      case 'bool':
      case 'int':
      case 'str':
        return { ...expr, id }
      case 'app': {
        if (this.shouldAddNamespace(expr.opcode)) {
          return {
            ...expr,
            opcode: this.namespacedName(name, expr.opcode),
            args: expr.args.map(arg => this.addNamespaceToExpr(name, arg)),
            id,
          }
        }

        return {
          ...expr,
          args: expr.args.map(arg => this.addNamespaceToExpr(name, arg)),
          id,
        }
      }
      case 'lambda':
        return {
          ...expr,
          params: expr.params.map(param => ({
            name: this.namespacedName(name, param.name),
            id: this.getNewIdWithSameData(param.id),
          })),
          expr: this.addNamespaceToExpr(name, expr.expr),
          id,
        }

      case 'let':
        return {
          ...expr,
          opdef: this.addNamespaceToOpDef(name, expr.opdef),
          expr: this.addNamespaceToExpr(name, expr.expr),
          id,
        }
    }
  }

  private addNamespaceToType(name: string | undefined, type: QuintType): QuintType {
    const id = type.id ? this.getNewIdWithSameData(type.id) : undefined

    switch (type.kind) {
      case 'bool':
      case 'int':
      case 'str':
      case 'var':
        return { ...type, id }
      case 'const':
        return { ...type, name: this.namespacedName(name, type.name), id }
      case 'set':
      case 'list':
        return { ...type, elem: this.addNamespaceToType(name, type.elem), id }
      case 'fun':
        return {
          ...type,
          arg: this.addNamespaceToType(name, type.arg),
          res: this.addNamespaceToType(name, type.res),
          id,
        }
      case 'oper':
        return {
          ...type,
          args: type.args.map(arg => this.addNamespaceToType(name, arg)),
          res: this.addNamespaceToType(name, type.res),
          id,
        }
      case 'tup':
      case 'rec':
        return {
          ...type,
          fields: this.addNamespaceToRow(name, type.fields),
          id,
        }
      case 'union':
        return {
          ...type,
          records: type.records.map(record => {
            return {
              ...record,
              fields: this.addNamespaceToRow(name, record.fields),
            }
          }),
          id,
        }
    }
  }

  private addNamespaceToRow(name: string | undefined, row: Row): Row {
    if (row.kind !== 'row') {
      return row
    }

    return {
      ...row,
      fields: row.fields.map(field => {
        return {
          ...field,
          fieldType: this.addNamespaceToType(name, field.fieldType),
        }
      }),
    }
  }

  private namespacedName(namespace: string | undefined, name: string): string {
    return namespace ? `${namespace}::${name}` : name
  }

  /**
   * Whether a name should be prefixed with the namespace.
   *
   * @param name the name to be prefixed
   *
   * @returns false if the name is on the curentModulesName list, true otherwise
   */
  private shouldAddNamespace(name: string): boolean {
    return !builtinNames.includes(name)
    // if (this.currentModuleNames.has(name)) {
    //   return false
    // }

    // return true
  }

  private getNewIdWithSameData(id: bigint): bigint {
    if (!this.newId) {
      return id
    }
    const newId = this.idGenerator!.nextId()

    const type = this.analysisOutput?.types.get(id)
    const effect = this.analysisOutput?.effects.get(id)
    const mode = this.analysisOutput?.modes.get(id)
    const source = this.sourceMap?.get(id)

    if (type) {
      this.analysisOutput!.types.set(newId, type)
    }
    if (effect) {
      this.analysisOutput!.effects.set(newId, effect)
    }
    if (mode) {
      this.analysisOutput!.modes.set(newId, mode)
    }
    if (source) {
      this.sourceMap!.set(newId, source)
    }

    return newId
  }
}
