import { match, P } from "ts-pattern";
import { z } from "zod";
import { filterNull, mapCollectResult, mapValues } from "@/utils";
import { APIResult, Err, Ok, Result, fetchFromAPI } from "@/services";
import { NixValue, nixValueSchema, Repo, EncryptedSecret } from "./nixValue";

export type { Repo, EncryptedSecret };

export type ModuleSchemaType =
  | { tag: "encryptedSecret" }
  | { tag: "bool" }
  | { tag: "path" }
  | { tag: "str" }
  | { tag: "nonEmptyStr" }
  | { tag: "unsignedInt16" }
  | { tag: "int" }
  | { tag: "enum"; variants: Array<string> }
  | { tag: "package" }
  | { tag: "submodule"; fields: Record<string, ModuleSchema> }
  | { tag: "attrsOf"; fieldType: ModuleSchemaType }
  | { tag: "listOf"; elementType: ModuleSchemaType }
  | { tag: "nullOr"; innerType: ModuleSchemaType };

const moduleSchemaTypeSchema: z.ZodType<ModuleSchemaType> =
  z.discriminatedUnion("tag", [
    z.object({ tag: z.literal("encryptedSecret") }),
    z.object({ tag: z.literal("bool") }),
    z.object({ tag: z.literal("path") }),
    z.object({ tag: z.literal("str") }),
    z.object({ tag: z.literal("nonEmptyStr") }),
    z.object({ tag: z.literal("unsignedInt16") }),
    z.object({ tag: z.literal("int") }),
    z.object({ tag: z.literal("enum"), variants: z.array(z.string()) }),
    z.object({ tag: z.literal("package") }),
    z.object({
      tag: z.literal("submodule"),
      fields: z.record(
        z.string(),
        z.lazy(() => moduleSchemaSchema),
      ),
    }),
    z.object({
      tag: z.literal("attrsOf"),
      fieldType: z.lazy(() => moduleSchemaTypeSchema),
    }),
    z.object({
      tag: z.literal("listOf"),
      elementType: z.lazy(() => moduleSchemaTypeSchema),
    }),
    z.object({
      tag: z.literal("nullOr"),
      innerType: z.lazy(() => moduleSchemaTypeSchema),
    }),
  ]);

export type ModuleSchema = {
  description?: string | null;
  example?: string | null;
  default?: NixValue | null;
  name?: string | null;
  typ: ModuleSchemaType;
};

export const moduleSchemaSchema: z.ZodType<ModuleSchema> = z.object({
  description: z.string().optional(),
  example: z.string().optional(),
  default: nixValueSchema.optional(),
  name: z.string().optional(),
  typ: moduleSchemaTypeSchema,
});

export function initialValueForSchema(m: ModuleSchemaType): NixValue {
  return match(m)
    .with({ tag: "encryptedSecret" }, () => ({
      tag: "encryptedSecret" as const,
      value: null,
    }))
    .with({ tag: "bool" }, () => ({ tag: "bool" as const, value: false }))
    .with({ tag: "path" }, () => ({ tag: "path" as const, value: "./." }))
    .with({ tag: "str" }, () => ({ tag: "string" as const, value: "" }))
    .with({ tag: "nonEmptyStr" }, () => ({ tag: "string" as const, value: "" }))
    .with({ tag: "unsignedInt16" }, () => ({ tag: "int" as const, value: 0 }))
    .with({ tag: "int" }, () => ({ tag: "int" as const, value: 0 }))
    .with({ tag: "enum" }, () => ({ tag: "string" as const, value: "" }))
    .with({ tag: "package" }, () => ({ tag: "raw" as const, value: "" }))
    .with({ tag: "attrsOf" }, () => ({ tag: "set" as const, value: {} }))
    .with({ tag: "listOf" }, () => ({ tag: "list" as const, value: [] }))
    .with({ tag: "submodule" }, (m) => ({
      tag: "set" as const,
      value: mapValues((schema) => {
        if (schema.default != null) return schema.default;
        return initialValueForSchema(schema.typ);
      }, m.fields),
    }))
    .with({ tag: "nullOr" }, () => ({ tag: "null" as const }))
    .exhaustive();
}

export type ModuleFormConfig = {
  stackName: string;
  gitCommit: string;
  projectName: string;
  moduleSchema: ModuleSchemaType;
  initialValue: NixValue;
};

// We assume that modules have a an `submodule` at the toplevel, with a single
// field that is a `attrsOf`.
export const peelOffToplevel = (
  toplevelSchema: ModuleSchema,
  savedValue: NixValue | null,
  commit: string,
): Result<ModuleFormConfig> => {
  const peelOffResult = peelOffToplevelSchema(toplevelSchema.typ);
  if (!peelOffResult.ok) return peelOffResult;
  let initialValue;
  if (savedValue != null) {
    const result = peelOffToplevelNixValue({
      stackName: peelOffResult.data.stackName,
      projectName: peelOffResult.data.projectName,
      value: savedValue,
    });
    if (!result.ok) return result;
    initialValue = result.data.moduleValue;
  } else {
    initialValue = initialValueForSchema(peelOffResult.data.moduleSchema);
  }
  return Ok({
    stackName: peelOffResult.data.stackName,
    gitCommit: commit,
    projectName: peelOffResult.data.projectName,
    moduleSchema: peelOffResult.data.moduleSchema,
    initialValue,
  });
};

const peelOffToplevelSchema = (
  schema: ModuleSchemaType,
): Result<{
  stackName: string;
  projectName: string;
  moduleSchema: Extract<ModuleSchemaType, { tag: "submodule" }>;
}> => {
  if (schema.tag != "submodule")
    return Err({
      message: 'toplevel must be of type "submodule", found ' + schema.tag,
    });
  const toplevelFields = Object.entries(schema.fields);
  if (toplevelFields.length != 1)
    return Err({
      message:
        `must have one toplevel field, has: ` +
        toplevelFields.map((x) => x[0]).join(", "),
    });
  const [stackName, attrsOf] = toplevelFields[0]!;
  if (attrsOf.typ.tag != "attrsOf")
    return Err({
      message: 'first level must be an "attrsOf", found: ' + attrsOf.typ.tag,
    });
  if (attrsOf.typ.fieldType.tag != "submodule")
    return Err({
      message:
        "second level must be a submodule, found: " + attrsOf.typ.fieldType.tag,
    });
  return Ok({
    stackName,
    projectName: `${stackName}-project`,
    moduleSchema: attrsOf.typ.fieldType,
  });
};

const peelOffToplevelNixValue = (args: {
  stackName: string;
  projectName: string;
  value: NixValue;
}): Result<{
  stackName: string;
  projectName: string;
  moduleValue: NixValue;
}> => {
  if (args.value.tag != "set")
    return Err({
      message: 'toplevel must be of type "set", found ' + args.value.tag,
    });
  const toplevelFields = Object.entries(args.value.value);
  if (toplevelFields.length != 1)
    return Err({
      message:
        `must have one toplevel field, has: ` +
        toplevelFields.map((x) => x[0]).join(", "),
    });
  const [stackName, attrsOf] = toplevelFields[0]!;
  if (args.stackName !== stackName)
    return Err({
      message: `stackNames don't match: ${args.stackName} != ${stackName}`,
    });
  if (attrsOf.tag != "set")
    return Err({
      message: 'first level must be an "set", found: ' + attrsOf.tag,
    });
  const attrsOfFields = Object.entries(attrsOf.value);
  if (attrsOfFields.length != 1)
    return Err({
      message:
        `must have one attrsOf field, has: ` +
        attrsOfFields.map((x) => x[0]).join(", "),
    });
  const [projectName, moduleValue] = attrsOfFields[0]!;
  if (args.projectName !== projectName)
    return Err({
      message: `projectNames don't match: ${args.projectName} != ${projectName}`,
    });
  return Ok({
    stackName,
    projectName,
    moduleValue,
  });
};

export type ValidationError = { message: string; path: Array<string> };

type ValidationResult = Result<null, ValidationError>;

export const validate = (
  {
    moduleSchema,
    moduleValue,
  }: {
    moduleSchema: ModuleSchemaType;
    moduleValue: NixValue;
  },
  path: Array<string> = [],
): ValidationResult => {
  return match(moduleSchema)
    .with(
      { tag: "submodule", fields: P.select() },
      (fields): ValidationResult => {
        if (moduleValue.tag !== "set")
          return Err(valueSchemaMismatchError(path, "submodule", moduleValue));
        const result = mapCollectResult(
          ([fieldName, moduleSchema]): ValidationResult => {
            const fieldValue = moduleValue.value[fieldName];
            if (fieldValue == null) {
              const fullPath = [...path, fieldName];
              return Err({
                message: `Missing submodule field: ${fullPath.join(".")}`,
                path: fullPath,
              });
            }
            return validate(
              {
                moduleValue: fieldValue,
                moduleSchema: moduleSchema.typ,
              },
              [...path, fieldName],
            );
          },
          Object.entries(fields),
        );
        if (!result.ok) return result;
        return Ok(null);
      },
    )
    .with(
      { tag: "nullOr", innerType: P.select() },
      (inner): ValidationResult => {
        if (moduleValue.tag === "null") return Ok(null);
        return validate(
          {
            moduleSchema: inner,
            moduleValue,
          },
          path,
        );
      },
    )
    .with({ tag: "nonEmptyStr" }, (): ValidationResult => {
      if (moduleValue.tag !== "string")
        return Err(valueSchemaMismatchError(path, "nonEmptyStr", moduleValue));
      if (moduleValue.value === "")
        return Err({ message: "Field cannot be empty.", path });
      return Ok(null);
    })
    .with({ tag: "path" }, (): ValidationResult => {
      if (moduleValue.tag !== "path")
        return Err(valueSchemaMismatchError(path, "path", moduleValue));
      if (moduleValue.value === "./")
        return Err({
          message: "Path cannot be empty.",
          path,
        });
      if (moduleValue.value.match(/[^a-zA-Z0-9/.-]/))
        return Err({
          message: "Path cannot contain illegal characters.",
          path,
        });
      return Ok(null);
    })
    .with({ tag: "str" }, () => {
      if (moduleValue.tag !== "string") {
        return Err(valueSchemaMismatchError(path, "str", moduleValue));
      }
      return Ok(null);
    })
    .with({ tag: "unsignedInt16" }, (): ValidationResult => {
      if (moduleValue.tag !== "int")
        return Err(
          valueSchemaMismatchError(path, "unsignedInt16", moduleValue),
        );
      if (moduleValue.value < 0)
        return Err({ message: "value must be positive", path });
      if (moduleValue.value >= 65536)
        return Err({ message: "value must be below 65536", path });
      return Ok(null);
    })
    .otherwise(() => Ok(null));
};

const valueSchemaMismatchError = (
  path: Array<string>,
  schemaTag: string,
  moduleValue: NixValue,
): ValidationError => {
  const value: unknown = match(moduleValue)
    .with({ tag: "null" }, () => null)
    .otherwise((value) => value.value);
  return {
    message: `Value does not match schema: expected type: ${schemaTag}, got: ${value} of type ${moduleValue.tag}`,
    path,
  };
};

export const wrapInToplevel = (args: {
  stackName: string;
  projectName: string;
  value: NixValue;
}): NixValue => {
  return {
    tag: "set",
    value: {
      [args.stackName]: {
        tag: "set",
        value: {
          [args.projectName]: args.value,
        },
      },
    },
  };
};

// * loading available modules

const moduleSchema = z.object({
  name: z.string(),
  repo_user: z.string(),
  repo_name: z.string(),
  git_commit: z.string(),
  schema: moduleSchemaSchema,
  description: z.union([z.string(), z.null()]),
});

const availableModulesReplySchema = z.object({
  modules: z.array(moduleSchema),
});

export type AvailableModulesReply = z.infer<typeof availableModulesReplySchema>;

export const toModuleRecord = (
  modules: Array<Module>,
): Record<string, Module> => {
  const record: Record<string, Module> = {};
  modules.sort((a, b) => a.name.localeCompare(b.name));
  for (const mod of modules) {
    record[mod.name] = mod;
  }
  return record;
};

export const getAvailableModules = async (): Promise<
  Result<Record<string, Module>>
> => {
  const result = await fetchFromAPI(
    availableModulesReplySchema,
    "GET",
    "modules/available",
  );
  if (!result.ok) return result;
  return Ok(toModuleRecord(result.data.modules));
};

// * loading and uploading module configs

const moduleValueSchema = z.object({
  module_name: z.string(),
  git_commit: z.string(),
  values: nixValueSchema,
});

export type Module = z.infer<typeof moduleSchema>;

export type ModuleValue = z.infer<typeof moduleValueSchema>;

const getRepoAndModuleValuesSchema = z.object({
  repo_user: z.string(),
  repo_name: z.string(),
  user_config: z.array(moduleValueSchema),
  modules: z.array(moduleSchema),
});

export type GetRepoAndModuleValues = z.infer<
  typeof getRepoAndModuleValuesSchema
>;

export type UpdateRepoAndModuleValues = {
  repo_user: string;
  repo_name: string;
  user_config: Array<ModuleValue>;
};

export const getModuleConfig = async (): Promise<
  Result<GetRepoAndModuleValues | null>
> => {
  const result = await fetchFromAPI(
    getRepoAndModuleValuesSchema,
    "GET",
    "modules",
  );
  if (!result.ok && result.error.status === 404) return Ok(null);
  return result;
};

export const uploadModuleConfigs = async (
  repoSelection: Repo | null,
  formConfigs: Record<string, ModuleFormConfig>,
  moduleNixValues: Record<string, NixValue>,
): Promise<Result<null>> => {
  if (repoSelection == null) {
    return Err({ message: "Please, pick a github repository at the top." });
  }
  const modules: Result<Array<null | ModuleValue>> = mapCollectResult(
    ([moduleName, moduleValue]): Result<null | ModuleValue> => {
      const moduleFormConfig = formConfigs[moduleName];
      if (moduleFormConfig == null) return Ok(null);
      const validationResult = validate({
        moduleSchema: moduleFormConfig.moduleSchema,
        moduleValue,
      });
      if (!validationResult.ok)
        return Err({
          message: `Errors in the module configuration for ${moduleName}: ${validationResult.error.path.join(
            ".",
          )}: ${validationResult.error.message}`,
        });
      return Ok({
        module_name: moduleName,
        git_commit: moduleFormConfig.gitCommit,
        values: wrapInToplevel({
          stackName: moduleFormConfig.stackName,
          projectName: moduleFormConfig.projectName,
          value: moduleValue,
        }),
      });
    },
    Object.entries(moduleNixValues),
  );
  if (!modules.ok) return modules;
  const config = Ok({
    repo_user: repoSelection.repoUser,
    repo_name: repoSelection.repoName,
    user_config: filterNull(modules.data),
  });
  const result = await putModuleConfig(config.data);
  if (!result.ok) return result;
  return Ok(null);
};

const putModuleConfig = async (
  userRepo: UpdateRepoAndModuleValues,
): Promise<APIResult<null>> => {
  const result = await fetchFromAPI(z.unknown(), "PUT", "modules", {
    body: JSON.stringify(userRepo),
  });
  if (!result.ok) return result;
  return Ok(null);
};

// * building

export const buildModule = async (): Promise<
  APIResult<{ commit: string; branch?: string }>
> => {
  return await fetchFromAPI(
    z.object({
      commit: z.string(),
      branch: z.optional(z.string()),
    }),
    "POST",
    "modules/run",
  );
};

// * opening a pull request

export const openPullRequest = async (): Promise<
  APIResult<{ url: string }>
> => {
  return await fetchFromAPI(
    z.object({
      url: z.string(),
    }),
    "POST",
    "modules/pull-request",
  );
};

// * resetting

export const resetModule = async () => {
  const result = await fetchFromAPI(z.unknown(), "POST", "modules/reset");
  if (!result.ok) return result;
  return Ok(null);
};
