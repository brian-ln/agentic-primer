# UGS State Backup - 2026-01-21 13:33

## Current Architecture (Before Pure Property Migration)
- Special 'type' field at root level
- Properties separate from type  
- Address system supports scopes (user:, project:, scratch:)

## Current Scope Distribution
{"scopes":[{"scope":"default","count":75},{"scope":"scratch","count":15},{"scope":"user","count":9},{"scope":"project","count":7}],"totalScopes":4}

{"scopes":[{"scope":"default","count":75},{"scope":"scratch","count":15},{"scope":"user","count":9},{"scope":"project","count":7}],"totalScopes":4}


## Key Insights Discovered
1. Types should be addressable nodes: @(person_type)
2. Schemas should be addressable nodes: @(person_schema_v1) 
3. Everything should be properties - no special fields
4. Universal addressing: types, schemas, even property keys as @(id)

## Next Steps
1. Flatten node structure - eliminate special 'type' field
2. Make 'type' just another property that references @(type_address)
3. Implement address resolution for property values
4. Test pure property-based model

## Scope Hygiene Established  
- scratch: All demos, experiments, tests, migration tracking
- user: Real people and personal data only
- project: Real work items and project data only
- default: Core system infrastructure only
