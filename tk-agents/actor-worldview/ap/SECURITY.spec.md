# Specification: Capability-Based Security

## 1. The Principle of Least Privilege
No actor has inherent permissions. Every restricted action (e.g., writing to a file, executing a shell command) requires a **Capability Token** (CT).

## 2. Capability Tokens (CT)
A CT is a signed, attenuated object (similar to a Macaroon) that contains:
- **Resource:** The target URI (e.g., `fs:///src/main.ts`).
- **Action:** The permitted operation (e.g., `read`, `patch`, `execute`).
- **Caveats:** Expiration, source IP restrictions, or "Only if node type is X".

## 3. Token Handover (The "Letter of Introduction")
When Actor A sends a message to Actor B that requires an external effect:
1. Actor A attaches the CT to the message metadata.
2. Actor B receives the message and the CT.
3. If Actor B needs to call a `ShellNode`, it passes the CT forward.
4. The `ShellNode` (Effect Actor) validates the signature and caveats before executing.

## 4. Attenuation
An actor can create a **weaker** version of its own token to pass to a sub-agent.
- **Example:** I have a token for `fs:///src/`. I give the `LinterAgent` a token restricted to `fs:///src/*.ts` and action `read-only`.

## 5. Datalog Validation
The security rules are defined in `ap/INTERFACES.rules.datalog`.

```datalog
authorized(sender_id, action, resource) :-
    has_token(sender_id, token_id),
    token_spec{id: token_id, action: action, resource: resource, active: true}
```
