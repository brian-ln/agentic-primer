;; Credential Management Protocol
;; Defines a standard interface for retrieving secrets and tokens.

(defprotocol CredentialProvider
  "Interface for a centralized secret store."
  (on GET_CREDENTIALS (service_id)
    (or
      (one CREDENTIALS (credential_type credential)) ; e.g., 'bearer', 'api_key'
      (one ERROR (message)))))

(system Credentials
  (actors
    (actor CredentialProviderActor
      (implements CredentialProvider)
      (state (secrets map)) ; service_id -> secret
      (behavior
        (on get-credentials (service_id)
          (let ((secret (get secrets service_id)))
            (if secret
              (yields credentials secret)
              (yields error "Secret not found"))))))))
